library(tidyverse)
library(ISOweek)

source('codes/functions_tibble_to_list.R')
source('codes/functions_fix_special_characters.R')
source('codes/functions_Lambda_and_R0.R')

# BRAZIL ONLY ------------------------------------------------------------
summarize_outbreaks <- function(df, casesCol){
  # fix spelling issues associated with translation
  df$Admin_unit <- replace_spanish_characters(x = df$Admin_unit)
  
  # split data to list
  df_list <- split_tibble(df, 'Admin_unit')
  
  # determine disease
  if(casesCol == 'Dengue_cases'){
    diseaseName = 'dengue'
  } else {
    diseaseName = 'malaria'
  }
  
  # calculate outbreak information for each location
  df_summary <- lapply(
    df_list
    , function(x)
      summarise_outbreaks(
        df = x
        , cases_colname = casesCol
        , date_colname = 'Date'
        , disease = diseaseName
      )
  )
  
  # Remove null dataframes from list, function from tidyverse
  df_summary <- compact(df_summary)

  # Add admin unit names
  df_summary <- Map(cbind, df_summary, 'Admin_unit' = names(df_summary))
  
  # return 
  df_summary
  
}

summarize_trends <- function(df){
  # calculate trends through time by location
  df2 <- lapply(
    df
    , function(x)
      summarise_trends(df = x)
  )
  
  # Add admin unit names
  df2 <- Map(cbind, df2, 'Admin_unit' = names(df2))
  
  # rbind data
  df2_long <- do.call('rbind', df2)
  
  # return
  df2_long
}

# Dengue data ------------------------------------------------------------------
# load data
brazil_dengue <- read.csv('../data/dengue/Lowe_etal_LPH_2021_brazil_dengue_data_2000_2019_weekly.csv')

# format data
# brazil_dengue <- brazil_dengue %>%
#   mutate(Country = 'Brazil'
#          , 'Admin_unit' = Admin1Name
#          , 'Date' = ISOweek2date(paste0(Year, '-W', sprintf('%02d', WOY), "-1"))
#   ) %>%
#   group_by(Country, Admin_unit, Date) %>%
#   summarise(Dengue_cases = sum(cases, na.rm = T)
#   ) %>%
#   as.data.frame()

# outbreak summary
brazil_dengue_outbreaks_summary <- summarize_outbreaks(
  df = brazil_dengue
  # , casesCol = 'Dengue_cases'
  , casesCol = 'cases'
  
)

# trends
brazil_dengue_trends_long <- summarize_trends(df = brazil_dengue_outbreaks_summary)

# Malaria data -----------------------------------------------------------------
brazil_malaria_1 <- read.csv('../data/malaria/brazil_exoamazonas_weekly.csv')
brazil_malaria_2 <- read.csv('../data/malaria/brazil_amazonas_weekly.csv')

brazil_malaria <- brazil_malaria_1 %>%
  bind_rows(brazil_malaria_2) %>%
  mutate(Country = 'Brazil'
         , 'Admin_unit' = Admin1Name
         , 'Date' = ISOweek2date(paste0(Year, '-W', sprintf('%02d', WOY), "-1"))
         ) %>%
  group_by(Country, Admin_unit, Date) %>%
  summarise(Falciparum_cases = sum(P_falciparum, na.rm = T)
            , Vivax_cases = sum(P_vivax, na.rm = T)
            ) %>%
  as.data.frame()

# P. falciparum  ----------------------
# outbreak summary
brazil_pfal_outbreaks_summary <- summarize_outbreaks(
  df = brazil_malaria
  , casesCol = 'Falciparum_cases'
)

# trends
brazil_pfal_trends_long <- summarize_trends(df = brazil_pfal_outbreaks_summary)

# P. vivax  --------------------------
# outbreak summary
# brazil_pvax_outbreaks_summary <- summarize_outbreaks(
#   df = brazil_malaria
#   , casesCol = 'Vivax_cases'
# )
# 
# # trends
# brazil_pvax_trends_long <- summarize_trends(df = brazil_pvax_outbreaks_summary)

# ESM climate data -------------------------------------------------------------
normal_land_grid <- read.csv('../data/esm/t_ref.land_daily.AM4_urban.amip1870_urban_luh2_wasteCool_tigercpu_intelmpi_18_576PE.Brazil.subregion.ens00.1999-2020.deg_k.csv')
rural_land_grid <- read.csv('../data/esm/t_ref.land_daily_rural.AM4_urban.amip1870_urban_luh2_wasteCool_tigercpu_intelmpi_18_576PE.Brazil.subregion.ens00.1999-2020.deg_k.csv')
urban_land_grid <- read.csv('../data/esm/t_ref.land_daily_urbn.AM4_urban.amip1870_urban_luh2_wasteCool_tigercpu_intelmpi_18_576PE.Brazil.subregion.ens00.1999-2020.deg_k.csv')

add_temp_data <- function(dz_df, temp_df, temp_metric_name){
  
  # fix spelling
  colnames(temp_df) <- replace_spanish_characters(x = colnames(temp_df))
  # specific to Brazil 
  colnames(temp_df) <- gsub('Esparito', 'Espirito', colnames(temp_df))
  colnames(temp_df) <- gsub('Paraaba', 'Paraiba', colnames(temp_df))
  colnames(temp_df) <- gsub('Piaua', 'Piaui', colnames(temp_df))
  
  # format date
  temp_df$Date <- as.Date(temp_df$Time, '%Y-%m-%d')

  # add new column for data
  dz_df[, temp_metric_name] <- NA
  
  # figure out which column in temp_df corresponds to dz_df
  colIndex <- which(colnames(temp_df) == unique(dz_df$Admin_unit))
  
  # calculate temperature in 30 day window before each outbreak
  for(i in 1:nrow(dz_df)){
    rowIndex <- which(temp_df$Time == dz_df$outbreak_start[i])
    if(rowIndex > 30){
      dz_df[i, temp_metric_name] <- mean(temp_df[(rowIndex-30):rowIndex, colIndex]) 
    } else {
      dz_df[i, temp_metric_name] <- NA 
    }
  }
  
  # change from K to C
  dz_df[, temp_metric_name] <- dz_df[, temp_metric_name] - 273.15

  # output data
  return(dz_df)

}

# Add temperature for each metric
# lots of warnings here, should look into this
brazil_r0_temp <- lapply(
  brazil_dengue_outbreaks_summary
  , function(x)
    add_temp_data(
      dz_df = x
      , temp_df = normal_land_grid
      , temp_metric_name = 'Temp_30d_mean_normal'
    )
)

brazil_r0_temp <- lapply(
  brazil_r0_temp
  , function(x)
    add_temp_data(
      dz_df = x
      , temp_df = rural_land_grid
      , temp_metric_name = 'Temp_30d_mean_rural'
    )
)

brazil_r0_temp <- lapply(
  brazil_r0_temp
  , function(x)
    add_temp_data(
      dz_df = x
      , temp_df = urban_land_grid
      , temp_metric_name = 'Temp_30d_mean_urban'
    )
)

brazil_r0_dengue <- do.call('rbind', brazil_r0_temp)

# remove unrealistic data
# brazil_r0_dengue <- subset(brazil_r0_dengue, R0 <= 5)

# brazil_r0_dengue$Year <- as.numeric(brazil_r0_dengue$Year)

# library(ncdf4)
# nc_file <- nc_open('../data/esm/t_ref.land_daily.AM4_urban.amip1870_urban_luh2_wasteCool_tigercpu_intelmpi_18_576PE.Brazil.subregion.ens00.1999-2020.nc')

# Population data? -------------------------------------------------------------
brazil_pop <- read.csv('../data/population/brazil_admin_pop_density.csv')
brazil_pop <- subset(brazil_pop, ADM1_NAME != 'Name Unknown')

# new col names
colnames(brazil_pop)[c(3, 4, 6, 8)] <- c('Admin_unit', 'Pop_density_mean', 'Pop_density_max', 'Pop_density_median')

# combine
brazil_r0_dengue <- brazil_r0_dengue %>%
  left_join(brazil_pop[, c('Admin_unit', 'Pop_density_mean', 'Pop_density_max', 'Pop_density_median')])

# glmmTMB models ---------------------------------------------------------------
library(glmmTMB)
library(dotwhisker)
library(broom)
library(ggeffects)
library(ggplot2)

# run models
br_mod_fun <- function(df, temp_metric){
  df$Temperature <- df[, temp_metric]
  glmmTMB(
    R0 ~ scale(Temperature)
    + scale(Temperature^2)
    + scale(susceptibility_index)
    + scale(Pop_density_median)
    + (1|Year)
    + (1|Admin_unit)
    , data = df)
}

br_dengue_mod_urban <- br_mod_fun(df = brazil_r0_dengue, temp_metric = 'Temp_30d_mean_urban')
br_dengue_mod_rural <- br_mod_fun(df = brazil_r0_dengue, temp_metric = 'Temp_30d_mean_rural')
br_dengue_mod_normal <- br_mod_fun(df = brazil_r0_dengue, temp_metric = 'Temp_30d_mean_normal')

# Plots ------------------------------------------------------------------------
# scatterplots of predicted vs observed
br_mod_pred_obs_plot <- function(df, temp_metric, br_mod, titleName){
  df$Temperature <- df[, temp_metric]
  
  df$predicted_r0 <- predict(
    object = br_mod
    , newdata = df
    , allow.new.levels = TRUE
    )
  
  df %>%
    ggplot(aes(x = predicted_r0, y = R0)) +
    geom_point(alpha = 0.1) +
    labs(x = "Predicted R0", y = "Calculated R0")+
    geom_smooth(method = lm) +
    theme_classic() +
    ylim(1, 2.5) +
    xlim(1, 2.5) +
    ggtitle(titleName)
  
}

urban_scatterplot <- br_mod_pred_obs_plot(df = brazil_r0_dengue
                                          , temp_metric = 'Temp_30d_mean_urban'
                                          , br_mod = br_dengue_mod_urban
                                          , titleName = 'Urban model')


rural_scatterplot <- br_mod_pred_obs_plot(df = brazil_r0_dengue
                                          , temp_metric = 'Temp_30d_mean_rural'
                                          , br_mod = br_dengue_mod_rural
                                          , titleName = 'Rural model')


normal_scatterplot <- br_mod_pred_obs_plot(df = brazil_r0_dengue
                                          , temp_metric = 'Temp_30d_mean_normal'
                                          , br_mod = br_dengue_mod_urban
                                          , titleName = 'Normal model')



# compare model coefficients
modList <- list(br_dengue_mod_urban, br_dengue_mod_rural, br_dengue_mod_normal)
names(modList) <- c('Urban', 'Rural', 'Normal')
coefficient_plot <- dwplot(modList
       , effects = 'fixed'
       , size = 3) + 
  theme_classic() +
  # theme(legend.position = 'none') +
  geom_vline(xintercept = 0, linetype = 'dashed')


# combine plots
library(cowplot)

plot_grid(urban_scatterplot, rural_scatterplot, normal_scatterplot, coefficient_plot, ncol = 4, rel_widths = c(1, 1, 1, 2))
ggsave('../figures/regression_models/dengue.pdf', width = 12, height = 3)



# library(DHARMa)
# 
# br_dengue_simres <- simulateResiduals(br_dengue_mod_urban)
# 
# plot(br_dengue_simres)

# histograms of trends ---------------------------------------------------------
brazil_dengue_trends_summary_long <- summarise_trends(df = outbreaks_summary)

slope_cols <- colnames(brazil_dengue_trends_summary_long)[grep('slope$', colnames(brazil_dengue_trends_summary_long))]

trends <- brazil_dengue_trends_summary_long[, slope_cols] %>%
  gather('key' = 'Trend', 'value' = 'Slope')

trends$Trend <- gsub('_', ' ', trends$Trend)

trends_plot <- ggplot(trends, aes(x = Slope)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(.~Trend, scales = 'free') + 
  theme_classic() +
  ggtitle('Dengue trends in Brazil'
          , subtitle = 'N = 27 states (slopes from 19 years of data/state)') +
  xlab('') +
  ylab('')

ggsave('../figures/trends/dengue_trends.pdf', width = 8.5, height = 6)

# Plots ------------------------------------------------------------------------
# 'predicted' vs 'observed' R0 with uncertainty in X and Y directions
# one plot for each ESM = 3 plots for dengue, 3 for malaria, 3 for ratio
# If time, plot R0 curves (with new R0 or relative R0?) vs data 

# ------------------------------------------------------------------------------
# All dengue data --------------------------------------------------------------
# ------------------------------------------------------------------------------
source('../codes/format_data_for_lambda_R0_analyses.R')

outbreaks_summary <- lapply(dengue_data, function(x)
  summarise_outbreaks(
    df = x
    , cases_colname = 'Dengue_cases'
    , date_colname = 'Date'
    , disease = 'dengue'
  )
)

# Remove NULL data frames within list 
outbreaks_summary <- outbreaks_summary[!sapply(outbreaks_summary, is.null)]

# Remove data frames with rows but no data
outbreaks_summary <- outbreaks_summary[sapply(outbreaks_summary, nrow)>0]


outbreaks_trends <- lapply(outbreaks_summary, function(x)
  summarise_trends(df = x)
)

outbreaks_trends_long <- do.call('rbind', outbreaks_trends)

# histograms
slope_cols <- colnames(outbreaks_trends_long)[grep('slope$', colnames(outbreaks_trends_long))]

trends <- outbreaks_trends_long[, slope_cols] %>%
  gather('key' = 'Trend', 'value' = 'Slope')

trends$Trend <- gsub('_', ' ', trends$Trend)

trends_plot <- ggplot(trends, aes(x = Slope)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(.~Trend, scales = 'free') + 
  theme_classic() +
  ggtitle('Dengue trends'
          , subtitle = paste0('N = ', nrow(trends), ' administration units')
  )+
  xlab('') +
  ylab('')

ggsave('../figures/trends/dengue_trends.pdf', width = 9.5, height = 6)
