# load libraries
library(tidyverse)
library(ggplot2)

# ENSO -------------------------------------------------------------------------
# load data
nino3.4 <- read.csv('../data/climate/nino3.4.csv')

# determine el nino and la nina years based on nino 3.4 index of Oct - Dec -----
EN_anom <- subset(nino3.4, Month10 >= 1 & Month11 >= 1 & Month12 >= 1)
LN_anom <- subset(nino3.4, Month10 <= -1 & Month11 <= -1 & Month12 <= -1)

# make vector of 24 months from ENSO years
EN_years_24mo <- sort(c(as.integer(EN_anom$Year), (as.integer(EN_anom$Year) + 1)))
LN_years_24mo <- sort(c(as.integer(LN_anom$Year), (as.integer(LN_anom$Year) + 1)))


# Vectors ----------------------------------------------------------------------
# load data
aedes <- read.csv('../data/vectors/gbif_aedes.csv', sep = '\t')
anopheles <- read.csv('../data/vectors/gbif_anopheles.csv', sep = '\t')

# aggregate to find good time series to work with
aggregate_vectors <- function(df, location){
  if(location == 'country'){
    df$Location <- df$countryCode
  } else if(location == 'state') {
    df$Location <- paste0(df$stateProvince, ', ', df$countryCode)
  }
  df <- df  %>%
    filter(year >= 1870) %>%
    filter(Location != '') %>%
    group_by(Location, year, month) %>%
    summarise(count = sum(individualCount, na.rm = T)) %>%
    drop_na()
  
  yearly <- df %>%
    group_by(Location, year) %>%
    mutate(yearlytotal = sum(count, na.rm = T))
  
  df <- df %>%
    left_join(yearly) %>%
    group_by(Location) %>%
    ungroup() %>%
    complete(year, month) %>%
    fill(Location) %>%
    mutate(normalized_count = count/yearlytotal)
  
  df$YearMonth <- as.Date(paste(df$year, df$month, '01', sep = '-'), '%Y-%m-%d')

  df
}


# aedes -----------------------------------------
ae_vec <- aggregate_vectors(df = aedes
                            , location = 'country'
                            # , location = 'state'
                            )

ae_highlySampledLocations <- names(which(table(ae_vec$Location) > 100))

ae_vec <- ae_vec[ae_vec$Location %in% ae_highlySampledLocations, ]

# anopheles -------------------------------------
an_vec <- aggregate_vectors(df = anopheles
                            , location = 'country'
                            # , location = 'state'
                            )
an_highlySampledLocations <- names(which(table(an_vec$Location) > 100))

an_vec <- an_vec[an_vec$Location %in% an_highlySampledLocations, ]

# an_vec <- an_vec %>%
#   # Anopheles, top countries
#   filter(Location == 'US' | Location == 'MX' | Location == 'BR'| Location == 'AU' | Location == 'GB' | Location == 'IN' | Location == 'ZA')


# heatmaps --------------------------------------
aedes_heatmap_subset <- ggplot(ae_vec, aes(YearMonth, Location, fill = normalized_count)) + 
  geom_tile() +
  theme_bw() +
  xlab('Year-Month') +
  ylab('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle('Aedes')

ggsave(aedes_heatmap_subset, file = '../figures/vector_climatologies/Aedes_heatmap.pdf', width = 10, height = 3)

anopheles_heatmap_subset <- ggplot(an_vec, aes(YearMonth, Location, fill = normalized_count)) + 
  geom_tile() +
  theme_bw() +
  xlab('Year-Month') +
  ylab('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle('Anopheles')

ggsave(anopheles_heatmap_subset, file = '../figures/vector_climatologies/Anopheles_heatmap.pdf', width = 10, height = 5)

# long-term monthly values ----------------------
vector_clim_plots <- function(df, title, filename, aedes){
  plt <- ggplot(df, aes(x = as.factor(month), y = normalized_count, fill = Location)) +
    geom_boxplot() +
    facet_wrap(~Location) + 
    theme_bw() +
    xlab('Month') + 
    ggtitle(title)
  
  newfilename <- paste0('../figures/vector_climatologies/', filename, '.pdf')
  if(aedes == TRUE){
    ggsave(plt, file = newfilename, width = 10, height = 6)
  } else
    ggsave(plt, file = newfilename, width = 10, height = 8)
}

vector_clim_plots(
  df = ae_vec
  , title = 'Aedes monthly climatology'
  , filename = 'Aedes_monthly_boxplot'
  , aedes = TRUE
)

vector_clim_plots(
  df = an_vec
  , title = 'Anopheles monthly climatology'
  , filename = 'Anopheles_monthly_boxplot'
  , aedes = FALSE
) 



# compare monthly climatologies with ENSO years --------------------------------
vector_clim_enso_plots <- function(lt_vec, en_vec, ln_vec, title, filename, aedes){
  lt_vec$treatment <- 'climatology'
  en_vec$treatment <- 'el nino'
  ln_vec$treatment <- 'la nina'
  
  df <- bind_rows(list(lt_vec, en_vec, ln_vec))
  
  plt <- ggplot(df, aes(x = as.factor(month), y = normalized_count, fill = treatment)) +
    geom_boxplot() +
    facet_wrap(~Location) + 
    theme_bw() +
    xlab('Month') + 
    ggtitle(title)
  
  newfilename <- paste0('../figures/vector_climatologies/', filename, '.pdf')
  if(aedes == TRUE){
    ggsave(plt, file = newfilename, width = 14, height = 8)
  } else
    ggsave(plt, file = newfilename, width = 14, height = 10)
}

# aedes
ae_vec_EN <- ae_vec[ae_vec$year %in% EN_years_24mo, ]
ae_vec_LN <- ae_vec[ae_vec$year %in% LN_years_24mo, ]

vector_clim_enso_plots(lt_vec = ae_vec
                       , en_vec = ae_vec_EN
                       , ln_vec = ae_vec_LN
                       , title = 'Aedes climatologies',
                       filename = "aedes_enso_clim_distr",
                       aedes = TRUE)

# anopheles
an_vec_EN <- an_vec[an_vec$year %in% EN_years_24mo, ]
an_vec_LN <- an_vec[an_vec$year %in% LN_years_24mo, ]

vector_clim_enso_plots(lt_vec = an_vec
                       , en_vec = an_vec_EN
                       , ln_vec = an_vec_LN
                       , title = 'Anopheles climatologies',
                       filename = "anopheles_enso_clim_distr",
                       aedes = FALSE)


# Log ratio of ENSO events : 'normal years' ------------------------------------
lollipop_clim_plots <- function(lt_vec, en_vec, ln_vec, title, filename, aedes){
  lt_vec <- lt_vec %>%
    group_by(Location, month) %>%
    summarise(lt_normalized_median = median(normalized_count, na.rm = T))
  
  en_vec <- en_vec %>%
    group_by(Location, month) %>%
    summarise(en_normalized_median = median(normalized_count, na.rm = T))
  
  ln_vec <- ln_vec %>%
    group_by(Location, month) %>%
    summarise(ln_normalized_median = median(normalized_count, na.rm = T))
  
  df <- lt_vec %>%
    left_join(en_vec) %>%
    left_join(ln_vec) %>%
    mutate(el_nino = log(en_normalized_median / lt_normalized_median)
           , la_nina = log(ln_normalized_median / lt_normalized_median)
    ) %>%
    select(-c('lt_normalized_median', 'en_normalized_median', 'ln_normalized_median')) %>%
    gather('type', 'log_ratio', el_nino:la_nina)
  
  plt <- ggplot(df, aes(x = month, y = log_ratio, color = type)) +
    geom_segment(aes(x = month, xend = month, y = 0, yend = log_ratio)) +
    geom_point(size = 4) +
    facet_wrap(~Location) + 
    theme_bw() +
    xlab('Month') +
    ylab('Log ratio of monthly medians') +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(1, 12, 1)) + 
    ggtitle(title)
  
  
  newfilename <- paste0('../figures/vector_climatologies/', filename, '.pdf')
  if(aedes == TRUE){
    ggsave(plt, file = newfilename, width = 14, height = 8)
  } else
    ggsave(plt, file = newfilename, width = 14, height = 10)
}


lollipop_clim_plots(lt_vec = ae_vec
                    , en_vec = ae_vec_EN
                    , ln_vec = ae_vec_LN
                    , title = 'Aedes'
                    , filename = 'lollipop_climatologies_aedes'
                    , aedes = TRUE
                    )

lollipop_clim_plots(lt_vec = an_vec
                    , en_vec = an_vec_EN
                    , ln_vec = an_vec_LN
                    , title = 'Anopheles'
                    , filename = 'lollipop_climatologies_anopheles'
                    , aedes = FALSE
                    )





# 
# 
# ltm <- function(df){
#   df %>%
#     group_by(countryCode, month) %>%
#     summarise(LT_mean = mean(logCount, na.rm = T)
#               , LT_SD = sd(logCount, na.rm = T)
#               , LT_var = var(logCount, na.rm = T)
#               , LT_25 = quantile(logCount, 0.25, na.rm = T)
#               , LT_50 = quantile(logCount, 0.50, na.rm = T)
#               , LT_75 = quantile(logCount, 0.75, na.rm = T)
#     )
# }
# 
# LT_aedes <- ltm(df = ae_vec)
# LT_anopheles <- ltm(df = an_vec)
# 
# vector_anomalies <- function(vector_df, lt_df){
#   df <- vector_df %>%
#     left_join(lt_df) %>%
#     mutate(mean_anomaly = LT_mean - logCount)
# }
# 
# ae_anom <- vector_anomalies(vector_df = ae_vec, lt_df = LT_aedes)
# an_anom <- vector_anomalies(vector_df = an_vec, lt_df = LT_anopheles)
# 
# # determine el nino and la nina years based on nino 3.4 index of Oct - Dec -----
# EN_anom <- subset(nino3.4, Month10 >= 1 & Month11 >= 1 & Month12 >= 1)
# LN_anom <- subset(nino3.4, Month10 <= -1 & Month11 <= -1 & Month12 <= -1)
# 
# # el nino
# ae_en_anom <- ae_anom[ae_vec$year %in% as.integer(EN_anom$Year), ]
# an_en_anom <- an_vec[an_vec$year %in% as.integer(EN_anom$Year), ]
# 
# ae_en_anom_plus1 <- ae_anom[ae_vec$year %in% (as.integer(EN_anom$Year)+1), ]
# an_en_anom_plus1 <- an_vec[an_vec$year %in% (as.integer(EN_anom$Year)+1), ]
# 
# # ae_en_anom <- ae_en_anom %>%
# #   group_by(countryCode, month) %>%
# #   summarise(mean_logcount = mean(logCount, na.rm = T))
# 
# vector_clim_plots(
#   df = ae_en_anom
#   , title = 'Aedes El Nino anomalies'
#   , filename = 'Aedes_EN_anomalies_boxplot'
#   , aedes = TRUE
# )
# 
# vector_clim_plots(
#   df = ae_en_anom_plus1
#   , title = 'Aedes El Nino + 1 yr anomalies'
#   , filename = 'Aedes_EN_plus1yr_anomalies_boxplot'
#   , aedes = TRUE
# )
# 
# vector_clim_plots(
#   df = an_en_anom
#   , title = 'Anopheles El Nino anomalies'
#   , filename = 'Anopheles_EN_anomalies_boxplot'
#   , aedes = FALSE
# ) 
# 
# vector_clim_plots(
#   df = an_en_anom_plus1
#   , title = 'Anopheles El Nino + 1 yr anomalies'
#   , filename = 'Anopheles_EN_plus1yr_anomalies_boxplot'
#   , aedes = FALSE
# ) 

