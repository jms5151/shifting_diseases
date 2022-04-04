library(tidyverse)
library(stringr)
library(lomb)

# malaria ---------------
# malaria_percapita <- read.csv('../data/malaria/malaria_per_capita_incidence_global.csv')
# malaria_total <- read.csv('../data/malaria/malaria_total_confirmed_cases_global.csv')
# all together
# malaria_dfs <- read.csv('../data/malaria/malaria_estimated_total_cases_global.csv')
# 
# malaria_dfs$malaria <- malaria_dfs$FactValueNumeric
# malaria_sub <- malaria_dfs[, c('Location', 'Period', 'falciparum_malaria')]

# split by type
malaria_f <- read.csv('../data/malaria/malaria_indigenous_falciparum_cases_global.csv')
malaria_v <- read.csv('../data/malaria/malaria_indigenous_vivax_cases_global.csv')

malaria_f$falciparum_malaria <- malaria_f$FactValueNumeric
malaria_v$vivax_malaria <- malaria_v$FactValueNumeric

malaria_sub <- malaria_f[, c('Location', 'Period', 'falciparum_malaria')] %>%
  full_join(malaria_v[, c('Location', 'Period', 'vivax_malaria')])

# format names to match with other datasets
malaria_sub$Location[malaria_sub$Location == 'Bolivia (Plurinational State of)'] <- 'Bolivia'
malaria_sub$Location[malaria_sub$Location == 'Democratic People\'s Republic of Korea'] <- 'Dem People\'s Rep of Korea'
malaria_sub$Location[malaria_sub$Location == 'Venezuela (Bolivarian Republic of)'] <- 'Venezuela'

# dengue ---------------
dengue_filepaths <- list.files("../data/dengue", full.names = TRUE)
dengue_filepaths <- dengue_filepaths[grep("tycho", dengue_filepaths)]
dengue_filepaths <- dengue_filepaths[!grepl("weekly", dengue_filepaths)]

# load files into global environment
dengue_dfs = lapply(dengue_filepaths, read.csv)
tychoDF <- do.call('rbind', dengue_dfs)

# could be useful for expanding dengue observations in Africa
# https://www.nature.com/articles/s41598-019-50135-x#Sec15
# could be useful for outside of africa, longer time frame
# https://ntdhq.shinyapps.io/dengue5/

# format
tychoDF$PeriodEndDate <- as.Date(tychoDF$PeriodEndDate, '%Y-%m-%d')

tychoDF_yearly_totals <- tychoDF %>%
  mutate(Period = as.integer(format(PeriodEndDate, '%Y'))) %>%
  filter(Period >= 2000) %>%
  group_by(CountryName2, Period) %>%
  summarise(dengue = sum(CountValue)) %>%
  select(-CountryName2)

tychoDF_yearly_totals$Location <- gsub('_', ' ', tychoDF_yearly_totals$CountryName2)
tychoDF_yearly_totals$Location <- str_to_title(tychoDF_yearly_totals$Location)

# fix some location names to match malaria dataset
# sort(setdiff(unique(tychoDF_yearly_totals$Location), unique(malaria_dfs$Location)))
# sort(setdiff(unique(malaria_dfs$Location), unique(tychoDF_yearly_totals$Location)))
tychoDF_yearly_totals$Location[tychoDF_yearly_totals$Location == 'Bolivia Plurinational State Of'] <- 'Bolivia'
tychoDF_yearly_totals$Location[tychoDF_yearly_totals$Location == 'Korea Republic Of'] <- 'Dem People\'s Rep of Korea'
tychoDF_yearly_totals$Location[tychoDF_yearly_totals$Location == 'Lao Peoples Democratic Republic'] <- 'Lao People\'s Democratic Republic'
tychoDF_yearly_totals$Location[tychoDF_yearly_totals$Location == 'Timorleste'] <- 'Timor-Leste'
tychoDF_yearly_totals$Location[tychoDF_yearly_totals$Location == 'Venezuela Bolivarian Republic Of'] <- 'Venezuela'

# combine
mbd <- tychoDF_yearly_totals %>%
  right_join(malaria_sub)

# mbd <- mbd[complete.cases(mbd), ]
# mbd$dengue[mbd$dengue == 0] <- 1

# better calculations?
# mbd$ratio <- mbd$dengue/mbd$malaria
# mbd$ratio <- mbd$malaria/mbd$dengue
mbd$ratio_fd <- mbd$falciparum_malaria/mbd$dengue
mbd$ratio_vd <- mbd$vivax_malaria/mbd$dengue

hist(mbd$ratio, breaks = 100)
plot(mbd$dengue, mbd$malaria, pch = 16)


mbd2 <- subset(mbd, ratio < 1)
boxplot(mbd2$ratio ~ mbd2$CountryName2)

mbd$percent_change <- ifelse(mbd$dengue < mbd$malaria,
                              -(mbd$dengue/mbd$malaria) * 100,
                              (mbd$malaria/mbd$dengue) * 100)

boxplot(mbd$percent_change ~ mbd$CountryName2)
abline(h = 0, lty = 2)

# mbd2 <- subset(mbd, percent_change > 0 )
# boxplot(mbd2$percent_change ~ mbd2$CountryName2)

mbd_lagged <- mbd %>%
  group_by(Location, Period) %>%
  mutate(ratio_lagged = lag(ratio))


# Periodograms -----------------------------------------------------------------
# malaria
malaria_dfs <- read.csv('../data/malaria/malaria_estimated_total_cases_global.csv')

# periodograms_malaria <- data.frame()

plotSigPeriodograms <- function(df, observationColName, commonName){
  location_names <- unique(df$Location)
  
  for(i in location_names){
    df_tmp <- subset(df, Location == i)
    df_tmp <- df_tmp[!is.na(df_tmp[observationColName]),]

    if(nrow(df_tmp) > 2 & any(df_tmp[, observationColName] > 0)) {
      pds <- lsp(
        x = df_tmp[, observationColName]
        , times = df_tmp$Period
        , type = 'period'
        , plot = FALSE
      )
      
      if(!is.na(mean(summary(pds$p.value))) & mean(summary(pds$p.value)) <= 0.01){
        plotFilename <- paste0('../figures/Periodograms/', i, '_', commonName, '.pdf')
        plotFilename <- gsub(' ', '_', plotFilename)
        pdf(plotFilename, width = 8, height = 6)
        plot.lsp(pds, main = paste0(i, ' ', commonName), xlab = 'Period (years)', level = TRUE)
        dev.off()
      }
    }

    # df_new <- data.frame(
    #   'Location' = i
    #   , 'Pvalue' = mean(summary(pds$p.value))
    #   )
    # 
    # periodograms_malaria <- rbind(
    #   periodograms_malaria
    #   , df_new
    # )
    
  }
  
}

plotSigPeriodograms(
  df = malaria_dfs
  , observationColName = 'FactValueNumeric'
  , commonName = 'malaria total'
)

plotSigPeriodograms(
  df = malaria_sub
  , observationColName = 'falciparum_malaria'
  , commonName = 'falciparum malaria'
)

plotSigPeriodograms(
  df = malaria_sub
  , observationColName = 'vivax_malaria'
  , commonName = 'vivax malaria'
)

plotSigPeriodograms(
  df = as.data.frame(tychoDF_yearly_totals)
  , observationColName = 'dengue'
  , commonName = 'dengue'
)




# compare with population density, gdp, urbanization, precip, temp
# Temperature -----------------------------------------------------------------
era5_temp <- read.csv('../data/climate/monthly_temperature_by_country.csw')

# population density ----------------------------------------------------------
load('../data/population/country_pop_density_interpolated.RData')
colnames(country_pop_density_full)[1:2] <- c('Location', 'Period')

mbd2 <- mbd %>%
  left_join(country_pop_density_full)

plot(mbd2$Interpolated_pop_density_min, mbd2$malaria, pch = 16)
points(mbd2$Interpolated_pop_density_min, mbd2$dengue, pch = 18, col = 'blue')

plot(mbd2$Interpolated_pop_density_min, mbd2$dengue, pch = 18, col = 'blue')
points(mbd2$Interpolated_pop_density_min, mbd2$malaria, pch = 16)

plot(mbd2$percent_change, mbd2$Interpolated_pop_density_median, pch = 16)
# plot(mbd2$Interpolated_pop_density, mbd2$percent_change, pch = 16)
plot(mbd2$percent_change, mbd2$Interpolated_pop_density_mean, pch = 16)
plot(mbd2$percent_change, mbd2$Interpolated_pop_density_min, pch = 16)
plot(mbd2$percent_change, mbd2$Interpolated_pop_density_max, pch = 16)

# wealth ---------------------------------------------------------------------
# source: https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.MKTP.CD&country=#
gdp <- read.csv('../data/wealth/The_World_Bank_Data_Extract_From_World_Development_Indicators_GDP.csv')

gdp_long <- gdp %>%
  gather(Period, GDP, X1990..YR1990.:X2020..YR2020.)

gdp_long$Period <- as.numeric(substr(gdp_long$Period, 2, 5))
gdp_long$GDP <- as.numeric(gdp_long$GDP)
gdp_long$Location <- gdp_long$Country.Name

mbd3 <- mbd %>%
  left_join(gdp_long)

plot(mbd3$percent_change, mbd3$GDP, pch = 16)

plot(mbd3$GDP, mbd3$dengue, pch = 18, col = 'blue')
points(mbd3$GDP, mbd3$malaria, pch = 16)

# test with kenya data
dengue <- read.csv('../data/Non_malaria_fever_by_site_and_temp.csv')
malaria <- read.csv('../data/Malaria_smear_positive_by_site_and_temp.csv')

lancetData <- dengue %>%
  left_join(malaria)

lancetData$ratio <- lancetData$Non_malarial_fever_positive / lancetData$Malaria_smear_positive
lancetData$ratio <- lancetData$Malaria_smear_positive / lancetData$Non_malarial_fever_positive

lancetData$percent_change <- ifelse(lancetData$Non_malarial_fever_positive < lancetData$Malaria_smear_positive,
                             -(lancetData$Non_malarial_fever_positive/lancetData$Malaria_smear_positive) * 100,
                             (lancetData$Malaria_smear_positive/lancetData$Non_malarial_fever_positive) * 100)

lancetData$fold_change <- ifelse(lancetData$Non_malarial_fever_positive < lancetData$Malaria_smear_positive,
                                 -1 * (lancetData$Malaria_smear_positive/lancetData$Non_malarial_fever_positive),
                                 lancetData$Non_malarial_fever_positive/lancetData$Malaria_smear_positive)


# this seems to be the clearest plot
plot(
  lancetData$Temperature_30day_lag
  , lancetData$percent_change
  , pch = 16
  , ylab = '% change'
  , xlab = 'Monthly mean temperature lagged'
  # , xlim = c(14, 35)
  )
abline(h = 0)
legend('topright'
       , legend = 'More dengue'
       , bty = 'n'
       )
legend('bottomleft'
       , legend = 'More malaria'
       , bty = 'n'
)

plot(lancetData$Temperature_30day_lag, lancetData$ratio, pch = 16)
abline(h = 1, lty = 2)

plot(lancetData$Temperature_30day_lag, lancetData$percent_change, pch = 16)
abline(h = 0, lty = 2)

plot(lancetData$Temperature_30day_lag, lancetData$fold_change, pch = 16)
abline(h = 0, lty = 2)

# vectors
aedes <- read.csv('../data/vectors/gbif_aedes.csv', sep = '\t')
anopheles <- read.csv('../data/vectors/gbif_anopheles.csv', sep = '\t')

# format
# may need to save as shapefile for GEE
vectors <- rbind(aedes, anopheles)

vectors$Latitude <- vectors$decimalLatitude
vectors$Longitude <- vectors$decimalLongitude

vectors$Date <- as.Date(vectors$eventDate, '%Y-%m-%d')

vectors2 <- vectors[, c(
  'gbifID'
  , 'genus'
  , 'species'
  , 'Latitude'
  , 'Longitude'
  , 'countryCode'
  , 'stateProvince'
  , 'elevation'
  , 'Date'
  , 'year'
  , 'month'
  , 'day'
  , 'individualCount'
)]

vectors2[vectors2 == ''] <- NA

write.csv(vectors2, '../data/vectors/gbif_formatted.csv', row.names = F)

# vectors3 <- subset(vectors2, !is.na(Latitude) & !is.na(Longitude) & Date >= '2000-01-02')
# write.csv(vectors3, '../data/vectors/gbif_formatted_subset.csv', row.names = F)

# heatmaps
library(ggplot2)

heatmap_fun <- function(df, period, site, fillVar, logTF){
  df[,period] <- as.factor(df[,period])
  if(logTF == T){
    df[,fillVar] <- log(df[,fillVar]+1)
    fillVarName <- paste0(fillVar, '_logged')
  } else if(fillVar == 'individualCount'){
    fillVarName <- unique(df$genus)
  } else {
    fillVarName <- fillVar
  }
  htmap <- ggplot(df, aes(!!sym(period), !!sym(site), fill = !!sym(fillVar))) + 
    geom_tile() +
    theme_bw() +
    xlab('Year') +
    ylab('') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(htmap, file = paste0('../figures/heatmaps/', fillVarName, '.pdf'), width = 14, height = 18)
}

heatmap_fun(df = malaria_sub
            , period = 'Period'
            , site = 'Location'
            , fillVar = 'falciparum_malaria'
            , logTF = TRUE)

heatmap_fun(df = malaria_sub
            , period = 'Period'
            , site = 'Location'
            , fillVar = 'falciparum_malaria'
            , logTF = FALSE)

heatmap_fun(df = malaria_sub
            , period = 'Period'
            , site = 'Location'
            , fillVar = 'vivax_malaria'
            , logTF = TRUE)

heatmap_fun(df = malaria_sub
            , period = 'Period'
            , site = 'Location'
            , fillVar = 'vivax_malaria'
            , logTF = FALSE)

heatmap_fun(df = as.data.frame(tychoDF_yearly_totals)
            , period = 'Period'
            , site = 'Location'
            , fillVar = 'dengue'
            , logTF = TRUE)

heatmap_fun(df = as.data.frame(tychoDF_yearly_totals)
            , period = 'Period'
            , site = 'Location'
            , fillVar = 'dengue'
            , logTF = FALSE)

heatmap_fun(df = aedes
            , period = 'year'
            , site = 'countryCode'
            , fillVar = 'individualCount'
            , logTF = FALSE)

heatmap_fun(df = anopheles
            , period = 'year'
            , site = 'countryCode'
            , fillVar = 'individualCount'
            , logTF = FALSE)

# vectors with pop data --------------------------------------------------------
# vector_pop <- read.csv('../data/population/vectors_pop_density.csv')
# vector_pop$Latitude <- substr(vector_pop$.geo, 32, 50)
# vector_pop$Longitude <- substr(vector_pop$.geo, 51, 68)


# test plots of incidence ------------------------------------------------------
all_joined <- tychoDF_yearly_totals %>%
  full_join(malaria_sub) %>%
  full_join(malaria_dfs[, c('Period', 'Location', 'FactValueNumeric')])

countries <- unique(all_joined$Location)

for(i in countries){
  df <- subset(all_joined, Location == i)
  df <- df[order(df$Period), ]
  
  fileName <- paste0('../figures/global_incidence/', i, '.pdf')
  fileName <- gsub(' ', '_', fileName)
  
  pdf(fileName, width = 8, height = 6)
  plot(df$Period, df$dengue, type = 'b', xlim = c(min(df$Period), max(df$Period)), ylim = c(0, max(df$dengue, df$falciparum_malaria, df$vivax_malaria, df$FactValueNumeric, na.rm = T)), pch = 16, ylab = 'incidence', xlab = 'Year', main = i)
  lines(df$Period, df$falciparum_malaria, type = 'b', col = 'red', pch = 16)
  lines(df$Period, df$vivax_malaria, type = 'b', col = 'blue', pch = 16)
  lines(df$Period, df$FactValueNumeric, type = 'b', col = 'green', pch = 16)
  legend("topright", legend = c('dengue', 'falciparum malaria', 'vivax malaria', 'estimated total malaria'),
         pch = rep(16, 4), lty = rep(1, 4), col = c('black', 'red', 'blue', 'green'), bty = 'n')
  dev.off()
  
}
