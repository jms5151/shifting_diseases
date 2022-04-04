

# load libraries
library(tidyverse)
library(lubridate)
library(data.table)

# load climate data
climate_dir <- '../data/climate/'
climate_files_from_Wengchang <- list.files(climate_dir)
climate_files_from_Wengchang <- climate_files_from_Wengchang[grep('era5|chirps', climate_files_from_Wengchang)]

for(i in climate_files_from_Wengchang){
  filePath <- paste0(climate_dir, i)
  x <- read.csv(filePath)
  if(grepl('chirps', i) == TRUE){
    clim_metric = 'precip'
  } else {
    clim_metric <- substr(i, 6, 9)
  }
  if(grepl('Brazil', i) == TRUE){
    site = 'brazil'
  } else {
    site = 'thailand'
  }
  newFileName <- paste(site, clim_metric, 'popwt', sep = '_')
  assign(newFileName, x)
}

# tempweighted_brazil <- read.csv('../data/climate/era5.t2m.daily.Brazil.subregion.popwgt.1999-2020.K.csv')
# tempweighted_thailand <- read.csv('../data/climate/era5.t2m.daily.Thailand.subregion.popwgt.1993-2010.K.csv')

# load disease data
load('../data/tsir_outputs/brazil_dengue_empirical_betas.RData')
load('../data/tsir_outputs/thailand_dengue_empirical_betas.RData')
load('../data/tsir_outputs/brazil_malaria_vivax_empirical_betas.RData')
load('../data/tsir_outputs/brazil_malaria_falciparum_empirical_betas.RData')

# custom functions
formatBetas <- function(myList, betaName){
  df <- data.frame()
  sites <- names(myList)
  for(i in sites){
    startPoint <- length(myList[[i]]$Year) - length(myList[[i]]$betas) + 1
    x <- data.frame(
      'Admin1Name' = i,
      'Year' = myList[[i]]$Year[startPoint:length(myList[[i]]$Year)],
      'WOY' = myList[[i]]$WOY[startPoint:length(myList[[i]]$Year)],
      'Beta' = myList[[i]]$betas
    )
    df <- rbind(df, x)
  }
  row.names(df) <- NULL
  colnames(df)[4] <- paste0(betaName, "_betas")
  df
}

# format climate data
formatClimateData <- function(df, metric_name){ # , k2c
  x <- df %>%
    gather('Admin1Name', 'Value', -time)
  # if(k2c == TRUE){
  #   x$Value <- x$Value - 273.15
  # }
  colnames(x)[3] <- metric_name
  x$time <- as.Date(x$time, '%Y-%m-%d')
  x$Year <- as.numeric(substr(x$time, 1, 4))
  x$WOY <- week(x$time)
  x
}

formatBrazilAdminNames <- function(adminNames){
  x <- gsub('_', ' ', adminNames)
  x <- gsub('\\.', '', x)
  x <- gsub('Ã', 'a', x)
  x <- iconv(x, "latin1", "ASCII//TRANSLIT")
  x <- str_to_title(x)
  x <- gsub('Esparito Santo', 'Espirito Santo', x)
  x <- gsub('Paraaba', 'Paraiba', x)
  x <- gsub('Piaua', 'Piaui', x)
  x <- gsub('Rondania', 'Rondonia', x)
  x <- gsub('Tocantis', 'Tocantins', x)
}

formatThailandAminNames <- function(adminNames){
  x <- gsub('_', ' ', adminNames)
  x <- gsub('Bangkok', 'Krung Thep Maha Nakhon', x)
  x <- gsub('Phachinburi', 'Prachin Buri', x)
  x <- gsub('Buriram', 'Buri Ram', x)
  x <- gsub('Chainat', 'Chai Nat', x)
  x <- gsub('Chonburi', 'Chon Buri', x)
  x <- gsub('Lopburi', 'Lop Buri', x)
  x <- gsub('Kampaeng Phet', 'Kamphaeng Phet', x)
  x <- gsub('Phra Nakhon Si Ayudhya', 'Phra Nakhon Si Ayutthaya', x)
  x <- gsub('Prachuap Khilikhan', 'Prachuap Khiri Khan', x)
  x <- gsub('Samut Prakarn', 'Samut Prakan', x)
  x <- gsub('Si Saket', 'Si Sa Ket', x)
  x <- gsub('Singburi', 'Sing Buri', x)
  x <- gsub('Suphanburi', 'Suphan Buri', x)
  x <- gsub('Trad', 'Trat', x)
}

# Brazil -----------------------------------------------------------------------
brazil_dengue_betas_df <- formatBetas(myList = brazil_dengue_betas, betaName = 'dengue')
brazil_malaria_f_betas_df <- formatBetas(myList = brazil_malaria_falciparum_betas, betaName = 'malaria_F')
brazil_malaria_v_betas_df <- formatBetas(myList = brazil_malaria_vivax_betas, betaName = 'malaria_V')


brazil_clim <- list(
  brazil_t2m._popwt
  , brazil_mn2t_popwt
  , brazil_mx2t_popwt
  , brazil_swvl_popwt
  , brazil_precip_popwt
  )

metric_names <- c('mean_temp', 'min_temp', 'max_temp', 'soil_moisture', 'precip')

climate_brazil <- data.frame()

for(j in 1:length(brazil_clim)){
  x <- brazil_clim[[j]]
  x2 <- formatClimateData(
    df = x,
    metric_name = metric_names[j]
  )
  if(grepl('temp', metric_names[j]) == TRUE){
    x2[, metric_names[j]] <- x2[, metric_names[j]] - 273.15
  }
  if(j == 1){
    climate_brazil <- x2
  } else {
    climate_brazil <- climate_brazil %>%
      full_join(x2)
  }
}

climate_brazil$Admin1Name <- formatBrazilAdminNames(adminNames = climate_brazil$Admin1Name)

weekly_climate_brazil <- climate_brazil %>%
  group_by(Admin1Name, Year, WOY) %>%
  summarise('mean_temp' = mean(mean_temp)
            , 'min_temp' = min(min_temp)
            , 'max_temp' = max(max_temp)
            , 'soil_moisture' = mean(soil_moisture)
            , 'total_precip' = sum(precip)
            , 'max_precip' = max(precip)
            ) %>%
  # should add in some precip measurements
  mutate('mean_temp_lagged_1M' = lag(mean_temp, 4)
         , 'mean_temp_lagged_2M' = lag(mean_temp, 8)
         , 'soil_moisture_lagged_1M' = lag(soil_moisture, 4)
         )

# combine data
brazil_weekly <- brazil_dengue_betas_df %>%
  left_join(brazil_malaria_f_betas_df) %>%
  left_join(brazil_malaria_v_betas_df) %>%
  mutate(mf2d_ratio = malaria_F_betas / dengue_betas,
         mv2d_ratio = malaria_V_betas / dengue_betas) %>%
  left_join(weekly_climate_brazil) 

# play plots
brazil_weekly_A <- subset(brazil_weekly, Admin1Name == 'Amazonas')

clim_metrics <- colnames(brazil_weekly)[grep('temp|soil|precip', colnames(brazil_weekly))] 
malariaType <- c('mf2d_ratio', 'mv2d_ratio')

for(k in 1:length(clim_metrics)){
  for(l in 1:length(malariaType)){
    pdf(paste0('../figures/empirical_betas_vs_climate/Brazil_Amazonas_', malariaType[l], '_', clim_metrics[k], '.pdf'), width = 8, height = 6)
    plot(brazil_weekly_A[, clim_metrics[k]], brazil_weekly_A[, malariaType[l]], pch = 16, ylim = c(0, 5), xlab = clim_metrics[k], ylab = malariaType[l], col = alpha('black', 0.4), main = 'Amazonas, Brazil')
    abline(h = 1, lty = 2)
    legend('topleft', legend = 'Higher malaria', bty = 'n')
    legend('bottomleft', legend = 'Higher dengue', bty = 'n')
    dev.off()
  }
}

for(k in 1:length(clim_metrics)){
  xMin <- min(brazil_weekly_A[, clim_metrics[k]], na.rm = T)
  xMax <- max(brazil_weekly_A[, clim_metrics[k]], na.rm = T)
  plot(brazil_weekly_A[, clim_metrics[k]], brazil_weekly_A$dengue_betas, pch = 16, xlim = c(xMin, xMax), xlab = clim_metrics[k], ylab = 'Emp betas', col = 'lightblue')
  points(brazil_weekly_A[, clim_metrics[k]], brazil_weekly_A$malaria_F_betas, pch = 16, xlim = c(xMin, xMax))
}



# Thailand ---------------------------------------------------------------------
thailand_dengue_betas_df <- formatBetas(myList = thailand_dengue_betas, betaName = 'dengue')

daily_mean_temp_thailand <- formatClimateData(
  df = tempweighted_thailand,
  metric_name = 't2m',
  k2c = TRUE
)

daily_mean_temp_thailand$Admin1Name <- formatThailandAminNames(adminNames = daily_mean_temp_thailand$Admin1Name)

weekly_mean_temp_thailand <- daily_mean_temp_thailand %>%
  group_by(Admin1Name, Year, WOY) %>%
  summarise('Temp_mean' = mean(t2m))

weekly_mean_temp_thailand$lagged_1M_mean_temp <- c(rep(NA, 4), weekly_mean_temp_thailand$Temp_mean[1:(nrow(weekly_mean_temp_thailand)-4)])
weekly_mean_temp_thailand$lagged_2M_mean_temp <- c(rep(NA, 8), weekly_mean_temp_thailand$Temp_mean[1:(nrow(weekly_mean_temp_thailand)-8)])

# combine data
thailand_weekly <- thailand_dengue_betas_df %>%
  left_join(weekly_mean_temp_thailand)

# plot -------------------------------------------------------------------------
load("../data/R0_curves/malaria_R0.Rsave")
Aedes.R0.out = read.csv("../data/R0_curves/AedesR0Out.csv", header = T)
# combine data
R0_curves <- cbind(Aedes.R0.out, "anopheles.R0.median" = malaria / max(malaria))

# format
R0_curves$MalariaToDengueRatio <- R0_curves$anopheles.R0.median / R0_curves$aegypti.R0.median

# plot dengue
# plot(Aedes.R0.out$temperature,
#      Aedes.R0.out$aegypti.R0.median, 
#      xlim = c(12, 37),
#      lwd = 2, 
#      type = "l", 
#      col = "black", 
#      xlab="", 
#      ylab="")
# par(new = TRUE)
# plot(brazil_weekly$lagged_1M_mean_temp, brazil_weekly$Beta, xlim = c(12, 37), pch = 16, axes = F, xlab = '', ylab = '', col = 'blue')
# par(new = TRUE)
# plot(thailand_weekly$lagged_1M_mean_temp, thailand_weekly$Beta, xlim = c(12, 37), pch = 16, axes = F, xlab = '', ylab = '', col = 'orange')


# plot both for the amazon
brazil_weekly_A <- subset(brazil_weekly, Admin1Name == 'Amazonas')
pdf('../figures/R0/Brazil_Amazonas_temp.pdf', width = 8, height = 5)
par(mar = c(5,4,3,4))
plot(R0_curves$temperature,
     R0_curves$aegypti.R0.median, 
     xlim = c(12, 37),
     lwd = 2, 
     lty = 2,
     type = "l", 
     col = "black", 
     xlab="Temperature (lagged one month)", 
     ylab="R0 scaled",
     main = 'Amazonas, Brazil')
par(new = TRUE)
plot(R0_curves$temperature,
     R0_curves$anopheles.R0.median, 
     xlim = c(12, 37),
     lwd = 2, 
     type = "l", 
     col = "black", 
     xlab="", 
     ylab="")
par(new = TRUE)
plot(brazil_weekly_A$lagged_1M_mean_temp, brazil_weekly_A$dengue_betas, xlim = c(12, 37), ylim = c(0, 10e-12), pch = 16, axes = F, xlab = '', ylab = '', col = 'lightblue')
par(new = TRUE)
plot(brazil_weekly_A$lagged_1M_mean_temp, brazil_weekly_A$malaria_F_betas, xlim = c(12, 37), ylim = c(0, 10e-12), pch = 16, axes = F, xlab = '', ylab = '', col = 'blue')
par(new = TRUE)
plot(brazil_weekly_A$lagged_1M_mean_temp, brazil_weekly_A$malaria_V_betas, xlim = c(12, 37), ylim = c(0, 10e-12), pch = 16, axes = F, xlab = '', ylab = '', col = 'yellow')
axis(side = 4)
mtext('Empirical betas', side = 4, line = 2.5)
legend('topleft', bty = 'n', legend = c('Dengue', 'P. falciparum malaria', 'P. vivax malaria'),
       pch = c(16, 16, 16), col = c('lightblue', 'blue', 'yellow'))
legend('topright', bty = 'n', legend = c('Dengue R0', 'Malaria R0'), lty = c(2, 1))
dev.off()

# # ratio
# plot(R0_curves$temperature,
#      R0_curves$MalariaToDengueRatio, 
#      xlim = c(12, 37),
#      lwd = 2, 
#      type = "l", 
#      col = "black", 
#      xlab="", 
#      ylab="")
# par(new = TRUE)
# plot(brazil_weekly$lagged_1M_mean_temp, brazil_weekly$mf2d_ratio, xlim = c(12, 37), ylim = c(0, 7), pch = 16, axes = F, xlab = '', ylab = '', col = 'blue')
# par(new = TRUE)
# plot(brazil_weekly$lagged_1M_mean_temp, brazil_weekly$mv2d_ratio, xlim = c(12, 37), ylim = c(0, 7), pch = 16, axes = F, xlab = '', ylab = '', col = 'lightblue')
# 
# brazil_weekly$lagged_1M_mean_temp_round <- round(brazil_weekly$lagged_1M_mean_temp)
# boxplot(brazil_weekly$mf2d_ratio ~ brazil_weekly$lagged_1M_mean_temp_round, ylim = c(0,2))
# boxplot(brazil_weekly$mv2d_ratio ~ brazil_weekly$lagged_1M_mean_temp_round)
