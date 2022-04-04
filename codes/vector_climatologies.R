# load libraries
library(tidyverse)
library(ggplot2)

# load data
aedes <- read.csv('../data/vectors/gbif_aedes.csv', sep = '\t')
anopheles <- read.csv('../data/vectors/gbif_anopheles.csv', sep = '\t')

nino3.4 <- read.csv('../data/climate/nino3.4.csv')

# aggregate to find good time series to work with
aggregate_vectors <- function(df){
  df <- df  %>%
    filter(year >= 1870) %>%
    filter(countryCode != '') %>%
    group_by(countryCode, year, month) %>%
    summarise(count = sum(individualCount, na.rm = T)) %>%
    drop_na()
  
  yearly <- df %>%
    group_by(countryCode, year) %>%
    mutate(yearlytotal = sum(count, na.rm = T))
  
  df <- df %>%
    left_join(yearly) %>%
    group_by(countryCode) %>%
    ungroup() %>%
    complete(year, month) %>%
    fill(countryCode) %>%
    mutate(normalized_count = count/yearlytotal)
  
  df$YearMonth <- as.Date(paste(df$year, df$month, '01', sep = '-'), '%Y-%m-%d')
  
  # df$logCount = log(df$count + 1)
  
  df
}

# aedes -----------------------------------------
ae_vec <- aggregate_vectors(df = aedes)

ae_vec <- ae_vec %>%
  # Aedes, top countries
  filter(countryCode == 'US' | countryCode == 'MX' | countryCode == 'BR') 

# anopheles -------------------------------------
an_vec <- aggregate_vectors(df = anopheles)

an_vec <- an_vec %>%
# Anopheles, top countries
  filter(countryCode == 'US' | countryCode == 'MX' | countryCode == 'BR'| countryCode == 'AU' | countryCode == 'GB' | countryCode == 'IN' | countryCode == 'ZA' | countryCode == 'ZL')


# heatmaps --------------------------------------
aedes_heatmap_subset <- ggplot(ae_vec, aes(YearMonth, countryCode, fill = normalized_count)) + 
  geom_tile() +
  theme_bw() +
  xlab('Year-Month') +
  ylab('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle('Aedes')

ggsave(aedes_heatmap_subset, file = '../figures/vector_climatologies/Aedes_heatmap.pdf', width = 10, height = 3)

anopheles_heatmap_subset <- ggplot(an_vec, aes(YearMonth, countryCode, fill = normalized_count)) + 
  geom_tile() +
  theme_bw() +
  xlab('Year-Month') +
  ylab('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle('Anopheles')

ggsave(anopheles_heatmap_subset, file = '../figures/vector_climatologies/Anopheles_heatmap.pdf', width = 10, height = 5)

# long-term monthly values ----------------------
vector_clim_plots <- function(df, title, filename, aedes){
  plt <- ggplot(df, aes(x = as.factor(month), y = normalized_count, fill = countryCode)) +
    geom_boxplot() +
    facet_wrap(~countryCode) + 
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
  
ltm <- function(df){
  df %>%
    group_by(countryCode, month) %>%
    summarise(LT_mean = mean(logCount, na.rm = T)
              , LT_SD = sd(logCount, na.rm = T)
              , LT_var = var(logCount, na.rm = T)
              , LT_25 = quantile(logCount, 0.25, na.rm = T)
              , LT_50 = quantile(logCount, 0.50, na.rm = T)
              , LT_75 = quantile(logCount, 0.75, na.rm = T)
    )
}

LT_aedes <- ltm(df = ae_vec)
LT_anopheles <- ltm(df = an_vec)

vector_anomalies <- function(vector_df, lt_df){
  df <- vector_df %>%
    left_join(lt_df) %>%
    mutate(mean_anomaly = LT_mean - logCount)
}

ae_anom <- vector_anomalies(vector_df = ae_vec, lt_df = LT_aedes)
an_anom <- vector_anomalies(vector_df = an_vec, lt_df = LT_anopheles)

# determine el nino and la nina years based on nino 3.4 index of Oct - Dec -----
EN_anom <- subset(nino3.4, Month10 >= 1 & Month11 >= 1 & Month12 >= 1)
LN_anom <- subset(nino3.4, Month10 <= -1 & Month11 <= -1 & Month12 <= -1)

# el nino
ae_en_anom <- ae_anom[ae_vec$year %in% as.integer(EN_anom$Year), ]
an_en_anom <- an_vec[an_vec$year %in% as.integer(EN_anom$Year), ]

ae_en_anom_plus1 <- ae_anom[ae_vec$year %in% (as.integer(EN_anom$Year)+1), ]
an_en_anom_plus1 <- an_vec[an_vec$year %in% (as.integer(EN_anom$Year)+1), ]

# ae_en_anom <- ae_en_anom %>%
#   group_by(countryCode, month) %>%
#   summarise(mean_logcount = mean(logCount, na.rm = T))

vector_clim_plots(
  df = ae_en_anom
  , title = 'Aedes El Nino anomalies'
  , filename = 'Aedes_EN_anomalies_boxplot'
  , aedes = TRUE
)

vector_clim_plots(
  df = ae_en_anom_plus1
  , title = 'Aedes El Nino + 1 yr anomalies'
  , filename = 'Aedes_EN_plus1yr_anomalies_boxplot'
  , aedes = TRUE
)

vector_clim_plots(
  df = an_en_anom
  , title = 'Anopheles El Nino anomalies'
  , filename = 'Anopheles_EN_anomalies_boxplot'
  , aedes = FALSE
) 

vector_clim_plots(
  df = an_en_anom_plus1
  , title = 'Anopheles El Nino + 1 yr anomalies'
  , filename = 'Anopheles_EN_plus1yr_anomalies_boxplot'
  , aedes = FALSE
) 

