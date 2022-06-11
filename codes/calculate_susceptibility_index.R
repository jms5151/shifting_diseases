# load libraries
library(zoo)
library(tidyverse)
library(ggplot2)

# source custom functions
source('codes/functions_tibble_to_list.R')
source('codes/functions_temporal_disaggregation.R')
source('codes/functions_Lambda_and_R0.R')

# Doesn't seem like the results differ if using weekly disaggregated or monthly data
# so maybe use the monthly form

# calculate susceptibility index -----------------------------------------------
pr2 <- read.csv('../data/dengue/SJPR_NN_test.csv')
pr2$week_start_date <- as.Date(pr2$week_start_date, '%m/%d/%Y')

test <- summarise_outbreaks(
  df = pr2
  , cases_colname = 'cases'
  , date_colname = 'week_start_date'
  , disease = 'dengue'
)

# plot
ggplot(test, aes(Year, R0)) + # can change Year with period       
  geom_point() +
  geom_errorbar(aes(ymin = R0_lower, ymax = R0_upper)) +
  theme_classic()

par(mfrow = c(1,3), mar = c(4,4,0.5,0.5), cex = 1.1)
plot(test$susceptibility_index, test$R0, pch = 16, xlab = 'Susceptibility index', ylab = 'R0')
plot(test$susceptibility_index, test$total_cases, pch = 16, xlab = 'Susceptibility index', ylab = 'Total cases')
plot(test$susceptibility_index, test$peak_cases, pch = 16, xlab = 'Susceptibility index', ylab = 'Peak cases')

# brazil
br <- read.csv('../data/dengue/Lowe_etal_LPH_2021_brazil_dengue_data_2000_2019.csv')

# format
br$Date <- paste0(br$year, '-', br$month, '-01')
br$Date <- as.Date(br$Date, '%Y-%m-%d')

## 1. Summarize monthly cases by area
br_meso_summary <- br %>%
  group_by(meso_name, Date) %>%
  summarise(cases = sum(dengue_cases, na.rm = T))

br_state_summary <- br %>%
  group_by(state_name, Date) %>%
  summarise(cases = sum(dengue_cases, na.rm = T))

br_biome_summary <- br %>%
  group_by(biome_name, Date) %>%
  summarise(cases = sum(dengue_cases, na.rm = T))

# Run susceptibility index analyses and save output
# save_dir <- '../data/susceptibility_index/Brazil_dengue_'
save_dir <- '../data/susceptibility_index/Dengue_'

susceptibility <- function(df, area_names){
  
  ## 2. Weekly disaggregation by area
  x <- as.data.frame(df)
  
  x <- format_for_temp_disagg(df = x, dateCol = 'Date')
  
  # split df to list
  x2 <- split_tibble(x, area_names)
  
  x2 <- lapply(x2, function(x) monthly_to_weekly_disagg(dfMonthly = x, dateCol = 'Date', casesCol = 'cases'))
  
  ## 3. Apply summaries by area and save
  outbreaks_summary <- lapply(x2, function(x)
    summarise_outbreaks(
      df = x
      , cases_colname = 'cases'
      , date_colname = 'Date'
      , disease = 'dengue'
    )
  )
  
  save(outbreaks_summary, file = paste0(save_dir, 'outbreaks_summary_', gsub('_[^_]*$', '', area_names), '.RData'))
  
  trends_summary <- lapply(outbreaks_summary, function(x) summarise_trends(df = x))
  
  save(trends_summary, file = paste0(save_dir, 'trends_summary_', gsub('_[^_]*$', '', area_names), '.RData'))
  
}

# Save Brazil data
susceptibility(df = br_biome_summary, area_names = 'biome_name')
susceptibility(df = br_state_summary, area_names = 'state_name')
susceptibility(df = br_meso_summary, area_names = 'meso_name')
susceptibility(df = br, area_names = 'micro_name')

# load('../data/susceptibility_index/Brazil_dengue_trends_summary_state.RData')
# load('../data/susceptibility_index/Brazil_dengue_outbreaks_summary_state.RData')

x <- do.call('rbind', trends_summary) %>% as.data.frame()
x$state <- row.names(x)

x2 <- x %>%
  # add column, mutate is not the correct notation
  # mutate('Country' == 'Brazil') %>%
  summarise(
    'Duration_sig_pos' = sum(Outbreak_duration_pvalue <= 0.05 & Outbreak_duration_slope > 0)
    , 'Duration_sig_neg' = sum(Outbreak_duration_pvalue <= 0.05 & Outbreak_duration_slope < 0)
    , 'R0_sig_pos' = sum(R0_pvalue <= 0.05 & R0_slope > 0)
    , 'R0_sig_neg' = sum(R0_pvalue <= 0.05 & R0_slope < 0)
  )



xx <- trends_summary[[1]] %>% as.data.frame()

x5 <- do.call('rbind', x4)
x5 <- as.data.frame(x5)
x5$biome <- row.names(x5)

summary(lm(as.numeric(xx$Year) ~ xx$R0))

## 4. Make plots: 1) R0 through time, 2) susceptibility index vs peak cases
x4 <- do.call('rbind', x3)
plot(x4$susceptibility_index, x4$R0, pch = 16, xlim = c(0,1.5), xlab = 'susceptibility index', ylab = 'R0')
plot(x4$susceptibility_index, x4$peak_cases, pch = 16, xlim = c(0,1.5), xlab = 'susceptibility index', ylab = 'Peak cases')

i = 87
ggplot(x3[[i]], aes(Year, R0)) + # can change Year with period       
  geom_point() +
  geom_errorbar(aes(ymin = R0_lower, ymax = R0_upper)) +
  theme_classic()

# plot(x3[[i]]$Year, x3[[i]]$R0, pch = 16)

plot(x3[[i]]$susceptibility_index, x3[[i]]$peak_cases, pch = 16)

plot(x3[[i]]$susceptibility_index, x3[[i]]$R0, pch = 16)

plot(x3[[i]]$Year, x3[[i]]$interoutbreak_duration, pch = 18, type = 'b', col = 'blue')
lines(x3[[i]]$Year, x3[[i]]$outbreak_duration, pch = 16, type = 'b')

### TYCHO data -------------------------------
outbreaks_summary <- lapply(tycho_data_formatted, function(x)
  summarise_outbreaks(
    df = x
    , cases_colname = 'Dengue_cases'
    , date_colname = 'Date'
    , disease = 'dengue'
  )
)

test_df <- tycho_data_formatted[[2]]
test_df <- subset(test_df, Admin_unit == test_df$Admin_unit[2])

xx <- summarise_outbreaks(
  df = test_df
  , cases_colname = 'Dengue_cases'
  , date_colname = 'Date'
  , disease = 'dengue'
)

xxx <- summarise_trends(df = xx)
