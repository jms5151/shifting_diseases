# concatenate data for tsir models -----------------------
library(tsiR)
library(tidyverse)

# set infectious period to 1 to keep weekly case data format
IP = 1

# Brazil -------------------------------------------------
## need to run for each state
source('codes/format_brazil_data.R')

brazil_times <- brazil_dengue_data_weekly$Year

brazil_tsir_data <- tsiRdata(time = brazil_times
                             , cases = brazil_dengue_data_weekly$cases
                             , births = brazil_demography$Total_live_births
                             , pop = brazil_demography$Interpolated_pop_sum
                             , IP = IP)

brazil_tsir_data <- brazil_tsir_data %>%
  left_join(brazil_dengue_data_weekly)

# Thailand -----------------------------------------------
## need to run for each province
source('codes/format_thailand_data.R')

thailand_times <- thailand_dengue_data_weekly$Year

thailand_tsir_data <- tsiRdata(time = thailand_times
                             , cases = thailand_dengue_data_weekly$cases
                             , births = (thailand_demography$Crude_birth_rate_interpolated * thailand_demography$Interpolated_pop_sum/1000)
                             , pop = thailand_demography$Interpolated_pop_sum
                             , IP = IP)

