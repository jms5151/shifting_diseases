# Format Kenya data ----------------------------
library(tidyverse)

# source custom function to split tibbles to list of dataframes
source('codes/functions_tibble_to_list.R')

# population data ------------------------------

# set up list of sites
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda")

# population size
population <- c(7304, 547557, 240698, 154048)

# combine
kenya_pop <- data.frame(
  'Admin1Name' = sites
  , 'Interpolated_pop_sum' = population
  ) 


# years 
years <- seq(2014, 2019, 1)

# set crude birth rates
CBR <- c(31.522, 30.688, 29.943, 29.296, 28.748, 28.298) # birth rates from https://data.worldbank.org/indicator/SP.DYN.CBRT.IN

# combine
kenya_cbr <- data.frame(
  'Year' = years
  , 'CBR' = CBR
  )
  
# expand dataframe for all locations and years
kenya_demography <- expand.grid(sites, years)
colnames(kenya_demography) <- c('Admin1Name', 'Year')

kenya_demography <- kenya_demography %>%
  left_join(kenya_pop) %>%
  left_join(kenya_cbr)

# calculate total births
kenya_demography$Total_live_births <- kenya_demography$CBR * (kenya_demography$Interpolated_pop_sum/1000)

# split df to list
kenya_demography <- split_tibble(kenya_demography, 'Admin1Name')

# dengue (really arboviruses) ------------------
kenya_dengue_weekly <- read.csv('../data/dengue/kenya_r01_arbovirus_data_weekly.csv')

# split df to list
kenya_dengue_data_weekly <- split_tibble(kenya_dengue_weekly, 'Admin1Name')

# malaria --------------------------------------