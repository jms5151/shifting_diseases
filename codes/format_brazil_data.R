library(tidyverse)
library(stringr)

# source custom function to split tibbles to list of dataframes
source('codes/functions_tibble_to_list.R')

# population data ------------------------------------------
# load data
load('../data/population/admin_unit_pop_count_interpolated.RData')

# subset to Brazil data
brazil_pop <- subset(admin_pop_full, ADM0_NAME == 'Brazil')
brazil_pop$Admin1Name <- brazil_pop$ADM1_NAME

# microregion-state data -----------------------------------
# load data
orig_dengue_data <- read.csv('../data/dengue/Lowe_etal_LPH_2021_brazil_dengue_data_2000_2019.csv')

# subset to microregion and state data
orig_dengue_data <- orig_dengue_data[, c('micro_code', 'state_name')]
orig_dengue_data <- orig_dengue_data[!duplicated(orig_dengue_data), ]

# convert Spanish to English characters for state names
orig_dengue_data$Admin1Name <- iconv(orig_dengue_data$state_name, "latin1", "ASCII//TRANSLIT")
orig_dengue_data$Admin1Name <- gsub('[[:punct:]]', '', orig_dengue_data$Admin1Name)
orig_dengue_data$Admin1Name <- str_to_title(orig_dengue_data$Admin1Name)

# fix specific names
orig_dengue_data$Admin1Name <- gsub('Esparito Santo', 'Espirito Santo', orig_dengue_data$Admin1Name)
orig_dengue_data$Admin1Name <- gsub('Paraaba', 'Paraiba', orig_dengue_data$Admin1Name)
orig_dengue_data$Admin1Name <- gsub('Piaua', 'Piaui', orig_dengue_data$Admin1Name)
orig_dengue_data$Admin1Name <- gsub('Rondania', 'Rondonia', orig_dengue_data$Admin1Name)

# if using microregion names - convert Spanish to English characters
# orig_dengue_data$micro_name <- gsub('<c2>', 'A', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<e1>', 'a', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<e3>', 'a', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<e2>', 'a', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<e7>', 'c', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<e9>', 'e', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<ea>', 'e', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<cd>', 'I', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<ed>', 'i', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<d3>', 'O', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<f5>', 'o', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<f3>', 'o', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<f4>', 'o', orig_dengue_data$micro_name)
# orig_dengue_data$micro_name <- gsub('<fa>', 'u', orig_dengue_data$micro_name)

# birth data -----------------------------------------------
# load data
brazil_live_births <- read.csv('../data/birth_rates/Brazil_live_births_by_microregion_2000-2019.csv')

# format microregion names and code
brazil_live_births$micro_code <- as.integer(substr(brazil_live_births$Microregion_IBGE, 1, 5))
# brazil_live_births$micro_name <- substr(brazil_live_births$Microregion_IBGE, 7, nchar(brazil_live_births$Microregion_IBGE))

# if using microregions, convert Spanish to English characters 
# brazil_live_births$micro_name <- iconv(brazil_live_births$micro_name, "latin1", "ASCII//TRANSLIT")
# brazil_live_births$micro_name <- gsub('^Ignorado.*', 'Ignorado', brazil_live_births$micro_name)

# combine population and birth data ------------------------
brazil_demography <- brazil_live_births %>%
  left_join(orig_dengue_data, by = 'micro_code') %>%
  group_by(Admin1Name, Year) %>%
  summarise('Total_live_births' = sum(Live_births_mothers_residence)) %>%
  left_join(brazil_pop, by = c('Admin1Name', 'Year')) %>%
  filter(!is.na(Admin1Name)) %>%
  dplyr::select(-c('ADM0_NAME'
            , 'ADM1_NAME'
            , 'system.index'
            , 'sum'))

# split df to list
brazil_demography <- split_tibble(brazil_demography, 'Admin1Name')

# dengue data ---------------------------------------------
# load data
brazil_dengue_data_weekly <- read.csv('../data/dengue/Lowe_etal_LPH_2021_brazil_dengue_data_2000_2019_weekly.csv')

# convert Spanish to English characters
brazil_dengue_data_weekly$Admin1Name <- iconv(brazil_dengue_data_weekly$Admin1Name, "latin1", "ASCII//TRANSLIT")
brazil_dengue_data_weekly$Admin1Name <- gsub('[[:punct:]]', '', brazil_dengue_data_weekly$Admin1Name)
brazil_dengue_data_weekly$Admin1Name <- str_to_title(brazil_dengue_data_weekly$Admin1Name)

# fix specific names
brazil_dengue_data_weekly$Admin1Name <- gsub('Esparito Santo', 'Espirito Santo', brazil_dengue_data_weekly$Admin1Name)
brazil_dengue_data_weekly$Admin1Name <- gsub('Paraaba', 'Paraiba', brazil_dengue_data_weekly$Admin1Name)
brazil_dengue_data_weekly$Admin1Name <- gsub('Piaua', 'Piaui', brazil_dengue_data_weekly$Admin1Name)
brazil_dengue_data_weekly$Admin1Name <- gsub('Rondania', 'Rondonia', brazil_dengue_data_weekly$Admin1Name)

# split df to list
brazil_dengue_data_weekly <- split_tibble(brazil_dengue_data_weekly, 'Admin1Name')

# malaria data ----------------------------------------------
brazil_a <- read.csv('../data/malaria/brazil_amazonas_weekly.csv')
brazil_b <- read.csv('../data/malaria/brazil_exoamazonas_weekly.csv')

# combine data
brazil_malaria_data_weekly <- rbind(brazil_a, brazil_b)
brazil_malaria_data_weekly <- brazil_malaria_data_weekly %>%
  filter(Year < 2020)

# format state names
brazil_malaria_data_weekly$Admin1Name <- iconv(brazil_malaria_data_weekly$Admin1Name, "latin1", "ASCII//TRANSLIT")
brazil_malaria_data_weekly$Admin1Name <- str_to_title(brazil_malaria_data_weekly$Admin1Name)

# fix specific names
brazil_malaria_data_weekly$Admin1Name <- gsub('Esparito Santo', 'Espirito Santo', brazil_malaria_data_weekly$Admin1Name)
brazil_malaria_data_weekly$Admin1Name <- gsub('Paraaba', 'Paraiba', brazil_malaria_data_weekly$Admin1Name)
brazil_malaria_data_weekly$Admin1Name <- gsub('Piaua', 'Piaui', brazil_malaria_data_weekly$Admin1Name)
brazil_malaria_data_weekly$Admin1Name <- gsub('Rondania', 'Rondonia', brazil_malaria_data_weekly$Admin1Name)
brazil_malaria_data_weekly$Admin1Name <- gsub('Tocantis', 'Tocantins', brazil_malaria_data_weekly$Admin1Name)

# split df to list
brazil_malaria_data_weekly <- split_tibble(brazil_malaria_data_weekly, 'Admin1Name')

