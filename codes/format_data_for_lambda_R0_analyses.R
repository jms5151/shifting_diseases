# ------------------------------------------------------------------------------
# Dengue data 
# ------------------------------------------------------------------------------
library(tidyverse)

source('codes/functions_tibble_to_list.R')
source('codes/functions_fix_special_characters.R')

# do we want to do this?
dengue_data_filepaths <- list.files('../data/dengue/', full.names = T)
dengue_data_filepaths <- dengue_data_filepaths[grepl('weekly', dengue_data_filepaths)]
dengue_data_filepaths <- dengue_data_filepaths[!grepl('tycho_BRAZIL|tycho_COLOMBIA|tycho_PERU', dengue_data_filepaths)]

# load data
dengue_data <- lapply(dengue_data_filepaths, read.csv)

# Format spelling? Round cases? Flatten and then concat with other datasets
dengue_data <- lapply(dengue_data, function(x) format_capitalization_and_punctuation(df = x, colName = 'Country'))
dengue_data <- lapply(dengue_data, function(x) replace_char_in_list(df = x))

# combine
dengue_data_long <- do.call('rbind', dengue_data)

# format dataframe
colnames(dengue_data_long) <- gsub('cases', 'Dengue_cases', colnames(dengue_data_long)) 
dengue_data_long <- dengue_data_long[, c('Country', 'Admin_unit', 'Date', 'Dengue_cases')]

# CDC Forecasting challenge data  ------------------
SJ_dengue <- read.csv('../data/dengue/cdc_san_juan_training_data.csv')
IQ_dengue <- read.csv('../data/dengue/cdc_iquitos_training_data.csv')

SJ_dengue <- SJ_dengue %>%
  mutate(
    Date = as.Date(week_start_date, '%Y-%m-%d')
    , Country = 'Puerto Rico'
    , Admin_unit = 'San Juan'
    , Dengue_cases = total_cases
  ) %>%
  dplyr::select(Country, Admin_unit, Date, Dengue_cases)

IQ_dengue <- IQ_dengue %>%
  mutate(
    Date = as.Date(week_start_date, '%Y-%m-%d')
    , Country = 'Peru'
    , Admin_unit = 'Iquitos'
    , Dengue_cases = total_cases
  ) %>%
  dplyr::select(Country, Admin_unit, Date, Dengue_cases)

# Colombia data from McDonald lab ------------------
colombia_dengue <- read.csv('../data/dengue/dengue_colombia_char_fixed.csv', check.names = F)

# departments (source: https://en.wikipedia.org/wiki/Municipalities_of_Colombia)
departments <- read.csv('../data/regions/Colombia_muni_departments.csv', check.names = F)

# format names
colombia_dengue$Muni_Name <- replace_spanish_characters(x = colombia_dengue$Muni_Name)
departments$Muni_Name <- replace_spanish_characters(x = departments$Muni_Name)
departments$Department <- replace_spanish_characters(x = departments$Department)

# format df
colombia_dengue <- colombia_dengue %>%
  left_join(departments) %>%
  mutate(Country = 'Colombia'
         , 'Admin_unit' = Department
         , 'Date' = as.Date(Week_Yr, '%m/%d/%Y')
  ) %>%
  group_by(Country, Admin_unit, Date) %>%
  summarise(Dengue_cases = sum(Cases)) %>%
  dplyr::select(Country, Admin_unit, Date, Dengue_cases)

# Peru data from McDonald lab ----------------------
peru_dengue <- read.csv('../data/dengue/dengue_peru.csv')
peru_dengue$District <- replace_spanish_characters(x = peru_dengue$District)

# add provinces (source: https://en.wikipedia.org/wiki/Provinces_of_Peru)
peru_provinces <- read.csv('../data/regions/Peru_districts_provinces.csv')
peru_provinces$District <- replace_spanish_characters(x = peru_provinces$District)

peru_dengue <- peru_dengue %>%
  left_join(peru_provinces) %>%
  mutate(Country = 'Peru'
         , 'Admin_unit' = Province
         , 'Date' = as.Date(Week_Yr, '%Y-%m-%d')
         ) %>%
  group_by(Country, Admin_unit, Date) %>%
  summarise(Dengue_cases = sum(Cases)) %>%
  dplyr::select(Country, Admin_unit, Date, Dengue_cases)

# Combine -----------------------------------------
dengue_data <- rbind(
  dengue_data_long
  , colombia_dengue
  , IQ_dengue
  , SJ_dengue
  , peru_dengue
  ) 

# Add region, country column
dengue_data$Region <- paste0(dengue_data$Admin_unit, ', ', dengue_data$Country)

# split to list with df for each admin unit
dengue_data <- split_tibble(dengue_data, 'Region')

remove_regions_with_low_incidence <- function(df, cases_name){
  x <- sum(df[, cases_name])
  if(x < 150){
    return(NULL)
  } else {
    return(df)
  }
}

dengue_data <- lapply(
  dengue_data, 
  function(x) 
    remove_regions_with_low_incidence(
      df = x
      , cases_name = 'Dengue_cases'
      )
  )

dengue_data <- dengue_data[!sapply(dengue_data, is.null)]

# ------------------------------------------------------------------------------
# Malaria data 
# ------------------------------------------------------------------------------

# malaria_data_filepaths <- list.files('../data/malaria/', full.names = T)

# Brazil data --------------------------------------

# Colombia data from McDonald lab ------------------

# Peru data from McDonald lab ----------------------

# Mozambique, Uganda, and Ethiopia digitized data?

# Combine? -----------------------------------------

