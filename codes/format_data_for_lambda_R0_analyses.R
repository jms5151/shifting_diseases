# ------------------------------------------------------------------------------
# Dengue data 
# ------------------------------------------------------------------------------
library(tidyverse)

source('codes/functions_tibble_to_list.R')
source('codes/functions_fix_special_characters.R')


dengue_data_filepaths <- list.files('../data/dengue/', full.names = T)

# Maybe make a separate function for mutate, (group_by), and select since
# it's use for every dataset?

# project Tycho data ------------------------
# get filepaths
tycho_data_filepaths <- dengue_data_filepaths[grep('project_tycho', dengue_data_filepaths)]
tycho_data_filepaths <- tycho_data_filepaths[!grepl('BRAZIL|weekly', tycho_data_filepaths)]

# load data
tycho_data <- lapply(tycho_data_filepaths, read.csv)

format_tycho_fun <- function(df){
  df <- df %>%
    filter(time_period <= 4) %>%
    mutate(
      Date = as.Date(PeriodStartDate, '%Y-%m-%d')
      , Country = CountryName2
      , Admin_unit = Admin1Name
      , Dengue_cases = CountValue
    ) %>%
    dplyr::select(Country, Admin_unit, Date, Dengue_cases)
  
  if(nrow(df) != 0){
    df$Country <- gsub('_', ' ', df$Country)
    df$Country <- str_to_title(df$Country)
    df
    }
  }

tycho_data <- lapply(tycho_data, function(x) format_tycho_fun(df = x))
tycho_data <- tycho_data[!sapply(tycho_data, is.null)]

# combine
tycho_data_long <- do.call('rbind', tycho_data)

# # format
# tycho_data_long <- tycho_data_long %>%
#   filter(time_period <= 4) %>%
#   mutate(
#     Date = as.Date(PeriodStartDate, '%Y-%m-%d')
#     , Country = CountryName2
#     , Admin_unit = Admin1Name
#     , Dengue_cases = CountValue
#   ) %>%
#   dplyr::select(Country, Admin_unit, Date, Dengue_cases)
# 
# tycho_data_long$Country <- str_to_title(tycho_data_long$Country)
# x <- replace_spanish_characters(x = tycho_data_long$Admin_unit)

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

# Brazil data from Lowe et al. 2019 ----------------
brazil_dengue <- read.csv('../data/dengue/Lowe_etal_LPH_2021_brazil_dengue_data_2000_2019.csv')
brazil_dengue$state_name <- replace_spanish_characters(x = brazil_dengue$state_name)

# format and summarize by state
brazil_dengue <- brazil_dengue %>%
  mutate(Country = 'Brazil'
         , 'Admin_unit' = state_name
         , 'Date' = as.Date(paste0(brazil_dengue$year, '-', brazil_dengue$month, '-01'), '%Y-%m-%d')
         ) %>%
  group_by(Country, Admin_unit, Date) %>%
  summarise(Dengue_cases = sum(dengue_cases, na.rm = T)) %>%
  as.data.frame()

# Colombia data from McDonald lab ------------------
colombia_dengue <- read.csv('../data/dengue/dengue_colombia_char_fixed.csv', check.names = F)

# add departments (source: https://en.wikipedia.org/wiki/Municipalities_of_Colombia)
departments <- read.csv('../data/regions/Colombia_muni_departments.csv', check.names = F)

# format names
departments$Muni_Name <- iconv(departments$Muni_Name, "latin1", "ASCII//TRANSLIT")
departments$Muni_Name <- gsub('^ ', '', departments$Muni_Name)
departments$Muni_Name <- toupper(departments$Muni_Name)
departments$Department <- iconv(departments$Department, "latin1", "ASCII//TRANSLIT")

colombia_dengue <- colombia_dengue %>%
  left_join(departments) %>%
  mutate(Country = 'Colombia'
         , 'Admin_unit' = Department
         , 'Date' = as.Date(Week_Yr, '%Y-%m-%d')
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
  tycho_data_long
  , brazil_dengue
  , colombia_dengue
  , IQ_dengue
  , SJ_dengue
  ) # , peru_dengue

# format?
dengue_data$Region <- paste0(dengue_data$Admin_unit, ', ', dengue_data$Country)

# combine long and then split?
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

