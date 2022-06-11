# ------------------------------------------------------------------------------
# Dengue data 
# ------------------------------------------------------------------------------
source('codes/functions_tibble_to_list.R')

dengue_data_filepaths <- list.files('../data/dengue/', full.names = T)

# project Tycho data ------------------------
# get filepaths
tycho_data_filepaths <- dengue_data_filepaths[grep('project_tycho', dengue_data_filepaths)]
tycho_data_filepaths <- tycho_data_filepaths[!grepl('BRAZIL|weekly', tycho_data_filepaths)]

# load data
tycho_data <- lapply(tycho_data_filepaths, read.csv)

# combine
tycho_data_long <- do.call('rbind', tycho_data)

# format
tycho_data_long <- tycho_data_long %>%
  filter(time_period <= 4) %>%
  mutate(
    Date = as.Date(PeriodStartDate, '%Y-%m-%d')
    , Country = CountryName2
    , Admin_unit = 'Admin1Name'
    , Dengue_cases = CountValue
  ) %>%
  dplyr::select(Country, Admin_unit, Date, Dengue_cases)


# CDC Forecasting challenge data  ------------------
# # get filepaths
# cdc_data_filepaths <- dengue_data_filepaths[grep('cdc', dengue_data_filepaths)]
# 
# # load data
# cdc_data <- lapply(cdc_data_filepaths, read.csv)
# 
# cdcNames <- gsub('../data/dengue/cdc_|_training|_data.csv', '', cdc_data_filepaths)

# Brazil data from Lowe et al. 2019 ----------------
brazil_dengue <- read.csv('../data/dengue/Lowe_etal_LPH_2021_brazil_dengue_data_2000_2019.csv')

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
colombia_dengue <- read.csv('../data/dengue/dengue_colombia.csv')

# need to add muni:department: https://en.wikipedia.org/wiki/Municipalities_of_Colombia

colombia_dengue <- colombia_dengue %>%
  mutate(Country = 'colombia'
         , 'Admin_unit' = Muni
         , 'Date' = as.Date(Week_Yr, '%Y-%m-%d')
         , 'Dengue_cases' = Cases) %>%
  dplyr::select(Country, Admin_unit, Date, Dengue_cases)

# Peru data from McDonald lab ----------------------
peru_dengue <- read.csv('../data/dengue/dengue_peru.csv')

peru_dengue <- peru_dengue %>%
  mutate(Country = 'Peru'
         , 'Admin_unit' = District
         , 'Date' = as.Date(Week_Yr, '%Y-%m-%d')
         , 'Dengue_cases' = Cases) %>%
  dplyr::select(Country, Admin_unit, Date, Dengue_cases)

# Combine? -----------------------------------------
dengue_data <- rbind(tycho_data_long, brazil_dengue, colombia_dengue, peru_dengue)

# combine long and then split?
dengue_data <- split_tibble(dengue_data, 'Admin_unit')


# ------------------------------------------------------------------------------
# Malaria data 
# ------------------------------------------------------------------------------

# malaria_data_filepaths <- list.files('../data/malaria/', full.names = T)

# Brazil data --------------------------------------

# Colombia data from McDonald lab ------------------

# Peru data from McDonald lab ----------------------

# Mozambique, Uganda, and Ethiopia digitized data?

# Combine? -----------------------------------------

