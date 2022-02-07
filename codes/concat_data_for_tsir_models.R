# concatenate data for tsir models -----------------------

# source custom function to format data
source('codes/functions_format_tsir_data.R')

# Brazil -------------------------------------------------
source('codes/format_brazil_data.R')

# list Brazilian states
brazilian_states <- names(brazil_dengue_data_weekly)

# format data
brazil_tsir_data <- format_tsir_data(
  region_names = brazilian_states
  , disease_list = brazil_dengue_data_weekly
  , demography_list = brazil_demography
  )

# Thailand -------------------------------------------------
source('codes/format_thailand_data.R')

# list Thai provinces
thailand_provinces <- names(thailand_dengue_data_weekly)

# format data
thailand_tsir_data <- format_tsir_data(
  region_names = thailand_provinces
  , disease_list = thailand_dengue_data_weekly
  , demography_list = thailand_demography
)
