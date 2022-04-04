# concatenate data for tsir models -----------------------

# source custom function to format data
source('codes/functions_format_tsir_data.R')

# Brazil -------------------------------------------------
source('codes/format_brazil_data.R')

# format data
brazil_dengue_tsir_data <- format_tsir_data(
  region_names = names(brazil_dengue_data_weekly)
  , disease_list = brazil_dengue_data_weekly
  , demography_list = brazil_demography
  , incidence_metric = 'cases'
  )

brazil_malaria_falciparum_tsir_data <- format_tsir_data(
  region_names = names(brazil_malaria_data_weekly)
  , disease_list = brazil_malaria_data_weekly
  , demography_list = brazil_demography
  , incidence_metric = 'P_falciparum'
)

brazil_malaria_vivax_tsir_data <- format_tsir_data(
  region_names = names(brazil_malaria_data_weekly)
  , disease_list = brazil_malaria_data_weekly
  , demography_list = brazil_demography
  , incidence_metric = 'P_vivax'
)

# Thailand -------------------------------------------------
source('codes/format_thailand_data.R')

# list Thai provinces
# format data
thailand_dengue_tsir_data <- format_tsir_data(
  region_names = names(thailand_dengue_data_weekly)
  , disease_list = thailand_dengue_data_weekly
  , demography_list = thailand_demography
  , incidence_metric = 'cases'
)

# Kenya ----------------------------------------------------
source('codes/format_kenya_data.R')

# format data
kenya_dengue_tsir_data <- format_tsir_data(
  region_names = names(kenya_dengue_data_weekly)
  , disease_list = kenya_dengue_data_weekly
  , demography_list = kenya_demography
  , incidence_metric = 'cases'
)
