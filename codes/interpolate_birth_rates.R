# source custom functions
source('codes/functions_interpolate_birth_rates.R')

# load global birth rate data
br_data <- read.csv('../data/birth_rates/API_SP.DYN.CBRT.IN_DS2_en_csv_v2_3363348.csv', 
                    skip = 4,
                    head = TRUE)


# Thailand -----------------------------------------------
# load data
thailand_dengue <- read.csv('../data/dengue/project_tycho_THAILAND_weekly.csv')
cbr_by_region <- read.csv('../data/birth_rates/thailand_region_birth_rates.csv')
prov_x_region <- read.csv('../data/metadata/thailand_province_regions.csv')

thailand_CBR <- national_CBR(
  earliest_year = NA, 
  latest_year = NA,
  birth_rate_df = br_data,
  disease_df = thailand_dengue,
  countryName = 'Thailand'
  )

thailand_CBR_interpolated <- interpolate_regional_CBR(
  nat_CBR = thailand_CBR,
  regional_CBR = cbr_by_region
)

# add province information
thailand_CBR_interpolated <- thailand_CBR_interpolated %>%
  left_join(prov_x_region, by = 'Region') %>%
  filter(Year >= min(thailand_dengue$Year))

# save data
save(thailand_CBR_interpolated,
     file = '../data/birth_rates/Thailand_CBR_interpolated.RData')
