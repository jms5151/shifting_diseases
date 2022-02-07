library(tidyverse)
library(stringr)

# source custom function to split tibbles to list of dataframes
source('codes/functions_tibble_to_list.R')

# population data ------------------------------------------
# load data
load('../data/population/admin_unit_pop_count_interpolated.RData')

# subset to Thailand data
# missing data from Samut Songkhram
thailand_pop <- subset(admin_pop_full, ADM0_NAME == 'Thailand')

# format province names
thailand_pop$Admin1Name <- gsub('Bangkok', 'Krung Thep Maha Nakhon', thailand_pop$ADM1_NAME)
thailand_pop$Admin1Name <- gsub('Phachinburi', 'Prachin Buri', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Buriram', 'Buri Ram', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Chainat', 'Chai Nat', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Chonburi', 'Chon Buri', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Lopburi', 'Lop Buri', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Kampaeng Phet', 'Kamphaeng Phet', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Phra Nakhon Si Ayudhya', 'Phra Nakhon Si Ayutthaya', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Prachuap Khilikhan', 'Prachuap Khiri Khan', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Samut Prakarn', 'Samut Prakan', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Si Saket', 'Si Sa Ket', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Singburi', 'Sing Buri', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Suphanburi', 'Suphan Buri', thailand_pop$Admin1Name)
thailand_pop$Admin1Name <- gsub('Trad', 'Trat', thailand_pop$Admin1Name)

# birth data -----------------------------------------------
# load data
load('../data/birth_rates/Thailand_CBR_interpolated.RData')

# format province names
thailand_CBR_interpolated$Admin1Name <- gsub('Bangkok.*', 'Krung Thep Maha Nakhon', thailand_CBR_interpolated$Province_original)
thailand_CBR_interpolated$Admin1Name <- gsub('Phang Nga', 'Phangnga', thailand_CBR_interpolated$Admin1Name)

thailand_CBR_interpolated$Admin1Name[thailand_CBR_interpolated$Province == 'Narathiwat'] <- 'Narathiwat'
thailand_CBR_interpolated$Admin1Name[thailand_CBR_interpolated$Province == 'Pattani'] <- 'Pattani'

# combine population and birth data ------------------------
thailand_demography <- thailand_CBR_interpolated %>%
  left_join(thailand_pop) %>%
  filter(!is.na(Admin1Name)) %>%
  filter(Year >= 1995) %>%
  mutate("Total_live_births" = Crude_birth_rate_interpolated * Interpolated_pop_sum/1000) %>%
  dplyr::select(-c('Crude_birth_rate'
            , 'Province_original'
            , 'Province'
            , "ADM0_NAME"
            , "ADM1_NAME"
            , "system.index"
            , "sum"))

thailand_demography <- thailand_demography[complete.cases(thailand_demography), ]

# split df to list
thailand_demography <- split_tibble(thailand_demography, 'Admin1Name')

# dengue data ---------------------------------------------
# load data
thailand_dengue_data_weekly <- read.csv('../data/dengue/project_tycho_THAILAND_weekly.csv')
thailand_dengue_data_weekly$Admin1Name <- str_to_title(thailand_dengue_data_weekly$Admin1Name)

# split df to list
thailand_dengue_data_weekly <- split_tibble(thailand_dengue_data_weekly, 'Admin1Name')

