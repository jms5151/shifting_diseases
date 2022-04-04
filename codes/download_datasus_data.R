# https://www.scielo.br/j/csp/a/gdJXqcrW5PPDHX8rwPDYL7F/?lang=pt#
# https://github.com/rfsaldanha/microdatasus/blob/master/R/process_sinan_malaria.R
# devtools::install_github('rfsaldanha/microdatasus')

# load libraries
library('tidyverse')
library('microdatasus')
library('read.dbc')
library('lubridate')

# data from 2007-2017 can be automatically downloaded: 
# malaria_df <- fetch_datasus(
#   year_start = 2007
#   , year_end = 2017
#   , information_system = "SINAN-MALARIA-FINAL"
#   )

# load data
malaria_files <- list.files("../data/malaria/", full.names = T)
malaria_files <- malaria_files[grep('MALABR', malaria_files)]

malaria_datasus_df <- data.frame()

for(i in malaria_files){
  x <- read.dbc(i)
  malaria_datasus_df <- rbind(malaria_datasus_df, x)
}

# pre-process data
malaria_datasus_df <- process_sinan_malaria(
  malaria_datasus_df
  , municipality_data = FALSE
  )

# format data
malaria_datasus_df$DT_NOTIFIC <- as.Date(malaria_datasus_df$DT_NOTIFIC, "%Y-%m-%d")
malaria_datasus_df$Year <- format(malaria_datasus_df$DT_NOTIFIC, '%Y')
malaria_datasus_df$WOY <- week(malaria_datasus_df$DT_NOTIFIC)

malaria_datasus_df <- malaria_datasus_df %>%
  group_by(Year, WOY, SG_UF_NOT, RESULT) %>% # other option is SG_UF, residence rather than notification?
  summarise(cases = length(RESULT)) %>%
  spread(key = RESULT, cases)%>%
  mutate('Admin1Name' = SG_UF_NOT
         , 'P_falciparum' = F
         , 'P_vivax' = V)

malaria_datasus_df <- malaria_datasus_df[,c('Year'
                                            , 'WOY'
                                            , 'P_falciparum'
                                            , 'P_vivax'
                                            , 'Admin1Name')]

# save
write.csv(malaria_datasus_df, '../data/malaria/brazil_exoamazonas_weekly.csv', row.names = F)

