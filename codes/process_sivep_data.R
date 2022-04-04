# Format malaria data for the Amazonas region
library('lubridate')

# load data
malaria_files <- list.files('../data/malaria/', full.names = T)
malaria_files <- malaria_files[grep('UF_Full_Data', malaria_files)]

malaria_sivep_df <- data.frame()

for(i in malaria_files){
  x <- read.csv(i)
  malaria_sivep_df <- rbind(malaria_sivep_df, x)
}

# format dates
malaria_sivep_df$Date <- as.Date(malaria_sivep_df$BR.Data.de.NotificaÃ.Ã.o, '%m/%d/%Y')
malaria_sivep_df$Year <- format(malaria_sivep_df$Date, '%Y')
malaria_sivep_df$WOY <- week(malaria_sivep_df$Date)

# aggregate data
malaria_sivep_df <- malaria_sivep_df %>%
  group_by(Year, WOY, BR.EspÃ.cie.ParasitÃ.ria) %>%
  summarise(cases = length(BR.EspÃ.cie.ParasitÃ.ria)) %>%
  spread(key = BR.EspÃ.cie.ParasitÃ.ria, cases)

# format columns
colnames(malaria_sivep_df) <- gsub('. ', '_', colnames(malaria_sivep_df))

# subset data
malaria_sivep_df <- malaria_sivep_df[, c('Year'
                                         , 'WOY'
                                         , 'P_falciparum'
                                         , 'P_vivax')]

malaria_sivep_df$Admin1Name <- 'Amazonas'

# save
write.csv(malaria_sivep_df, '../data/malaria/brazil_amazonas_weekly.csv', row.names = F)
