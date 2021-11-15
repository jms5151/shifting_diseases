# https://www.sharpsightlabs.com/blog/map-oil-production-country-r/

# load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tools)

# load files
dengue_filepaths <- list.files("../data/dengue", full.names = TRUE)

# load files into global environment
dengue_dfs = lapply(dengue_filepaths, read.csv)

# get world map data
map.world <- map_data('world')

# format data ------------------------------------------------------
tychoIDs <- grep("tycho", dengue_filepaths)
tychoDF <- do.call("rbind", dengue_dfs[tychoIDs])

# format country name to match map.world
tychoDF$country <- tolower(tychoDF$CountryName2)
tychoDF$country <- gsub("_", " ", tychoDF$country)
tychoDF$country <- tools::toTitleCase(tychoDF$country) # slow function, not sure why

# find mismatched country names
mismatches <- anti_join(tychoDF, map.world, by = c('country' = 'region'))
unique(mismatches$country)

# list map.world country names
map.world %>%
  group_by(region) %>%
  summarise() %>%
  print(n = Inf)

# update mismatched country names in dataset
tychoDF_updated <- tychoDF %>% 
  mutate(country = recode(country
                          , 'Antigua and Barbuda' = "Antigua"
                          , "Bolivia Plurinational State of" = "Bolivia"
                          , 'Brunei Darussalam' = "Brunei"
                          , "Curaçao" = "Curacao"
                          , "Hong Kong" = "China"
                          , "Korea Republic of" = "South Korea"
                          , "Lao Peoples Democratic Republic" = "Laos"
                          , "Macao" = "China"
                          , "Micronesia Federated States of" = "Micronesia"
                          , "Pitcairn" = "Pitcairn Islands"
                          , "Saint Barthélemy" = "Saint Barthelemy"
                          , "Saint Kitts and Nevis" = "Saint Kitts"
                          , "Saint Martin French Part" = "Saint Martin"
                          , "Saint Vincent and the Grenadines" = "Saint Vincent"
                          , "Timorleste" = "Timor-Leste"
                          , "Tokelau" = "New Zealand"
                          , "Trinidad and Tobago" = "Trinidad"
                          # , "Tuvalu" # not included in world.map:(
                          , "United States of America" = "USA"
                          , "Venezuela Bolivarian Republic of" = "Venezuela"
                          , "Viet Nam" = "Vietnam"
                          , "Virgin Islands British" = "Virgin Islands"
                          , "Virgin Islands Us" = "Virgin Islands"
                          ))

tychoDFsum <- tychoDF %>%
  group_by(country, Admin1Name) %>%
  filter(PeriodStartDate > "1970-01-01") %>%
  summarise(median_time_period = median(time_period),
            earliest = min(PeriodStartDate),
            latest = max(PeriodEndDate)) %>%
  mutate("Yrs" = as.numeric(substr(latest, 1, 4)) - as.numeric(substr(earliest, 1, 4))) 

tychoDFsum <- as.data.frame(tychoDFsum)

tychoDF_monthly <- tychoDFsum %>%
  filter(median_time_period <= 4) %>%
  group_by(country) %>%
  summarise("Yrs" = max(Yrs))
  
tychoDF_yearly <- tychoDFsum %>%
  filter(median_time_period == 52) %>%
  group_by(country) %>%
  summarise("Yrs" = max(Yrs))

# join data with world map data
coverage_yearly <- left_join(map.world, tychoDF_yearly, by = c('region' = 'country'))
coverage_monthly <- left_join(map.world, tychoDF_monthly, by = c('region' = 'country'))

# create mapping function -----------------------------------------------
mapFun <- function(df, response, titleName){
  ggplot(df, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = response)) +
    # scale_fill_gradientn(colours = standardColors
    #                      ,labels = standardLabels
    #                      ,breaks = standardBreaks
    # ) +
    guides(fill = guide_legend(reverse = T)) +
    labs(fill = 'Years of coverage'
         ,title = titleName
         ,x = NULL
         ,y = NULL
    ) +
    theme(text = element_text(color = 'black')
          ,plot.title = element_text(size = 28)
          ,plot.subtitle = element_text(size = 14)
          ,axis.ticks = element_blank()
          ,axis.text = element_blank()
          ,panel.grid = element_blank()
          ,panel.background = element_rect(fill = 'white')
          ,plot.background = element_rect(fill = 'white')
          ,legend.position = c(0.10, 0.36)
          ,legend.background = element_blank()
          ,legend.key = element_blank()
    )
}


# create maps ---------------------------------------------------------------
fig_dir <- "../figures/maps/dengue_"

yearlyMap <- mapFun(df = coverage_yearly,
         response = coverage_yearly[, "Yrs"],
         titleName = "Global dengue coverage - annual data"
         )

ggsave(file = paste0(fig_dir, 'coverage_annual.pdf'), yearlyMap)

monthlyMap <- mapFun(df = coverage_monthly,
                     response = coverage_monthly[, "Yrs"],
                     titleName = "Global dengue coverage - monthly data"
                     )

ggsave(file = paste0(fig_dir, 'coverage_monthly.pdf'), monthlyMap)
