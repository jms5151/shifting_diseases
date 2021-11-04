library(stringr)

source("../data/project_tycho_api_key.R")

# query locations from Project Tycho
locationNames <- paste0('https://www.tycho.pitt.edu/api/country?apikey=',
                     APIKEY)

locations <- read.table(locationNames,
                   header = TRUE,
                   sep=",",
                   fill=TRUE, 
                   quote="", 
                   encoding="UTF-8")

# there was an issue with name wrapping for Taiwan (line 90), remove extra row
extraRow <- which(locations$CountryName == "")
locations <- locations[-extraRow, ]

# create country names with no special characters for saving files
locations$countryName2 <- locations$CountryName
locations$countryName2 <- str_replace_all(locations$countryName2, 
                                          "[[:punct:]]", 
                                          "")
locations$countryName2 <- gsub(" ", "_", locations$countryName2)

# create function to pull and save country level disease data
pullTychoData <- function(diseaseName, countryISO){
  # create web address and download data
  # project tycho only allows downloads of 5000 rows at a time 
  htmlInfo <- paste0('https://www.tycho.pitt.edu/api/query?apikey=', 
                     APIKEY,
                     '&ConditionName=',
                     diseaseName,
                     '&CountryISO=',
                     countryISO)
  df <- read.table(htmlInfo,
                   header = TRUE, 
                   sep=",")
  # download in increments of 5000 if needed
  n <- 0
  df2 <- df
  while(nrow(df) == 5000){
    n <- n + 5000
    htmlInfo2 <- paste0(htmlInfo,
                        '&offset=',
                        n)
    df <- read.table(htmlInfo2,
                     header = TRUE, 
                     sep=",")
    df2 <- rbind(df2, df)
  }
  # output
  df2
}

# save data for all countries
for(i in 1:nrow(locations)){
  # download data
  x <- pullTychoData(diseaseName = "Dengue",
                     countryISO = locations$CountryISO[i])
  
  x$time_period <- difftime(x$PeriodEndDate, 
                            x$PeriodStartDate, 
                            unit = "weeks")
  
  x$time_period <- round(x$time_period)
  x$CountryName2 <- locations$countryName2[i]
  # create filename
  fileName <- paste0("../data/dengue/project_tycho_", 
                     locations$countryName2[i], 
                     ".csv")
  # save
  write.csv(x, fileName, row.names = F)
}




#####################################################################
### old ####
pullTychoData <- function(diseaseName, countryName, countryISO){
  # create web address and download data
  # project tycho only allows downloads of 5000 rows at a time 
  htmlInfo <- paste0('https://www.tycho.pitt.edu/api/query?apikey=', 
                     APIKEY,
                     '&ConditionName=',
                     diseaseName)
  # edit html address based on country name or ISO 
  if(!is.na(countryName)){
    htmlInfo <- paste0(htmlInfo,
                       '&CountryName=',
                       countryName)
  } else if(!is.na(countryISO)){
    htmlInfo <- paste0(htmlInfo,
                       '&CountryISO=',
                       countryISO)
  }
  df <- read.table(htmlInfo,
                   header = TRUE, 
                   sep=",")
  # download in increments of 5000 if needed
  n <- 0
  df2 <- df
  while(nrow(df) == 5000){
    n <- n + 5000
    htmlInfo2 <- paste0(htmlInfo,
                       '&offset=',
                       n)
    df <- read.table(htmlInfo2,
                     header = TRUE, 
                     sep=",")
    df2 <- rbind(df2, df)
  }
  # output
  df2
}

dengue_countries_of_interest <- c("China",
                                  "Viet Nam",
                                  NA,
                                  "Thailand")

dengue_countries_ISO_of_interest <- c(NA,
                                      NA,
                                      "LA",
                                      NA)

names <- c("China",
           "Vietnam",
           "Laos",
           "Thailand")


for(i in 1:length(names)){
  x <- pullTychoData(diseaseName = "Dengue",
                     countryName = dengue_countries_of_interest[i],
                     countryISO = dengue_countries_ISO_of_interest[i])
  fileName <- paste0("../data/dengue/project_tycho_", 
                     names[i], 
                     ".csv")
  write.csv(x, fileName, row.names = F)
}

library(ggmap)
geocode("ANHUI")
x$Date <- as.Date(x$PeriodStartDate, "%Y-%m-%d")
plot(x$Date[x$Admin1ISO=="TH-95"], 
     x$CountValue[x$Admin1ISO=="TH-95"], 
     type = 'l',
     xlab = '',
     ylab = '')
