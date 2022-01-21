# summarize project tycho data

# function to format data
dz_format <- function(name, filepaths, df, casesVar, dataSource, otherColsToKeep){
  ids <- grep(name, filepaths)
  x <- do.call("rbind", df[ids])
  x$Disease <- "Dengue"
  x$Country <- name
  x$Total_cases <- x[,casesVar]
  x$Source <- dataSource
  x[, c("Disease", "Country", "Total_cases", "Source", otherColsToKeep)]
}

cdc_format <- function(df, adminName, countryName){
  df$Admin <- adminName
  df$Country <- countryName
  df$PeriodStartDate <- df$week_start_date
  df$PeriodStartDate <- as.Date(df$PeriodStartDate, "%Y-%m-%d")
  df$PeriodEndDate <- lead(df$PeriodStartDate) - 1
  df$time_period <- round(difftime(df$PeriodEndDate, df$PeriodStartDate, units = "weeks"))
  df[, c("Disease", 
         "Country", 
         "Admin", 
         "PeriodStartDate",
         "PeriodEndDate", 
         "time_period", 
         "Total_cases", 
         "Source")
     ]
}


# x$Date <- as.Date(x[, dateColName], "%Y-%m-%d")

# load files

dengue_filepaths <- list.files("../data/dengue", full.names = TRUE)

# load files into global environment
dengue_dfs = lapply(dengue_filepaths, read.csv)

# format CDC data ----------------------------------------------
iquitos <- dz_format(name = "iquitos",
                     filepaths = dengue_filepaths,
                     df = dengue_dfs, 
                     casesVar = "total_cases",
                     dataSource = "CDC",
                     otherColsToKeep = "week_start_date")

iquitos <- cdc_format(df = iquitos, 
                      adminName = "Iquitos",
                      countryName = "PERU")

sanjuan <- dz_format(name = "san_juan",
                     filepaths = dengue_filepaths,
                     df = dengue_dfs, 
                     casesVar = "total_cases",
                     dataSource = "CDC",
                     otherColsToKeep = "week_start_date")


sanjuan <- cdc_format(df = sanjuan, 
                      adminName = "San Juan",
                      countryName = "PUERTO RICO")

brazilID <- grep("brazil", dengue_filepaths)
brazil <- dengue_dfs[[brazilID]]

# combine and summarize data 
tychoIDs <- grep("tycho", dengue_filepaths)
tychoDF <- do.call("rbind", dengue_dfs[tychoIDs])


# USA is the only country with cumulative counts, but there's minimal 
# data so ignore, code below shows extent of data
# x <- tychoDFsum %>%
#   filter(CountryName2 == "UNITED_STATES_OF_AMERICA" & earliest > "1960-01-01") %>%
#   group_by(Admin1Name) %>%
#   summarise(timeperiod = max(Yrs))

tychoDFsum <- tychoDF %>%
  filter(PeriodStartDate > "1940-01-01") %>% # untrustworthy data earlier (really there's no data until after 1970s)
  group_by(CountryName2, Admin1Name) %>%
  summarise(min_time_period = min(time_period),
            median_time_period = median(time_period),
            max_time_period = max(time_period),
            earliest = min(PeriodStartDate),
            latest = max(PeriodEndDate),
            prop_zeros = sum(CountValue == 0)/length(CountValue)) %>%
  mutate("Yrs" = as.numeric(substr(latest, 1, 4)) - as.numeric(substr(earliest, 1, 4)))

# & prop_zeros < 0.8 may want to limit to higher levels of disease later

tychoDF_monthly <- tychoDFsum %>%
  filter(median_time_period <= 4) 

# ~400 unique admin units with monthly data

tychoDF_yearly <- tychoDFsum %>%
  filter(median_time_period == 52) 

# ~100 countries with yearly data