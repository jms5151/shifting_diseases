# https://cran.r-project.org/web/packages/tempdisagg/vignettes/hf-disagg.html

# load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(tempdisagg)
library(tsbox)
library(stringr)
library(ISOweek)

format_for_temp_disagg <- function(df, dateCol){
  # remove duplicated data
  df <- df[!duplicated(df), ]
  
  # order by date
  df <- df[order(df[, dateCol]), ]
  
  # format
  df$week_start_date <- as.Date(df[, dateCol], "%Y-%m-%d")
  df$YearMonth <- format(df[, dateCol], "%Y-%m")
  df$Year <- format(df[, dateCol], "%Y")
  df$WOY <- week(df[, dateCol])
  
  # return dataframe
  df
}

# may need to check for gaps in data there are missing weeks
# sj <- sj %>%
#   group_by(Year) %>%
#   complete(season_week = seq(1, 52, 1)) %>%
#   filter(week_start_date >= min(sj$week_start_date))

# Disaggregate from monthly to daily because
# irregular time interval is problematic and then
# aggregate to weekly

monthly_to_weekly_disagg <- function(dfMonthly, dateCol, casesCol){
  # create new data frame
  df <- data.frame()
  # loop through all months in data frame and disaggregate
  # from monthly to daily data
  for(i in 1:nrow(dfMonthly)){
    # calculate number of days in given month with data
    daysInMonth <- as.numeric(lubridate::days_in_month(dfMonthly[[i, dateCol]]))
    # dissaggregate to daily data
    sj_disagg <- td(dfMonthly[[i, casesCol]] ~ 1, 
                    conversion = "sum",
                    to = daysInMonth, 
                    method = "denton-cholette")
    # create a new dataframe with date and daily cases
    tempDF <- data.frame("Date" = seq.Date(from = dfMonthly[[i, dateCol]],
                                           to = dfMonthly[[i, dateCol]] + daysInMonth - 1,
                                           by = "days"),
                         "cases" = sj_disagg$values
    )
    df <- rbind(df, tempDF)
  }
  
  # format data
  df$Year <- format(df$Date, "%Y")
  df$WOY <- week(df$Date)
  
  # add date back in
  df$Date <- str_pad(df$WOY, 2, pad = '0')
  df$Date <- paste0(df$Year, '-W', df$Date, '-7')
  df$Date <- ISOweek::ISOweek2date(df$Date)
  
  # aggregate daily data to weekly values based on year and week of year (WOY)
  df2 <- df %>%
    group_by(Date) %>% #Year, WOY
    summarise(cases = sum(cases))

  df2$cases[df2$cases < 0] <- 0
  
  # return weekly cases dataframe
  as.data.frame(df2)
}

