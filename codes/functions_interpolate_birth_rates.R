# load library
library(tidyverse)

# format national crude birth rates ----------------------
national_CBR <- function(earliest_year, latest_year, birth_rate_df, disease_df, countryName){
  # get year range
  if(is.na(earliest_year)){
    earliest_year <- min(disease_df$Year)
  }
  if(is.na(latest_year)){
    latest_year <- max(disease_df$Year)
  }
  # find rows and columns associated with disease in country of interest
  cbr_row <- which(birth_rate_df$Country.Name == countryName)
  cbr_col_start <- which(colnames(birth_rate_df) == paste0("X", earliest_year))
  cbr_col_end <- which(colnames(birth_rate_df) == paste0("X", latest_year))
  # subset and format data
  cbr <- birth_rate_df[cbr_row, cbr_col_start : cbr_col_end]
  cbr <- cbr %>% 
    gather(key = "Year", "Crude_birth_rate")
  cbr$Year <- as.integer(gsub("X", "", cbr$Year))
  cbr$Region <- "National"
  cbr
}

# interpolate regional crude birth rates ----------------------
interpolate_regional_CBR <- function(nat_CBR, regional_CBR){
  # combine data
  df <- bind_rows(
    nat_CBR,
    regional_CBR
  )
  # get year ranges
  minYear <- min(df$Year)
  maxYear <- max(df$Year)
  # model relationship between year and CBR
  CBR_mod <- lm(Crude_birth_rate ~ Year + Region, data = df)
  # expand dataset to include all years and regions
  df_expanded <- df %>%
    group_by(Region) %>%
    complete(Year = seq(minYear, maxYear, by = 1))
  # predict CBR for all years and regions
  df_expanded$Crude_birth_rate_interpolated <- predict(
    object = CBR_mod,
    newdata = df_expanded
  )
  df_expanded
}
