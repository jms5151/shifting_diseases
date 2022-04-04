# take a list of disease and demography dataframes and format for tsir model 

# load library
library(tsiR)
library(tidyverse)

# set infectious period to 1 to keep weekly case data format
IP = 1

# custom function
format_tsir_data <- function(region_names, disease_list, demography_list, incidence_metric){
  # create empty list
  temp_tsir_data <- list()
  # go through each region and format tsir data 
  for(i in region_names){
    x1 <- disease_list[[i]]
    x1 <- x1 %>%
      complete(Admin1Name, Year, WOY)
    x1 <- as.data.frame(x1)
    x1[, incidence_metric][is.na(x1[, incidence_metric])] <- 0
    x2 <- demography_list[[i]]
    # this function isn't working properly!! 
    if(length(x1) > 0 & length(x2) > 0){ # if both datasets exist
      x3 <- tsiRdata(
        time = x1$Year
        , cases = x1[, incidence_metric]
        , births = x2$Total_live_births
        , pop = x2$Interpolated_pop_sum
        , IP = IP
      )
      x3$WOY <- x1$WOY
      # add to region data to list
      temp_tsir_data[length(temp_tsir_data) + 1] <- list(x3)
    } else { # remove region name from vector
      region_names <- region_names[! region_names %in% i]
    }
  }
  # add names to list
  names(temp_tsir_data) <- region_names
  # return list of formatted data
  return(temp_tsir_data)
  
}
