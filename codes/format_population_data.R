library(tidyverse)

# interpolate between census years, may eventually do this in GEE -------
# load data
admin_pop <- read.csv('../data/population/admin_unit_pop_count.csv')
# format data
admin_pop$Year <- as.numeric(substr(admin_pop$system.index, 31, 34))
# create empty dataframe
admin_pop_full <- data.frame()
# list administration units to loop through
admin_units <- unique(admin_pop$ADM1_NAME)

# interpolate missing data by administration unit
for(i in admin_units){
  # subset data by region
  x <- subset(admin_pop, ADM1_NAME == i)
  # run linear model
  linearMod <- lm(sum ~ Year, data = x)
  # fill in missing years
  x_complete <- x %>%
    group_by(ADM0_NAME, ADM1_NAME) %>%
    complete(Year = seq(1995, 2020, 1)) %>%
    filter(Year >= 1995)
  # interpolate population sum for missing years
  x_complete$Interpolated_pop_sum <- predict(linearMod, newdata = x_complete)
  # reduce population by 10% if interpolated to below zero (only affects one admin unit)
  if(min(x_complete$Interpolated_pop_sum) < 0){
    x_complete$Interpolated_pop_sum[x_complete$Interpolated_pop_sum < 0] <- min(x_complete$Interpolated_pop_sum[x_complete$Interpolated_pop_sum > 0]) * 0.90
  }
  # add new data to dataset
  admin_pop_full <- rbind(admin_pop_full, x_complete)
}

# save data
save(admin_pop_full, file = '../data/population/admin_unit_pop_count_interpolated.RData')


# Adjust above code for pop density -------------------------------------------
# load data
country_pop_density <- read.csv('../data/population/country_pop_density.csv')
# format data
country_pop_density <- subset(country_pop_density, !is.na(mean))
country_pop_density$Year <- as.numeric(substr(country_pop_density$system.index, 71, 74))
# create empty dataframe
country_pop_density_full <- data.frame()
# list administration units to loop through
countries <- unique(country_pop_density$ADM0_NAME)
# list density metrics
dens_metrics <- c('mean', 'median', 'min', 'max')

# interpolate missing data by administration unit
for(i in countries){
  # subset data by region
  x <- subset(country_pop_density, ADM0_NAME == i)
  # fill in missing years
  x_complete <- x %>%
    group_by(ADM0_NAME) %>%
    complete(Year = seq(2000, 2020, 1)) %>%
    filter(Year >= 2000)
  # interpolate population sum for missing years
  x_complete$Interpolated_pop_density_median <- predict(lm(median ~ Year, data = x), newdata = x_complete)
  x_complete$Interpolated_pop_density_mean <- predict(lm(mean ~ Year, data = x), newdata = x_complete)
  x_complete$Interpolated_pop_density_min <- predict(lm(min ~ Year, data = x), newdata = x_complete)
  x_complete$Interpolated_pop_density_max <- predict(lm(max ~ Year, data = x), newdata = x_complete)
  # add new data to dataset
  country_pop_density_full <- rbind(country_pop_density_full, x_complete)
}

# save data
save(country_pop_density_full, file = '../data/population/country_pop_density_interpolated.RData')
