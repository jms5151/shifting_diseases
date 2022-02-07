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
  # add new data to dataset
  admin_pop_full <- rbind(admin_pop_full, x_complete)
}

# save data
save(admin_pop_full, file = '../data/population/admin_unit_pop_count_interpolated.RData')
