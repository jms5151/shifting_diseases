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
  # subset data by ragion
  x <- subset(admin_pop, ADM1_NAME == i)
  # run linear model
  linearMod <- lm(sum ~ Year, data = x)
  # fill in missing years
  x_complete <- x %>%
    group_by(ADM0_NAME, ADM1_NAME) %>%
    complete(Year = seq(1995, 2020, 1))
  # interpolate population sum for missing years
  x_complete$Interpolated_pop_sum <- predict(linearMod, newdata = x_complete)
  # add new data to dataset
  admin_pop_full <- rbind(admin_pop_full, x_complete)
}

# save data
save(admin_pop_full, file = '../data/population/admin_unit_pop_count_interpolated.RData')

# test tsir model
# doesn't seem to work well at monthly scale
library(tsiR)
IP = 4
pr_times <- seq(earliest_year, latest_year, by = 1/(52/IP))

PR <- tsiRdata(time = pr_times,
               # cases = pr_data$cumulative_cases,
               cases = y$CountValue,
               births = (br_xx$birth_rate * xx$sum/1000),
               # births = pr_data$cumulative_births,
               pop = xx$sum,
               IP = IP)


PR_tsir <- runtsir(data = PR,
                   IP = IP,
                   xreg = 'cumcases',
                   regtype = 'lm', # gaussian produces errors
                   alpha = 0.74, #NULL, 
                   sbar = NULL,
                   family = 'poisson', #gaussian
                   link = 'log', #identity,
                   # inits.fit = T,
                   # epidemics = 'break',
                   method = 'negbin',
                   nsim = 100)

plotres(PR_tsir)

