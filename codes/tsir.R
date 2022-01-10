# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0185528

library(tsiR)
library(tidyverse)
library(data.table)

# load data
x <- read.csv("../data/dengue/cdc_san_juan_training_data.csv")
x2 <- read.csv("../data/dengue/cdc_san_juan_testing_data.csv")
xx <- rbind(x, x2)

pop <- read.csv("../data/population/cdc_san_juan_population_data.csv")

BRpr <- read.csv("../data/birth_rates/puerto_rico_birth_rates.csv")
BR <- read.csv("../data/birth_rates/API_SP.DYN.CBRT.IN_DS2_en_csv_v2_3363348.csv",
               skip = 4)

# format
xx$Date <- as.Date(xx$week_start_date, "%Y-%m-%d")
xx$Year <- as.integer(substr(xx$week_start_date, 1, 4))
xx$WOY <- week(xx$Date)
# THIS IS WHAT I WANT TO DO FOR USING SERIAL INTERVAL, IF USING MONTH, IT'S
# NOT NECESSARY
xx$WOY <- ifelse((xx$WOY %% 2) == 0, xx$WOY, xx$WOY + 1)

xx <- xx %>%
  group_by(Year, WOY) %>%
  summarise(total_cases = sum(total_cases))

BRpr$Year <- as.integer(substr(BRpr$date, 7, 10))

# combine
pr_data <- xx %>%
  left_join(pop) %>%
  left_join(BRpr) %>%
  # better to interpolate values so don't need to cutoff at specified year
  filter(Year >= 2000)

# don't really need this, but useful to see
# cumulative values
pr_data$cumulative_cases <- cumsum(pr_data$total_cases)
pr_data$cumulative_births <- cumsum((pr_data$Estimated_population/1000) * pr_data$Births.per.1000.People)

plot(pr_data$cumulative_births, pr_data$cumulative_cases, pch = 16)

# MAY WANT TO CHANGE IP, CURRENTLY SET AT 2 WEEKS
IP = 2
pr_times <- seq(min(pr_data$Year), max(pr_data$Year), by = 1/(52/IP))

PR <- tsiRdata(time = pr_times,
               # cases = pr_data$cumulative_cases,
               cases = pr_data$total_cases,
               births = (pr_data$Births.per.1000.People * pr_data$Estimated_population/1000),
               # births = pr_data$cumulative_births,
               pop = pr_data$Estimated_population,
               IP = 2)


PR_tsir <- runtsir(data = PR,
                   IP = 2,
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

PR_Parms <- estpars(data = PR, IP = 2,
                          
                          alpha = 0.74, sbar = NULL,
                          
                          regtype = 'loess',
                          
                          family = 'poisson', link = 'log')

plotbreaks (data = PR, threshold = 3)

PR_Res <- simulatetsir(data = PR, IP = 2,
                             
                             parms = PR_Parms,
                             
                             epidemics = 'break', threshold = 3,
                             
                             method = 'pois', nsim = 100)

plotcomp (PR_Res)

data("twentymeas")
x <- twentymeas[["London"]]
plot(x$births, x$cases, pch = 16, type = 'b')

x$cumsum_cases <- cumsum(x$cases)
x$cumsum_births <- cumsum(x$births*x$pop)
plot(x$cumsum_births, x$cumsum_cases, pch = 16, type = 'b', xlab = "cumulative births", ylab = "cumulative cases")
lmx <- lm(x$cumsum_cases ~ x$cumsum_births)
abline(lmx, col = "red", lwd = 2)

# reporting rate
rho = lmx$coefficients[[2]]

# residuals, shape of susceptible population
Zt <- resid(lmx)

plot.ts(Zt)

## Caroline Wagner's Proc B code with dengue: 
# https://github.com/cewagner/Dengue-Sri-Lanka/blob/master/DengueSL_tsiR_TempBriere_09232019.R
