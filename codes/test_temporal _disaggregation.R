# Test disaggregation functions by comparing weekly data
# with weekly disaggregated data.
# This is done by first aggregating weekly data to monthly
# data and then disaggregating the monthly data.
# Finally, the actualy weekly data is compared with the 
# weekly disaggregated data
source("codes/functions_temporal_disaggregation.R")

# San Juan, PR ----------------------------------------------
x1 <- read.csv("../data/dengue/cdc_san_juan_training_data.csv")
x2 <- read.csv("../data/dengue/cdc_san_juan_testing_data.csv")
sj <- rbind(x1, x2)

sj <- format_for_temp_disagg(df = sj,
                             dateCol = "week_start_date")

# aggregate to monthly
sj_monthly <- sj %>%
  group_by(YearMonth) %>%
  summarise(total_cases = sum(total_cases)) %>%
  mutate(month_start_date = as.Date(paste0(YearMonth, "-01"), "%Y-%m-%d"))

sj_dis_weekly <- monthly_to_weekly_disagg(dfMonthly = sj_monthly,
                                          dateCol = "month_start_date",
                                          casesCol = "total_cases"
)

all_data <- sj %>%
  left_join(sj_dis_weekly, by = c("Year", "WOY"))

plot(all_data$week_start_date, 
     all_data$total_cases, 
     type = 'l', 
     xlab = "Date", 
     ylab = "cases",
     main = "San Juan, PR")
lines(all_data$week_start_date, all_data$cases, col = 'blue')
legend("topright",
       c("observed", "time disaggregated"),
       bty = 'n',
       col = c('black', 'blue'),
       lty = c(1, 1),
       lwd = c(2,2),
       cex = 1.2)

# Iquitos, Peru -------------------------------------------
x1 <- read.csv("../data/dengue/cdc_iquitos_training_data.csv")
x2 <- read.csv("../data/dengue/cdc_iquitos_testing_data.csv")
iq <- rbind(x1, x2)

iq <- format_for_temp_disagg(df = iq,
                             dateCol = "week_start_date")

# aggregate to monthly
iq_monthly <- iq %>%
  group_by(YearMonth) %>%
  summarise(total_cases = sum(total_cases)) %>%
  mutate(month_start_date = as.Date(paste0(YearMonth, "-01"), "%Y-%m-%d"))

iq_dis_weekly <- monthly_to_weekly_disagg(dfMonthly = iq_monthly,
                                          dateCol = "month_start_date",
                                          casesCol = "total_cases"
)

all_data <- iq %>%
  left_join(iq_dis_weekly, by = c("Year", "WOY"))

plot(all_data$week_start_date, 
     all_data$total_cases, 
     type = 'l', 
     xlab = "Date", 
     ylab = "cases",
     main = "Iquitos, Peru")
lines(all_data$week_start_date, all_data$cases, col = 'blue')
legend("topleft",
       c("observed", "time disaggregated"),
       bty = 'n',
       col = c('black', 'blue'),
       lty = c(1, 1),
       lwd = c(2,2),
       cex = 1.2)
