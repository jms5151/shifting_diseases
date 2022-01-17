library(tidyverse)

# dengue data -----------------------------------
brazil_dengue <- read.csv("../data/dengue/project_tycho_BRAZIL.csv")
## better dengue data
brazil <- read.csv('../data/dengue/Lowe_etal_LPH_2021_brazil_dengue_data_2000_2019.csv')

y <- subset(brazil_dengue, Admin1Name == "ACRE")
plot.ts(y$CountValue, type = 'b')

earliest_year <- substr(min(y$PeriodStartDate), 1, 4)
latest_year <- substr(max(y$PeriodEndDate), 1, 4)

# admin population data ------------------------- 
admin_pop <- read.csv("../data/population/admin_unit_pop_count.csv")
admin_pop$Year <- as.numeric(substr(admin_pop$system.index, 31, 34))

x <- subset(admin_pop, ADM1_NAME == "Acre")

lmx <- lm(sum ~ Year, data = x)
xx <- data.frame("Year" = seq(earliest_year, latest_year, 1))
xx <- merge(xx, x[, c("sum", "Year")], all.x = T)

# library(lme4)
# predict values for years without gridded census data
xx$sum_predict <- predict.lm(lmx, newdata = xx)
xx$sum <- ifelse(is.na(xx$sum) == TRUE, xx$sum_predict, xx$sum)

# plot(x$Year, x$sum, type = 'b')

# birth rates -----------------------------------
br_data <- read.csv("../data/birth_rates/API_SP.DYN.CBRT.IN_DS2_en_csv_v2_3363348.csv", 
                    skip = 4,
                    head = TRUE)

br_row <- which(br_data$Country.Name == "Brazil")
br_col_start <- which(colnames(br_data) == paste0("X", earliest_year))
br_col_end <- which(colnames(br_data) == paste0("X", latest_year))

br_x <- br_data[br_row, br_col_start:br_col_end]
br_xx <- br_x %>% gather(key = "Year", "birth_rate")
br_xx$Year <- gsub("X", "", br_xx$Year)

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

# Thailand comparisons
tfr_by_province <- read.csv('../data/birth_rates/thailand_province_fertlity_rates.csv')
cbr_by_region <- read.csv('../data/birth_rates/thailand_region_birth_rates.csv')
prov_x_region <- read.csv('../data/metadata/thailand_province_regions.csv')

tfr_with_region <- tfr_by_province %>%
  left_join(prov_x_region)

# boxplot TFR within/across regions
pdf('../figures/birth_rates/Thailand_TFR_by_region.pdf', width = 6, height = 4)
ggplot(tfr_with_region, 
       aes(
         x = Region,
         y = Total_fertility_rate,
         fill = Region
       )
) +
  geom_boxplot() +
  geom_jitter(
    color="black",
    size=0.4,
    alpha=0.9
  ) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Thailand") +
  xlab("Region") +
  ylab("Total fertility rate")
dev.off()

# CBR through time
thailand_dengue <- read.csv("../data/dengue/project_tycho_THAILAND_weekly.csv")
range(thailand_dengue$Year)
br_row <- which(br_data$Country.Name == "Thailand")
br_x <- br_data[br_row, 30:56]
br_xx <- br_x %>% gather(key = "Year", "Crude_birth_rate")
br_xx$Year <- gsub("X", "", br_xx$Year)
br_xx$Region <- "National"

cbr_by_region <- rbind(cbr_by_region, 
                       br_xx[,c("Region", "Year", "Crude_birth_rate")])

pdf('../figures/birth_rates/Thailand_CBR_by_region.pdf', 
    width = 11, 
    height = 6)

ggplot(cbr_by_region, 
       aes(
         x = as.factor(Year),
         y = Crude_birth_rate,
         group = Region,
         color = Region
         )
       ) +
  geom_line() +
  geom_point() +
  annotate("rect", 
           xmin = as.factor(1993), 
           xmax = as.factor(2011), 
           ymin = 10, 
           ymax = 35,
           alpha = .2) +
  annotate("text", 
           label = "Dengue data years", 
           x = as.factor(2004), 
           y = 30) +
  theme_bw() +
  ylab("Crude birth rate") +
  xlab("") +
  ggtitle("Thailand")
dev.off()  