# source custom functions
source('codes/functions_interpolate_birth_rates.R')

# load global birth rate data
br_data <- read.csv("../data/birth_rates/API_SP.DYN.CBRT.IN_DS2_en_csv_v2_3363348.csv", 
                    skip = 4,
                    head = TRUE)


# Thailand -----------------------------------------------
# load data
thailand_dengue <- read.csv("../data/dengue/project_tycho_THAILAND_weekly.csv")
cbr_by_region <- read.csv('../data/birth_rates/thailand_region_birth_rates.csv')
prov_x_region <- read.csv('../data/metadata/thailand_province_regions.csv')
tfr_by_province <- read.csv('../data/birth_rates/thailand_province_fertlity_rates.csv')

thailand_CBR <- national_CBR(
  earliest_year = 1985, 
  latest_year = NA,
  birth_rate_df = br_data,
  disease_df = thailand_dengue,
  countryName = "Thailand"
)

# plot CBR reported -------------------------------
thailand_CBR2 <- bind_rows(
  thailand_CBR,
  cbr_by_region[, colnames(thailand_CBR)]
)

pdf('../figures/birth_rates/Thailand_CBR_by_region.pdf',
    width = 11,
    height = 6)

ggplot(thailand_CBR2, 
       aes(
         x = as.factor(Year),
         y = Crude_birth_rate,
         # linetype = Type,
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
           ymax = 33,
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

# plot CBR reported and interpolated -----------------------
thailand_CBR_interpolated <- interpolate_regional_CBR(
  nat_CBR = thailand_CBR,
  regional_CBR = cbr_by_region
)

# format for plotting
thailand_CBR_long <- thailand_CBR_interpolated %>%
  gather(key = "Type",
         value = "CBR",
         Crude_birth_rate : Crude_birth_rate_interpolated) %>%
  filter(!is.na(CBR))

# plot CBR reported vs interpolated
pdf('../figures/birth_rates/Thailand_CBR_interpolated_by_region.pdf',
    width = 12,
    height = 7)

ggplot(thailand_CBR_long, 
       aes(
         x = as.factor(Year),
         y = CBR,
         linetype = Type,
         group = interaction(Region, Type),
         color = Region
       )
) +
  geom_line() +
  geom_point() +
  annotate("rect", 
           xmin = as.factor(1993), 
           xmax = as.factor(2011), 
           ymin = 5, 
           ymax = 33,
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

# plot TFR within and across regions ------------------------
tfr_with_region <- tfr_by_province %>%
  left_join(prov_x_region)

pdf('../figures/birth_rates/Thailand_TFR_by_region.pdf', 
    width = 6, 
    height = 4)

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
