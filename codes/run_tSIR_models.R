# Run tSIR models with serial intervals to calculate transmission rates

# source codes to load and format data
source('codes/concat_data_for_tsir_models.R')

# load custom functions to calculate serial interval weights and tsir model
source('codes/functions_tSIR_model_with_serial_interval.R')

# Calculate weights for the serial interval
# https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0005797 
# varies based on temperature, could be interesting to include
# https://www.sciencedirect.com/science/article/pii/S1755436517300907#bib0170
# useful if using single values
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4099473/#:~:text=The%20analyses%20indicate%20that%2015,interval%20between%20successive%20dengue%20illnesses.

dengue_weights <- calculate_serial_interval_weights(
  serial_interval_mean = 16.5
  , serial_interval_sd = 5
)

# variation in malaria SI: 
# https://malariajournal.biomedcentral.com/articles/10.1186/s12936-016-1537-6
malaria_weights <- calculate_serial_interval_weights(
  serial_interval_mean = 49.1 
  , serial_interval_sd = 16.5 # this is wrong, only gives 5-95 percentiles in paper
)

# body(tsiR::derivative)
# Brazil ---------------------------------------------

# dengue
s1 <- Sys.time()

brazil_dengue_betas <- lapply(
  brazil_dengue_tsir_data
  , function(x) tsir_with_serial_interval(df = x
                                          , weights = dengue_weights)
  )

save(brazil_dengue_betas
     , file = '../data/tsir_outputs/brazil_dengue_empirical_betas.RData')

s2 <- Sys.time()

cat('Brazil dengue: ', s2 - s1)

# malaria - vivax
brazil_malaria_vivax_tsir_data <- brazil_malaria_vivax_tsir_data['Amazonas']

brazil_malaria_vivax_betas <- lapply(
  brazil_malaria_vivax_tsir_data
  , function(x) tsir_with_serial_interval(df = x
                                          , weights = malaria_weights)
  )

save(brazil_malaria_vivax_betas
     , file = '../data/tsir_outputs/brazil_malaria_vivax_empirical_betas.RData')

# malaria - falciparum
brazil_malaria_falciparum_tsir_data <- brazil_malaria_falciparum_tsir_data['Amazonas']

brazil_malaria_falciparum_betas <- lapply(
  brazil_malaria_falciparum_tsir_data
  , function(x) tsir_with_serial_interval(df = x
                                          , weights = malaria_weights)
)

save(brazil_malaria_falciparum_betas
     , file = '../data/tsir_outputs/brazil_malaria_falciparum_empirical_betas.RData')


# Thailand --------------------------------------------
s3 <- Sys.time()

thailand_dengue_betas <- lapply(
  thailand_dengue_tsir_data
  , function(x) tsir_with_serial_interval(df = x
                                          , weights = dengue_weights)
)

save(thailand_dengue_betas
     , file = '../data/tsir_outputs/thailand_dengue_empirical_betas.RData')

s4 <- Sys.time()

cat('Thailand dengue: ', s4 - s3)


# plot.ts(beta)
# hist(beta)
# range(beta)
# 
# # not sure if this is correct
# foi <- (beta * Ieffective)/N
# # hist(foi)
# # range(foi)
# plot.ts(foi)

# Kenya ----------------------------------------------

kenya_dengue_betas <- lapply(
  kenya_dengue_tsir_data
  , function(x) tsir_with_serial_interval(df = x
                                          , weights = dengue_weights)
)
save(kenya_dengue_betas
     , file = '../data/tsir_outputs/kenya_dengue_empirical_betas.RData')

# # alternative without births
# # This doesn't seem to work so well either
# kenya_dengue_data_weekly$Chulaimbo$Population <- 7304
# kenya_dengue_data_weekly$Kisumu$Population <- 547557
# kenya_dengue_data_weekly$Msambweni$Population <- 240698
# kenya_dengue_data_weekly$Ukunda$Population <- 154048
# 
# kenya_dengue_betas_simple <- lapply(
#   kenya_dengue_data_weekly
#   , function(x) simple_tsir_with_serial_interval(df = x
#                                                  , weights = dengue_weights
#                                                  )
# )
# 
