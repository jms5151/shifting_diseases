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

dengue_weights <- calculate_serial_interval_weights(
  serial_interval_mean = 20
  , serial_interval_sd = 7.4
)


# Brazil ---------------------------------------------
brazil_betas <- lapply(
  brazil_tsir_data
  , function(x) tsir_with_serial_interval(df = x
                                          , weights = dengue_weights)
)

save(brazil_betas
     , file = '../data/tsir_outputs/brazil_empirical_betas.RData')

# Thailand --------------------------------------------
thailand_betas <- lapply(
  thailand_tsir_data
  , function(x) tsir_with_serial_interval(df = x
                                          , weights = dengue_weights)
)

save(thailand_betas
     , file = '../data/tsir_outputs/thailand_empirical_betas.RData')

# plot.ts(beta)
# hist(beta)
# range(beta)
# 
# # not sure if this is correct
# foi <- (beta * Ieffective)/N
# # hist(foi)
# # range(foi)
# plot.ts(foi)
# 
