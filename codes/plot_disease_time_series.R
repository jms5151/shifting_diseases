# plot disease time series -----------------------------------------------
# load libraries
library(tidyverse)
library(ggplot2)
library(ggforce)

# source plotting functions
source("codes/functions_plot_disease_ts.R")
source("codes/format_dengue_data.R")

# Dengue -----------------------------------------------------------------
# load files
dengue_filepaths <- list.files("../data/dengue", full.names = TRUE)

# load files into global environment
dengue_dfs = lapply(dengue_filepaths, read.csv)


fig_dir <- "../figures/disease_time_series/dengue_"


long_plot(df = iquitos,
          name = "iquitos")

long_plot(df = sanjuan,
          name = "san_juan")



total_cases_plot(df = iquitos,
                 name = "iquitos")

total_cases_plot(df = sanjuan,
                 name = "san_juan")


total_cases_plot_across_pages(df = x,
                              name = "usa",
                              n = 1)

for(i in 1:nrow(tychoDFsum)){
  x <- dz_format(name = tychoDFsum$CountryName2[i],
                 filepaths = dengue_filepaths,
                 df = dengue_dfs,
                 dateColName = "PeriodStartDate")
  x <- subset(x, !is.na(Admin1Name))
  adminN <- ceiling(length(unique(x$Admin1Name))/12)
  for(j in 1:adminN){
    total_cases_plot_across_pages(df = x,
                                  name = tychoDFsum$CountryName2[i],
                                  n = j)
  }
}
