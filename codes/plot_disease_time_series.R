# load libraries
library(tidyverse)
library(ggplot2)

# load files
dengue_filepaths <- list.files("../data/dengue", full.names = TRUE)

# load files into global environment
dengue_dfs = lapply(dengue_filepaths, read.csv)

# format data
dz_format <- function(name, filepaths, df, dateColName){
  ids <- grep(name, filepaths)
  x <- do.call("rbind", df[ids])
  x$Date <- as.Date(x[, dateColName], "%Y-%m-%d")
  x
}

iquitos <- dz_format(name = "iquitos",
                     filepaths = dengue_filepaths,
                     df = dengue_dfs, 
                     dateColName = "week_start_date")

sanjuan <- dz_format(name = "san_juan",
                     filepaths = dengue_filepaths,
                     df = dengue_dfs, 
                     dateColName = "week_start_date")


fig_dir <- "../figures/disease_time_series/dengue_"

long_plot <- function(df, name){
  fig_file_path <- paste0(fig_dir, name, ".pdf")
  
  df_long <- df %>%
    gather(key = dengue_type, 
           value = dengue_cases, 
           denv1_cases:other_positive_cases)
  
  ts_plot <- ggplot(df_long, 
                    aes(x = Date, 
                        y = dengue_cases
                        )
                    ) +
    geom_line() +
    facet_wrap(~dengue_type, nrow = 5) +
    theme_classic() +
    ylab("Dengue cases")
  
  ggsave(ts_plot, file = fig_file_path, width = 7.09, height = 8.28)
}

long_plot(df = iquitos,
          name = "iquitos"
          )

long_plot(df = sanjuan,
          name = "san_juan"
          )


total_cases_plot <- function(df, name){
  fig_file_path <- paste0(fig_dir, 
                          name, 
                          "_total_cases.pdf")
  
  total_plot <- ggplot(df, aes(x = Date, 
                               y = total_cases)) +
    geom_line() +
    theme_bw() + 
    ylab("Dengue cases")
  
  ggsave(total_plot, 
         file = fig_file_path, 
         width = 8.94, 
         height = 5.56)
  
}

total_cases_plot(df = iquitos,
                 name = "iquitos"
                 )

total_cases_plot(df = sanjuan,
                 name = "san_juan"
                 )

library(ggforce)
total_cases_plot_across_pages <- function(df, name, n){
  fig_file_path <- paste0(fig_dir, 
                          name, 
                          "_total_cases_", 
                          n, 
                          ".pdf")
  
  if(length(unique(df$Admin1Name)) <= 9){
    total_plot <- ggplot(df, aes(x = Date,
                                 y = CountValue)) +
      geom_line() +
      # geom_point() +
      theme_classic() +
      ylab("Dengue cases") +
      facet_wrap_paginate(.~Admin1Name,
                          scales = "free")
  } else {
    total_plot <- ggplot(df, aes(x = Date, 
                                 y = CountValue)) +
      geom_line() +
      # geom_point() +
      theme_classic() + 
      ylab("Dengue cases") +
      facet_wrap_paginate(.~Admin1Name, 
                          scales = "free", 
                          nrow = 3, 
                          ncol = 3, 
                          page = n
      )
    
  }
  
  ggsave(total_plot, 
         file = fig_file_path, 
         width = 8.94, 
         height = 5.56)
  
}


# combine and summarize tycho data
tychoIDs <- grep("tycho", dengue_filepaths)
tychoDF <- do.call("rbind", dengue_dfs[tychoIDs])

tychoDFsum <- tychoDF %>%
  group_by(CountryName2) %>%
  summarise(adminUnits = length(unique(Admin1Name)),
            min_time_period = min(time_period),
            median_time_period = median(time_period),
            max_time_period = max(time_period),
            earliest = min(PeriodStartDate),
            latest = max(PeriodEndDate)) %>%
  filter(median_time_period <= 4)

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
