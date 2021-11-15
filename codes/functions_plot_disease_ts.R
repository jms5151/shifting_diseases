# custom functions for plotting disease time series ------------------------

# format data
dz_format <- function(name, filepaths, df, dateColName){
  ids <- grep(name, filepaths)
  x <- do.call("rbind", df[ids])
  x$Date <- as.Date(x[, dateColName], "%Y-%m-%d")
  x
}

# stacked plots, for different serotypes
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

# plot cases vs time
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

# plot cases versus time, for multiple regions on same page
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
