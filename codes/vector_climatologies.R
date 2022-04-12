# load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Figure directory
fig_dir <- '../figures/vector_climatologies/'

# ENSO -------------------------------------------------------------------------
# load data
nino3.4 <- read.csv('../data/climate/nino3.4.csv')

# determine el nino and la nina years based on nino 3.4 index of Oct - Dec -----
EN_anom <- subset(nino3.4, Month10 >= 1 & Month11 >= 1 & Month12 >= 1)
LN_anom <- subset(nino3.4, Month10 <= -1 & Month11 <= -1 & Month12 <= -1)

# make vector of 24 months from ENSO years
# EN_years_24mo <- sort(c(as.integer(EN_anom$Year), (as.integer(EN_anom$Year) + 1)))
# LN_years_24mo <- sort(c(as.integer(LN_anom$Year), (as.integer(LN_anom$Year) + 1)))

create_enso_df <- function(ensoYR){
  enso_months <- expand.grid('year' = as.integer(ensoYR), 'month' = seq(1, 12, 1))
  enso_months$enso_month <- enso_months$month
  enso_months_yr2 <- expand.grid('year' = as.integer(ensoYR) + 1, 'month' = seq(1, 12, 1))
  enso_months_yr2$enso_month <- enso_months_yr2$month + 12
  enso_months <- rbind(enso_months, enso_months_yr2)
  enso_months
}

EN_months <- create_enso_df(ensoYR = EN_anom$Year)
LN_months <- create_enso_df(ensoYR = LN_anom$Year)


# Vectors ----------------------------------------------------------------------
# load data
aedes <- read.csv('../data/vectors/gbif_aedes.csv', sep = '\t')
anopheles <- read.csv('../data/vectors/gbif_anopheles.csv', sep = '\t')

# function to aggregate data
aggregate_vectors <- function(df, location){
  if(location == 'country'){
    df$Location <- df$countryCode
  } else if(location == 'state') {
    df$Location <- paste0(df$stateProvince, ', ', df$countryCode)
  }
  df <- df  %>%
    filter(year >= 1870) %>%
    filter(Location != '') %>%
    group_by(Location, year, month) %>%
    summarise(count = sum(individualCount, na.rm = T)) %>%
    drop_na()
  
  yearly <- df %>%
    group_by(Location, year) %>%
    mutate(yearlytotal = sum(count, na.rm = T))
  
  df <- df %>%
    left_join(yearly) %>%
    group_by(Location) %>%
    ungroup() %>%
    complete(year, month) %>%
    fill(Location) %>%
    mutate(normalized_count = count/yearlytotal)
  
  df$YearMonth <- as.Date(paste(df$year, df$month, '01', sep = '-'), '%Y-%m-%d')

  df
}

# plotting functions
vector_heatmap <- function(df, title){
  ggplot(df, aes(YearMonth, Location, fill = normalized_count)) + 
    geom_tile() +
    theme_bw() +
    xlab('Year-Month') +
    ylab('') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          , axis.text.y = element_blank()
          , axis.ticks.y = element_blank()
          ) + 
    ggtitle(title)
}

vector_monthly_climatology <- function(df){
  ggplot(df, aes(x = as.factor(month), y = normalized_count, fill = Location)) +
    geom_boxplot() +
    facet_wrap(~Location) + 
    theme_bw() +
    xlab('Month') +
    ylab('Count (normalized)') +
    theme(legend.position="none") +
    scale_fill_manual(name = 'Location', values = 'grey')
  
}

vector_monthly_climatology_with_enso <- function(df){
  df <- subset(df, !is.na(Location))
  ggplot(df, aes(x = as.factor(enso_month), y = normalized_count, fill = treatment)) +
    geom_boxplot() +
    facet_wrap(~Location) + 
    theme_bw() +
    xlab('Month') +
    ylab('Count (normalized)') +
    scale_fill_manual(name = 'treatment', values = c('red', 'grey', 'blue')) +
    theme(legend.position = "bottom")
  }

lollipop_clim_plots <- function(lt_vec, en_vec, ln_vec){
  lt_vec <- lt_vec %>%
    group_by(Location, enso_month) %>%
    summarise(lt_normalized_median = median(normalized_count, na.rm = T))
  
  en_vec <- en_vec %>%
    group_by(Location, enso_month) %>%
    summarise(en_normalized_median = median(normalized_count, na.rm = T))
  
  ln_vec <- ln_vec %>%
    group_by(Location, enso_month) %>%
    summarise(ln_normalized_median = median(normalized_count, na.rm = T))
  
  df <- lt_vec %>%
    left_join(en_vec) %>%
    left_join(ln_vec) %>%
    mutate(el_nino = log(en_normalized_median / lt_normalized_median)
           , la_nina = log(ln_normalized_median / lt_normalized_median)
    ) %>%
    select(-c('lt_normalized_median', 'en_normalized_median', 'ln_normalized_median')) %>%
    gather('type', 'log_ratio', el_nino:la_nina)

  ggplot(df, aes(x = enso_month, y = log_ratio, color = type)) +
    geom_segment(aes(x = enso_month, xend = enso_month, y = 0, yend = log_ratio)) +
    geom_point(size = 4) +
    facet_wrap(~Location) + 
    theme_bw() +
    xlab('Month') +
    ylab('Log ratio of monthly medians') +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(1, 24, 1)) +
    scale_color_manual(name = 'type', values = c('red', 'blue'))
}


# write loop to go through data by vector genus and location -------------------
vectors <- list(aedes, anopheles)
vector_names <- c('Aedes', 'Anopheles')
location_names <- c('country', 'state')

for(i in 1:length(vectors)){
  
  for(j in 1:length(location_names)){
    
    # aggregate/summarize vector data by location and time
    vector_df <- aggregate_vectors(
      df = vectors[[i]]
      , location = location_names[[j]]
      )

    # subset highly sampled data
    highlySampledLocations <- names(which(table(vector_df$Location) > 50))
    highlySampledLocations <- highlySampledLocations[!startsWith(x = highlySampledLocations, prefix = ',')] #remove errors
    vector_df <- vector_df[vector_df$Location %in% highlySampledLocations, ]
    
    # for each highly sampled location
    for(k in highlySampledLocations){
      
      # subset data
      vector_df_sub <- subset(vector_df, Location == k)
      
      # only go on if there are values other than 0, 1, NA, and NaN
      unique_vals <- unique(na.exclude(vector_df_sub$normalized_count))
      
      if(any(unique_vals > 0 & unique_vals < 1) == TRUE){
        # create heatmap of time series data
        fig1 <- vector_heatmap(df = vector_df_sub, title = paste0(vector_names[i], '; ', k))
        
        # create monthly climatology boxplot
        fig2 <- vector_monthly_climatology(df = vector_df_sub)
        
        # subset ENSO data
        vectors_el_nino <- EN_months %>%
          left_join(vector_df_sub)
        
        vectors_la_nina <- LN_months %>%
          left_join(vector_df_sub)
        # vectors_el_nino <- vector_df_sub[vector_df_sub$year %in% EN_years_24mo, ]
        # vectors_la_nina <- vector_df_sub[vector_df_sub$year %in% LN_years_24mo, ]
        
        # add enso month column to vector df
        vector_df_sub_a <- add_column(
          vector_df_sub
          , 'enso_month' = vector_df_sub$month
          , .after = "month"
          )
        
        vector_df_sub_b <- add_column(
          vector_df_sub
          , 'enso_month' = vector_df_sub$month + 12
          , .after = "month"
        )
        
        vector_df_sub_2 <- rbind(vector_df_sub_a, vector_df_sub_b)
        
        # add treatment labels
        vectors_el_nino$treatment <- 'a. el nino'
        vector_df_sub_2$treatment <- 'b. climatology'
        vectors_la_nina$treatment <- 'c. la nina'
        
        # combine data
        vector_df_sub_new <- bind_rows(
          list(
            vector_df_sub_2
            , vectors_el_nino
            , vectors_la_nina
          )
        )
        
        # create monthly climatology boxplots by ENSO condition
        fig3 <- vector_monthly_climatology_with_enso(df = vector_df_sub_new)
        
        # create lollipop plots of ENSO anomalies
        fig4 <- lollipop_clim_plots(
          lt_vec = vector_df_sub_2
          , en_vec = vectors_el_nino
          , ln_vec = vectors_la_nina
        )
        
        # combine all plots
        combined_fig <- ggarrange(fig1, fig4, fig2, fig3, ncol = 2, nrow = 2)

        # save
        country <- substr(k, nchar(k)-1, nchar(k))
        state <- gsub("\\,.*", "", k)
        if(country != state){
          location_plain_name <- paste(country, state, sep = '_')
        } else {
          location_plain_name <- country
        }
        location_plain_name <- gsub(' ', '_', location_plain_name)
        filename <- paste0(fig_dir, vector_names[i], '_', location_plain_name, '.pdf')
        ggsave(filename, combined_fig, width = 14, height = 8)
        
      }
    }
  }
}
