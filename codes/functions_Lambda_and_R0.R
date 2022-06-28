library(zoo)

# Regress I(t) on I(t-1) over a moving window of length x  ---------------------
lambda_fun <- function(x) { 
  x1 = x[2:length(x)]
  x2 = x[1:(length(x)-1)]
  mod = lm(x1 ~ 0 + x2) # intercept = zero 
  unname(mod$coefficients)
}

# determine outbreak periods based on sign change in lambda from t to t+1 ------
outbreak_start_fun <- function(x) {
  if(anyNA(x) == FALSE){
    if((x[1] < 1 & x[2] > 1) == TRUE) {
      'outbreak_onset'
    } else if((x[1] > 1 & x[2] < 1) == TRUE){
      'inter-outbreak_onset'
    }
    else {
      NA
    }    
  }
}

# Calculate smoothed weekly lambda values based on 12-week moving window -------
susceptibility_index_fun <- function(x) { 
  x1 = c(x[1:12]) # 16 weeks slightly better
  mean(x1)
}

susceptibility_index_var_fun <- function(x) { 
  x1 = c(x[1:12]) # 16 weeks slightly better
  var(x1)
}

# Estimate R0 by max likelihood ------------------------------------------------
library(R0)

calc_ts_R0 <- function(df, disease){
  num_outbreaks = na.omit(unique(df$period[df$outbreak == 'yes']))
  
  # may want to do a sensitivity analysis for these  
  if(disease == 'dengue'){
    # serial interval
    mean_wks = 16.5/7
    sd_wks = 5/7
  } else if(disease == 'malaria'){
    # generation interval for treated infections with P. falciparum
    # https://malariajournal.biomedcentral.com/articles/10.1186/s12936-016-1537-6#Sec2
    mean_wks = 49.1/7
    upper_ci = 68
    lower_ci = 35
    n = 1000
    malaria_sd <- (upper_ci - lower_ci)*sqrt(n)/1.96
    sd_wks = malaria_sd/7
  }
  gen_time <- generation.time("gamma", c(mean_wks, sd_wks))
  
  # may want df to include site name, year (or date range)
  R0_df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(R0_df) <- c('period', 'R0', 'R0_lower', 'R0_upper')
  
  for(i in num_outbreaks){
    df_tmp <- subset(df, outbreak == 'yes' & period == i)
    if(nrow(df_tmp) >= 3){
      outbreak_dates <- df_tmp$Date
      # this is causing problems in some cases, may want to adjust dynamically
      outbreak_begin_date <- min(outbreak_dates)
      outbreak_end_date <- max(outbreak_dates)
      outbreak_incidence <- df_tmp$cases
      
      if(sum(outbreak_incidence) >= 150){ 
        
        tmp_row_vals <- c(i, NA, NA, NA)
        
        tryCatch(
          
          expr = {
            R0_out <- est.R0.ML(
              epid = as.integer(outbreak_incidence)
              , GT = gen_time
              , t = outbreak_dates
              # , date.first.obs = outbreak_begin_date
              , begin = outbreak_begin_date
              , end = outbreak_end_date
              , time.step = 7#round(as.numeric(min(diff(sort(outbreak_dates))))) # number of days between each incidence observation (although suspicious that ts length needs to be more than 8)
            )
            
            tmp_row_vals <- c(i, R0_out$R, R0_out$conf.int[1], R0_out$conf.int[2])
            
          }, 
          
          error = function(e){
            cat(i, "ERROR :", conditionMessage(e), "\n")
          },
          
          finally = {R0_df[i,] <- tmp_row_vals}
        )
        }
      }
    }
  # return dataframe
  R0_df
}

# ------------------------------------------------------------------------------
# summarize outbreaks from time series -----------------------------------------
# ------------------------------------------------------------------------------
summarise_outbreaks <- function(df, cases_colname, date_colname, disease){
  
  df$cases <- df[, cases_colname]
  df$date <- df[, date_colname]
  df$date <- as.Date(df$date, '%Y-%m-%d')
  
  df <- df[order(df$date), ]
  
  # calculate lambda
  df$lambda <- rollapply(
    df$cases
    , width = 13 # to have 12 weeks of data
    , by = 1
    , fill = NA
    , FUN = lambda_fun
    , by.column = FALSE
    , align = "right"
    )
  
  # calculate smoothed lambda
  df$lambda_smoothed <- rollapply(
    df$lambda
    , width = 10
    , by = 1
    , fill = NA
    , FUN = mean
    , by.column = FALSE
    , align = "right"
    )
  
  # determine outbreak periods
  df$onset <- rollapply(
    df$lambda_smoothed
    , width = 2
    , by = 1
    , fill = NA
    , FUN = outbreak_start_fun
    , by.column = FALSE
    , align = "right"
    )
  
  # outbreak designation
  df$outbreak  <- ifelse(df$lambda_smoothed < 1, 'no', 'yes')
  
  # If there were outbreaks, continue calculations
  if(any(df$outbreak == 'yes', na.rm = T) == TRUE){
    # label events
    outbreak_n = 0
    interoutbreak_n = 0
    df$period <- NA
    start_row <- which(!is.na(df$onset) == TRUE)[1]
    
    for(i in start_row:nrow(df)){
      # if an outbreak or inter-outbreak period has started
      if(!is.na(df$outbreak[i]) & !is.na(df$onset[i])){
        # if it's a new outbreak period, update outbreak n value 
        if(df$outbreak[i] == 'yes' & df$onset[i] == 'outbreak_onset'){
          outbreak_n = outbreak_n + 1
          # else if it's a new inter-outbreak period, update inter-outbreak n value 
        } else if(df$outbreak[i] == 'no' & df$onset[i] == 'inter-outbreak_onset'){
          interoutbreak_n = interoutbreak_n + 1
        }
      }
      # label outbreak/inter-outbreaks
      if(outbreak_n > 0 | interoutbreak_n > 0){
        df$period[i] <- ifelse(df$outbreak[i] == 'yes', outbreak_n, interoutbreak_n)
      }
    }
    
    # Calculate smoothed weekly lambda values based on 12-week moving window, 12 weeks prior 
    df$susceptibility_index <- rollapply(
      df$lambda
      # width = 32 weeks better, but need 16 in susceptibility_index_fun
      , width = 25 # to have 12 weeks of data; 24?;
      , by = 1
      , fill = NA
      , FUN = susceptibility_index_fun
      , by.column = FALSE
      , align="right"
    )
    
    df$susceptibility_index_var <- rollapply(
      df$lambda
      # width = 32 weeks better, but need 16 in susceptibility_index_fun
      , width = 25 # to have 12 weeks of data; 24?;
      , by = 1
      , fill = NA
      , FUN = susceptibility_index_var_fun
      , by.column = FALSE
      , align="right"
    )
    
    df$susceptibility_index <- ifelse(!is.na(df$onset), df$susceptibility_index, NA)
    df$susceptibility_index_var <- ifelse(!is.na(df$onset), df$susceptibility_index_var, NA)
    
    # Estimate R0 by max likelihood
    
    r0 <- calc_ts_R0(
      df = df
      , disease = disease
    )
    
    # summarize data
    interoutbreak_summary <- df %>%
      filter(outbreak == 'no' & !is.na(period)) %>%
      group_by(period) %>%
      summarise(interoutbreak_duration = difftime(max(date), min(date), unit = 'days'))
    
    outbreak_summary <- df %>%
      filter(outbreak == 'yes' & !is.na(period)) %>%
      group_by(period) %>%
      summarise(
        total_cases = sum(cases)
        , peak_cases = max(cases)
        , outbreak_peak = date[cases == peak_cases][1] # take first date in case of ties
        , outbreak_start = min(date)
        , outbreak_end = max(date)
        , susceptibility_index = mean(susceptibility_index, na.rm = T)
        , susceptibility_index_var = mean(susceptibility_index_var, na.rm = T)
        , outbreak_duration = difftime(outbreak_end, outbreak_start, unit = 'days')
      ) %>%
      left_join(interoutbreak_summary) %>%
      left_join(r0) %>%
      mutate(Year = format(outbreak_peak, '%Y') # as.numeric(substr(outbreak_peak, 1, 4))
             # , peak_week = format(outbreak_peak, '%W')
             # , start_week = format(outbreak_start, '%W')
             # , end_week = format(outbreak_end, '%W')
      )
    
    outbreak_summary$duration_btwn_peaks <- difftime(outbreak_summary$outbreak_peak, lag(outbreak_summary$outbreak_peak))
    
    outbreak_summary <- as.data.frame(outbreak_summary)
    
    # return from function
    return(outbreak_summary)
    
  }
  
}

# may want to change year to yearx = year - start_year to compare across datasets
# or just rename as 1...N years
## Summarise trends in data
slope_and_pval_function <- function(df, xvar, yvar){

  df <- df[, c(xvar, yvar)]
  df <- df[complete.cases(df), ]
  
  if(nrow(df) > 2){
    xvar <- seq(from = 1, to = length(df[, xvar]), by = 1)

    
    if(is.numeric(yvar) == FALSE) {
      yvar <- as.numeric(df[, yvar])
    } else {
      yvar <- df[, yvar]
    }
    
    df_trend <- lm(yvar ~ xvar)
    df_slope <- unname(df_trend$coefficients[2])
    df_pval <- cor.test(xvar, yvar)$p.value
    
    # return(
    list(
      'slope' = round(df_slope, 2)
      , 'pvalue' = round(df_pval, 2)
    )
  } else {
    list(
      'slope' = NA
      , 'pvalue' = NA
    )
  }
}

time_shift_calc <- function(date_vec){
  date_vec_lagged <- date_vec[1:(length(date_vec)-1)]
  date_vec_updated <- date_vec[2:length(date_vec)]
  diff_vec <- difftime(date_vec_updated, date_vec_lagged, units = 'weeks') - 53 # 53 = weeks in year
  data.frame('Date' = date_vec_updated, 'Time_shift' = diff_vec)
}

summarise_trends <- function(df){
  # R0 vs year
  r0 <- slope_and_pval_function(df = df, xvar = 'Year', yvar = 'R0')
  names(r0) <- paste0('R0_', names(r0))
  
  # outbreak duration vs year
  out_dur <- slope_and_pval_function(df = df, xvar = 'Year', yvar = 'outbreak_duration')
  names(out_dur) <- paste0('Outbreak_duration_', names(out_dur))
  
  # duration between peaks vs year
  dur_peaks <- slope_and_pval_function(df = df, xvar = 'Year', yvar = 'duration_btwn_peaks')
  names(dur_peaks) <- paste0('Duration_btwn_peaks_', names(dur_peaks))
  
  # Frequency of outbreaks (Number of outbreaks / number of years)
  outbreak_frequency <- round(length(df$period) / (max(as.numeric(df$Year)) - min(as.numeric(df$Year))), 2)
  names(outbreak_frequency) <- 'Outbreak_frequency'
  
  # Estimate annual shifts in outbreak onset and peak, use only largest outbreak/year
  df_annual_outbreaks <- df %>%
    group_by(Year) %>%
    slice_max(total_cases)
  
  # Shift in outbreak onset vs year
  start_time_df <- time_shift_calc(date_vec = df_annual_outbreaks$outbreak_start)
  onset <- slope_and_pval_function(df = start_time_df, xvar = 'Date', yvar = 'Time_shift')
  names(onset) <- paste0('Annual_shift_in_start_week_', names(onset))
  
  # Shift in outbreak onset vs year
  peak_time_df <- time_shift_calc(date_vec = df_annual_outbreaks$outbreak_peak)
  peak <- slope_and_pval_function(df = peak_time_df, xvar = 'Date', yvar = 'Time_shift')
  names(peak) <- paste0('Annual_shift_in_peak_week_', names(peak))
  
  # output
  as.data.frame(c(r0, out_dur, dur_peaks, outbreak_frequency, onset, peak))
}
