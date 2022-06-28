# load libraries
library(tidyverse)

# source temporal disaggration function
source("codes/functions_temporal_disaggregation.R")

# dengue -----------------------------------------------------------------------
# load files
dengue_files <- list.files('../data/dengue/', full.names = TRUE)
dengue_files_to_use <- dengue_files[grepl('tycho|Lowe|kenya', dengue_files)]
# after this code has been run at least once, this is needed
dengue_files_to_use <- dengue_files_to_use[!grepl('weekly|tycho_BRAZIL|tycho_COLOMBIA|tycho_PERU', dengue_files_to_use)]

# save monthly data to weekly data for all countries listed above
for(i in 1:length(dengue_files_to_use)){
  # create new empty data frame
  new_df <- data.frame()
  # open file
  x <- read.csv(dengue_files_to_use[i])
  # if Lowe et al., initial formatting --------
  if(dengue_files_to_use[i] == '../data/dengue/Lowe_etal_LPH_2021_brazil_dengue_data_2000_2019.csv'){
    x$PeriodStartDate <- paste(x$year, x$month, '01', sep = '-')
    x <- x %>%
      mutate(
        'Country' = 'Brazil'
        , 'Admin_unit' = state_name) %>%
      group_by(Country, Admin_unit, PeriodStartDate) %>%
      summarise(CountValue = sum(dengue_cases)) %>%
      filter(!is.na(CountValue))
    x <- as.data.frame(x)
  }
  # --------------------------------------------
  if(dengue_files_to_use[i] == '../data/dengue/kenya_r01_arbovirus_data.csv'){
    x$PeriodStartDate <- paste0(x$YearMonth, '-01')
    x$Country <- 'Kenya'
    x$Admin_unit <- x$Site
    x$CountValue <- x$arboviruses_positive
  }
  
  # --------------------------------------------
  if(length(grep('tycho', dengue_files_to_use[i])) == 1) {
    x$Country <- gsub('../data/dengue/project_tycho_|.csv', '', dengue_files_to_use[i]) 
    x$Admin_unit <- x$Admin1Name
  }
  
  # format date
  x$PeriodStartDate <- as.Date(x$PeriodStartDate, '%Y-%m-%d')
  
  # remove data with no admin units
  x <- subset(x, !is.na(Admin_unit))
  
  # if there is data
  if(nrow(x) > 0){
    # cat(dengue_files_to_use[i], '\n')
    
    # complete monthly time series, in case there are missing data
    x <- x %>%
      group_by(Country, Admin_unit, PeriodStartDate) %>%
      summarise(CountValue = sum(CountValue, na.rm = T)) %>%
      complete(PeriodStartDate = seq.Date(min(PeriodStartDate), max(PeriodStartDate), by = 'month')) %>%
      as.data.frame()
    
    # list admin units
    admin_units <- unique(x$Admin_unit)

    # for each admin unit
    for(j in 1:length(admin_units)){
      # subset data
      x_admin <- subset(x, Admin_unit == admin_units[j])
      
      # remove discontinuous data
      if(anyNA(x_admin$CountValue)){
        x_admin <- na.contiguous(x_admin[, c('PeriodStartDate', 'CountValue')])
      }
      
      # format data
      x_admin <- format_for_temp_disagg(df = x_admin,
                                        dateCol = 'PeriodStartDate')

      # disaggregate from monthly to weekly data
      x_admin <- monthly_to_weekly_disagg(dfMonthly = x_admin,
                                          dateCol = 'PeriodStartDate',
                                          casesCol = 'CountValue')
      # add country and admin name
      x_admin$Country <- unique(x$Country)
      x_admin$Admin_unit <- admin_units[j]
      
      # combine data
      new_df <- rbind(new_df, x_admin)
    }
    # create new filename and save
    new_filename <- paste0(strsplit(dengue_files_to_use[i], ".csv"),
                           '_weekly.csv')
    write.csv(new_df,
              file = new_filename,
              row.names = F)
  }
}
