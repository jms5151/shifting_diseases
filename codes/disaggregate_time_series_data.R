# load libraries
library(tidyverse)

# source temporal disaggration function
source("codes/functions_temporal_disaggregation.R")

# load files
dengue_files <- list.files('../data/dengue/', full.names = TRUE)
dengue_files_to_use <- dengue_files[grepl('THAILAND|brazil', dengue_files)]
# after this code has been run at least once, this is needed
dengue_files_to_use <- dengue_files_to_use[!grepl('weekly', dengue_files_to_use)]

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
      mutate('Admin1Name' = state_name) %>%
      group_by(Admin1Name, PeriodStartDate) %>%
      summarise(CountValue = sum(dengue_cases)) %>%
      filter(!is.na(CountValue))
    x <- as.data.frame(x)
  }
  # --------------------------------------------
  # format date
  x$PeriodStartDate <- as.Date(x$PeriodStartDate, '%Y-%m-%d')
  # list admin units
  admin_units <- unique(x$Admin1Name)
  admin_units <- admin_units[!is.na(admin_units)]
  # for each admin unit
  for(j in 1:length(admin_units)){
    # subset data
    x_admin <- subset(x, Admin1Name == admin_units[j])
    # format data
    x_admin <- format_for_temp_disagg(df = x_admin,
                                      dateCol = 'PeriodStartDate')
    # disaggregate from monthly to weekly data
    x_admin <- monthly_to_weekly_disagg(dfMonthly = x_admin, 
                                        dateCol = 'PeriodStartDate', 
                                        casesCol = 'CountValue')
    # add admin name
    x_admin$Admin1Name <- admin_units[j]
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
