# create a list of dataframes
split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[, col])
