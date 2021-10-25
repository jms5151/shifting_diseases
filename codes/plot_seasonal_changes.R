# load libraries
library(tidyverse)
library(ggplot2)

# load files
dengue_filepaths <- list.files("../data/dengue", full.names = TRUE)
climate_filepaths <- list.files("../data/climate", full.names = TRUE)

# load files into global environment
dengue_dfs = lapply(dengue_filepaths, read.csv)
climate_dfs = lapply(climate_filepaths, read.csv)

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


ws_climate_format <- function(name, filepaths, df){
  id <- grep(name, filepaths)
  x <- df[[id]]
  x$Date <- paste(x$YYYY, x$MM, x$DD, sep = "-")
  x$Date <- as.Date(x$Date, "%Y-%m-%d")
  x[x == -9999] <- NA
  x
}


iquitos_ws_climate <- ws_climate_format(name = "weather_station_Iquitos",
                                        filepaths = climate_filepaths,
                                        df = climate_dfs
                                        )
  
sanjuan_ws_climate <- ws_climate_format(name = "weather_station_SanJuan",
                                        filepaths = climate_filepaths,
                                        df = climate_dfs
                                        )


era_climate_format <- function(name, filepaths, df, clim_vars){
  for(i in 1:length(clim_vars)){
    name2 <- paste(name, clim_vars[i], sep = "_")
    id <- grep(name2, filepaths)
    x2 <- df[[id]]
    if(i == 1){
      x <- x2
    } else {
      x <- x %>%
        left_join(x2)
    }
  }
  x$Date <- paste(x$Year, x$Month, x$Day, sep = "-")
  x$Date <- as.Date(x$Date, "%Y-%m-%d")
  kelvin_cols <- colnames(x)[grep("K", colnames(x))]
  celsius_cols <- gsub("K", "C", kelvin_cols)
  x[, celsius_cols] <- lapply(x[, kelvin_cols], function(x) x - 273.15)
  as.data.frame(x)
}

iquitos_era_climate <- era_climate_format(name = "reanalysis_iquitos",
                                          filepaths = climate_filepaths,
                                          df = climate_dfs,
                                          clim_vars <- c("humidity", "temp", "precip")
                                          )


sanjuan_era_climate <- era_climate_format(name = "reanalysis_sanjuan",
                                          filepaths = climate_filepaths,
                                          df = climate_dfs,
                                          clim_vars <- c("humidity", "temp", "precip")
                                          )

# exploring ----------------------------------------------------------------



plot(iquitos$week_start_date, iquitos$total_cases, col = "white")
lines(iquitos$week_start_date, iquitos$denv1_cases, col = "blue")
lines(iquitos$week_start_date, iquitos$denv2_cases, col = "red")
lines(iquitos$week_start_date, iquitos$denv3_cases, col = "purple")
lines(iquitos$week_start_date, iquitos$denv4_cases, col = "green")
lines(iquitos$week_start_date, iquitos$other_positive_cases, type = 'l', col = "black")



plot(sanjuan$week_start_date, sanjuan$total_cases, col = "white")
lines(sanjuan$week_start_date, sanjuan$denv1_cases, col = "blue")
lines(sanjuan$week_start_date, sanjuan$denv2_cases, col = "red")
lines(sanjuan$week_start_date, sanjuan$denv3_cases, col = "purple")
lines(sanjuan$week_start_date, sanjuan$denv4_cases, col = "green")
lines(sanjuan$week_start_date, sanjuan$other_positive_cases, type = 'l', col = "black")




plot(iquitos_climate$Date, iquitos_climate$TMAX, col = "white", 
     ylim = c(min(iquitos_climate$TMIN, na.rm = T), max(iquitos_climate$TMAX, na.rm = T)),
     xlim = c(min(iquitos$week_start_date), max(iquitos$week_start_date)))
lines(iquitos_climate$Date, iquitos_climate$TAVG, type = 'l', col = "red",
      ylim = c(min(iquitos_climate$TMIN, na.rm = T), max(iquitos_climate$TMAX, na.rm = T)),
      xlim = c(min(iquitos$week_start_date), max(iquitos$week_start_date)))
lines(iquitos_climate$Date, iquitos_climate$TMIN, type = 'l', col = "orange",
      ylim = c(min(iquitos_climate$TMIN, na.rm = T), max(iquitos_climate$TMAX, na.rm = T)),
      xlim = c(min(iquitos$week_start_date), max(iquitos$week_start_date)))
lines(iquitos_climate$Date, iquitos_climate$TMAX, type = 'l', col = "maroon",
      ylim = c(min(iquitos_climate$TMIN, na.rm = T), max(iquitos_climate$TMAX, na.rm = T)),
      xlim = c(min(iquitos$week_start_date), max(iquitos$week_start_date)))
abline(h = mean(iquitos_climate$TMAX, na.rm = T))
abline(h = mean(iquitos_climate$TMIN, na.rm = T))
abline(h = mean(iquitos_climate$TAVG, na.rm = T))


plot(sanjuan_climate$Date, sanjuan_climate$TMAX, col = "white", 
     ylim = c(min(sanjuan_climate$TMIN, na.rm = T), max(sanjuan_climate$TMAX, na.rm = T)),
     xlim = c(min(sanjuan$week_start_date), max(sanjuan$week_start_date)))
lines(sanjuan_climate$Date, sanjuan_climate$TAVG, type = 'l', col = "red",
      ylim = c(min(sanjuan_climate$TMIN, na.rm = T), max(sanjuan_climate$TMAX, na.rm = T)),
      xlim = c(min(sanjuan$week_start_date), max(sanjuan$week_start_date)))
lines(sanjuan_climate$Date, sanjuan_climate$TMIN, type = 'l', col = "orange",
      ylim = c(min(sanjuan_climate$TMIN, na.rm = T), max(sanjuan_climate$TMAX, na.rm = T)),
      xlim = c(min(sanjuan$week_start_date), max(sanjuan$week_start_date)))
lines(sanjuan_climate$Date, sanjuan_climate$TMAX, type = 'l', col = "maroon",
      ylim = c(min(sanjuan_climate$TMIN, na.rm = T), max(sanjuan_climate$TMAX, na.rm = T)),
      xlim = c(min(sanjuan$week_start_date), max(sanjuan$week_start_date)))
abline(h = mean(sanjuan_climate$TMAX, na.rm = T))
abline(h = mean(sanjuan_climate$TMIN, na.rm = T))
abline(h = mean(sanjuan_climate$TAVG, na.rm = T))



x1 <- subset(sanjuan_climate, Date >= "1990-01-01" & Date < "1991-01-01")
x2 <- subset(sanjuan_climate, Date >= "2012-01-01" & Date < "2013-01-01")

plot(x1$MM, 
     x1$TAVG, 
     type = 'l', 
     col = "orange"
     # ylim = c(min(sanjuan_climate$TMIN, na.rm = T), max(sanjuan_climate$TMAX, na.rm = T)),
     )

lines(x2$MM,
      x2$TAVG,
      type = 'l',
      col = "black"
     # ylim = c(min(sanjuan_climate$TMIN, na.rm = T), max(sanjuan_climate$TMAX, na.rm = T)),
)

# format dz dfs to have date, year, and month
disease_df <- iquitos
# disease_df <- sanjuan
climate_df <- iquitos_climate


# plot climate data ----------------------------------------------------------------

# custom functions
stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

summarize_seasonal_data <- function(disease_df, climate_df, clim_vars, year, month){
  min_year <- as.numeric(min(format(disease_df$Date, "%Y")))
  max_year <- as.numeric(max(format(disease_df$Date, "%Y")))
  early_years <- seq(min_year - 2, min_year + 2, 1)
  late_years <- seq(max_year - 2, max_year + 2, 1) 
  
  climate_df$Year <- climate_df[, year]
  
  climate_df$time_period[climate_df$Year %in% early_years] <- paste0(min(early_years), "-", max(early_years)) #"early"
  climate_df$time_period[climate_df$Year %in% late_years] <- paste0(min(late_years), "-", max(late_years)) #"late"
  
  climate_df$month <- climate_df[, month]
  
  clim <- climate_df %>%
    filter(!is.na(time_period)) %>%
    group_by(time_period, month) %>%
    summarize_at(vars(clim_vars), 
                 funs(mean(., na.rm = TRUE),
                      stderr(., na.rm = TRUE)
                      )
                 )
  }


sj_ws_df <- summarize_seasonal_data(disease_df = sanjuan, 
                                 climate_df = sanjuan_ws_climate, 
                                 clim_vars = c("TMAX", 
                                               "TMIN", 
                                               "TAVG",
                                               "TDTR",
                                               "PRCP"), 
                                 year = "YYYY",
                                 month = "MM")

iq_ws_df <- summarize_seasonal_data(disease_df = iquitos, 
                                 climate_df = iquitos_ws_climate, 
                                 clim_vars = c("TMAX", 
                                               "TMIN", 
                                               "TAVG",
                                               "TDTR",
                                               "PRCP"),
                                 year = "YYYY",
                                 month = "MM")


sj_era_df <- summarize_seasonal_data(disease_df = sanjuan,
                                     climate_df = sanjuan_era_climate,
                                     clim_vars = c("relative_humidity_percent",
                                                  "specific_humidity_gkg",
                                                  "precipitation_amount_kgm2",
                                                  "TDTR_K",
                                                  "air_temperature_C",
                                                  "minimum_air_temperature_C",
                                                  "maximum_air_temperature_C",
                                                  "TAVG_C"),
                                     year = "Year",
                                     month = "Month")

iq_era_df <- summarize_seasonal_data(disease_df = iquitos, 
                                    climate_df = iquitos_era_climate, 
                                    clim_vars = c("relative_humidity_percent",
                                                  "specific_humidity_gkg",
                                                  "precipitation_amount_kgm2",
                                                  "TDTR_K",
                                                  "air_temperature_C",
                                                  "minimum_air_temperature_C",
                                                  "maximum_air_temperature_C",
                                                  "TAVG_C"),
                                    year = "Year",
                                    month = "Month")


seasonal_change_plot <- function(seasonal_df, yvar, month, location){
  # set filepath to save plot
  localeName <- gsub(" |,", "_", location)
  plot_filepath <- paste0(fig_dir, localeName, "_", yvar, ".pdf")
  
  # create new columns for upper and lower values for ribbons
  mean_col <- paste0(yvar, "_mean")
  seasonal_df$se_upr <- unlist(seasonal_df[, mean_col]) + unlist(seasonal_df[,paste0(yvar, "_stderr")])
  seasonal_df$se_lwr <- unlist(seasonal_df[, mean_col]) - unlist(seasonal_df[,paste0(yvar, "_stderr")])

  # plot
  seasonal_plot <- ggplot(data = seasonal_df, 
         aes_string(x = "month", 
                    y = mean_col, 
                    fill = "time_period"
                    )
  ) + 
    geom_point(size = 2) +
    geom_line(size = 1.2) + 
    geom_ribbon(aes(ymax = se_upr,
                    ymin = se_lwr), 
                alpha = 0.5) +
    theme_classic() +
    theme(text=element_text(size = 18)) +
    scale_x_continuous(breaks = seq(1, 12, 1)) +
    ylab(yvar) +
    xlab("Month") +
    ggtitle(paste0(location, ", ", yvar))
  
  ggsave(seasonal_plot, file = plot_filepath, width = 10.8, height = 5.74)
}

fig_dir <- "../figures/seasonal_change_plots/"


dengue_df_list <- list(sj_ws_df,
                       iq_ws_df,
                       sj_era_df,
                       iq_era_df)

location_names <- c("San Juan, PR",
                    "Iquitos, Peru",
                    "San Juan, PR",
                    "Iquitos, Peru")

# TDTR = Diurnal Temperature Range (difference between daily max and min)
ws_vars <- c("TMAX",
              "TMIN",
              "TAVG",
              "TDTR",
              "PRCP")

era_vars <- c("relative_humidity_percent",
              "specific_humidity_gkg",
              "precipitation_amount_kgm2",
              "TDTR_K",
              "air_temperature_C",
              "minimum_air_temperature_C",
              "maximum_air_temperature_C",
              "TAVG_C")


for(i in 1:length(dengue_df_list)){
  x <- dengue_df_list[[i]]
  if(i == 1 | i == 2){
    yvar_vec <- ws_vars
  } else {
    yvar_vec <- era_vars
  }
  for(j in 1:length(yvar_vec)){
    seasonal_change_plot(seasonal_df = x,
                         yvar = yvar_vec[j],
                         month = month,
                         location = location_names[i]
    )
    }
  }

