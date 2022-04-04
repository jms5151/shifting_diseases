# load data
load('../data/tsir_outputs/brazil_dengue_empirical_betas.RData')
load('../data/tsir_outputs/thailand_dengue_empirical_betas.RData')

# plot results
plot_Ieff_and_betas <- function(x, country_name, disease_name){
  for(n in 1:length(x)){
    
    site_name <- gsub(' ', '_', names(x)[n])
    
    plot_filepath <- paste0('../figures/empirical_betas/'
                            , country_name
                            , '_' 
                            , site_name
                            , '_'
                            , disease_name
                            , '.pdf')
    
    pdf(plot_filepath
        , width = 9
        , height = 7)
    
    par(mfrow = c(3,1)
        , mar = c(4,4,1,1)
        , cex = 1.3)
    
    last_time_point_to_plot <- length(x[[n]]$betas)-3
    
    plot.ts(x[[n]]$S[1:last_time_point_to_plot]
            , ylab = 'Susceptible'
            , xlab = ''
            , main = paste0(names(x)[n], ', ', country_name)
            )
    legend("bottomright"
           , legend = paste0("Sbar = ", round(x[[n]]$S_bar))
           , bty = 'n'
           )
    
    plot.ts(x[[n]]$Ieffective[1:last_time_point_to_plot]
            , ylab = 'Ieffective'
            , xlab = ''
            )
    legend("topright"
           , legend = paste0("mean(rho) = ", round(mean(x[[n]]$rho), 2))
           , bty = 'n'
           )
    
    plot.ts(x[[n]]$betas[1:last_time_point_to_plot]
            , ylab = 'beta'
        )
    
    dev.off()
    
  }
}

plot_Ieff_and_betas(x = brazil_dengue_betas
                    , country_name = 'Brazil'
                    , disease_name = 'dengue')

plot_Ieff_and_betas(x = thailand_dengue_betas
                    , country_name = 'Thailand'
                    , disease_name = 'dengue')

plot_Ieff_and_betas(x = kenya_dengue_betas
                    , country_name = 'Kenya'
                    , disease_name = 'dengue')

# hist(x[[n]]$betas)
# range(x[[n]]$betas)

# test <- data.frame()
# 
# for(i in 1:length(brazil_betas)){
#   temp_df <- data.frame("betas" = brazil_betas[[i]]$rho
#                      , "Admin1Name" = names(brazil_betas)[i]
#                      , "WOY" = brazil_betas[[i]]$WOY
#                      , "Year" = brazil_betas[[i]]$Year
#                      )
#   test <- rbind(test
#                 , temp_df)
# }
# 
# boxplot(betas ~ Admin1Name, data = test)
# boxplot(betas ~ WOY, data = test)
# boxplot(betas ~ Year, data = test)
# 
# test2 <- subset(test, Admin1Name == "Acre")
# boxplot(betas ~ Year, data = test2)
# boxplot(betas ~ WOY, data = test2)