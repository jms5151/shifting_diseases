# https://www.scielo.br/j/csp/a/gdJXqcrW5PPDHX8rwPDYL7F/?lang=pt#
# https://github.com/rfsaldanha/microdatasus/blob/master/R/process_sinan_malaria.R
# devtools::install_github('rfsaldanha/microdatasus')

library('microdatasus')

# dados_brutos <- fetch_datasus(year_start = 2014
#                               , year_end = 2014
#                               , information_system = 'SIM-DO')

# 2007 might be first year and last year might be 2017
malaria_df <- fetch_datasus(year_start = 2007
                            # , month_start = 1
                            , year_end = 2020
                            # , month_end = 6
                            # , uf = "RJ"
                            , information_system = "SINAN-MALARIA-FINAL")

df_a <- process_sinan_malaria(malaria_df
                              , municipality_data = FALSE)

sort(unique(df_a$DT_NOTIFIC)) # notification date

df_a$DT_NOTIFIC <- as.Date(df_a$DT_NOTIFIC, "%Y-%m-%d")
df_a$Year_Month <- format(df_a$DT_NOTIFIC, '%Y-%m')

library(tidyverse)

df_b <- df_a %>%
  group_by(Year_Month, ID_MUNICIP) %>%
  summarise(cases = length(TP_NOT))

test <- subset(df_b, ID_MUNICIP == '330455')
plot.ts(test$cases)

