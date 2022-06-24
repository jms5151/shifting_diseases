# # Remove special characters - peru data
# 
# source('codes/functions_fix_special_characters.R')
# 
# # load data
# peru_dengue <- read.csv('../data/dengue/dengue_peru.csv', check.names = F)
# peru_malaria_falciparum <- read.csv('../data/malaria/P_falciparum_malaria_Peru.csv', check.names = F)
# peru_provinces <- read.csv('../data/regions/Peru_districts_provinces.csv')
# 
# # replace characters
# xperu_dengue <- replace_spanish_characters(df = peru_dengue, col_name = 'District')
# xperu_malaria_falciparum <- replace_spanish_characters(df = peru_malaria_falciparum, col_name = 'District')
# xperu_provinces <- replace_spanish_characters(df = peru_provinces, col_name = 'District')
# 
# 
# sort(setdiff(xperu_dengue$District, xperu_provinces$District))
# # sort(setdiff(xperu_provinces$District, xperu_dengue$District))
# # 'Balsapuerto', 'Balsa Puerto'
# 
# 
# sort(setdiff(xperu_malaria_falciparum$District, xperu_provinces$District))
# # sort(setdiff(xperu_provinces$District, xperu_malaria_falciparum$District)0
# 
# brazil_dengue <- read.csv('../data/dengue/Lowe_etal_LPH_2021_brazil_dengue_data_2000_2019.csv')
# xbrazil_dengue <- replace_spanish_characters(df = brazil_dengue, col_name = 'state_name')
# unique(xbrazil_dengue$state_name)
# 
# # # save data
# # write.csv(peru_dengue, '../data/dengue/dengue_peru_char_fixed.csv', row.names = F)
# # write.csv(peru_malaria_vivax, '../data/malaria/malaria_vivax_peru_char_fixed.csv', row.names = F)
# # write.csv(peru_malaria_falciparum, '../data/malaria/malaria_pfal_peru_char_fixed.csv', row.names = F)
