# Remove special characters - Colombia data

# load data
colombia_dengue <- read.csv('../data/dengue/dengue_colombia.csv', check.names = F)
colombia_malaria_vivax <- read.csv('../data/malaria/malaria_vivax_colombia.csv', check.names = F)
colombia_malaria_falciparum <- read.csv('../data/malaria/malaria_pfal_colombia.csv', check.names = F)

# create function specific to this dataset
replace_characters <- function(df){
  df$Muni_Name <- gsub('BRICEï¿½ï¿½O', 'BRICENO', df$Muni_Name)
  df$Muni_Name <- gsub('CAï¿½ï¿½ASGORDAS', 'CANASGORDAS', df$Muni_Name)
  df$Muni_Name <- gsub('COVEï¿½ï¿½AS', 'COVENAS', df$Muni_Name)
  df$Muni_Name <- gsub('EL PEï¿½ï¿½OL', 'EL PENOL', df$Muni_Name)
  df$Muni_Name <- gsub('EL PEï¿½ï¿½ON', 'EL PENON', df$Muni_Name)
  df$Muni_Name <- gsub('EL PIï¿½ï¿½ON', 'EL PINON', df$Muni_Name)
  df$Muni_Name <- gsub('LA MONTAï¿½ï¿½ITA', 'LA MONTANITA', df$Muni_Name)
  df$Muni_Name <- gsub('LA PEï¿½ï¿½A', 'LA PENA', df$Muni_Name)
  df$Muni_Name <- gsub('NARIï¿½ï¿½O', 'NARINO', df$Muni_Name)
  df$Muni_Name <- gsub('OCAï¿½ï¿½A', 'OCANA', df$Muni_Name)
  df$Muni_Name <- gsub('PEï¿½ï¿½OL', 'PENOL', df$Muni_Name)
  df$Muni_Name <- gsub('PIJIï¿½ï¿½O DEL CARMEN', 'PIJINO', df$Muni_Name)
  df$Muni_Name <- gsub('PUERTO CARREï¿½ï¿½O', 'PUERTO CARRENO', df$Muni_Name)
  df$Muni_Name <- gsub('PUERTO NARIï¿½ï¿½O', 'PUERTO NARINO', df$Muni_Name)
  df$Muni_Name <- gsub('SALDAï¿½ï¿½A', 'SALDANA', df$Muni_Name)
  df$Muni_Name <- gsub('SANTAFE DE BOGOTA D.C.', 'SANTA FE', df$Muni_Name)
  df$Muni_Name <- gsub('CHIVOLO', 'CHIBOLO', df$Muni_Name)
  df$Muni_Name <- gsub('ITSMINA', 'ISTMINA', df$Muni_Name)
  df$Muni_Name <- gsub('MOï¿½ï¿½ITOS', 'MONITOS', df$Muni_Name)
  df$Muni_Name <- gsub('PUEBLOVIEJO', 'PUEBLO VIEJO', df$Muni_Name)
  df$Muni_Name <- gsub('SAN JOSE DE LA MONTAï¿½ï¿½A', 'SAN JOSE DE LA MONTANA', df$Muni_Name)
  df$Muni_Name <- gsub('SANTAFE DE ANTIOQUIA', 'SANTA FE DE ANTIOQUIA', df$Muni_Name)
  df$Muni_Name <- gsub('VISTAHERMOSA', 'VISTA HERMOSA', df$Muni_Name)
  df$Muni_Name <- gsub('  ', ' ', df$Muni_Name)
  df
}

# replace characters
colombia_dengue <- replace_characters(colombia_dengue)
colombia_malaria_vivax <- replace_characters(colombia_malaria_vivax)
colombia_malaria_falciparum <- replace_characters(colombia_malaria_falciparum)

# save data
write.csv(colombia_dengue, '../data/dengue/dengue_colombia_char_fixed.csv', row.names = F)
write.csv(colombia_malaria_vivax, '../data/malaria/malaria_vivax_colombia_char_fixed.csv', row.names = F)
write.csv(colombia_malaria_falciparum, '../data/malaria/malaria_pfal_colombia_char_fixed.csv', row.names = F)
