# Create shapefile from vector occurrence data

library(rgdal)
library(sp)

library(sf)

# load data
aedes <- read.csv('../data/vectors/gbif_aedes.csv', sep = '\t')
anopheles <- read.csv('../data/vectors/gbif_anopheles.csv', sep = '\t')

# combine data
vectors <- rbind(aedes, anopheles)

# format
vectors$LAT <- vectors$decimalLatitude
vectors$LON <- vectors$decimalLongitude
vectors$Date <- as.Date(vectors$eventDate, '%Y-%m-%d')

# subset, might be useful to keep gbif id
vectors2 <- vectors[, c('LAT', 'LON', 'Date', 'gbifID', 'genus', 'species')]

# remove rows with no coordinates or dates
# later can fill in some missing coordinates by geolocating stateProvinces col
vectors2 <- vectors2[complete.cases(vectors2), ]

# remove data earlier than 1980, may want earlier data later
vectors2 <- subset(vectors2, Date >= '1980-01-01')

# convert XYZ data to shapefile

# Define coordinate reference system
prj4string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
my.projection <- st_crs(prj4string)

# Create sf object
vectors_sf <- st_as_sf(vectors2, coords = c("LON", "LAT"))
st_crs(vectors_sf) <- my.projection

# check output
# plot(vectors_sf)

# Export shapefile
st_write(vectors_sf, "../data/vectors/vectors_shp/vectors.shp")
