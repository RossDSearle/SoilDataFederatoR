library(rgdal)
library(sf)
library(aqp)
library(jsonlite)
library(RColorBrewer)

source(paste0('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/Backends.R'))

usr <- 'ross.searle@csiro.au'; key <- 'a';

datasets <- getDataSets(usr, key)

aus <- st_read('C:/Projects/GIS/National/Australia.shp')
plot(st_geometry(aus))

n <- length(datasets)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
cols <- sample(col_vector, n)

plot(st_geometry(aus))
odf <- data.frame()
for (i in 1:n) {
  ds <- datasets$DataSet[i]
  print(ds)
  df <- getSiteLocations(DataSets=ds, usr=usr, key=key)
  df.SP <- st_as_sf(df, coords = c("Longitude", "Latitude"), na.fail=F, remove=F, crs = 4326)
 # plot(st_geometry(df.SP), col = cols[i], add=T)
  st_write(df.SP, paste0('C:/Projects/TernLandscapes/SDF/Locations/', ds, '.shp'))
  odf <- rbind(odf, df.SP)
}

st_write(odf, paste0('C:/Projects/TernLandscapes/SDF/Locations/all_locations.shp'))

