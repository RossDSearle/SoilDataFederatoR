library(httr)
library(jsonlite)
library(raster)
library(MASS)
library(sf)

# use your own usr & key
usr <- 'a.macleod@federation.edu.au'
key <- 'gkMicmUcKsE8Iq'


# Get the available datasets
datasetsDF <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets')
datasets<-datasetsDF$DataSet


#iterate through the data sets to get locations where there is any soil data

res <- vector("list", length = length(datasets))
# Iterate Datasets
for (j in 1:length(datasets)) {
  d <- datasets[j]
  print(paste0(d, ' : ', d))
  url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/ObservationLocations?DataSet=", d, "&format=json&usr=", usr, "&key=", key))
  resp <- GET(url, timeout(300))
  response <- content(resp, "text", encoding = 'UTF-8')
  odf <- fromJSON(response)

  # If there is data returned slot it into the list
  if(is.data.frame(odf)){
    if(nrow(odf)>0){
      res[[j]] <- odf
    }
  }
}
outDF = as.data.frame(data.table::rbindlist(res, fill=T))
write.csv(outDF, file='C:/Projects/Small/soilsites/soilLocs.csv', row.names = F)

ptsDF <- na.omit(data.frame(Longitude=outDF$Longitude, Latitude=outDF$Latitude))
pts = st_as_sf(ptsDF, coords = c("Longitude", "Latitude"), crs = 4326)
plot(st_geometry(pts))
st_write(pts, 'C:/Projects/Small/soilsites/soilLocs.shp')

#####  Make a heatmap

templateR <- raster('C:/Projects/SMIPS/SMEstimates/BOM/AWRANetCDFs/geotifs/sm_pct/sm_pct_2016-01-01.tif')

outDF <- read.csv('C:/Projects/Small/soilsites/soilLocs.csv')

xy = na.omit(data.frame(outDF$Longitude, outDF$Latitude))
m <- kde2d(xy[,1],xy[,2],h=1,n=c( ncol(templateR), nrow(templateR)), lims = c(112, 154, -44, -10))

r <- raster(vals=t(m$z), nrows=nrow(templateR), ncols=ncol(templateR))
extent(r) <- extent(templateR)
r1 = flip(r, direction='y')
r2 <- clamp(r1, upper=0.01)
r3 <- mask(r2, templateR)
plot(sqrt(r3))

writeRaster(r3, 'C:/Projects/Small/soilsites/sitesHeatMap.tif')






#####  Extract individual soil attributes

# get the available soil properties
attDF <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups')
atts <-attDF$PropertyGroup
atts



# Get the available properties for a PropertyGroup
propsDF <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=Carbon')
props <-propsDF$Property


# Iterate Properties
for(i in 1:length(props)){
  # Make an empty list to put individual results in for this property
  res <- vector("list", length = length(datasets))
  # Iterate Datasets
  for (j in 1:length(datasets)) {
    p <- props[i]
    d <- datasets[j]
    print(paste0(d, ' : ', p))
    url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", p ,"&DataSet=", d, "&format=json&usr=", usr, "&key=", key))
    resp <- GET(url, timeout(300))
    response <- content(resp, "text", encoding = 'UTF-8')
    odf <- fromJSON(response)

    # If there is data returned slot it into the list
    if(is.data.frame(odf)){
      if(nrow(odf)>0){
        res[[j]] <- odf
      }
    }
  }
  # Merge the dataframes in the list into one dataframe
  outDF = as.data.frame(data.table::rbindlist(res, fill=T))
  # Write it to a csv
  write.csv(outDF, paste0('c:/temp/SDF_', p, '.csv'))
}
