library(rgdal)
library(sf)
library(aqp)
library(jsonlite)


source(paste0('Backends.R'))

usr <- 'ross.searle@csiro.au'; key <- 'a';


vic <- getSoilData(DataSets='VicGovernment', observedProperty='6A1', usr='ross.searle@csiro.au', key='a')


pts <- st_as_sf(vic, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
plot(pts)
head(pts)
plot(st_geometry(pts), col = sf.colors(12, categorical = F),  axes = TRUE)
aus <- st_read('C:/Projects/GIS/National/Australia.shp')
plot(st_geometry(aus), add=T)
vicPoly<- st_crop(aus, xmin = 140, xmax = 151, ymin = -40, ymax = -33)
plot(vicPoly)
st_write(pts, 'c:/temp/vic.shp')


DataSet='VicGovernment'

pts <- st_as_sf(sdf, coords = c("site_east", "site_north"), crs = 4326, na.fail = F)



sdf <- df
sdf$Longitude <- NA
sdf$Latitude <- NA


plot(st_geometry(aus))

sdf <- fdf
write.csv(sdf, 'c:/temp/vicpts.csv')
allcrs <- VicCRSs()
ucrs <- VicUniqueDatums(sdf)

library(RColorBrewer)
n <- 19
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
cols <- sample(col_vector, n)

plot(st_geometry(vicPoly))
for (i in 1:nrow(ucrs)) {
  rec <- ucrs[i,]
  print(i)
  if(rec$site_zone != '' & rec$site_zone != 65){
    idxs <- which(sdf$site_zone == rec$site_zone & sdf$site_datum == rec$site_datum)
    indf <- sdf[idxs,]
    epsg <- VicEPSG(availDatums=allcrs, zone=rec$site_zone, datum=rec$site_datum)
    #epsg <- VicEPSG(availDatums=allcrs, zone=56, datum=rec$site_datum)
    df.SP <- st_as_sf(sdf[idxs,], coords = c("site_east", "site_north"), na.fail=F, remove=F, crs=epsg)
    projCoords <- st_transform(df.SP ,  crs = 4326)
    #plot(st_geometry(projCoords))
    xys <- as.data.frame(st_coordinates(projCoords))

    sdf$Longitude[idxs] <- xys$X
    sdf$Latitude[idxs] <- xys$Y

    #print(paste0(rec$site_zone, ' : ', rec$site_datum))
    plot(st_geometry(projCoords), col = cols[i], add=T)
   Sys.sleep(2)
  }
}



