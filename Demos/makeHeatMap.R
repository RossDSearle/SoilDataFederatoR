library(raster)
library(MASS)


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
