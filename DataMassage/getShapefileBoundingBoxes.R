require(sp)
require(rgdal)
lapply(polys@polygons, bbox)


aus <- readOGR('C:/temp/bboxes.shp')

crds <- lapply(aus@polygons, bbox)
aus@data


