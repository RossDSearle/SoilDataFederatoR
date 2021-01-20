library(httr)
library(rgdal)
library(gdalUtils)      # wrappers for GDAL utility programs that could be
#  called from the command line
library(sp)             # spatial data types, ASDAR structures
library(sf)
library(XML)
library(xml2)


wfs <- "http://maps.isric.org/mapserv?map=/map/wosis_latest.map&SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=ms:wosis_latest_totc&TYPENAME=ms:wosis_latest_totc&SRSNAME=urn:ogc:def:crs:EPSG::4326&BBOX=-227.65729865618612848,-223.59341419347103397,217.58785974381481765,144.63912230687341776,urn:ogc:def:crs:EPSG::4326"

#wfs <- "http://maps.isric.org/mapserv?map=/map/wosis_latest.map&REQUEST=GetCapabilities"

d <- GET(wfs, verbose())


xml <- content(d, "text")
xmltree  <- xmlTreeParse(xml, asText = TRUE,useInternalNodes=T)


prop<-'clay'

xmlValue(xml, '//wfs:FeatureCollection/wfs:member/ms:wosis_latest_clay/ms:country_name')

projpaths <- xml_text(xml_find_all(xmltree, '//wfs:FeatureCollection/wfs:member/ms:wosis_latest_clay/ms:country_name'))

country <- xpathSApply(xmltree, '/wfs:FeatureCollection/wfs:member/ms:wosis_latest_totc/ms:country_name', xmlValue)
country <- xpathSApply(xmltree, '/wfs:FeatureCollection/wfs:member/ms:wosis_latest_totc/ms:profile_id', xmlValue)
country <- xpathSApply(xmltree, '/wfs:FeatureCollection/wfs:member/ms:wosis_latest_totc/ms:profile_layer_id', xmlValue)
country <- xpathSApply(xmltree, '/wfs:FeatureCollection/wfs:member/ms:wosis_latest_totc/ms:upper_depth', xmlValue)
country <- xpathSApply(xmltree, '/wfs:FeatureCollection/wfs:member/ms:wosis_latest_totc/ms:lower_depth', xmlValue)
country <- xpathSApply(xmltree, '/wfs:FeatureCollection/wfs:member/ms:wosis_latest_totc/ms:layer_name', xmlValue)
country <- xpathSApply(xmltree, '/wfs:FeatureCollection/wfs:member/ms:wosis_latest_totc/ms:', xmlValue)






