library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)
library(RSQLite)
library(DBI)






getLocationData_TERNSurveillance <- function(DataSet){

  OrgName <- getOrgName(DataSet)
  locs <- fromJSON("http://swarmapi.ausplots.aekos.org.au/ross" )

  bits1<-str_split(locs$visit_date, 'T')
  bits <- str_split(sapply(bits1, function (x) x[1]), '-')
  y <- sprintf("%04d", as.numeric(sapply(bits, function (x) x[1])))
  m <- sprintf("%02d", as.numeric(sapply(bits, function (x) x[2])))
  d <-sprintf("%02d", as.numeric(sapply(bits, function (x) x[3])))
  DateOut <- paste0(d, '-', m, '-', y)

  dfl <- data.frame(paste0( locs$site_location_name, '_', locs$site_location_visit_id), DateOut, locs$latitude, locs$longitude )
  colnames(dfl) <- c('ObsID', 'Date',  'Lat', 'Lon')
  df <- distinct(dfl)
  oOutDF <-  generateResponseAllLocs(DataSet, df$ObsID, df$Lon, df$Lat, df$Date )
}

getData_TERNSurveillance <- function(DataSet=NULL, observedProperty=NULL, observedPropertyGroup=NULL ){

  OrgName <- getOrgName(DataSet)


  allInfo <- fromJSON("http://swarmapi.ausplots.aekos.org.au/ross" )


  #ps <- getPropertiesList(observedProperty, observedPropertyGroup)
  pl <- getPropertiesList(observedProperty, observedPropertyGroup)
  mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))
  nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)


  lodfs <- list(length(nativeProps))

  if(length(nativeProps) == 0){
    return(blankResponseDF())
  }


  for (i in 1:length(nativeProps)) {

   nprop <- nativeProps[i]
   fdf <- na.omit(allInfo[allInfo$observed_property == nprop, ])
   prop <- pl[i]

    if(nrow(fdf) > 0){

      bits1<-str_split(fdf$visit_date, 'T')
      bits <- str_split(sapply(bits1, function (x) x[1]), '-')
      y <- sprintf("%04d", as.numeric(sapply(bits, function (x) x[1])))
      m <- sprintf("%02d", as.numeric(sapply(bits, function (x) x[2])))
      d <-sprintf("%02d", as.numeric(sapply(bits, function (x) x[3])))
      fdf$DateOut <- paste0(d, '-', m, '-', y)

      propertyType <- getPropertyType(prop)
      units <- getUnits(propertyType = propertyType, prop = prop)
      oOutDF <- generateResponseDF(DataSet, paste0( fdf$site_location_name, '_', fdf$site_location_visit_id), fdf$site_location_visit_id, fdf$DateOut, fdf$longitude, fdf$latitude,
                                   fdf$upper_depth, fdf$lower_depth, propertyType, prop, fdf$value, units = units)

      lodfs[[i]] <- oOutDF
    }else{

    }
  }


  outDF = as.data.frame(data.table::rbindlist(lodfs))

  if(nrow(outDF) == 0){
    return(blankResponseDF() )
  }
  return(outDF)

#     df <- data.frame(Organisation='TERN_Surveillance', ObservedProperty=unique(allInfo$observed_property), labMcode='NA')
#
#     #mappings <- na.omit(read.csv(paste0(projectRoot,'/Mappings/TERNSurveillance_PropertyMappings.csv'), stringsAsFactors = F))
#     mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))
#
#     con <- dbConnect(RSQLite::SQLite(), dbPathSoilsFed)
#
#     if(str_to_lower(propertyType) == str_to_lower(PropertyTypes$LabMethod)){
#
#       nativeProp <- mappings[mappings$labMcode == observedProperty, ]
#       fdf <- na.omit(allInfo[allInfo$observed_property == nativeProp$ObservedProperty, ])
#
#       sql <- paste0("Select * from LabMethods where LABM_CODE = '", observedProperty, "'")
#       method = doQuery(con, sql)
#       units <- method$LABM_UNITS
#     }else if(str_to_lower(propertyType) == str_to_lower(PropertyTypes$LabGroup)){
#       sql <- paste0("Select * from LabMethods where LABP_CODE = '", observedProperty, "'")
#       methods = doQuery(con, sql)
#       nativeProp <- mappings[mappings$labMcode %in% methods$LABM_CODE, ]
#       fdf <- na.omit(allInfo[allInfo$observed_property %in% nativeProp$ObservedProperty, ])
#     }
#     else if(str_to_lower(propertyType) ==  str_to_lower(PropertyTypes$Morphology)){
#       nativeProp <- mappings[mappings$labMcode == OP, ]
#       fdf <- na.omit(allInfo[allInfo$observed_property == nativeProp$ObservedProperty, ])
#       units <- 'None'
#     }
#
#     dbDisconnect(con)
#
#     outDF <- data.frame(Organisation=OrgName, OrgName, Observation_ID=paste0(fdf$site_location_name), SampleID=fdf$site_location_visit_id, Date=fdf$visit_date, Longitude=fdf$longitude, Latitude= fdf$latitude,
#                         UpperDepth=fdf$upper_depth, LowerDepth=fdf$lower_depth, DataType=propertyType, ObservedProperty=observedProperty, Value=fdf$value, Units= units, "Brilliant")
#     oOutDF <- outDF[order(outDF$Observation_ID, outDF$UpperDepth, outDF$SampleID),]
#     head(oOutDF)
#
# return(oOutDF)
}
