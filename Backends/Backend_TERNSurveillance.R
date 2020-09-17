library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)
library(RSQLite)
library(DBI)






getLocationData_TERNSurveillance <- function(DataSet){

  OrgName <- getOrgName(DataSet)
  ep <- getNativeAPIurl(DataSet)
  locs <- fromJSON(ep )

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

  ep <- getNativeAPIurl(DataSet)
  allInfo <- fromJSON(ep )

  propRecs <- getNativeProperties(DataSet=DataSet, observedProperty, observedPropertyGroup)


  if(nrow(propRecs) == 0){
    return(blankResponseDF())
  }


  for (i in 1:nrow(propRecs)) {

    nProp <- propRecs$nativeProp[i]
    sProp <- propRecs$standardProp[i]

    propertyType <- propRecs$propertyType[i]
    units <- getUnits(propertyType = propertyType, prop = sProp)

   fdf <- na.omit(allInfo[allInfo$observed_property == nProp, ])


    if(nrow(fdf) > 0){

      bits1<-str_split(fdf$visit_date, 'T')
      bits <- str_split(sapply(bits1, function (x) x[1]), '-')
      y <- sprintf("%04d", as.numeric(sapply(bits, function (x) x[1])))
      m <- sprintf("%02d", as.numeric(sapply(bits, function (x) x[2])))
      d <-sprintf("%02d", as.numeric(sapply(bits, function (x) x[3])))
      fdf$DateOut <- paste0(d, '-', m, '-', y)

      oOutDF <- generateResponseDF(DataSet, paste0( fdf$site_location_name, '_', fdf$site_location_visit_id), fdf$site_location_visit_id, fdf$DateOut, fdf$longitude, fdf$latitude,
                                   fdf$upper_depth, fdf$lower_depth, propertyType, sProp, fdf$value, units = units)

      lodfs[[i]] <- oOutDF
    }else{

    }
  }


  outDF = as.data.frame(data.table::rbindlist(lodfs))

  if(nrow(outDF) == 0){
    return(blankResponseDF() )
  }
  return(outDF)

}