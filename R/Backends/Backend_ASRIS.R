library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)

####     This needs to be finalised once Ramneek has finished building the endpoint   ########################

# df <- fromJSON("http://kharak-bm/WebApplication1/api/LabResults")
# write.csv(df, 'c:/temp/Asris.csv')
#
# jsn <- getURL("http://kharak-bm/WebApplication1/api/LabResults")
# df <- fromJSON(jsn)
#
# write.csv(df, 'c:/temp/Asris.csv')
# cat(jsn, file='c:/temp/Asris.json')
#
# df <- read.csv( 'c:/temp/Asris.csv')


#df <- fromJSON('http://kharak-bm:53568/api/LabResults?method_code=15_NR_CA')

ep <- 'http://asris-daas02/WebApplication1_deploy/api'


getData_ASRIS<- function( observedProperty=NULL, observedPropertyGroup=NULL ){

  OrgName <- 'ASRIS'

  mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))
  nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)

  if(length(nativeProps) == 0){
    return(blankResponseDF())
  }

  lodfs <- vector("list", length(nativeProps))

  for (i in 1:length(nativeProps)) {

    prop <- nativeProps[i]

    fdf <- fromJSON(paste0(ep, '/LabResults?method_code=', observedProperty))
    if(nrow(fdf) > 0){

      propType <- getPropertyType(prop)

      units <- getUnits(propertyType = propertyType, prop = prop)

      day <- str_sub(fdf$o_date_desc, 1,2)
      mnth <- str_sub(fdf$o_date_desc, 3,4)
      yr <- str_sub(fdf$o_date_desc, 5,8)

      oOutDF <- generateResponseDF(OrgName, 'ASRIS', paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$samp_no ,paste0(day, '-', mnth, '-', yr,'T00:00:00' ) , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                   fdf$samp_upper_depth , fdf$samp_lower_depth , propType, prop, fdf$labr_value , units, 'Brilliant')
      lodfs[[i]] <- oOutDF
    }else{
      return(blankResponseDF())
    }
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  return(outDF)
}
