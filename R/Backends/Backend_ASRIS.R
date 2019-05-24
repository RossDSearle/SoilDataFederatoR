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

getData_ASRIS<- function( observedProperty=NULL, observedPropertyGroup=NULL ){

  OrgName <- 'ASRIS'
  print(paste0('Extracting data from ', OrgName))

  mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))

  nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)

  if(length(nativeProp) == 0){
    return(blankResponseDF())
  }

  lodfs <- list(length(nativeProps))

  for (i in 1:length(nativeProps)) {

    prop <- nativeProps[i]
    cat(prop, '\n')
    sd <- ASRIS_df[str_to_upper(ASRIS_df$labm_code) == str_to_upper(prop), ]
    if(nrow(sd) > 0){
      fdf <- sd
      propType <- getPropertyType(prop)

      units <- getUnits(propertyType = propertyType, prop = prop)

      oOutDF <- generateResponseDF(OrgName, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$samp_no , '01-01-2000T00:00:00' , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                   fdf$samp_upper_depth , fdf$samp_lower_depth , propType, prop, fdf$labr_value , units)
      lodfs[[i]] <- oOutDF
    }
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  return(outDF)
}
