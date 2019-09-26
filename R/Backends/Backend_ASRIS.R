library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)

ep <- 'http://asris-daas02/NatSoil_Services/api'


getData_ASRIS<- function(provider=NULL, observedProperty=NULL, observedPropertyGroup=NULL ){




  OrgName <- provider
  ps <- getPropertiesList(observedProperty, observedPropertyGroup)

  mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))
  nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)

  if(length(nativeProps) == 0){
    return(blankResponseDF())
  }

  lodfs <- list(length(nativeProps))


  for (i in 1:length(nativeProps)) {

    nprop <- nativeProps[i]
    propertyType <- getPropertyType(observedProperty)

    if(propertyType == 'LaboratoryMeasurement'){
      ### Hit the Laboratory endpoint

        fdf <- fromJSON(paste0(ep, '/LabResults?method_code=', observedProperty))
        if(nrow(fdf) > 0){

          propType <- getPropertyType(prop)

          units <- getUnits(propertyType = propType, prop = prop)

          day <- str_sub(fdf$o_date_desc, 1,2)
          mnth <- str_sub(fdf$o_date_desc, 3,4)
          yr <- str_sub(fdf$o_date_desc, 5,8)

          oOutDF <- generateResponseDF(provider, OrgName, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$samp_no ,paste0(day, '-', mnth, '-', yr,'T00:00:00' ) , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                       fdf$samp_upper_depth , fdf$samp_lower_depth , propType, ps[i], fdf$labr_value , units, 'Brilliant')
          lodfs[[i]] <- oOutDF
        }else{
          return(blankResponseDF())
        }
    }else{

    ### Hit the morpholgy endpoint
      fdf <- fromJSON(paste0(ep, '/MorphResults?morphology_attribute=', observedProperty))
     print(head(fdf))

     if(nrow(fdf) > 0){

       propType <- getPropertyType(nprop)

       units <- getUnits(propertyType = propType, prop = nprop)

       day <- str_sub(fdf$o_date_desc, 1,2)
       mnth <- str_sub(fdf$o_date_desc, 3,4)
       yr <- str_sub(fdf$o_date_desc, 5,8)

       oOutDF <- generateResponseDF(provider, OrgName, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$samp_no ,paste0(day, '-', mnth, '-', yr,'T00:00:00' ) , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                    fdf$samp_upper_depth , fdf$samp_lower_depth , propType, ps[i], fdf$morphology_attribute_value , units, 'Brilliant')
       lodfs[[i]] <- oOutDF
     }else{
       return(blankResponseDF())
     }

    }
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  return(outDF)
}
