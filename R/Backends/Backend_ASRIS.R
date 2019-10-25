library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)



# http://asris-daas02/NT_Services/api/MorphologyMetaData
# http://asris-daas02/NT_Services/api/MorphResults?morphology_attribute=h_upper_depth
# http://asris-daas02/NT_Services/api/MorphResults?morphology_attribute=h_upper_depth&page_num=1&page_size=10
#
# Lab Results end points:
# http://asris-daas02/NT_Services/api/LabResults
# http://asris-daas02/NT_Services/api/LabResults?page=1&count=10
# http://asris-daas02/NT_Services/api/LabResults?method_code=15_NR_CA&page_num=1&page_size=10
# http://asris-daas02/NT_Services/api/LabResults?method_code=15_NR_CA&horizon_num=4&page_num=1&page_size=15
# http://asris-daas02/NT_Services/api/LabResults?method_code=15_NR_CA&horizon_num=4&horizon_lower_depth=1&horizon_upper_dept=2&upper_bound_lat=-13&lower_bound_lat=-14.514&upper_bound_lon=131.092&lower_bound_lon=130


getService <- function(Dataset){

  if(Dataset=='NatSoil'){
    return('http://asris-daas02/NatSoil_Services/api')

  }else if(Dataset=='NTGovernment'){
    return('http://asris-daas02/NT_Services/api')
  }
  return(NULL)
}


getLocationData_ASRIS<- function(DataSet){

  ep <- getService(DataSet)
  OrgName <- getOrgName(DataSet)
  fdf <- fromJSON(paste0(ep, '/MorphResults?morphology_attribute=s_date_desc'))
  day <- str_sub(fdf$o_date_desc, 1,2)
  mnth <- str_sub(fdf$o_date_desc, 3,4)
  yr <- str_sub(fdf$o_date_desc, 5,8)
  oOutDF <-  generateResponseAllLocs(OrgName, DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$o_longitude_GDA94, fdf$o_latitude_GDA94, paste0(day, '-', mnth, '-', yr,'T00:00:00' ) )
  return(oOutDF)
}



getData_ASRIS<- function(DataSet=NULL, DataStore, observedProperty=NULL, observedPropertyGroup=NULL ){

 ep <- getService(DataSet)

  OrgName <- getOrgName(DataSet)
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

          oOutDF <- generateResponseDF(OrgName, DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$samp_no ,paste0(day, '-', mnth, '-', yr,'T00:00:00' ) , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                       fdf$samp_upper_depth , fdf$samp_lower_depth , propType, ps[i], fdf$labr_value , units)
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

       oOutDF <- generateResponseDF(OrgName, DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$samp_no ,paste0(day, '-', mnth, '-', yr,'T00:00:00' ) , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                    fdf$samp_upper_depth , fdf$samp_lower_depth , propType, ps[i], fdf$morphology_attribute_value , units)
       lodfs[[i]] <- oOutDF
     }else{
       return(blankResponseDF())
     }

    }
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  return(outDF)
}
