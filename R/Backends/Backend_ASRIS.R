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


getASRISService <- function(Dataset){

  if(str_to_upper(Dataset)=='NATSOIL'){
    return('http://asris-daas02/NatSoil_Services/api')
  }else if(str_to_upper(Dataset)=='NTGOVERNMENT'){
    return('http://asris-daas02/NT_Services/api')
  }else if(str_to_upper(Dataset)=='WAGOVERNMENT'){
    return('http://asris-daas02/WA_Services/api')
  }  else if(str_to_upper(Dataset)=='SCARP'){
    return('http://asris-daas02/NatSoil_restricted_Services/api')
  }

  return(NULL)
}


getLocationData_ASRIS<- function(DataSet){

  ep <- getASRISService(DataSet)
  OrgName <- getOrgName(DataSet)
  fdf <- fromJSON(paste0(ep, '/MorphResults?morphology_attribute=s_date_desc'))
  day <- str_sub(fdf$o_date_desc, 1,2)
  mnth <- str_sub(fdf$o_date_desc, 3,4)
  yr <- str_sub(fdf$o_date_desc, 5,8)
  oOutDF <-  generateResponseAllLocs(DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$o_longitude_GDA94, fdf$o_latitude_GDA94, paste0(day, '-', mnth, '-', yr,'T00:00:00' ) )
  return(oOutDF)
}



getData_ASRIS<- function(DataSet=NULL, observedProperty=NULL, observedPropertyGroup=NULL ){

 ep <- getASRISService(DataSet)

  OrgName <- getOrgName(DataSet)
  propList <- getPropertiesList(observedProperty, observedPropertyGroup)

  if(length(propList) == 0){
    return(blankResponseDF())
  }

  lodfs <- list(length(propList))


  for (i in 1:length(propList)) {

    ObsProp <- propList[i]
    propertyType <- getPropertyType(ObsProp)
    units <- getUnits(propertyType = propertyType, prop = ObsProp)

    if(propertyType == 'LaboratoryMeasurement'){
      ### Hit the Laboratory endpoint

      if(ep=='http://asris-daas02/NatSoil_restricted_Services/api'){
        url <- paste0(ep, '/LabResults?method_code=', ObsProp)
        #url <- paste0(ep, '/LabResults?method_code=', ObsProp , '&proj_code=', DataSet)
      }else{
        url <- paste0(ep, '/LabResults?method_code=', ObsProp)
      }

      fdfRaw <- fromJSON(url)


        if(nrow(fdfRaw) > 0){

          if(ep=='http://asris-daas02/NatSoil_restricted_Services/api'){
            fdf <- fdfRaw[fdfRaw$proj_code == DataSet, ]
          }else{
            fdf <- fdfRaw
          }

          day <- str_sub(fdf$o_date_desc, 1,2)
          mnth <- str_sub(fdf$o_date_desc, 3,4)
          yr <- str_sub(fdf$o_date_desc, 5,8)

          oOutDF <- generateResponseDF(DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$samp_no ,paste0(day, '-', mnth, '-', yr,'T00:00:00' ) , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                       fdf$samp_upper_depth , fdf$samp_lower_depth , propertyType, ObsProp, fdf$labr_value , units)
          lodfs[[i]] <- oOutDF
        }else{
          return(blankResponseDF())
        }
    }else{

    ### Hit the morpholgy endpoint

      if(ep=='http://asris-daas02/NatSoil_restricted_Services/api'){
        url <- paste0(ep, '/MorphResults?morphology_attribute=', ObsProp)
        #url <- paste0(ep, '/MorphResults?morphology_attribute=', ObsProp , '&proj_code=', DataSet)
      }else{
        url <- paste0(ep, '/MorphResults?morphology_attribute=', ObsProp)
      }

    fdfRaw <- fromJSON(url)

     if(nrow(fdfRaw) > 0){

       if(ep=='http://asris-daas02/NatSoil_restricted_Services/api'){
         fdf <- fdfRaw[fdfRaw$proj_code == DataSet, ]
       }else{
           fdf <- fdfRaw
       }

       day <- str_sub(fdf$o_date_desc, 1,2)
       mnth <- str_sub(fdf$o_date_desc, 3,4)
       yr <- str_sub(fdf$o_date_desc, 5,8)

       oOutDF <- generateResponseDF(DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$samp_no ,paste0(day, '-', mnth, '-', yr,'T00:00:00' ) , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                    fdf$h_upper_depth , fdf$h_lower_depth , propertyType, ObsProp, fdf$morphology_attribute_value , units)
       NoOutDF <- oOutDF[!is.na(oOutDF$Value), ]
       NoOutDF <- oOutDF[oOutDF$Value != '', ]

       lodfs[[i]] <- NoOutDF
     }else{
       return(blankResponseDF())
     }

    }
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  return(outDF)
}
