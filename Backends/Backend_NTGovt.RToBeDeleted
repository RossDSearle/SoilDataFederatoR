library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)

ep <- 'http://asris-daas02/NT_Services/api/'


getData_NTGovt<- function(provider=NULL, observedProperty=NULL, observedPropertyGroup=NULL ){




  OrgName <- provider
  ps <- getPropertiesList(observedProperty, observedPropertyGroup)

  mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", "CSIRO", "'" ))
  nativeProps <- getNativeProperties("CSIRO", mappings, observedProperty, observedPropertyGroup)
 # mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))
 # nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)

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










# library(RSQLite)
# library(DBI)
# library(stringr)
#
# mo2Num <- function(x) match(tolower(x), tolower(month.abb))
#
# shortToFullYear <- function(x){
#
#   if(as.numeric(x) < 40){
#     return(paste0('20', x))
#   }else{
#     return(paste0('19', x))
#   }
# }
#
# machineName <- as.character(Sys.info()['nodename'])
# # if(!asPkg){
#   if(machineName=='soils-discovery'){
#     NT_DB_Path <- '/srv/plumber/TERNLandscapes/SoilDataFederatoR/HostedDBs/NT_DB.db'
#   }else{
#     NT_DB_Path <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/HostedDBs/NT_DB.db'
#   }
# # }
#
#
#
# getData_NTGovt <- function(provider=NULL, observedProperty=NULL, observedPropertyGroup=NULL){
#
#
#   OrgName <- provider
#   ps <- getPropertiesList(observedProperty, observedPropertyGroup)
#
#   mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))
#   nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)
#
#   lodfs <- list(length(nativeProps))
#
#   for (i in 1:length(nativeProps)) {
#
#     nprop <- nativeProps[i]
#     propertyType <- getPropertyType(observedProperty)
#
#     if(propertyType == 'LaboratoryMeasurement'){
#
#         sql <- paste0('SELECT SITE.SURVEY_ID, SITE.SITE_NO, SITE.DATE_DESCRIBED, CHEMICAL.SOIL_ID, SITE.GDA94_LONGITUDE, SITE.GDA94_LATITUDE, CHEMICAL.U_DEPTH, CHEMICAL.L_DEPTH, CHEMICAL.', nprop,
#                       ' FROM CHEMICAL_METHODS LEFT JOIN (SURVEY LEFT JOIN ((SITE LEFT JOIN SOIL ON SITE.ID = SOIL.SITE_ID) LEFT JOIN CHEMICAL ON SOIL.ID = CHEMICAL.SOIL_ID) ON SURVEY.ID = SITE.SURVEY_ID) ON CHEMICAL_METHODS.SURVEY_ID = SURVEY.ID
#                       WHERE (((CHEMICAL.L_DEPTH) Is Not Null) AND ((CHEMICAL.', nprop, ') Is Not Null) AND ((CHEMICAL_METHODS.', nprop, ')="', observedProperty, '"))
#                       ORDER By SITE.SURVEY_ID, SITE.SITE_NO, CHEMICAL.SOIL_ID, CHEMICAL.U_DEPTH;')
#
#         conNT <- DBI::dbConnect(RSQLite::SQLite(), NT_DB_Path)
#
#         res <- dbSendQuery(conNT, sql)
#         fdf <- dbFetch(res)
#        # head(fdf, 40)
#
#         if(nrow(fdf) > 0){
#
#         bits <- str_split(fdf$DATE_DESCRIBED, '-')
#         day <- str_pad( sapply(bits, function (x) x[1]), 2, "left", pad = "0")
#
#         mnth <-sapply(bits, function (x) x[2])
#         fulMonth <- str_pad( mo2Num(mnth), 2, "left", pad = "0")
#         yr <-  sapply(bits, function (x) x[3])
#         fullYr <- unlist(lapply(yr, shortToFullYear))
#
#
#         units <- getUnits(propertyType = propertyType, prop = observedProperty)
#
#         oOutDF <- generateResponseDF(OrgName, 'NT_SALI', paste0(fdf$SURVEY_ID, '_', fdf$SITE_NO, '_', fdf$SOIL_ID ),
#                                      fdf$SOIL_ID ,paste0(day, '-', fulMonth, '-', fullYr,'T00:00:00' ) , fdf$GDA94_LONGITUDE, fdf$GDA94_LATITUDE ,
#                                      fdf$U_DEPTH , fdf$L_DEPTH , propertyType, observedProperty, fdf[nprop] , '', 'Brilliant')
#
#
#         lodfs[[i]] <- oOutDF
#
#         }else{
#           return(blankResponseDF())
#         }
#     }else{
#
#       tabName <- as.character(doQueryFromFed(paste0("select PropertyGroup from Properties WHERE Property = '", prop, "'")))
#       # tabName <- Properties[str_to_upper(Properties$Property) == str_to_upper(prop), ]$PropertyGroup
#       tabLev <- as.numeric(doQueryFromFed(paste0("select Level from TableLevels WHERE TableName = '", tabName, "'")))
#
#
#     }
#   }
#
#
#   outDF = as.data.frame(data.table::rbindlist(lodfs))
#   return(outDF)
#
# }
