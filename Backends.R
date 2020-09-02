library(RSQLite)
library(DBI)
library(stringr)
library(data.table)
require(dplyr)

require(sf)
library(RColorBrewer)
library(ggplot2)


Devel = F

machineName <- as.character(Sys.info()['nodename'])

  if(machineName=='soils-discovery'){
    setwd('/srv/plumber/TERNLandscapes/SoilDataFederatoR')
  }else{
    # path below is - C:/R/R-3.6.0/library/SoilDataFederatoR/extdata/soilsFederator.sqlite
    setwd('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR')
  }


administrator <- 'ross.searle@csiro.au'

source(paste0('apiConfig.R'))
source(paste0('Helpers/dbHelpers.R'))
source(paste0('Helpers/webHelpers.R'))

source(paste0('Backends/Backend_TERNSurveillance.R'))
source(paste0('Backends/Backend_SALI.R'))
source(paste0('Backends/Backend_LawsonGrains.R'))
source(paste0('Backends/Backend_ASRIS.R'))
source(paste0('Backends/Backend_TERNLandscapesDB.R'))
source(paste0('Backends/Backend_NSSC.R'))
source(paste0('Backends/Backend_ASRISHostedAPI.R'))

source(paste0('Dev/TestingLocationSpecificOnAsris.R'))



PropertyTypes <- data.frame(LaboratoryMeasurement='LaboratoryMeasurement', FieldMeasurement='FieldMeasurement', stringsAsFactors = F)


doChecks <- function(DataSets, observedProperty, observedPropertyGroup, bBox){

  if(is.null(observedProperty) & is.null(observedPropertyGroup))
    return('You need to specify a value for either the "observedProperty" parameter or the "observedPropertyGroup" parameter')

  if(!is.null(DataSets)){
    bits <- str_split(DataSets, ';')
    availDataSets <- bits[[1]]

      for(i in 1:length(availDataSets)) {
        dataset <- availDataSets[[i]]
       sql <- paste0("Select * from DataSets where DataSet = '", dataset, "'")
        prov = doQueryFromFed(sql)
         if(nrow(prov)==0){
          return(paste0('DataSet "', dataset, '" is not known to the system' ))
         }
      }

  }


   return('OK')
}


#' Returns Observed Soil Properties
#'
#' This function will query all the available data providers to return all of the available soil observed property data
#' @param providers List of the Providers to query
#' @param observedProperty the Observed Soil Property code to query. It can be a ':' delimited list eg 4A1;3B2
#' @param providers observedPropertyGroup The Observed Soil property Group to Query on
#' @return Dataframe of Observed Property values



getSoilData <- function(DataSets=NULL, observedProperty=NULL, observedPropertyGroup=NULL, bBox=NULL, usr='Demo', key='Demo', verbose=F){

  error <- doChecks(DataSets, observedProperty, observedPropertyGroup, bBox)

  if(error!='OK'){
    stop(error)
  }

 auth  <- AuthenticateAPIKey(usr, key)

 if(auth == 'OK'){
      authDataSets <- getDataSets(usr=usr, key=key)

      if(!is.null(DataSets)){
        bits <- str_split(DataSets, ';')
        availDataSets <- bits[[1]]
      }else{
        availDataSets <- authDataSets$DataSet
      }

      if(verbose){
        cat(paste('Available DataSets\n'))
        cat(paste('====================\n '))
        cat(paste0(availDataSets, '\n'))
        cat(paste0('\n'))
      }

      outdfs <- vector("list", length(availDataSets))

       for(i in 1:length(availDataSets)) {
        dataset <- availDataSets[[i]]

        dStore <- getDataStore(dataset)
        if(verbose){
           cat(paste0('Extracting data from ', dataset, '\n'))
        }

        if(is.null(bBox)){

          odf <- sendRequest(DataSet=dataset, DataStore=dStore, observedProperty, observedPropertyGroup)
        }else{
          if(areasOverlap(DataSet=dataset, bBox=bBox)){

            odfAll <- sendRequest(DataSet=dataset, DataStore=dStore, observedProperty, observedPropertyGroup)
            odf <- getWindow(odfAll,bBox )
          }else{
            cat(paste0(dataset, ' -  Requested area and dataset extent do not overlap - skipping\n'))
            odf <- blankResponseDF()
          }
        }
       # possibleError <- tryCatch(
        if(is.data.frame(odf))
        {
          odf <- getDataQualityInfo(dataset, odf)
          outdfs[[i]] <- odf
        }else{
          outdfs[[i]] <- blankResponseDF()
        }
        #  error=function(e) e
        #)
        #if(inherits(possibleError, "error")) next
      }

     outDF = as.data.frame(data.table::rbindlist(outdfs, fill=T))

     if(usr=='Demo'){

       outDF <- outDF[1:5,]

     }

     if(nrow(outDF)==0){
       odf <- blankResponseDF()
       ndf <-  data.frame(odf, ExtractTime=character())
       return(ndf)
     }

     outDF2 <- convertToRequiredDataTypes(outDF)
     outDF2$ExtractTime<- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
     outDF2 <- outDF2[!is.na(outDF2$Value),]
     #outDF2[!is.na(outDF2$UpperDepth),]


     DT <- as.data.table(outDF2)
     DT %>% group_by(Provider, Dataset, Observation_ID) %>% arrange(UpperDepth, LowerDepth)

     return(DT)


 }else
   stop(auth)
}


#' Returns all locations of soil observations
#'
#' This function will query all the available data providers to return all of the available soil observation locations
#' @param DataSets List of the Providers to query
#' @return Dataframe of observation locations



getSiteLocations <- function(DataSets=NULL, bBox=NULL, usr='Demo', key='Demo'){

  #print('#########')
  #  print(bBox)


  authDataSets <- getDataSets(usr=usr, key=key)

  if(!is.null(DataSets)){
    bits <- str_split(DataSets, ';')
    availDataSets <- bits[[1]]
  }else{
    availDataSets <- authDataSets$OrgName
  }


  outdfs <- vector("list", length(availDataSets))

    for(i in 1:length(availDataSets)) {


      dataset <- availDataSets[[i]]
      dStore <- getDataStore(dataset)

      if(!is.null(getLocationDataFunctions[[dStore]]) ){
      #cat(paste0('Extracting data from ', dataset, '\n'))


      if(is.null(bBox)){

        odf <- getLocationDataFunctions[[dStore]](DataSet=dataset)

      }else{
        if(areasOverlap(DataSet=dataset, bBox=bBox)){
          odfAll <- getLocationDataFunctions[[dStore]](DataSet=dataset)
          odf <- getWindow(odfAll,bBox )
        }else{
          cat(paste0('   Requested area and dataset extent do not overlap - skipping\n'))
          odf <- blankResponseDF()
        }
      }
      outdfs[[i]] <- odf
      }
    }
    #outDF = as.data.frame(data.table::rbindlist(outdfs, fill=T))
    outDF = as.data.frame(data.table::rbindlist(outdfs, fill=T))
    outDF$ExtractTime<- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
   # DT <- as.data.table(outDF2)
   # DT %>% group_by(Provider, Dataset,Observation_ID) %>% arrange(UpperDepth, LowerDepth)

    return(outDF)
}



sendRequest<- function(DataSet, DataStore, observedProperty, observedPropertyGroup){
  tryCatch(
    expr = {
      odf <- getDataSetFunctions[[DataStore]](DataSet=DataSet, observedProperty, observedPropertyGroup)
    },
    error = function(e){
      print(e)
      blankResponseDF()
    },
    warning = function(w){
      #message('Caught a warning!')
      print(w)
    },
    finally = {
     # message('Success')
    }
  )
}



# getData_NSSC_Wrapper <- function(DataSet=NULL,  observedProperty=NULL, observedPropertyGroup=NULL){
#
#   url <- paste0('http://esoil.io/TERNLandscapes/NSSCapi/SoilDataAPI/SoilData?provider=', provider, '&observedProperty=', observedProperty, '&observedPropertyGroup=', observedPropertyGroup )
#
#   print(provider)
#   fdf <- fromJSON(paste0(url))
#   if(is.data.frame(fdf)){
#     return <- fdf
#   }else{
#     return(blankResponseDF())
#   }
# }




####### Functions Lists   #######################
getDataSetFunctions <-  c(
                          LawsonGrains=getData_LawsonGrains,
                          SALI=getData_QLDGovernment,
                          TERNSurveillance=getData_TERNSurveillance,
                          NSSC=getData_NSSC,
                          ASRIS=getData_ASRIS,
                          TERNLandscapesDB=getData_TERNLandscapesDB
                          #ASRISHostedNSWGovt=getData_NSWGovt

)

getLocationDataFunctions <- c(
                      LawsonGrains=getLocationData_LawsonGrains,
                      SALI=getLocationData_QLDGovernment,
                      TERNSurveillance=getLocationData_TERNSurveillance,
                      NSSC=getLocationData_NSSC,
                      ASRIS=getLocationData_ASRIS,
                      TERNLandscapesDB=getLocationData_TERNLandscapesDB
                      #ASRISHostedNSWGovt=getLocationData_NSWGovt
)


###########  Backend Helpers ############

getDataQualityInfo <- function(dataSetName, dataset){

  if (nrow(dataset) == 0){
  }else{

   quals <-  doQueryFromFed(paste0('select * from DataSets where DataSet = "', dataSetName, '"'))
   dataset$QualCollection <- quals$QualCollection[1]
   dataset$QualSpatialAgg <- quals$QualSpatialAgg[1]
   dataset$QualManagement <- quals$QualManagement[1]

  }
  return (dataset)
}


convertToRequiredDataTypes <- function(df){


  df$Provider <- as.character(df$Provider)
  df$Dataset <- as.character(df$Dataset)
  df$Observation_ID <- as.character(df$Observation_ID)
  df$SampleID <- as.character(df$SampleID)
  df$SampleDate <- as.character(df$SampleDate)
  df$Longitude <- as.numeric(as.character(df$Longitude))
  df$Latitude <- as.numeric(as.character(df$Latitude))
  df$PropertyType <- as.character(df$PropertyType)
  df$ObservedProperty <- as.character(df$ObservedProperty)
  df$Value <- as.character(df$Value)

  return(df)
}

blankResponseDF <- function(){

  outDF <- data.frame(DataStore=character(), Dataset=character(), Provider=character(), Observation_ID=character(), SampleID=character(), SampleDate=character() ,
                      Longitude=numeric() , Latitude= numeric(),
                      UpperDepth=numeric() , LowerDepth=numeric() , PropertyType=character(), ObservedProperty=character(), Value=numeric(),
                      Units= character(),   QualCollection=integer(), QualSpatialAgg=integer(), QualManagement=integer(), stringsAsFactors = F)
}

generateResponseDF <- function( dataset, observation_ID, sampleID, date, longitude, latitude, upperDepth, lowerDepth, dataType, observedProp, value, units ){

  provider=getOrgName(dataset)
  datastore=getDataStore(dataset)
  outDF <- data.frame(DataStore=datastore, Dataset=dataset,Provider=provider, Observation_ID=observation_ID, SampleID=sampleID , SampleDate=date ,
                      Longitude=longitude, Latitude=latitude ,
                      UpperDepth=upperDepth, LowerDepth=lowerDepth, PropertyType=dataType, ObservedProperty=observedProp,
                      Value=value , Units=units, QualCollection=NA, QualSpatialAgg=NA, QualManagement=NA, stringsAsFactors = F)
  oOutDF <- outDF[order(outDF$Observation_ID, outDF$Dataset, outDF$UpperDepth, outDF$SampleID),]
  print(head(oOutDF))
  return(oOutDF)
}

generateResponseAllLocs<- function(dataset, observation_ID, longitude, latitude, date ){
  provider=getOrgName(dataset)
  datastore=getDataStore(dataset)
  outDF <- data.frame(DataStore=datastore, Dataset=dataset, Provider=provider, Observation_ID=observation_ID, Longitude=longitude, Latitude=latitude ,SampleDate=date, stringsAsFactors = F)
  return(outDF)
}




plotObservationLocationsImage <- function(DF){

  pPath <- paste0('R/AncillaryData/Aust.shp')
  austBdy <- read_sf(pPath)
  meuse_sf = st_as_sf(DF, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

  bkd <-  as.numeric(unique(as.factor(meuse_sf$Dataset )))
  palt <-brewer.pal(length(bkd),"Set1")

  par(mar=c(0,0,0,0))
  plot(st_geometry(austBdy), border='black', reset=FALSE, col='beige')
  # plot(x, y, ..., pch = 1, cex = 1, col = 1, bg = 0, lwd = 1, lty = 1, type = "p", add = FALSE)
  plot(meuse_sf[1], pch=20, add=T, pal=palt  )
  legend("bottomleft", legend=levels(as.factor(meuse_sf$Dataset)),fill=palt )

 # meuse_sf %>% ggplot() + geom_sf(aes(color=Dataset))

#  ggplot(DF,aes(x = Longitude, y = Latitude),  color = richness_raw) + geom_point()

}

getWindow <- function(outDF, bBox){

  # bits <- str_split(bBox, ';')
  # l <- as.numeric(bits[[1]][1])
  # r <- as.numeric(bits[[1]][2])
  # t <- as.numeric(bits[[1]][4])
  # b <- as.numeric(bits[[1]][3])
  # bboxExt <- extent(l, r, b, t)

  #outdf <- outDF[(outDF$Longitude >= bboxExt@xmin & outDF$Longitude <= bboxExt@xmax & outDF$Latitude >= bboxExt@ymin & outDF$Latitude <= bboxExt@ymax), ]

  idx <- which(outDF$Longitude >= bboxExt@xmin & outDF$Longitude <= bboxExt@xmax & outDF$Latitude >= bboxExt@ymin & outDF$Latitude <= bboxExt@ymax)
  outdf <- outDF[idx, ]

  #print(head(outdf))

   return(outdf)
}




