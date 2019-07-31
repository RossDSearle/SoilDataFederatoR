library(RSQLite)
library(DBI)
library(stringr)
library(data.table)

asPkg = F
Devel = T

#system.file("extdata", "NSSC_2.0.0.sqlite", package = "SoilDataFederatoR")

machineName <- as.character(Sys.info()['nodename'])
if(!asPkg){
  if(machineName=='soils-discovery'){
    #projectRoot <-'/srv/plumber/TERNLandscapes/SoilDataFederatoR'
    setwd('/srv/plumber/TERNLandscapes/SoilDataFederatoR')
    dbPathSoilsFed <- '/srv/plumber/TERNLandscapes/SoilDataFederatoR/DB/soilsFederator.sqlite'
  }else{
    setwd('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR')
    # path below is - C:/R/R-3.6.0/library/SoilDataFederatoR/extdata/soilsFederator.sqlit
    dbPathSoilsFed <- system.file("extdata", "soilsFederator.sqlite", package = "SoilDataFederatoR")
  }
}


source(paste0('R/Helpers/dbHelpers.R'))

source(paste0('R/Backends/Backend_TERNSurveillance.R'))
source(paste0('R/Backends/Backend_SALI.R'))
source(paste0('R/Backends/Backend_LawsonGrains.R'))
source(paste0('R/Backends/Backend_ASRIS.R'))
source(paste0('R/Backends/Backend_TERNLandscapesDB.R'))

source(paste0('R/Helpers/Functions_BackendLists.R'))


PropertyTypes <- data.frame(LaboratoryMeasurement='LaboratoryMeasurement', FieldMeasurement='FieldMeasurement', stringsAsFactors = F)


#ASRIS_df <- read.csv( 'c:/temp/Asris.csv')  ## This is a temporary hack until the endpoint is finished

#' Returns Observed Soil Properties
#'
#' This function will query all the available data providers to return all of the available soil observed property data
#' @param providers List of the Providers to query
#' @param observedProperty the Observed Soil Property code to query. It can be a ':' delimited list eg 4A1;3B2
#' @param providers observedPropertyGroup The Observed Soil property Group to Query on
#' @return Dataframe of Observed Property values



getSoilData <- function(providers=NULL, observedProperty=NULL, observedPropertyGroup=NULL, usr='Public', pwd='Public'){

  #orgs <- getProviders(activeOnly=T,usr=usr, pwd=pwd)
  orgs <- getProviders(usr=usr, pwd=pwd)

  if(!is.null(providers)){
    bits <- str_split(providers, ';')
    availProviders <- bits[[1]]
  }else{
    availProviders <- orgs$OrgName
  }

  cat(paste('Available Providers\n'))
  cat(paste('====================\n '))
  cat(paste0(availProviders, '\n'))
  cat(paste0('\n'))

  outdfs <- list(length(availProviders))

   for(i in 1:length(availProviders)) {
    prov <- availProviders[[i]]
    cat(paste0('Extracting data from ', prov, '\n'))
   # possibleError <- tryCatch(
    outdfs[[i]] <- getDataFunctions[[prov]](observedProperty, observedPropertyGroup)
    head(outdfs[[i]])
    #  error=function(e) e
    #)
    #if(inherits(possibleError, "error")) next
  }

 outDF = as.data.frame(data.table::rbindlist(outdfs))
 outDF$ExtractTime<- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")



 return(outDF)

}




#' CGet Providers
#' Blah Blah Blah
#' @param Bob The temperature in Fahrenheit.
#' @return The temperature in Kelvin.

#' getProviders1

getProviders1 <- function(activeOnly=T,usr=NULL, pwd=NULL){

  return(NULL)

}



