library(RSQLite)
library(DBI)
library(stringr)
library(data.table)


dbPathSoilsFed <- paste0("R/DB/soilsFederator.sqlite")


source(paste0('R/Helpers/dbHelpers.R'))

source(paste0('R/Backends/Backend_TERNSurveillance.R'))
source(paste0('R/Backends/Backend_SALI.R'))
source(paste0('R/Backends/Backend_LawsonGrains.R'))
source(paste0('R/Backends/Backend_ASRIS.R'))
source(paste0('R/Backends/Backend_TERNLandscapes.R'))

source(paste0('R/Helpers/Functions_BackendLists.R'))
#source(paste0( 'R/WebAPI/apiEndPoints.R'))


# getData_WAGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   getData_TERNLandscapes(provider='WAGovernment', observedProperty, observedPropertyGroup)
# }
# getData_SAGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   getData_TERNLandscapes(provider='SAGovernment', observedProperty, observedPropertyGroup)
# }
# getData_VicGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   getData_TERNLandscapes(provider='VicGovernment', observedProperty, observedPropertyGroup)
# }
# getData_TasGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   getData_TERNLandscapes(provider='TasGovernment', observedProperty, observedPropertyGroup)
# }
# getData_NSWGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   getData_TERNLandscapes(provider='NSWGovernment', observedProperty, observedPropertyGroup)
# }
# getData_QLDGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   getData_TERNLandscapes(provider='QLDGovernment', observedProperty, observedPropertyGroup)
# }
# getData_NTGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   getData_TERNLandscapes(provider='NTGovernment', observedProperty, observedPropertyGroup)
# }
# getData_CSIRO <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   getData_TERNLandscapes(provider='CSIRO', observedProperty, observedPropertyGroup)
# }
#
# getDataFunctions <- c(LawsonGrains=getData_LawsonGrains, QLDGovernment=getData_QLDGovernment, ASRIS=getData_ASRIS, WAGovernment=getData_WAGovernment,
#                       NSWGovernment=getData_NSWGovernment, VicGovernment=getData_VicGovernment, QLDGovernment=getData_QLDGovernment, SAGovernment=getData_SAGovernment,
#                       TasGovernment=getData_TasGovernment, NTGovernment=getData_NTGovernment, CSIRO=getData_CSIRO)


PropertyTypes <<- data.frame(LaboratoryMeasurement='LaboratoryMeasurement', FieldMeasurement='FieldMeasurement', stringsAsFactors = F)
#ASRIS_df <- read.csv( 'c:/temp/Asris.csv')  ## This is a temporary hack until the endpoint is finished

#' Ctetsysjh
#'
#' This function converts input temperatures in Fahrenheit to Kelvin.
#' @param temp_F The temperature in Fahrenheit.
#' @return The temperature in Kelvin.

#' getSoilData

getSoilData <- function(providers=NULL, observedProperty=NULL, observedPropertyGroup=NULL, usr='Admin', pwd='c'){

  orgs <- getProviders(activeOnly=T,usr=usr, pwd=pwd)

  if(!is.null(providers)){
    bits <- str_split(providers, ';')
    availProviders <- bits[[1]]
  }else{
    availProviders <- orgs$OrgName
  }

  cat(paste('Available Providers\n'))
  cat(paste('====================\n'))
  cat(paste0(availProviders, '\n'))

  outdfs <- list(length(availProviders))
  for(i in 1:length(availProviders)) {
    prov <- availProviders[[i]]
   # possibleError <- tryCatch(
    #outdfs[[i]] <- getDataFunctions[[prov]](observedProperty, observedPropertyGroup)
    #  error=function(e) e
    #)

    #if(inherits(possibleError, "error")) next
  }

 outDF = as.data.frame(data.table::rbindlist(outdfs))

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



