library(RSQLite)
library(DBI)
library(stringr)
library(data.table)
require(dplyr)
require(dtplyr)

asPkg = F
Devel = F

machineName <- as.character(Sys.info()['nodename'])
if(!asPkg){
  if(machineName=='soils-discovery'){
    setwd('/srv/plumber/TERNLandscapes/SoilDataFederatoR')
  }else{
    # path below is - C:/R/R-3.6.0/library/SoilDataFederatoR/extdata/soilsFederator.sqlite
    setwd('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR')
  }
}

administrator <- 'ross.searle@csiro.au'

source(paste0('R/Helpers/dbHelpers.R'))

source(paste0('R/Backends/Backend_TERNSurveillance.R'))
source(paste0('R/Backends/Backend_SALI.R'))
source(paste0('R/Backends/Backend_LawsonGrains.R'))
source(paste0('R/Backends/Backend_ASRIS.R'))
source(paste0('R/Backends/Backend_TERNLandscapesDB.R'))
source(paste0('R/Backends/Backend_NTGovt.R'))

source(paste0('R/Helpers/Functions_BackendLists.R'))


PropertyTypes <- data.frame(LaboratoryMeasurement='LaboratoryMeasurement', FieldMeasurement='FieldMeasurement', stringsAsFactors = F)


#' Returns Observed Soil Properties
#'
#' This function will query all the available data providers to return all of the available soil observed property data
#' @param providers List of the Providers to query
#' @param observedProperty the Observed Soil Property code to query. It can be a ':' delimited list eg 4A1;3B2
#' @param providers observedPropertyGroup The Observed Soil property Group to Query on
#' @return Dataframe of Observed Property values



getSoilData <- function(providers=NULL, observedProperty=NULL, observedPropertyGroup=NULL, usr='Demo', key='Demo'){

 auth  <- AuthenticateAPIKey(usr, key)

 if(auth == 'OK'){
      orgs <- getProviders(usr=usr, key=key)

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
        odf <- sendRequest(provider=prov, observedProperty, observedPropertyGroup)
        print(head(odf))
        if(is.data.frame(odf))
        {
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
       return(blankResponseDF())
     }

     outDF2 <- convertToRequiredDataTypes(outDF)
     outDF2$ExtractTime<- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
     outDF2 <- outDF2[!is.na(outDF2$Value),]
     outDF2[!is.na(outDF2$UpperDepth),]


     DT <- as.data.table(outDF2)
     DT %>% group_by(Provider, Dataset,Observation_ID) %>% arrange(UpperDepth, LowerDepth)

     return(DT)


 }else
   stop(auth)
}



sendRequest<- function(provider, observedProperty, observedPropertyGroup){
  tryCatch(
    expr = {
      odf <- getDataFunctions[[provider]](provider=provider, observedProperty, observedPropertyGroup)
    },
    error = function(e){
      blankResponseDF()
    },
    warning = function(w){
      message('Caught an warning!')
      print(w)
    },
    finally = {
      message('All done, quitting.')
    }
  )
}



