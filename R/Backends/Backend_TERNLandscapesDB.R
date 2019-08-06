library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)

getData_TERNLandscapes <- function(provider=NULL, observedProperty=NULL, observedPropertyGroup=NULL){

  if(Devel){
    ep <- 'http://localhost:8055/TERNSoilDB'
  }else{
    ep <- getEndPointURL('http://esoil.io/TERNLandscapes/SoilDataFederatoRDatabase/TERNSoilDB')
  }

  OrgName <- provider
 # cat(paste0('Extracting data from ', OrgName, '\n'))
  ps <- getPropertiesList(observedProperty, observedPropertyGroup)

  lodfs <- list(length(ps))

  for (i in 1:length(ps)) {

      fdf <- fromJSON(paste0(ep, '/SoilData?observedProperty=', observedProperty))

      if(nrow(fdf) > 0){

        bits <- str_split(fdf$Date, '/')
        day <- str_pad( sapply(bits, function (x) x[1]), 2, "left", pad = "0")
        mnth <- str_pad( sapply(bits, function (x) x[2]), 2, "left", pad = "0")
        yr <- sapply(bits, function (x) x[3])

        oOutDF <- generateResponseDF(fdf$Provider, fdf$Dataset, paste0(fdf$Provider, '_', fdf$Dataset, '_', fdf$Observation_ID , '_', fdf$SampleID),
                                     fdf$SampleID ,paste0(day, '-', mnth, '-', yr,'T00:00:00' ) , fdf$Longitude, fdf$Latitude ,
                                     fdf$UpperDepth , fdf$LowerDepth , fdf$PropertyType, fdf$ObservedProperty, fdf$Value , fdf$Units, fdf$Quality)

        lodfs[[i]] <- oOutDF
      }else{
        return(blankResponseDF())
      }
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))
  return(outDF)
}

