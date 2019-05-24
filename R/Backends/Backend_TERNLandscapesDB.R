library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)


getData_TernLandscapes <- function(observedProperty, observedPropertyGroup){

  ep <- getEndPointURL('TERNSoilDB')

  ps <- getPropertiesList(ObserverdProperties, observedPropertyGroup)

  lodfs <- list(length(ps))

  for (i in 1:length(ps)) {

      fdf <- fromJSON(paste0(ep, '/SoilData?observedProperty=', observedProperty))
      lodfs[[i]] <- fdf
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))
  return(outDF)
}

