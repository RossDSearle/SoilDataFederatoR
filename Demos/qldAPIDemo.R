

library(jsonlite)

fromJSON(paste0('http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=4A1&DataSet=QLDGovernment&usr=ross.searle@csiro.au&key=a'))
fromJSON(paste0('http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=H_TEXTURE&DataSet=QLDGovernment&usr=ross.searle@csiro.au&key=a'))
fromJSON(paste0('http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/ObservationLocations?DataSet=QLDGovernment'))
