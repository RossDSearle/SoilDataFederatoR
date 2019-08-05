
getData_NSWGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  provider <- 'NSWGovernment'
  url <- paste0('http://esoil.io/TERNLandscapes/NSSCapi/SoilDataAPI/SoilData?provider=', provider, '&observedProperty=', observedProperty, '&observedPropertyGroup=', observedPropertyGroup )
  fdf <- fromJSON(paste0(url))
  return <- fdf
  }

getData_WAGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  provider <- 'WAGovernment'
  url <- paste0('http://esoil.io/TERNLandscapes/NSSCapi/SoilDataAPI/SoilData?provider=', provider, '&observedProperty=', observedProperty, '&observedPropertyGroup=', observedPropertyGroup )
  fdf <- fromJSON(paste0(url))
  return <- fdf
}

getData_VicGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  provider <- 'VicGovernment'
  url <- paste0('http://esoil.io/TERNLandscapes/NSSCapi/SoilDataAPI/SoilData?provider=', provider, '&observedProperty=', observedProperty, '&observedPropertyGroup=', observedPropertyGroup )
  fdf <- fromJSON(paste0(url))
  return <- fdf
}

getData_SAGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  provider <- 'SAGovernment'
  url <- paste0('http://esoil.io/TERNLandscapes/NSSCapi/SoilDataAPI/SoilData?provider=', provider, '&observedProperty=', observedProperty, '&observedPropertyGroup=', observedPropertyGroup )
  fdf <- fromJSON(paste0(url))
  return <- fdf
}

getData_TasGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  provider <- 'TasGovernment'
  url <- paste0('http://esoil.io/TERNLandscapes/NSSCapi/SoilDataAPI/SoilData?provider=', provider, '&observedProperty=', observedProperty, '&observedPropertyGroup=', observedPropertyGroup )
  fdf <- fromJSON(paste0(url))
  return <- fdf
}

getData_NTGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  provider <- 'NTGovernment'
  url <- paste0('http://esoil.io/TERNLandscapes/NSSCapi/SoilDataAPI/SoilData?provider=', provider, '&observedProperty=', observedProperty, '&observedPropertyGroup=', observedPropertyGroup )
  fdf <- fromJSON(paste0(url))
  return <- fdf
}



getDataFunctions <- c(LawsonGrains=getData_LawsonGrains,
                      QLDGovernment=getData_QLDGovernment,
                      ASRIS=getData_ASRIS,
                      TERNLandscapes=getData_TERNLandscapes,
                      TERNSurveillance=getData_TERNSurveillance,
                      WAGovernment=getData_WAGovernment,
                      NSWGovernment=getData_NSWGovernment,
                      VicGovernment=getData_VicGovernment,
                      SAGovernment=getData_SAGovernment,
                      TasGovernment=getData_TasGovernment,
                      NTGovernment=getData_NTGovernment
                      #CSIRO=getData_ASRIS
                      )






