


getData_WAGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  getData_TERNLandscapes(provider='WAGovernment', observedProperty, observedPropertyGroup)
}
getData_SAGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  getData_TERNLandscapes(provider='SAGovernment', observedProperty, observedPropertyGroup)
}
getData_VicGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  getData_TERNLandscapes(provider='VicGovernment', observedProperty, observedPropertyGroup)
}
getData_TasGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  getData_TERNLandscapes(provider='TasGovernment', observedProperty, observedPropertyGroup)
}
getData_NSWGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  getData_TERNLandscapes(provider='NSWGovernment', observedProperty, observedPropertyGroup)
}
getData_QLDGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  getData_TERNLandscapes(provider='QLDGovernment', observedProperty, observedPropertyGroup)
}
getData_NTGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  getData_TERNLandscapes(provider='NTGovernment', observedProperty, observedPropertyGroup)
}
getData_CSIRO <- function(observedProperty=NULL, observedPropertyGroup=NULL){
  getData_TERNLandscapes(provider='CSIRO', observedProperty, observedPropertyGroup)
}


getDataFunctions <- c(LawsonGrains=getData_LawsonGrains, QLDGovernment=getData_QLDGovernment, ASRIS=getData_ASRIS, WAGovernment=getData_WAGovernment,
                      NSWGovernment=getData_NSWGovernment, VicGovernment=getData_VicGovernment, QLDGovernment=getData_QLDGovernment, SAGovernment=getData_SAGovernment,
                      TasGovernment=getData_TasGovernment, NTGovernment=getData_NTGovernment, CSIRO=getData_CSIRO)

