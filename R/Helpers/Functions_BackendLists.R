

getData_NSSC_Wrapper <- function(provider=NULL, observedProperty=NULL, observedPropertyGroup=NULL){

  url <- paste0('http://esoil.io/TERNLandscapes/NSSCapi/SoilDataAPI/SoilData?provider=', provider, '&observedProperty=', observedProperty, '&observedPropertyGroup=', observedPropertyGroup )

  print(provider)
  fdf <- fromJSON(paste0(url))
  if(is.data.frame(fdf)){
    return <- fdf
  }else{
    return(blankResponseDF())
  }
}



getDataFunctions <- c(LawsonGrains=getData_LawsonGrains,
                      QLDGovernment=getData_QLDGovernment,
                      TERNLandscapes=getData_TERNLandscapes,
                      TERNSurveillance=getData_TERNSurveillance,
                      WAGovernment=getData_NSSC_Wrapper,
                      NSWGovernment=getData_NSSC_Wrapper,
                      VicGovernment=getData_NSSC_Wrapper,
                      SAGovernment=getData_NSSC_Wrapper,
                      TasGovernment=getData_NSSC_Wrapper,
                      NTGovernment=getData_NTGovt,
                      CSIRO=getData_ASRIS
                      )






