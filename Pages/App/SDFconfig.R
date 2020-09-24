

DEBUG <- F


serverLoc  <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR'
shinyLoc <- 'https://shiny.esoil.io/SoilDataFederator'

DefPropGrp <- 'Nitrogen'
DefProp <- '7A1'


if(DEBUG){
  DefUser <- 'ross.searle@csiro.au'
  DefKey <- 'a'
}else{
  DefUser <- 'Demo'
  DefKey <- 'Demo'
}

url <- paste0(serverLoc, '/SoilDataAPI/DataSets')
DatasetList <- fromJSON(url)
