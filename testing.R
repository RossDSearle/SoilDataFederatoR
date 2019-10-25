library(rgdal)
library(sf)
library(aqp)


machineName <- as.character(Sys.info()['nodename'])
#if(!asPkg){
  if(machineName=='soils-discovery'){
    source(paste0('/srv/plumber/TERNLandscapes/SoilDataFederatoR/R/Backends.R'))
  }else{
    source(paste0('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/R/Backends.R'))
  }
#}


usr <- 'ross.searle@csiro.au'; key <- 'a'
usr='Public'; pwd='Public'

##########  Key authentication

usr <- 'ross.searle@csiro.au'
key <- 'a'
usr <- 'Admin'
key <- '2jpft6WvquTLjwO9XqFD'
AuthenticateAPIKey(usr, key)
getProviders(usr = usr, key = key)




observedPropertyGroup = 'NITROGEN'
observedPropertyGroup = NULL
observedProperty = '3A1'
observedProperty = 'h_texture'
observedProperty = 'SEG_FORM'
observedProperty = 'RO_ABUN'
observedProperty = 'S_DESC_BY'

getPropertyType(observedProperty)

getProviders()
getProviders(usr = usr, key = key)

getProviders( usr = 'LawsonGrains', key = 'b')
getProviders( usr = 'Demo', key = 'Demo')

df <- getSoilData( observedProperty='3A1', usr = 'Admin', pwd = 'c')
head(df)
tail(df)

op = 'h_texture'
op = 'SEG_FORM'
op = 'RO_ABUN'
op = 'S_DESC_BY'



usr='Demo'; key='Demo'
usr <- 'ross.searle@csiro.au'
key <- 'a'




getLocations(usr=usr, key=key)


getSoilData(DataSets = 'TERNSurveillance',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getLocations(DataSets= 'TERNSurveillance', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets = 'LawsonGrains_AgCatalyst', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getLocations(DataSets= 'LawsonGrains_AgCatalyst', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets = 'NatGeoChemicalSurvey',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getLocations(DataSets= 'NatGeoChemicalSurvey', usr='ross.searle@csiro.au', key='a')


getSoilData(DataSets = 'SALI',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets = 'NatSoil',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getLocations(DataSets= 'NatSoil', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets = 'NTGovernment',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getLocations(DataSets= 'NTGovernment', usr='ross.searle@csiro.au', key='a')


plotObservationLocationsImage()
  NatGeoChemicalSurvey



#########  spatial extent clipping   ###########################
provider = 'LawsonGrains'
areasOverlap(provider)
#
bbox <- extent(142.6, 143.1, -35.9, -35.48)
wext <- '142.6;143.1;-35.9;-35.48'
bbox <- extent(110.6, 153.1, -43, -9)
wext <- '110;153;-43;-9'

bits <- str_split(wext, ';')
l <- as.numeric(bits[[1]][1])
r <- as.numeric(bits[[1]][2])
t <- as.numeric(bits[[1]][4])
b <- as.numeric(bits[[1]][3])
bboxExt <- extent(l, r, b, t)



df <- getSoilData('LawsonGrains', observedProperty='4A1', bbox=bboxExt, usr='ross.searle@csiro.au', key='a')


















