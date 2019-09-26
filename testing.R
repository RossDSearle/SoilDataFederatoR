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


df <- getSoilData(providers='CSIRO', observedProperty='3A1', usr='ross.searle@csiro.au', key='a')

df <- getSoilData(providers='CSIRO', observedProperty='mr_type', usr='ross.searle@csiro.au', key='a')

df <- getSoilData(providers='NTGovernment', observedProperty='4A1')
df <- getSoilData(providers='NTGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
nrow(df)




df <- getSoilData('LawsonGrains', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')

df <- getSoilData('TERNSurveillance', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
df <- getSoilData(providers='TERNSurveillance', observedProperty='4A1', usr='Demo', key='Demo')


df <- getSoilData('TERNLandscapes', observedProperty='4A1')
df <- getSoilData( observedProperty='4A1')
df <- getSoilData( observedProperty='4A1', usr='ross.searle@csiro.au', key='a')

df <- getSoilData('TERNLandscapes', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')

df <- getSoilData(providers='QLDGovernment', observedProperty='3A1')
df <- getSoilData(providers='CSIRO', observedProperty='3A1')
df <- getSoilData(providers='LawsonGrains', observedProperty='3A1')
df <- getSoilData(providers='TERNSurveillance', observedProperty='4A1')

df <- getSoilData(providers='NTGovernment', observedProperty='4A1') #OK


df <- getSoilData(providers=NULL, observedProperty='3A1')

df <- getSoilData('TERNLandscapes', observedProperty='3A1') #OK
df <- getSoilData(providers='QLDGovernment', observedProperty='3A1') #OK
df <- getSoilData(providers='CSIRO', observedProperty='3A1') #OK
df <- getSoilData(providers='LawsonGrains', observedProperty='3A1') #OK
df <- getSoilData(providers='TERNSurveillance', observedProperty='4A1') #OK
df <- getSoilData(providers='NSWGovernment', observedProperty='4A1') #OK

df <- getSoilData(providers='NTGovernment', observedProperty='3A1') #OK


df <- getData_NSSC(provider='NSWGovernment', observedProperty='3A1')
df <- getData_NSSC(provider='NSWGovernment', observedProperty='RO_ABUN')
df <- getData_NSSC(provider='NSWGovernment', observedProperty='h_texture;RO_ABUN')
head(df)
tail(df)

df <- getSoilData( observedProperty='3A1', usr = 'Admin', pwd = 'c')
head(df)
tail(df)

op = 'h_texture'
op = 'SEG_FORM'
op = 'RO_ABUN'
op = 'S_DESC_BY'

df <- getSoilData(providers='WAGovernment', observedProperty=op, observedPropertyGroup = NULL)



df <- getSoilData( observedProperty=op, observedPropertyGroup = NULL)
head(df)


getData_TERNLandscapes(provider='WAGovernment', observedProperty=op, observedPropertyGroup = NULL)
getData_TERNLandscapes(provider='QLDGovernment', observedProperty=op, observedPropertyGroup = NULL)






df <- getSoilData('QLDGovernment', observedPropertyGroup=og)

df <- getSoilData(providers = provs, observedProperty='3A1')

provs='LawsonGrains'

providers='LawsonGrains'


provs='LawsonGrains;ASRIS'
df <- getSoilData(providers = provs, observedProperty='3A1')
tail(df)
head(df)


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

