
machineName <- as.character(Sys.info()['nodename'])
#if(!asPkg){
  if(machineName=='soils-discovery'){
    source(paste0('/srv/plumber/TERNLandscapes/SoilDataFederatoR/R/Backends.R'))
  }else{
    source(paste0('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/R/Backends.R'))
  }
#}


usr <- 'ross.searle@csiro.au'; pwd <- 'a'
usr='Public'; pwd='Public'

observedPropertyGroup = 'NITROGEN'
observedPropertyGroup = NULL
observedProperty = '3A1'
observedProperty = 'h_texture'
observedProperty = 'SEG_FORM'
observedProperty = 'RO_ABUN'
observedProperty = 'S_DESC_BY'

getPropertyType(observedProperty)

getProviders()
getProviders(usr = 'Admin', pwd = 'c')

getProviders(activeOnly = T, usr = 'LawsonGrains', pwd = 'b')
getProviders(activeOnly = T, usr = 'Admin', pwd = 'c')

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

provs='LawsonGrains;QLDGovernment'

providers='LawsonGrains'


provs='LawsonGrains;ASRIS'
df <- getSoilData(providers = provs, observedProperty='3A1')
tail(df)
head(df)





