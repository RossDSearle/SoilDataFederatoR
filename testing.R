
source(paste0('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/R','/Backends.R'))


usr <- 'ross.searle@csiro.au'; pwd <- 'a'
usr='Public'; pwd='Public'

op = 'h_texture'
op = '4A1'
og = 'PH'
observedPropertyGroup = 'NITROGEN'
observedPropertyGroup = NULL
observedProperty = '3A1'
observedProperty = 'h_texture'
observedProperty = 'SEG_FORM'
observedProperty = 'RO_ABUN'
observedProperty = 'S_DESC_BY'
op= 'RO_ABUN'

op <- 'h_texture'
op <- 'SEG_FORM'

getPropertyType(observedProperty)

df <- getSoilData(NULL, observedProperty=op)
df <- getSoilData(NULL, observedPropertyGroup=og)

nrow(df)
head(df)


getProviders(activeOnly = T, usr = 'Public', pwd = 'Public')
getProviders(activeOnly = F)
getProviders()

getProviders(activeOnly = T, usr = 'LawsonGrains', pwd = 'b')
getProviders(activeOnly = T, usr = 'Admin', pwd = 'c')

df <- getSoilData('TERNLandscapes', observedProperty='3A1')
df <- getSoilData(providers='QLDGovernment', observedProperty='3A1')


op = 'h_texture'
op = 'SEG_FORM'
op = 'RO_ABUN'
op = 'S_DESC_BY'

df <- getSoilData(providers='WAGovernment', observedProperty=op, observedPropertyGroup = NULL)
head(df)


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





