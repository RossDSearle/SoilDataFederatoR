library(rgdal)
library(sf)
library(aqp)
library(jsonlite)
library(SoilDataFeder8R)

machineName <- as.character(Sys.info()['nodename'])
#if(!asPkg){
  if(machineName=='soils-discovery'){
    source(paste0('/srv/plumber/TERNLandscapes/SoilDataFederatoR/Backends.R'))
  }else{
    source(paste0('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/Backends.R'))
  }
#}


usr <- 'ross.searle@csiro.au'; key <- 'a';



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

usr <- 'ross.searle@csiro.au'
key <- 'a'

df <- getSoilData( observedProperty = observedProperty, usr = usr, key = key)
head(df)
tail(df)

dfSites <- getSiteLocations(DataSets=NULL, usr='ross.searle@csiro.au', key='a')
write.csv(dfSites, 'c:/temp/SoilDatafedSitesV2.csv')


plotObservationLocationsImage(DF=na.omit(dfSites))

op = 'h_texture'
op = 'SEG_FORM'
op = 'RO_ABUN'
op = 'S_DESC_BY'


datase

usr='Demo'; key='Demo'



datas

props <- c('3A1', 'h_texture')
props <- c('3A1', 'ph_value')




getLocations(usr=usr, key=key)


df <- getSoilData(DataSets='TERNSurveillance',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
DataSet='TERNSurveillance'
bboxExt=NULL
df <- getSoilData(DataSets='TERNSurveillance',observedProperty='7A1', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='TERNSurveillance',observedProperty='col_hue_val_chrom', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='TERNSurveillance', observedProperty='colour_when_dry', usr='ross.searle@csiro.au', key='a')

getSiteLocations(DataSets='TERNSurveillance', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='LawsonGrains_AgCatalyst', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='LawsonGrains_AgCatalyst', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='LawsonGrains_AgCatalyst', usr='ross.searle@csiro.au', key='a')


df <- getSoilData(DataSets='NatGeoChemicalSurvey',observedProperty='3A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NatGeoChemicalSurvey',observedProperty='ph_value', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='NatGeoChemicalSurvey', usr='ross.searle@csiro.au', key='a')

df <- getSoilData(DataSets='EastCentral_Australia',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='EastCentral_Australia',observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='EastCentral_Australia', usr='ross.searle@csiro.au', key='a')


getSoilData(DataSets='NatSoil',observedProperty='o_type', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NatSoil',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NatSoil',observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='NatSoil', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='NTGovernment',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NTGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NTGovernment', observedProperty='ph_value', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='NTGovernment', usr='ross.searle@csiro.au', key='a')


getSoilData(DataSets='SCARP',observedProperty='6B3', usr='ross.searle@csiro.au', key='a')
df <- getSoilData(DataSets='SCARP',observedProperty='o_date_desc', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SCARP',observedProperty='h_notes', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SCARP',observedProperty='o_asc_ord', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='SCARP', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='SAGovernment', observedProperty='3A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SAGovernment', observedProperty='O_DESC_BY', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SAGovernment', observedProperty='S_SLOPE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SAGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='SAGovernment', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='WAGovernment', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
df <- getSoilData(DataSets='WAGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='WAGovernment', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='QLDGovernment', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='QLDGovernment', observedProperty='6A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='QLDGovernment', observedProperty='DIST_TYPE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='QLDGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='QLDGovernment', observedProperty='col_hue_val_chrom', usr='ross.searle@csiro.au', key='a')


unique(dfc$projectID)
getSoilData(DataSets='Kimberley_Rainforest_Survey_1990', observedProperty='15C1_MG', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='South West Australian Transitional Transect (SWATT)', observedProperty='18F1_AL', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Fire_gimlet_woodlands_flora', observedProperty='7C1a', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Salinity_Action_Plan_Flora_Survey', observedProperty='15C1_CA', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Little Sandy Desert survey', observedProperty='P10_NR_Z', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Biological Survey of the Ravensthorpe Range (Phase 1)', observedProperty='18F1_NI', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Pilbara_Regional_Survey_Riparian_Flora', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Kimberley_Islands_Biodiversity_Survey', observedProperty='17A_NR', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Nullarbor_Regional_Survey', observedProperty='P10_GRAV', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Transects for Environmental Monitoring and Decision Making (TREND)', observedProperty='503.08a', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='Nullarbor_Regional_Survey', usr='ross.searle@csiro.au', key='a')


getSiteLocations(DataSets='QLDGovernment', usr='ross.searle@csiro.au', key='a')

df <- fromJSON('http://asris-daas02/NSW_Services/api/LabResults')
df[df$labm_code == 'N4B1',]
unique(df$labm_code)

df <- getSoilData(DataSets='NSWGovernment', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
df <- getSoilData(DataSets='NSWGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NSWGovernment', observedProperty='o_date_desc', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='NSWGovernment', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='VicGovernment', observedProperty='15E2_Al', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='VicGovernment', observedProperty='3A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='VicGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='VicGovernment', observedProperty='O_PPF', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='VicGovernment', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='TasGovernment', observedProperty='6A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='TasGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='TasGovernment', usr='ross.searle@csiro.au', key='a')

DataSet='TasGovernment'
DataSet='SAGovernment'
DataSet='NatSoil'

df <- getSoilData(DataSets=DataSet, observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
df <- getSoilData(DataSets=DataSet, observedPropertyGroup='PSA', usr='ross.searle@csiro.au', key='a')



makeLocations(df, drawit = T)

url<-"https://soil-chem.information.qld.gov.au/odata/SiteLabMethods?$expand=SiteLabMethodResults($expand=Sample($select=UpperDepth,LowerDepth);$select=NumericValue)&$filter=LabMethodCode eq 'P10_Grav'"

df <- fromJSON(URLencode(url))
nrow(df)
str(df)



f <- getSoilData(DataSets='QLDGovernment', observedProperty='O_PPF;O_GSG;O_ASC_ORD;O_ASC_SUBORD', usr='ross.searle@csiro.au', key='a')

f <- getSoilData(DataSets='NTGovernment', observedProperty='O_PPF;O_GSG;O_ASC_ORD;O_ASC_SUBORD', usr='ross.searle@csiro.au', key='a')


DataSet='TasGovernment'

obs <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/Properties?PropertyGroup=PSA')

lodfs <- vector("list", nrow(obs))
for (i in 1:nrow(obs)){
  prop <- obs[i]
  print(obs[i])
  df <- getSoilData(DataSets='WAGovernment', observedProperty=prop, usr='ross.searle@csiro.au', key='a')
  print(head(df))
  lodfs[[i]] <- df
}


lodfs <- vector("list", nrow(obs))
for (i in 1:nrow(obs)){
  prop <- obs$Property[i]
  url <- paste0('http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?DataSet=WAGovernment&observedProperty=', prop, '&usr=ross.searle@csiro.au&key=a')
  df <- fromJSON(url)
  print(head(df))
  lodfs[[i]] <- df
}
outDF = as.data.frame(data.table::rbindlist(lodfs, use.names = T))


nrow(outDF)

head(lodfs[[1]])
head(lodfs[[3]])

makeLocations(outDF, drawit = T)



#########  spatial extent clipping   ###########################
provider = 'LawsonGrains'
areasOverlap(provider)
#
bbox <- extent(142.6, 143.1, -35.9, -35.48)
wext <- '142.6;143.1;-35.9;-35.48'
bbox <- extent(110.6, 153.1, -43, -9)
wext <- '110;153;-43;-9'


bbox <- '130.3488;130.8577;-12.4212;-11.4207'


bbox <- '130.8488;130.9577;-12.7212;-12.5207'

bbox <- extent(130, 134, -20, -12)

# bits <- str_split(wext, ';')
# l <- as.numeric(bits[[1]][1])
# r <- as.numeric(bits[[1]][2])
# t <- as.numeric(bits[[1]][4])
# b <- as.numeric(bits[[1]][3])
# bboxExt <- extent(l, r, b, t)
sdf <-  getSiteLocations(DataSets='NTGovernment', bBox=bbox, usr='ross.searle@csiro.au', key='a')
nrow(sdf)
head(sdf)
coordinates(sdf) <- ~Longitude+Latitude
plot(sdf)


df <- getSoilData(DataSets='NTGovernment', observedProperty='4A1', bBox=bbox, usr='ross.searle@csiro.au', key='a')
nrow(df)
df <- getSoilData(DataSets='NTGovernment', observedProperty='h_texture', bBox=bboxExt, usr='ross.searle@csiro.au', key='a')
nrow(df)

df <- getSoilData(DataSets=NULL, observedProperty='4A1', bBox=bbox, usr='ross.searle@csiro.au', key='a')
nrow(df)

df <- getSoilData(DataSets='NTGovernment', observedProperty='4A1', bBox=bbox, usr='ross.searle@csiro.au', key='a')
nrow(df)

df <- getSoilData(DataSets='SCARP', observedProperty='4A1', bBox=bboxExt, usr='ross.searle@csiro.au', key='a')
nrow(df)

df <- getSoilData()
df <- getSoilData(DataSets='NTGovernment;Test', observedProperty='4A1')
nrow(df)


coordinates(df) <- ~Longitude+Latitude




DataSet <- 'QLDGovernment'
ObsProp <- 'cf_size'
ObsProp <- 'h_texture'
ObsProp <- 'o_asc_ord'
ObsProp <- 'o_permeability'
ObsProp <- 'o_asc_ord'
ObsProp <- 's_slope'
ObsProp <- 's_notes'






library(jsonlite)

df <- fromJSON('http://127.0.0.1:3287/SoilDataAPI/ObservationLocations?DataSet=TERNSurveillance')

df2 <- fromJSON('http://127.0.0.1:3287/SoilDataAPI/ObservationLocations')
nrow(df2)
write.csv(df2, 'c:/temp/locs.csv')





#12499

getDataForASite('12499')


sql <- 'select * from Datasets where Active=1'
dSets <- doQueryFromFed(sql)

ds <- dSets$DataSet


lodfs <- vector("list", length(ds))
for (i in 1:length(ds)){
  print(ds[i])
  df <- getSoilData(DataSets=ds[i], observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
  print(head(df))
  lodfs[[i]] <- df
}

outDF = as.data.frame(data.table::rbindlist(lodfs))

write.csv(outDF, 'c:/temp/soilDF.csv', row.names = F)
soilDF <- read.csv('c:/temp/soilDF.csv')







url <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=O_PPF;O_GSG;O_ASC_ORD;O_ASC_SUBORD&DataSet=WAGovernment&usr=brendan.malone%40csiro.au&key=djwjgrpt74ld7wm'
fromJSON(url)


df <- getSoilData(DataSets='WAGovernment', observedProperty=prop, usr='ross.searle@csiro.au', key='a')
df <- getSoilData(DataSets='WAGovernment', observedProperty='O_PPF', usr='ross.searle@csiro.au', key='a')
nrow(df)


url <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=P10_PB_C&DataSet=WAGovernment&usr=brendan.malone%40csiro.au&key=djwjgrpt74ld7wm'
df <- fromJSON(url)



observedProperty='P10_PB_C'

df <- getSoilData(DataSets='WAGovernment', observedProperty='P10106_150', usr='ross.searle@csiro.au', key='a')



df <- getSoilData(DataSets='SAGovernment', observedProperty='P10_GRAV', usr='ross.searle@csiro.au', key='a')

url <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=P10_GRAV&DataSet=SAGovernment&usr=brendan.malone%40csiro.au&key=djwjgrpt74ld7wm'
df <- fromJSON(url)

length(which(is.na(df$Latitude)))



