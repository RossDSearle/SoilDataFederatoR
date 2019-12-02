library(rgdal)
library(sf)
library(aqp)
library(jsonlite)
library(SoilDataFeder8R)

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


df <- getSoilData( observedProperty='3A1', usr = 'Admin', pwd = 'c')
head(df)
tail(df)

op = 'h_texture'
op = 'SEG_FORM'
op = 'RO_ABUN'
op = 'S_DESC_BY'


datase

usr='Demo'; key='Demo'

usr <- 'ross.searle@csiro.au'
key <- 'a'

datas

props <- c('3A1', 'h_texture')

sql <- 'select * from Datasets where Active=1'
dSets <- doQueryFromFed(sql)

ds <- dSets$DataSet


for (j in 1:length(props)) {

  OP <- props[j]

  if(j==1){
    cat(crayon::yellow('\n\nTesting Lab Data EndPoints', sep=''))
    cat(crayon::yellow('\n===============================', sep=''))
  }else{
    cat(crayon::yellow('\n\nTesting Morphology Data EndPoints', sep=''))
    cat(crayon::yellow('\n=====================================', sep=''))
  }

    for (i in 1:length(ds)) {
      dSt <- ds[i]

      cat(crayon::blue('\n', dSt,' : ', sep=''))
      r1 <- getSoilData(DataSets=dSt,observedProperty=OP, usr='ross.searle@csiro.au', key='a', verbose=F)
      #r2 <- getLocations(DataSets=dSt, usr='ross.searle@csiro.au', key='a')

      if(nrow(r1) > 0){
        cat(crayon::green('SUCCESS', sep=''))
        cat(' -  returned ', nrow(r1), ' records', sep='')
      }else{
        cat(crayon::red('POSSIBLE PROBLEM', sep=''))
      }
    }
}



####  Test the getLocations EndPoints

cat(crayon::yellow('\n\nTesting LOcations EndPoints', sep=''))
cat(crayon::yellow('\n=====================================', sep=''))
for (i in 1:length(ds)) {
  dSt <- ds[i]

  cat(crayon::blue('\n', dSt,' : ', sep=''))

  r2 <- getSiteLocations(DataSets=dSt, usr='ross.searle@csiro.au', key='a')

  if(nrow(r2) > 0){
    cat(crayon::green('SUCCESS', sep=''))
    cat(' -  returned ', nrow(r2), ' records', sep='')
  }else{
    cat(crayon::red('POSSIBLE PROBLEM', sep=''))
  }
}



getLocations(usr=usr, key=key)


getSoilData(DataSets='TERNSurveillance',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='TERNSurveillance', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='LawsonGrains_AgCatalyst', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='LawsonGrains_AgCatalyst', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='NatGeoChemicalSurvey',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NatGeoChemicalSurvey',observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='NatGeoChemicalSurvey', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='EastCentral_Australia',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='EastCentral_Australia', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='NatSoil',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NatSoil',observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='NatSoil', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='NTGovernment',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='NTGovernment', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='SCARP',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SCARP',observedProperty='3A1', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='SCARP', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='SAGovernment', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SAGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='SAGovernment', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='WAGovernment', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='WAGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='WAGovernment', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='QLDGovernment', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='QLDGovernment', observedProperty='h_texture', usr='ross.searle@csiro.au', key='a')
getSiteLocations(DataSets='QLDGovernment', usr='ross.searle@csiro.au', key='a')


DataSet='TasGovernment'
DataSet='WAGovernment'
DataSet='SAGovernment'
df <- getSoilData(DataSets=DataSet, observedPropertyGroup='PSA', usr='ross.searle@csiro.au', key='a')
makeLocations(df, drawit = T)

url<-"https://soil-chem.information.qld.gov.au/odata/SiteLabMethods?$expand=SiteLabMethodResults($expand=Sample($select=UpperDepth,LowerDepth);$select=NumericValue)&$filter=LabMethodCode eq 'P10_Grav'"

df <- fromJSON(URLencode(url))
nrow(df)
str(df)



f <- getSoilData(DataSets='QLDGovernment', observedProperty='O_PPF;O_GSG;O_ASC_ORD;O_ASC_SUBORD', usr='ross.searle@csiro.au', key='a')

f <- getSoilData(DataSets='NTGovernment', observedProperty='O_PPF;O_GSG;O_ASC_ORD;O_ASC_SUBORD', usr='ross.searle@csiro.au', key='a')


DataSet='TasGovernment'

lodfs <- vector("list", length(ds))
for (i in 1:length(ds)){
  print(ds[i])
  df <- getSoilData(DataSets=ds[i], observedProperty='O_ASC_ORD', usr='ross.searle@csiro.au', key='a')
  print(head(df))
  lodfs[[i]] <- df
}

outDF = as.data.frame(data.table::rbindlist(lodfs))
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

