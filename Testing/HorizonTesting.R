library(rgdal)
library(sf)
library(aqp)
library(jsonlite)

source(paste0('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/Backends.R'))
usr <- 'ross.searle@csiro.au'; key <- 'a';

dsl <- getDataSets(usr,key)
ds <- dsl$DataSet
pgs <-getPropertyGroups(verbose)

props <- getStandardProperties('PHS', T)


df <- fromJSON('http://asris-daas02/NatSoil_Services/api/LabResults?method_code=6A1')


getSoilData(DataSets='NatSoil',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='QLDGovernment',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='LawsonGrains_AgCatalyst',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='TERNSurveillance',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='TasGovernment',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='WAGovernment',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SAGovernment',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='VicGovernment',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NSWGovernment',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NTGovernment',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='NatGeoChemicalSurvey',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SCARP',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Salinity_Action_Plan_Flora_Survey',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='South West Australian Transitional Transect (SWATT)',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Fire_gimlet_woodlands_flora',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Kimberley_Rainforest_Survey_1990',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Little Sandy Desert survey',observedProperty='h_name', usr='ross.searle@csiro.au', key='a')


DataSet<-'NSWGovernment'
observedProperty='COL_HUE_VAL_CHROM'

