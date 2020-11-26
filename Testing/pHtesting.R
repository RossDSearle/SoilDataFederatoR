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

getSoilData(DataSets='NatSoil',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='QLDGovernment',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='LawsonGrains_AgCatalyst',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='TERNSurveillance',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='TasGovernment',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='WAGovernment',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SAGovernment',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='VicGovernment',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NSWGovernment',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')


#getSoilData(DataSets='NTGovernment',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NatGeoChemicalSurvey',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='SCARP',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Salinity_Action_Plan_Flora_Survey',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='South West Australian Transitional Transect (SWATT)',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Fire_gimlet_woodlands_flora',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Kimberley_Rainforest_Survey_1990',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Little Sandy Desert survey',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='Kimberley_Islands_Biodiversity_Survey',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Biological Survey of the Ravensthorpe Range (Phase 1)',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Transects for Environmental Monitoring and Decision Making (TREND)',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Nullarbor_Regional_Survey',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Pilbara_Regional_Survey_Riparian_Flora',observedProperty='PH_VALUE', usr='ross.searle@csiro.au', key='a')




DataSet<-'NTGovernment'
observedProperty='PH_VALUE'
observedPropertyGroup=NULL

unique(allInfo$observed_property)
