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

getSoilData(DataSets='NatSoil',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='QLDGovernment',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='LawsonGrains_AgCatalyst',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='TERNSurveillance',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='TasGovernment',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='WAGovernment',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SAGovernment',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='VicGovernment',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='NSWGovernment',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')

####  Still need to fix NT
getSoilData(DataSets='NTGovernment',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')

getSoilData(DataSets='NatGeoChemicalSurvey',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='SCARP',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Salinity_Action_Plan_Flora_Survey',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='South West Australian Transitional Transect (SWATT)',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Fire_gimlet_woodlands_flora',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Kimberley_Rainforest_Survey_1990',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')
getSoilData(DataSets='Little Sandy Desert survey',observedProperty='COL_HUE_VAL_CHROM', usr='ross.searle@csiro.au', key='a')


DataSet<-'NSWGovernment'
observedProperty='COL_HUE_VAL_CHROM'

