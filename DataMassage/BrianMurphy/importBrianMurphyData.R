library(RSQLite)
library(DBI)
library(stringr)
library(httr)
library(plyr)
library(dplyr)
library(leaflet)
library(sf)
library(sfheaders)

#dbPathSoilsFed <- 'C:/Users/sea084/OneDrive - CSIRO/ProjectAdmin/SoilDataFederator/DB/soilsFederator.sqlite'

doQuery <- function(con, sql){
  res <- dbSendQuery(con, sql)
  rows <- dbFetch(res)
  dbClearResult(res)
  return(rows)
}


copyTable <- function(conn, From, To){
  sql <- paste0('select * from ', From)
  res <- dbSendQuery(conn, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  dbWriteTable(conn, To, df, appen = F)
}

sendStatement <- function(con, sql){
  rs <- dbSendStatement(con, sql)
  dbHasCompleted(rs)
  nrec <- dbGetRowsAffected(rs)
  dbClearResult(rs)
  return(nrec)
}



getWebData <- function(url){
  resp <- GET(url, timeout = 300)
  response <- content(resp, "text", encoding = 'UTF-8')
  md <- fromJSON(response)
  return(md)
}





conn <- dbConnect(RSQLite::SQLite(), dbPathSoilsFed)



#df <- read.csv('C:/Projects/TernLandscapes/Site Data/BrianMurphy/ALL_BD_DIPNR.csv', stringsAsFactors = F)
df <- read.csv('/datasets/work/lw-soildatarepo/work/Ross/temp/ALL_BD_DIPNR.csv', stringsAsFactors = F)
nrow(df)
head(df)
str(df)


flds<-c('GSG','ASC','Horizon','BD','Clay','Silt','FS','CS','Sand','Electricial_Conductivity','pH_in_water','CEC','Exch_Ca','Exch_Mg','Exch_K','Exch_Na','Sum_Cations','ESPs','OC')
code<- c('O_GSG','O_ASC_ORD','H_DESIG_MASTER','P3A_NR','P10_NR_C','P10_NR_Z','P10_NR_FS','P10_NR_CS','P10_NR_S','3_NR','4A1','15_NR_CEC','EXCH_BASES_CA','EXCH_BASES_MG','EXCH_BASES_K','EXCH_BASES_NA','15_BASES','15N1','6Z')
units<- c('NA','NA','NA','g/cm3','Clay','%','%','%','%','NA','NA','meq/100g','meq/100g','meq/100g','meq/100g','meq/100g','meq/100g','%','%')
dtype<- c('FieldMeasurement','FieldMeasurement','FieldMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement','LaboratoryMeasurement')
rdf <- data.frame(flds, code, units, dtype)

for (i in 1:nrow(rdf)) {
  rec <- rdf[i,]
  print(i)
  vals<-df[rec$flds]
  colnames(vals) = NULL
  odf <- data.frame(Provider='BM',Dataset='BM', Observation_ID=paste0(df$Data, '_', df$Source, '_', df$Site), Longitude=df$Long, Latitude=df$Lat,
                    SampleID=1, Date=NA, UpperDepth=df$Lab_Upper_Depth/100, LowerDepth=df$Lab_Lower_Depth/100, PropertyType=rec$dtype,
                    ObservedProperty=rec$code, Value=vals, Units=rec$units)
  idxs <- which(odf$Value=='NA' | odf$Value=='na' | odf$Value=='' | is.null(odf$Value) | is.na(odf$Value))
  if(length(idxs) > 1){
  oodf <- odf[-idxs,]
  }else{
    oodf <- odf
  }
  write.csv(oodf, paste0('/datasets/work/lw-soildatarepo/work/Ross/temp/props/', rec$flds, '.csv'), row.names = F)
}

idf <- read.csv('C:/Projects/TernLandscapes/Site Data/BrianMurphy/props/pH_in_water.csv')
idf$Longitude <- as.numeric(idf$Longitude)
idf$Latitude <- as.numeric(idf$Latitude)
pts <- sf_point(idf, x='Longitude', y='Latitude')
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(idf$Longitude, idf$Latitude)
m  # Print the map

fls <- list.files('/datasets/work/lw-soildatarepo/work/Ross/temp/props', '*.csv', full.names = T)
#conD <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
conD <- dbConnect(RSQLite::SQLite(), '/datasets/work/lw-soildatarepo/work/TERNLandscapes/SoilsFederator/HostedDBs/SoilDataFederatorDatabase.db3')

for (i in 1:length(fls)) {
  print(i)
  odf<-read.csv(fls[i])
  dbAppendTable(conD, 'ObservedProperties', odf)
}

mappings <- data.frame(
  Dataset='BM',
    OrigPropertyCode = rdf$code,
  ObservedProperty=rdf$code,
  DataType='L',
  StandardCode=1, stringsAsFactors = F)

conn <- dbConnect(RSQLite::SQLite(), '/srv/DB/SoilDataFederator/soilsFederator.sqlite')



dbAppendTable(conn, 'Mappings', mappings)
