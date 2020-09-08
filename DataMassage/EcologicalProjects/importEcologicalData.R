library(RSQLite)
library(DBI)
library(stringr)
library(httr)
library(plyr)
library(dplyr)

dbPathSoilsFed <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DB/soilsFederator.sqlite'

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



df <- read.csv('C:/Projects/TernLandscapes/Site Data/Karel/soil_data_ecol_surveys/EcologicalSurverys_soil_data_2019-03-13.csv', stringsAsFactors = F)
nrow(df)
head(df)
str(df)

length(unique(df$literatureCitation))

df$Date <- '1/1/0001'
idxs <- which(df$literatureCitation == "Gosper, C.R., Yates, C.J. and Prober, S.M. (2013) Floristic diversity in fire-sensitive eucalypt woodlands shows a 'U'-shaped relationship with time since fire. Journal of Applied Ecology 50, 1187-1196")
df$Date[idxs] <- '1/1/2013'

idxs <- which(df$literatureCitation == "Lyons, M.N, Keighery, G.J., Gibson, L.A. and Handasyde, T. (2014). Flora and vegetation communities of selected islands off the Kimberley coast of Western Australia. Records of the Western Australian Museum, Supplement 81: 205-243")
df$Date[idxs] <- '1/1/2014'

idxs <- which(df$literatureCitation == "Kenneally, K.F., Keighery, G.J. and Hyland, B.P.M. (1991) Floristics and phytogeaography of Kimberley rainforests, Western Australia. In:  McKenzie, N.L., Johnston, R.B. & Kendrick, P.G., eds. Kimberley rainforests of Australia. Surrey Beatty, Chipping Norton, N.S.W. pp 93-131.")
df$Date[idxs] <- '1/1/1991'

idxs <- which(df$literatureCitation == "Van Leeuwen SJ, Bromilow RN (2002). Flora of the south-western Little Sandy Desert. In Biological Survey of the South-Western Little Sandy Desert: National Reserve System Project N709: Final Report, June 2002 Department of Conservation and Land Management, Karratha. pp. 33Ã±64")
df$Date[idxs] <- '1/1/2002'

idxs <- which(df$literatureCitation == "McKenzie, N.L. & Robinson, A.C., eds, 1984. A Biological Survey of the Nullarbor Region of South and Western Australia in 1984. South Australian Deptartment of Environment and Planning, Adelaide. 413pp")
df$Date[idxs] <- '1/1/1984'

idxs <- which(df$literatureCitation == "Lyons, M.N. (2015) The riparian fl ora and plant communities of the Pilbara region of Western Australia. Records of the Western Australian Museum, Supplement 78: 485–513.")
df$Date[idxs] <- '1/1/2015'

idxs <- which(df$literatureCitation == "N. Gibson, G.J. Keighery, M.N. Lyons and A. Webb (2004) Terrestrial flora and vegetation of the Western Australian wheatbelt. Records of the Western Australian Museum, Supplement No. 67: 139-189.   M.N. Lyons, N. Gibson, G.J. Keighery and S.D. Lyons (2004) Wetland flora and vegetation of the Western Australian wheatbelt. Records of the Western Australian Museum, Supplement No. 67: 39-89.")
df$Date[idxs] <- '1/1/2004'
odf<- df[idxs, ]

write.csv(unique(df$measurementMethod), 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/EcologicalProjects/Methods.csv')
unique(df$Date)

unique(odf$measurementMethod)

str(df)
meths2 <- df %>% group_by(measurementType, measurementUnit, measurementMethod) %>% summarise(n = n())
#write.csv(meths2, 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/EcologicalProjects/Methods2.csv')
nrow(meths2)

meths <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/EcologicalProjects/Methods3.csv')
mdf <- merge(df, meths,  by.x = c('measurementType',	'measurementUnit'), by.y = c('measurementType',	'measurementUnit'))
nrow(mdf)
write.csv(mdf, 'c:/temp/mdf.csv')

head(meths)
head(df)
str(meths)
str(df)

df2 <- df
df2$code <- 'x'
for (i in 1:nrow(meths)) {
  print(i)
  rec<- meths[i,]
  idxs <- which(df2$measurementType==rec$measurementType & df2$measurementUnit==rec$measurementUnit )
  df2$code[idxs] <- rec$code
}
write.csv(df2, 'c:/temp/mdf2.csv')


dfc <- read.csv('C:/Projects/TernLandscapes/Site Data/Karel/soil_data_ecol_surveys/EcologicalSurverys_soil_data_2019-03-13 WITH CODES.csv', stringsAsFactors = F)

ndf <- dfc[dfc$code=='x', ]
unique(ndf$measurementType)
unique(dfc$projectID)


############    Salinity_Action_Plan_Flora_Survey


indf <- dfc[dfc$projectID == 'Salinity_Action_Plan_Flora_Survey',]

idxs <- which(is.na(indf$measurementUnit))
indf$measurementUnit[idxs] <- 'None'

sdf <- data.frame(Provider='CSIRO_EcologicalProjects',
           Dataset  = 'Salinity_Action_Plan_Flora_Survey',
           Observation_ID = indf$plotObservationID,
           Longitude = indf$verbatimLongitude,
           Latitude = indf$verbatimLatitude,
           SampleID = 1,
           Date  = '1/1/2004',
           UpperDepth= 0,
           LowerDepth= 10,
           PropertyType= 'LaboratoryMeasurement',
           ObservedProperty = indf$code,
           Value= indf$measurementValue,
           Units = indf$measurementUnit,
           stringsAsFactors = F  )


conSDF <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
dbAppendTable(conSDF, 'ObservedProperties', sdf)

unique(sdf$ObservedProperty)
odf <- data.frame(Dataset='Salinity_Action_Plan_Flora_Survey', OrigPropertyCode=unique(sdf$ObservedProperty), ObservedProperty=unique(sdf$ObservedProperty), DataType='L', StandardCode=TRUE )
dbAppendTable(conn, 'Mappings', odf)






############    South West Australian Transitional Transect (SWATT)

sql <- "delete from ObservedProperties where DataSet = 'South West Australian Transitional Transect (SWATT)'"
sendStatement(conSDF, sql)

indf <- dfc[dfc$projectID == 'South West Australian Transitional Transect (SWATT)',]
head(indf)
nrow(indf)

idxs <- which(is.na(indf$measurementUnit))
indf$measurementUnit[idxs] <- 'None'

sdf <- data.frame(Provider='CSIRO_EcologicalProjects',
                  Dataset  = 'South West Australian Transitional Transect (SWATT)',
                  Observation_ID = indf$plotObservationID,
                  Longitude = indf$verbatimLongitude,
                  Latitude = indf$verbatimLatitude,
                  SampleID = 1,
                  Date  = '1/1/2014',
                  UpperDepth= 0,
                  LowerDepth= 10,
                  PropertyType= 'LaboratoryMeasurement',
                  ObservedProperty = indf$code,
                  Value= indf$measurementValue,
                  Units = indf$measurementUnit,
                  stringsAsFactors = F  )


conSDF <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
dbAppendTable(conSDF, 'ObservedProperties', sdf)

unique(sdf$ObservedProperty)
odf <- data.frame(Dataset='South West Australian Transitional Transect (SWATT)', OrigPropertyCode=unique(sdf$ObservedProperty), ObservedProperty=unique(sdf$ObservedProperty), DataType='L', StandardCode=TRUE )
dbAppendTable(conn, 'Mappings', odf)



############    Fire_gimlet_woodlands_flora


indf <- dfc[dfc$projectID == 'Fire_gimlet_woodlands_flora',]
head(indf)
nrow(indf)

idxs <- which(is.na(indf$measurementUnit))
indf$measurementUnit[idxs] <- 'None'

sdf <- data.frame(Provider='CSIRO_EcologicalProjects',
                  Dataset  = 'Fire_gimlet_woodlands_flora',
                  Observation_ID = indf$plotObservationID,
                  Longitude = indf$verbatimLongitude,
                  Latitude = indf$verbatimLatitude,
                  SampleID = 1,
                  Date  = '1/1/2010',
                  UpperDepth= 0,
                  LowerDepth= 10,
                  PropertyType= 'LaboratoryMeasurement',
                  ObservedProperty = indf$code,
                  Value= indf$measurementValue,
                  Units = indf$measurementUnit,
                  stringsAsFactors = F  )


conSDF <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
dbAppendTable(conSDF, 'ObservedProperties', sdf)

unique(sdf$ObservedProperty)
odf <- data.frame(Dataset='Fire_gimlet_woodlands_flora', OrigPropertyCode=unique(sdf$ObservedProperty), ObservedProperty=unique(sdf$ObservedProperty), DataType='L', StandardCode=TRUE )
dbAppendTable(conn, 'Mappings', odf)





############    Kimberley_Rainforest_Survey_1990


indf <- dfc[dfc$projectID == 'Kimberley_Rainforest_Survey_1990',]
head(indf)
nrow(indf)

idxs <- which(is.na(indf$measurementUnit))
indf$measurementUnit[idxs] <- 'None'

sdf <- data.frame(Provider='CSIRO_EcologicalProjects',
                  Dataset  = 'Kimberley_Rainforest_Survey_1990',
                  Observation_ID = indf$plotObservationID,
                  Longitude = indf$verbatimLongitude,
                  Latitude = indf$verbatimLatitude,
                  SampleID = 1,
                  Date  = '1/1/1991',
                  UpperDepth= 0,
                  LowerDepth= 10,
                  PropertyType= 'LaboratoryMeasurement',
                  ObservedProperty = indf$code,
                  Value= indf$measurementValue,
                  Units = indf$measurementUnit,
                  stringsAsFactors = F  )


#conSDF <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
dbAppendTable(conSDF, 'ObservedProperties', sdf)

unique(sdf$ObservedProperty)
odf <- data.frame(Dataset='Kimberley_Rainforest_Survey_1990', OrigPropertyCode=unique(sdf$ObservedProperty), ObservedProperty=unique(sdf$ObservedProperty), DataType='L', StandardCode=TRUE )
dbAppendTable(conn, 'Mappings', odf)



############    Little Sandy Desert survey

sql <- "delete from ObservedProperties where DataSet = 'Little Sandy Desert survey'"
sendStatement(conSDF, sql)

sql<- "select * from ObservedProperties where DataSet = 'Little Sandy Desert survey'"
doQuery(conSDF, sql)

indf <- dfc[dfc$projectID == 'Little Sandy Desert survey',]
head(indf)
nrow(indf)

indf$verbatimLatitude <- indf$verbatimLatitude * -1

idxs <- which(is.na(indf$measurementUnit))
indf$measurementUnit[idxs] <- 'None'

sdf <- data.frame(Provider='CSIRO_EcologicalProjects',
                  Dataset  = 'Little Sandy Desert survey',
                  Observation_ID = indf$plotObservationID,
                  Longitude = indf$verbatimLongitude,
                  Latitude = indf$verbatimLatitude,
                  SampleID = 1,
                  Date  = '1/1/2002',
                  UpperDepth= 0,
                  LowerDepth= 10,
                  PropertyType= 'LaboratoryMeasurement',
                  ObservedProperty = indf$code,
                  Value= indf$measurementValue,
                  Units = indf$measurementUnit,
                  stringsAsFactors = F  )


#conSDF <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
dbAppendTable(conSDF, 'ObservedProperties', sdf)

unique(sdf$ObservedProperty)
odf <- data.frame(Dataset='Little Sandy Desert survey', OrigPropertyCode=unique(sdf$ObservedProperty), ObservedProperty=unique(sdf$ObservedProperty), DataType='L', StandardCode=TRUE )
dbAppendTable(conn, 'Mappings', odf)



############   Biological Survey of the Ravensthorpe Range (Phase 1)


indf <- dfc[dfc$projectID == 'Biological Survey of the Ravensthorpe Range (Phase 1)',]
head(indf)
nrow(indf)

idxs <- which(!is.na(indf$measurementValue))
indf <- indf[idxs, ]

idxs <- which(is.na(indf$measurementUnit))
indf$measurementUnit[idxs] <- 'None'

sdf <- data.frame(Provider='CSIRO_EcologicalProjects',
                  Dataset  = 'Biological Survey of the Ravensthorpe Range (Phase 1)',
                  Observation_ID = indf$plotObservationID,
                  Longitude = indf$verbatimLongitude,
                  Latitude = indf$verbatimLatitude,
                  SampleID = 1,
                  Date  = '1/1/2008',
                  UpperDepth= 0,
                  LowerDepth= 10,
                  PropertyType= 'LaboratoryMeasurement',
                  ObservedProperty = indf$code,
                  Value= indf$measurementValue,
                  Units = indf$measurementUnit,
                  stringsAsFactors = F  )


#conSDF <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
dbAppendTable(conSDF, 'ObservedProperties', sdf)

unique(sdf$ObservedProperty)
odf <- data.frame(Dataset='Biological Survey of the Ravensthorpe Range (Phase 1)', OrigPropertyCode=unique(sdf$ObservedProperty), ObservedProperty=unique(sdf$ObservedProperty), DataType='L', StandardCode=TRUE )
dbAppendTable(conn, 'Mappings', odf)




############   Pilbara_Regional_Survey_Riparian_Flora


indf <- dfc[dfc$projectID == 'Pilbara_Regional_Survey_Riparian_Flora',]
head(indf)
nrow(indf)

idxs <- which(!is.na(indf$measurementValue))
indf <- indf[idxs, ]

idxs <- which(is.na(indf$measurementUnit))
indf$measurementUnit[idxs] <- 'None'

sdf <- data.frame(Provider='CSIRO_EcologicalProjects',
                  Dataset  = 'Pilbara_Regional_Survey_Riparian_Flora',
                  Observation_ID = indf$plotObservationID,
                  Longitude = indf$verbatimLongitude,
                  Latitude = indf$verbatimLatitude,
                  SampleID = 1,
                  Date  = '1/1/2008',
                  UpperDepth= 5,
                  LowerDepth= 15,
                  PropertyType= 'LaboratoryMeasurement',
                  ObservedProperty = indf$code,
                  Value= indf$measurementValue,
                  Units = indf$measurementUnit,
                  stringsAsFactors = F  )


#conSDF <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
dbAppendTable(conSDF, 'ObservedProperties', sdf)

unique(sdf$ObservedProperty)
odf <- data.frame(Dataset='Pilbara_Regional_Survey_Riparian_Flora', OrigPropertyCode=unique(sdf$ObservedProperty), ObservedProperty=unique(sdf$ObservedProperty), DataType='L', StandardCode=TRUE )
dbAppendTable(conn, 'Mappings', odf)


############   Kimberley_Islands_Biodiversity_Survey


indf <- dfc[dfc$projectID == 'Kimberley_Islands_Biodiversity_Survey',]
head(indf)
nrow(indf)

idxs <- which(!is.na(indf$measurementValue))
indf <- indf[idxs, ]

idxs <- which(is.na(indf$measurementUnit))
indf$measurementUnit[idxs] <- 'None'

sdf <- data.frame(Provider='CSIRO_EcologicalProjects',
                  Dataset  = 'Kimberley_Islands_Biodiversity_Survey',
                  Observation_ID = indf$plotObservationID,
                  Longitude = indf$verbatimLongitude,
                  Latitude = indf$verbatimLatitude,
                  SampleID = 1,
                  Date  = '1/1/2008',
                  UpperDepth= 0,
                  LowerDepth= 10,
                  PropertyType= 'LaboratoryMeasurement',
                  ObservedProperty = indf$code,
                  Value= indf$measurementValue,
                  Units = indf$measurementUnit,
                  stringsAsFactors = F  )


#conSDF <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
dbAppendTable(conSDF, 'ObservedProperties', sdf)

unique(sdf$ObservedProperty)
odf <- data.frame(Dataset='Kimberley_Islands_Biodiversity_Survey', OrigPropertyCode=unique(sdf$ObservedProperty), ObservedProperty=unique(sdf$ObservedProperty), DataType='L', StandardCode=TRUE )
dbAppendTable(conn, 'Mappings', odf)





############   Nullarbor_Regional_Survey


indf <- dfc[dfc$projectID == 'Nullarbor_Regional_Survey',]
head(indf)
nrow(indf)

idxs <- which(!is.na(indf$measurementValue))
indf <- indf[idxs, ]

idxs <- which(is.na(indf$measurementUnit))
indf$measurementUnit[idxs] <- 'None'

sdf <- data.frame(Provider='CSIRO_EcologicalProjects',
                  Dataset  = 'Nullarbor_Regional_Survey',
                  Observation_ID = indf$plotObservationID,
                  Longitude = indf$verbatimLongitude,
                  Latitude = indf$verbatimLatitude,
                  SampleID = 1,
                  Date  = '1/1/1984',
                  UpperDepth= 0,
                  LowerDepth= 10,
                  PropertyType= 'LaboratoryMeasurement',
                  ObservedProperty = indf$code,
                  Value= indf$measurementValue,
                  Units = indf$measurementUnit,
                  stringsAsFactors = F  )


#conSDF <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
dbAppendTable(conSDF, 'ObservedProperties', sdf)

unique(sdf$ObservedProperty)
odf <- data.frame(Dataset='Nullarbor_Regional_Survey', OrigPropertyCode=unique(sdf$ObservedProperty), ObservedProperty=unique(sdf$ObservedProperty), DataType='L', StandardCode=TRUE )
dbAppendTable(conn, 'Mappings', odf)




############   Transects for Environmental Monitoring and Decision Making (TREND)


indf <- dfc[dfc$projectID == 'Transects for Environmental Monitoring and Decision Making (TREND)',]
head(indf)
nrow(indf)

idxs <- which(!is.na(indf$measurementValue))
indf <- indf[idxs, ]

idxs <- which(is.na(indf$measurementUnit))
indf$measurementUnit[idxs] <- 'None'

bits <- str_split(indf$plotObservationID, '_')
dl2 <- sapply(bits, function (x) x[3])

bits2 <- str_split(dl2, '-')
yrs <- sapply(bits2, function (x) x[1])
mns <- sapply(bits2, function (x) x[2])
dys <- sapply(bits2, function (x) x[3])

sdf <- data.frame(Provider='CSIRO_EcologicalProjects',
                  Dataset  = 'Transects for Environmental Monitoring and Decision Making (TREND)',
                  Observation_ID = indf$plotObservationID,
                  Longitude = indf$verbatimLongitude,
                  Latitude = indf$verbatimLatitude,
                  SampleID = 1,
                  Date  = paste0(dys,'/', mns, '/', yrs),
                  UpperDepth= 0,
                  LowerDepth= 10,
                  PropertyType= 'LaboratoryMeasurement',
                  ObservedProperty = indf$code,
                  Value= indf$measurementValue,
                  Units = indf$measurementUnit,
                  stringsAsFactors = F  )


#conSDF <- dbConnect(RSQLite::SQLite(), 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
dbAppendTable(conSDF, 'ObservedProperties', sdf)

unique(sdf$ObservedProperty)
odf <- data.frame(Dataset='Transects for Environmental Monitoring and Decision Making (TREND)', OrigPropertyCode=unique(sdf$ObservedProperty), ObservedProperty=unique(sdf$ObservedProperty), DataType='L', StandardCode=TRUE )
odf$DataType[4] <- 'M'
dbAppendTable(conn, 'Mappings', odf)



projs <- unique(dfc$projectID)

for (i in 1:length(projs)) {

  sql <-paste0("select * from ObservedProperties where DataSet = '",projs[i], "'" )

  df <- doQuery(conSDF, sql)
  head(df)

  minX <- min(df$Longitude)
  maxX <- max(df$Longitude)
  minY <- min(df$Latitude)
  maxY <- max(df$Latitude)

  print(paste0(projs[i], '  ', minX, '  ', minY, '  ',  maxX, '  ',  maxY))
}



sql <- 'select * from ObservedProperties where DataSet = "Little Sandy Desert survey"'
df <- doQuery(conSDF, sql)
sql <-"UPDATE ObservedProperties SET Latitude = Latitude*-1 WHERE Dataset = 'Little Sandy Desert survey';"
sendStatement(conSDF, sql)
