library(RSQLite)
library(DBI)
library(stringr)
library(httr)

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


labm <- dbReadTable(conn, "Mappings")
dbWriteTable(conn, "Mappings_V3", labm)
dbListTables(conn)

#####   Added a new field so just populating it
sql <-"UPDATE Mappings SET StandardCode = TRUE;"
sendStatement(conn, sql)


######   NSW   ########

sql <-"UPDATE Mappings SET Organisation = 'NSWGovernmentNSSC' WHERE Organisation = 'NSWGovernment';"
sendStatement(conn, sql)


sql <-"Select * from Mappings WHERE Organisation = 'NSWGovernment';"
r <- doQuery(conn, sql)


copyTable(conn, 'Mappings', 'Mappings_V4')
sendStatement(conn, sql)

sql <- "delete from Mappings where Organisation = 'NSWGovernment'"
sendStatement(conn, sql)

sql <- "select * from Mappings where Organisation = 'NSWGovernmentNSCC'"
r <- doQuery(conn, sql)



inP <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/NSW/NSW code Mappings NEW.csv'
df <- read.csv(inP)
dbAppendTable(conn, 'Mappings', df)



######   NatSoil   ########
copyTable(conn, 'Mappings', 'Mappings_V5')
sql <-"UPDATE Mappings SET Dataset = 'NatSoil' WHERE Dataset = 'CSIRO';"
sendStatement(conn, sql)

sql <-"select * from Mappings WHERE Dataset = 'NatSoil';"
df <- doQuery(conn, sql)
head(df)
df$Dataset<- 'SCARP'
dbAppendTable(conn, 'Mappings', df)
sql <-"select * from Mappings WHERE Dataset = 'SCARP';"
dfnew <- doQuery(conn, sql)
head(dfnew)



#####  Vic   ##########

sql <-"select * from Mappings WHERE Dataset = 'VicGovernment' and DataType = 'M';"
df <- doQuery(conn, sql)
head(df)

copyTable(conn, 'Mappings', 'Mappings_V6')

sql <- "delete from Mappings where DataSet = 'VicGovernment'"
sendStatement(conn, sql)

df <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/Vic/VicMappings.csv')
head(df)
dbAppendTable(conn, 'Mappings', df)


######   Lawson Grains  #######

sql <-"UPDATE Mappings SET Dataset = 'LawsonGrains_AgCatalyst' WHERE Dataset = 'LawsonGrains';"
sendStatement(conn, sql)




#######  NatGeoChemicalSurvey  #########
conn <- dbConnect(RSQLite::SQLite(), "C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3")

sql <-"select * from Properties"
df <- doQuery(conn, sql)
odf <- data.frame(Dataset='NatGeoChemicalSurvey', OrigPropertyCode=df$Property, ObservedProperty=df$Property, DataType=df$PropertyType, StandardCode=TRUE )
idxs <- which(odf$DataType=='FieldMeasurement')
odf$DataType[idxs] <- 'M'
idxs2 <- which(odf$DataType=='LaboratoryMeasurement')
odf$DataType[idxs2] <- 'L'

sql <-"select * from ObservedProperties where Provider = 'GeoscienceAustralia'"
df <- doQuery(conn, sql)
head(df)
unique(df$ObservedProperty)
odf2 <-

dbAppendTable(conn, 'Mappings', odf)





########    SALI   ############
conSali <- dbConnect(RSQLite::SQLite(), "C:/Projects/TernLandscapes/Site Data/HostedDBs/SALI_Morphology.sqlite")


copyTable(conn, 'Mappings', 'Mappings_V7')


sql <-"select * from Mappings"
df <- doQuery(conSali, sql)
head(df)
write.csv(df, 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/QLD/SALImappings.csv')


sql <-"select * from Mappings WHERE Dataset = 'QLDGovernment' and DataType = 'M';"
df <- doQuery(conn, sql)
head(df)

sql <- "delete from Mappings WHERE Dataset = 'QLDGovernment' and DataType = 'M';"
sendStatement(conn, sql)

mdf <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/QLD/SALImappings.csv')
nrow(mdf)
dbAppendTable(conn, 'Mappings', mdf)




###########    EastCentral_Australia


Hosted_dbPath <- paste0('C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
cone <- DBI::dbConnect(RSQLite::SQLite(), Hosted_dbPath)

sql <-"select * from ObservedProperties WHERE Dataset = 'EastCentral_Australia';"
df <- doQuery(cone, sql)
head(df)

unique(df$ObservedProperty)

df <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/EastAust/EastMapping.csv')
copyTable(conn, 'Mappings', 'Mappings_V8')
sql <-"select * from Mappings where DataSet = 'EastCentral_Australia'"
sql <-"select * from Mappings where DataSet = 'QLDGovernment'"
df <- doQuery(conn, sql)
head(df)

dbAppendTable(conn, 'Mappings', df)


###########    GA


Hosted_dbPath <- paste0('C:/Projects/TernLandscapes/Site Data/HostedDBs/SoilDataFederatorDatabase.db3')
cone <- DBI::dbConnect(RSQLite::SQLite(), Hosted_dbPath)

sql <-"select * from ObservedProperties WHERE Dataset = 'NatGeoChemicalSurvey';"
sql <-"select * from ObservedProperties WHERE Dataset = 'NatGeoChemicalSurvey' and ObservedProperty = 'PH_VALUE';"
df <- doQuery(cone, sql)
head(df)

unique(df$ObservedProperty)

df <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/GA/GeoChemMapping.csv')
copyTable(conn, 'Mappings', 'Mappings_V8')
sql <-"select * from Mappings where DataSet = 'NatGeoChemicalSurvey'"
sql <-"select * from Mappings where DataSet = 'QLDGovernment'"
df <- doQuery(conn, sql)
head(df)

dbAppendTable(conn, 'Mappings', df)



sql <-"select * from ObservedProperties WHERE Dataset = 'EcologicalProjects';"
df <- doQuery(cone, sql)
head(df)










####### generate the new format Properties table

dfSS <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/StandardSchema.csv')
dbWriteTable(conn, 'StandardSchema', df)

sql <-"select * from StandardSchema"
dfSS <- doQuery(conn, sql)
head(dfSS)

sql <-"select * from properties"
dfProp <- doQuery(conn, sql)
head(dfProp)

dfm <- merge(dfProp, dfSS, by.x = 'Property', by.y='ColumnName', all.y = T)
str(dfm)
head(dfm)
write.csv(dfm, 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/StandardSchema2.csv')





df <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/StandardSchema2.csv')
head(df)
df2 <- df[df$PropertyType=='LaboratoryMeasurement',]
nrow(df)
nrow(df2)


url <- paste0('http://registry.it.csiro.au/def/soil/au/scma/',df2$Property[1], 'x' )

url <- 'http://registry.it.csiro.au/def/soil/au/scma/9G2x?_format=jsonld'


for (i in 1:nrow(df)) {

  if(df[i, 3] =='LaboratoryMeasurement'){
        print(i)
        u <- paste0('http://registry.it.csiro.au/def/soil/au/scma/',df2[i, 2])
        url <- paste0(u, '?_format=jsonld')
        resp <- GET(url, timeout = 300)
        resp$status_code
        response <- content(resp, "text", encoding = 'UTF-8')

        if(response==''){

        }else{
          md <- fromJSON(response)
          md$`dct:source`
          df2[i,9] <- u
        }

  }
}

write.csv(df2, 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/StandardSchema3.csv')
