library(RSQLite)
library(DBI)
library(stringr)
library(httr)
library(plyr)

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


copyTable(conn, 'Properties', 'Properties_V2')
props <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/StandardSchema3.csv', stringsAsFactors = F)

cnts <- props %>% distinct(Property) %>% count()
cntss <- cnts[order(freq), ]
sum(cnts$freq)

dbWriteTable(conn, "Properties", props)


sql <-'SELECT Mappings.Dataset, Mappings.OrigPropertyCode, Mappings.ObservedProperty, Properties.PropertyType, Properties.PropertyGroup, Properties.Description, Properties.TableName, Properties.DataType, Properties.VocabURL
FROM Mappings LEFT JOIN Properties ON Mappings.ObservedProperty = Properties.Property COLLATE NOCASE
WHERE (((Mappings.Dataset)="QLDGovernment"));'

df <- doQuery(conn, sql)
head(df)



df <- getAvailableProperties(DataSet = 'QLDGovernment', StandardTypes = 'ALL', PropertyGroup = 'TOTAL_ELEMENTS', verbose=T)
df <- getAvailableProperties(DataSet = 'QLDGovernment', StandardTypes = 'ALL', PropertyGroup = 'MOTtles', verbose=T)
df <- getAvailableProperties(DataSet = 'QLDGovernment', StandardTypes = 'STANDARD', PropertyGroup = 'MOTtles', verbose=T)
df <- getAvailableProperties(DataSet = 'QLDGovernment', StandardTypes = 'NONSTANDARD', PropertyGroup = 'MOTtles', verbose=T)
df <- getAvailableProperties( StandardTypes = 'NONSTANDARD', PropertyGroup = 'MOTtles', verbose=T)
df <- getAvailableProperties(DataSet = 'QLDGovernment', PropertyGroup = 'MOTtles', verbose=T)
df <- getAvailableProperties(DataSet = 'QLDGovernment', verbose=T)
df <- getAvailableProperties(DataSet = 'QLDGovernment', verbose=F)
df




