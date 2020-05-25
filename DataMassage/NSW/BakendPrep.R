library(RSQLite)
library(DBI)
library(stringr)
library(httr)



doQuery <- function(con, sql){
  res <- dbSendQuery(con, sql)
  rows <- dbFetch(res)
  dbClearResult(res)
  return(rows)
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

write.csv(md, 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/NSW/MorphCodes.csv')



con <- dbConnect(RSQLite::SQLite(), 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/R/DB/soilsFederator.sqlite', flags = SQLITE_RO)
sql <- "select * from mappings where Organisation = 'NSWGovernment'"
r <- doQuery(con, sql)
write.csv(r, 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/NSW/NatsoilMorphCodes.csv')



df <- getWebData('http://asris-daas02/NSW_Services/api/MorphResults?morphology_attribute=Colourmoistmunsellcolourhuevalue')
df2 <- getWebData('http://asris-daas02/NSW_Services/api/MorphResults?morphology_attribute=Colourmoistmunsellcolourchroma')
df3 <- getWebData('http://asris-daas02/NSW_Services/api/MorphResults?morphology_attribute=Colourmoistmunsellcolourhue')


nrow(df)
nrow(df2)
nrow(df3)
