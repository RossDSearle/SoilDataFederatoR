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

dbPath <- 'C:/Projects/TernLandscapes/Site Data/HostedDBs/SASoils.db'

conSA <- dbConnect(RSQLite::SQLite(), dbPath)
list.files('C:/Projects/TernLandscapes/Site Data/SA/soils', full.names = T)

df <- read.csv('C:/Projects/TernLandscapes/Site Data/SA/soils/soils_coarsefrags_20200902030016.csv')
colnames(df) <- str_replace_all(colnames(df), '[.]', '_')
dbWriteTable(conSA, 'coarsefrags', df)

df <-  read.csv('C:/Projects/TernLandscapes/Site Data/SA/soils/soils_horizonfile_20200902030009.csv')
colnames(df) <- str_replace_all(colnames(df), '[.]', '_')
dbWriteTable(conSA, 'horizons', df)


## there is an issue with commas in the texture fireld of this table - had to manually edit osme records to fix
sendStatement(conSA, 'DROP TABLE labresults')
df <- read.csv('C:/Projects/TernLandscapes/Site Data/SA/soils/soils_labresultsfile_20200902030005.csv')
colnames(df) <- str_replace_all(colnames(df), '[.]', '_')
dbWriteTable(conSA, 'labresults', df)

df <- read.csv('C:/Projects/TernLandscapes/Site Data/SA/soils/soils_landusehistoryfile_20200902030004.csv')
colnames(df) <- str_replace_all(colnames(df), '[.]', '_')
dbWriteTable(conSA, 'landusehistory',df)

df <- read.csv('C:/Projects/TernLandscapes/Site Data/SA/soils/soils_mottlefile_20200902030015.csv')
colnames(df) <- str_replace_all(colnames(df), '[.]', '_')
dbWriteTable(conSA, 'mottles', df)

df <- read.csv('C:/Projects/TernLandscapes/Site Data/SA/soils/soils_observationfile_20200902030005.csv')
colnames(df) <- str_replace_all(colnames(df), '[.]', '_')
dbWriteTable(conSA, 'observations', df)

df <- read.csv('C:/Projects/TernLandscapes/Site Data/SA/soils/soils_segregation_20200902030017.csv')
colnames(df) <- str_replace_all(colnames(df), '[.]', '_')
dbWriteTable(conSA, 'segregations', df)

df <- read.csv('C:/Projects/TernLandscapes/Site Data/SA/soils/soils_sitefile_20200902030001.csv')
colnames(df) <- str_replace_all(colnames(df), '[.]', '_')
dbWriteTable(conSA, 'sites', df)

df <- read.csv('C:/Projects/TernLandscapes/Site Data/SA/soils/soils_structurefile_20200902030013.csv')
colnames(df) <- str_replace_all(colnames(df), '[.]', '_')
dbWriteTable(conSA, 'structures', )




