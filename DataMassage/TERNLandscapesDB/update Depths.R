library(RSQLite)
library(DBI)
library(stringr)
library(httr)

TernDBPath <- 'C:/Projects/TernLandscapes/Site Data/HostedDBs'

Hosted_dbPath <- paste0(TernDBPath, '/SoilDataFederatorDatabase.db3')
conn <- DBI::dbConnect(RSQLite::SQLite(), Hosted_dbPath)

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


copyTable(conn, 'ObservedProperties', 'ObservedProperties_V1')

sql <-"UPDATE ObservedProperties SET UpperDepth = (UpperDepth * 0.01) WHERE UpperDepth > 0;"
sendStatement(conn, sql)

sql <-"UPDATE ObservedProperties SET LowerDepth = (LowerDepth * 0.01) WHERE LowerDepth > 0;"
sendStatement(conn, sql)

sql <-"select * from ObservedProperties WHERE UpperDepth > 0;"
df <- doQuery(conn, sql)
head(df)





