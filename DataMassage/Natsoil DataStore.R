library(RSQLite)
library(odbc)
library(DBI)
library(stringr)
library(httr)
library(jsonlite)
library(dplyr)

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



conn <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "asris-sql-stage.it.csiro.au\\sql2017",
                      #Database = "NatSoil",
                      Database = "vic_json_services",
                      UID      = 'NEXUS\\sea084',
                      PWD      = 'Chas4066',
                      Trusted_Connection = "True"
)


sql <- 'select * from dbo_VS_locations'
df <- doQuery(conn, sql)
head(df)
unique(df[,c('site_zone', 'site_datum')])
nrow(unique(df[,c('site_zone', 'site_datum')]))
count(df[,c(9,10)])
dts <- dplyr::count_(df, vars = c('site_zone', 'site_datum'))
dts$site_zone

write.csv(df, 'c:/temp/datums.csv')



-36.86866
143.4636
719600
5916850
54

E = 759700
N = 6041200
lat = -35.73885
lon = 143.8716


dfc = data.frame(X = c(E), Y = c(N))
cds <- st_as_sf(dfc, coords = c("X", "Y"), crs = 32754)
projCoords <- st_transform(cds,  crs = 4326)
projCoords
