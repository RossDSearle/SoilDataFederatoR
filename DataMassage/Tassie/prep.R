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

tcon <- DBI::dbConnect(odbc::odbc(), Driver   = "SQL Server",
                       Server   = "asris-sql-stage.it.csiro.au\\sql2017",
                       Database = "tasmania_json_services",
                       UID      = 'NEXUS\\sea084',
                       PWD      = 'Chas4066',
                       Trusted_Connection = "True"
)


sql <- 'SELECT soil_site.PROJECT, soil_site.SITE_ID, soil_site.EASTING, soil_site.NORTHING, soil_observation.OBSERVATION_DATE
FROM soil_site INNER JOIN soil_observation ON  soil_site.PROJECT = soil_observation.PROJECT AND soil_site.SITE_ID = soil_observation.SITE_ID'


sql <- 'SELECT        soil_site.PROJECT, soil_site.SITE_ID, soil_site.EASTING, soil_site.NORTHING, soil_horizon.UPPER_DEPTH, soil_horizon.LOWER_DEPTH, soil_horizon.TEXTURE
FROM            soil_site INNER JOIN
soil_observation ON soil_site.PROJECT = soil_observation.PROJECT AND soil_site.SITE_ID = soil_observation.SITE_ID INNER JOIN
soil_horizon ON soil_observation.SITE_ID = soil_horizon.SITE_ID AND soil_observation.PROJECT = soil_horizon.PROJECT AND soil_observation.OBSERVATION_NUMBER = soil_horizon.OBSERVATION_NUMBER'

doQuery(tcon, sql)

getData_TasGov(DataSet='TasGovernment', observedProperty='S_REL_MS_CLASS', observedPropertyGroup=NULL)
getData_TasGov(DataSet='TasGovernment', observedProperty='O_ASC_ORD', observedPropertyGroup=NULL)
getData_TasGov(DataSet='TasGovernment', observedProperty='h_texture', observedPropertyGroup=NULL)
getData_TasGov(DataSet='TasGovernment', observedProperty='C_LITHOLOGY', observedPropertyGroup=NULL)
getData_TasGov(DataSet='TasGovernment', observedProperty='SEG_FORM', observedPropertyGroup=NULL)

getData_TasGov(DataSet='TasGovernment', observedProperty='3A1', observedPropertyGroup=NULL)








