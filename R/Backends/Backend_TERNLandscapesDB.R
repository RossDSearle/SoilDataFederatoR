library(stringr)
library(RSQLite)
library(DBI)

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  TernDBPath <- '/OSM/CBR/LW_SOILDATAREPO/work/TERNLandscapes/SoilsFederator/HostedDBs'
}else{
  TernDBPath <<- 'C:/Projects/TernLandscapes/Site Data/HostedDBs'
}

Hosted_dbPath <- paste0(TernDBPath, '/SoilDataFederatorDatabase.db3')

doHostedQuery <- function(sql){
  conn <- DBI::dbConnect(RSQLite::SQLite(), Hosted_dbPath)
  qry <- dbSendQuery(conn, sql)
  res <- dbFetch(qry)
  dbClearResult(qry)
  dbDisconnect(conn)
  return(res)
}


hDB_getPropertiesList <- function( ObserverdProperties=NULL, observedPropertyGroup=NULL){

  if(!is.null(observedPropertyGroup)){
    sql <- paste0("select * from Properties where PropertyGroup = '", observedPropertyGroup, "'")
    props <- doHostedQuery(sql)
    ps <- na.omit(props$Property)
  }else{
    bits <- str_split(ObserverdProperties, ';')
    ps <- bits[[1]]
  }
  return(ps)
}

hDB_getDatasets <- function(DataSet=NULL, verbose=F){

  if(verbose){
    fields <- ' * '
  }else{
    fields <- ' Provider, Dataset '
  }

  if(is.null(Provider)){
    sql <- paste0('Select', fields, 'from DataSets')
    ds <- doHostedQuery(sql)
    return(ds)
  }else{
    sql <- paste0('Select', fields,  'from DataSets where Provider = "', Provider, '"GROUP BY Provider, Dataset')
    ds <- doHostedQuery(sql)
    return(ds)
  }
}

getData_TERNLandscapesDB <- function(DataSet, ObserverdProperties=NULL, observedPropertyGroup=NULL){

  OrgName <- getOrgName(DataSet)
  ps <- hDB_getPropertiesList(ObserverdProperties, observedPropertyGroup)
  propertyType <- getPropertyType(ps)
  lodfs <- vector("list", length(ps))
  if(is.null(DataSet)){
    provsql <- ''
  }else{
    provsql <- paste0(" and DataSet = '", DataSet, "'")
  }

  for (i in 1:length(ps)) {
    prop <- ps[i]
    sql <- paste0("select * from ObservedProperties where ObservedProperty = '", prop, "'", provsql, ' order by Dataset, Observation_ID, UpperDepth, SampleID')
    fdf <- doHostedQuery(sql)

    if(nrow(fdf) >0){
      bits<-str_split(fdf$Date, '/')
      y <- sprintf("%04d", as.numeric(sapply(bits, function (x) x[3])))
      m <- sprintf("%02d", as.numeric(sapply(bits, function (x) x[2])))
      d <-sprintf("%02d", as.numeric(sapply(bits, function (x) x[1])))
      fdf$DateOut <- paste0(d, '-', m, '-', y)
    oOutDF <- generateResponseDF(DataSet, fdf$Observation_ID, fdf$SampleID ,fdf$DateOut , fdf$Longitude, fdf$Latitude ,
                                 fdf$UpperDepth , fdf$LowerDepth ,propertyType, ps[i], fdf$Value , fdf$Units)
    lodfs[[i]] <- oOutDF
    }else{
      lodfs[[i]] <- blankResponseDF()
    }
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  return(outDF)
}

getLocationData_TERNLandscapesDB <- function(DataSet){

    OrgName <- getOrgName(DataSet)
    sql <- paste0("select DISTINCT Provider, Dataset, Observation_ID, Longitude, Latitude, Date from ObservedProperties where DataSet = '", DataSet, "' order by Dataset, Observation_ID")
    fdf <- doHostedQuery(sql)
    bits<-str_split(fdf$Date, '/')
    y <- sprintf("%04d", as.numeric(sapply(bits, function (x) x[3])))
    m <- sprintf("%02d", as.numeric(sapply(bits, function (x) x[2])))
    d <-sprintf("%02d", as.numeric(sapply(bits, function (x) x[1])))
    fdf$DateOut <- paste0(d, '-', m, '-', y)
    oOutDF <- generateResponseAllLocs(DataSet, fdf$Observation_ID, fdf$Longitude, fdf$Latitude, fdf$DateOut )

  return(oOutDF)

}

