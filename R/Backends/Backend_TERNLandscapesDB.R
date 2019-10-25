library(stringr)
library(RSQLite)
library(DBI)

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  projectRoot <<- '/srv/plumber/TERNLandscapes/SoilDataFederatoRDatabase'
}else{
  projectRoot <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/TERNLandscapes/APIs/SoilDataFederatoRDatabase'
}

Hosted_dbPath <- paste0(projectRoot, '/DB/SoilDataFederatorDatabase.db3')

doHostedQuery <- function(sql){
  conn <- DBI::dbConnect(RSQLite::SQLite(), Hosted_dbPath)
  qry <- dbSendQuery(conn, sql)
  res <- dbFetch(qry)
  dbClearResult(qry)
  dbDisconnect(conn)
  return(res)
}

# getData_NLWRA <- function(DataSet=NULL, observedProperty=NULL, observedPropertyGroup=NULL){
#   return(hDB_getSoilData(DataSet=DataSet, ObserverdProperties=observedProperty, observedPropertyGroup=observedPropertyGroup))
# }
# getLocationData_NLWRA <- function(DataSet){
#   return(hDB_getAllLocations(DataSet=DataSet))
# }

# getData_GeoScienceAustralia <- function(provider=NULL, observedProperty=NULL, observedPropertyGroup=NULL){
#   return(hDB_getSoilData(DataSet=DataSet, ObserverdProperties=observedProperty, observedPropertyGroup=observedPropertyGroup))
# }
# getLocationData_GeoScienceAustralia <- function(DataSet){
#   return(hDB_getAllLocations(DataSet=DataSet))
#}

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


# hDB_getDataSets <- function(){
#   sql <- 'Select DataSet from DataSets'
#   p <- doHostedQuery(sql)
#   return(p)
#
# }

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

getData_TERNLandscapesDB <- function(DataSet, DataStore, ObserverdProperties=NULL, observedPropertyGroup=NULL){

  print(paste0("DataSet = ", DataSet))

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
    print(sql)
    fdf <- doHostedQuery(sql)
    oOutDF <- generateResponseDF(OrgName, DataSet, fdf$Observation_ID, fdf$SampleID ,fdf$Date , fdf$Longitude, fdf$Latitude ,
                                 fdf$UpperDepth , fdf$LowerDepth ,propertyType, ps[i], fdf$Value , fdf$Units)
    lodfs[[i]] <- oOutDF
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  return(outDF)
}

getLocationData_TERNLandscapesDB <- function(DataSet){

    OrgName <- getOrgName(DataSet)
    sql <- paste0("select DISTINCT Provider, Dataset, Observation_ID, Longitude, Latitude, Date from ObservedProperties where DataSet = '", DataSet, "' order by Dataset, Observation_ID")

    fdf <- doHostedQuery(sql)
    oOutDF <- generateResponseAllLocs(OrgName, DataSet, fdf$Observation_ID, fdf$Longitude, fdf$Latitude, fdf$Date )

  return(oOutDF)

}

