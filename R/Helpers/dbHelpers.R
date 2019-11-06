library(DBI)
library(RSQLite)
library(raster)
library(sf)


machineName <- as.character(Sys.info()['nodename'])
#if(!asPkg){
  if(machineName=='soils-discovery'){
    dbPathSoilsFed <- '/srv/plumber/TERNLandscapes/SoilDataFederatoR/DB/soilsFederator.sqlite'
  }else{
    dbPathSoilsFed <- system.file("extdata", "soilsFederator.sqlite", package = "SoilDataFederatoR")
  }
#}




areasOverlap <- function(provider, bBox){

  sql <- paste0("Select * from Providers where OrgName = '", provider, "'")
  prov = doQueryFromFed(sql)

  if(nrow(prov) > 0){
    pext <- extent( prov$MinX[1], prov$MaxX[1], prov$MinY[1], prov$MaxY[1])
    ppoly <- makeBoundingBoxPolygon(pext)
    upoly <- makeBoundingBoxPolygon(bBox)
    res <- sf::st_intersects(ppoly, upoly, sparse=F)[1,1]
    return(res)
  }
  return(NULL)
}

makeBoundingBoxPolygon <- function(ext){

  m <- matrix(nrow = 5, ncol = 2)
  m[1,1] <- ext@xmin; m[1,2]<- ext@ymin;
  m[2,1] <- ext@xmin; m[2,2]<- ext@ymax;
  m[3,1] <- ext@xmax; m[3,2]<- ext@ymax;
  m[4,1] <- ext@xmax; m[4,2]<- ext@ymin;
  m[5,1] <- ext@xmin; m[5,2]<- ext@ymin;

  pts = list(m)
  p <- st_polygon(pts)

  return(p)
}

# getEndPointURL <- function(DataStoreName){
#
#   DataStoreName <- 'TERNSoilDB'
#     sql <- paste0("Select * from DataStores where Name = '", DataStoreName, "' COLLATE NOCASE")
#     store = doQueryFromFed(sql)
#     url <- store$Location
#     return(url)
# }

getUnits <- function(propertyType, property){
  if(str_to_lower(propertyType) == str_to_lower(PropertyTypes$LaboratoryMeasurement)){
    sql <- paste0("Select * from LabMethods where LABM_CODE = '", property, "' COLLATE NOCASE")
    method = doQueryFromFed(sql)
    units <- method$LABM_UNITS
  }else{
    units = 'NA'
  }
}


getTableNameFromMorphologicalObserveredProperty <- function(observedProperty){

  sql <- paste0("Select * from Properties where Property = '", observedProperty, "' COLLATE NOCASE")
  rec = doQueryFromFed(sql)
  if(nrow(rec) > 0){
    return(rec$PropertyGroup)
  }else{
    return(NULL)
  }
}

getNativeProperties <- function(OrgName, mappings, observedProperty, observedPropertyGroup){

    if(!is.null(observedPropertyGroup)){

      sql <- paste0("Select * from Properties where PropertyGroup = '", observedPropertyGroup, "' COLLATE NOCASE")
      methods = doQueryFromFed(sql)
      nativeProps <- getLGMethod(methods, mappings)
    }else{

      bits <- str_split(observedProperty, ';')
      recs <- bits[[1]]

      meths <- character()
      for (i in 1:nrow(mappings)) {
        if(str_to_upper(mappings$ObservedProperty[i]) %in% str_to_upper(recs) ){
          nativeProp <- mappings[i,]
          meths <- c(meths, nativeProp$OrigPropertyCode)

        }
      }
      nativeProps <- unique(meths)
    }

  return(nativeProps)
}


AuthenticateAPIKey <- function(usr=NULL, key=NULL){

  if(usr=='Demo'){return("OK")}

  sql <- paste0("SELECT * FROM AuthUsers WHERE usrID = '", usr, "'")
  idRec <- doQueryFromFed(sql)
  #print(sql)

  if(nrow(idRec) != 1){return('Error : You need to register to obtain an API key. To register go to https://shiny.esoil.io/SoilDataFederator/Register/')}
  if(idRec$Pwd != key){return(paste0('Error : Incorrect API key supplied. Have a look at the help pages at or otherwise contact ', administrator, ' for assistance.'))}

  return("OK")
}


# getProviders <- function( usr=NULL, key=NULL){
#
#
#   if(is.null(usr) | is.null(key)) {
#
#     sql <- paste0("Select * from Providers WHERE Active = 1 and not Restricted")
#     orgs = doQueryFromFed(sql)
#     return(orgs)
#     }
#
#     sql <- paste0("SELECT * FROM AuthUsers WHERE usrID = '", usr, "'")
#     idRec <- doQueryFromFed(sql)
#
#     cusr <- as.character(idRec$usrID[1])
#     cpwd <- as.character(idRec$Pwd[1])
#     cgrp <- as.character(idRec$GroupName[1])
#
#     sql <- paste0("SELECT * FROM AuthAccess WHERE GroupName = '", cgrp, "'")
#     accessRecs <- doQueryFromFed(sql)
#
#     accessList <- accessRecs$access
#
#     if(key == cpwd){
#
#       if(cgrp == 'Public'){
#         sql <- paste0("Select * from Providers WHERE Active = 1 and not Restricted")
#         orgs = doQueryFromFed(sql)
#
#         return(orgs)
#       }else if(cgrp == 'Admin'){
#         sql <- paste0("Select * from Providers")
#         orgs = doQueryFromFed(sql)
#         return(orgs)
#       }
#       else {
#         sql <- paste0("Select * from Providers
#         WHERE Active = 1 and ( not Restricted or ( Restricted and OrgName IN ( SELECT access FROM AuthAccess WHERE GroupName = '", cgrp, "')))")
#         orgs = doQueryFromFed(sql)
#
#         return(orgs)
#       }
#       stop('Login failed')
#     }
#     else{
#       stop('Incorrect user name or API Key')
#     }
#     stop('Login failed')
#
# }

getDataSets <- function( usr=NULL, key=NULL){


  if(is.null(usr) | is.null(key)) {

    sql <- paste0("Select * from DataSets WHERE Active = 1 and not Restricted")
    orgs = doQueryFromFed(sql)
    return(orgs)
  }

  sql <- paste0("SELECT * FROM AuthUsers WHERE usrID = '", usr, "'")
  idRec <- doQueryFromFed(sql)

  cusr <- as.character(idRec$usrID[1])
  cpwd <- as.character(idRec$Pwd[1])
  cgrp <- as.character(idRec$GroupName[1])

  sql <- paste0("SELECT * FROM AuthAccess WHERE GroupName = '", cgrp, "'")
  accessRecs <- doQueryFromFed(sql)

  accessList <- accessRecs$access

  if(key == cpwd){

    if(cgrp == 'Public'){
      sql <- paste0("Select * from DataSets WHERE Active = 1 and not Restricted")
      orgs = doQueryFromFed(sql)

      return(orgs)
    }else if(cgrp == 'Admin'){
      sql <- paste0("Select * from DataSets")
      orgs = doQueryFromFed(sql)
      return(orgs)
    }
    else {
      sql <- paste0("Select * from DataSets
        WHERE Active = 1 and ( not Restricted or ( Restricted and DataSet IN ( SELECT access FROM AuthAccess WHERE GroupName = '", cgrp, "')))")
      orgs = doQueryFromFed(sql)

      return(orgs)
    }
    stop('Login failed')
  }
  else{
    stop('Incorrect user name or API Key')
  }
  stop('Login failed')

}


getDataStore<- function(Dataset){
  sql <- paste0("Select DataStore from DataSets where DataSet = '", Dataset, "'")
  ds = doQueryFromFed(sql)

  return(as.character(ds))

}


getOrgName<- function(Dataset){

  sql <- paste0("Select OrgName from DataSets where DataSet = '", Dataset, "'")
  org = doQueryFromFed(sql)

  return(as.character(org))

}


getObservedProperties <- function(verbose=F){
  sql <- paste0("Select * from LabMethods")
  methods = doQueryFromFed(sql)

  if(verbose){
    return(methods)
  }else{
    return(methods$LABM_CODE)
  }

}

getProperties <- function(PropertyGroup=NULL, verbose=F){

  if(is.null(PropertyGroup)){
    sql <- paste0("Select * from Properties")
  }else{
    sql <- paste0("Select * from Properties where PropertyGroup = '", PropertyGroup, "' COLLATE NOCASE")
  }
  props = doQueryFromFed(sql)
  if(verbose){
    return(props)
  }else{
    return(props$Property)
  }

}

getPropertyGroups <- function( verbose=F){
  sql <- paste0("Select distinct PropertyGroup, PropertyType from Properties")
  props = doQueryFromFed(sql)
  return(props)
}

getPropertiesList <- function( ObserverdProperties=NULL, observedPropertyGroup=NULL){

  if(!is.null(observedPropertyGroup)){
    sql <- paste0("select * from Properties where PropertyGroup = '", observedPropertyGroup, "'")
    props <- doQueryFromFed(sql)
    ps <- na.omit(props$Property)
    # ps <- na.omit(Properties[str_to_upper(Properties$PropertyGroup)==str_to_upper(observedPropertyGroup), ]$Property )
  }else{
    bits <- str_split(ObserverdProperties, ';')
    ps <- bits[[1]]
  }
  return(ps)
}





sqlFromFile <- function(file){
  require(stringr)
  sql <- readLines(file)
  sql <- unlist(str_split(paste(sql,collapse=" "),";"))
  sql <- sql[grep("^ *$", sql, invert=T)]
  sql
}


dbSendQueries <- function(con,sql){
  dummyfunction <- function(sql,con){
    dbSendQuery(con,sql)
  }
  lapply(sql, dummyfunction, con)
}

doQuery <- function(conn, sql){

  q1 <-str_replace_all(sql, 'dbo_', '')
  #print(q1)
  q2 <-str_replace_all(sql, '"', "\'")
  qry <- dbSendQuery(conn, q1)
  res <- dbFetch(qry)
  dbClearResult(qry)
  return(res)
}


doQueryFromFed <- function(sql){
#print(dbPathSoilsFed)
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbPathSoilsFed)
  qry <- dbSendQuery(conn, sql)
  res <- dbFetch(qry)
  dbClearResult(qry)
  dbDisconnect(conn)
  return(res)
}


getPropertyType <- function(propCode){
  sql <- paste0("Select * from Properties where Property = '", propCode, "' COLLATE NOCASE")
  r <- doQueryFromFed(sql)
  return(r$PropertyType)
}

getPropertiesInGroup <- function(PropertyGroup){
  #PropertyGroup = 'PSA'
  sql <- paste0("Select * from LabMethods where LABP_CODE = '", PropertyGroup, "' COLLATE NOCASE")
  r <- doQueryFromFed(sql)
  return(r$PropertyType)
}


pointsInAust <- function(df, lat_fld, lon_fld){

  outdf <- df[df[lon_fld] > 112.9211 &  df[lon_fld] < 153.6386 &  df[lat_fld] > -43.64309 &  df[lat_fld] < -9.229727,  ]
  return(outdf)

}



NSSCTables <- c("AGENCIES"
                ,"ARCHIVE_SAMPLES"
                ,"COARSE_FRAGS"
                ,"CODES"
                ,"COLOURS"
                ,"CourseFragCodes"
                ,"CRACKS"
                ,"CUTANS"
                ,"DISTURBANCES"
                ,"ELEM_GEOMORPHS"
                ,"EXOTICS"
                ,"FABRICS"
                ,"HORIZONS"
                ,"LAB_METHOD_TYPES"
                ,"LAB_METHODS"
                ,"LAB_PROPERTIES"
                ,"LAB_RESULTS"
                ,"LabMethodQA"
                ,"LAND_CAPABILITIES"
                ,"LAND_USES"
                ,"MICRORELIEFS"
                ,"MOTTLES"
                ,"NatSoil_Lab_Method_GroupQA"
                ,"NatSoil_Lab_Method_QA"
                ,"OBS_MNG_PRACS"
                ,"OBSERVATIONS"
                ,"OFFICERS"
                ,"PANS"
                ,"PATT_GEOMORPHS"
                ,"PHS"
                ,"PORES"
                ,"PROJECTS"
                ,"ROCK_OUTCROPS"
                ,"ROOTS"
                ,"SAMPLES"
                ,"SEGREGATIONS"
                ,"SITE_MNG_PRACS"
                ,"SITES"
                ,"STATES"
                ,"STRENGTHS"
                ,"STRUCTURES"
                ,"SUB_MINERAL_COMPS"
                ,"SURF_COARSE_FRAGS"
                ,"SURF_CONDITIONS"
                ,"sysdiagrams"
                ,"VEG_SPECIES"
                ,"VEG_STRATA"
)




