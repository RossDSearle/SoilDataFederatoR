library(DBI)
library(RSQLite)
library(raster)
library(sf)


areasOverlap <- function(DataSet, bBox){


  # bits <- str_split(bBox, ';')
  # print('bob')
  # print(bits)
  # print(str(bits))
  # l <- as.numeric(bits[[1]][1])
  # r <- as.numeric(bits[[1]][2])
  # t <- as.numeric(bits[[1]][4])
  # b <- as.numeric(bits[[1]][3])
  # bboxExt <- extent(l, r, b, t)

  sql <- paste0("Select * from DataSets where DataSet = '", DataSet, "'")
  prov = doQueryFromFed(sql)

  #print(prov)

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

 # print(pts)
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

getNativeAPIurl <- function(dataset){
  sql <- paste0("Select * from Datasets where DataSet = '", dataset, "' COLLATE NOCASE")
  rec = doQueryFromFed(sql)
  return(rec$NativeAPIURL)
}

getNativeProperties <- function(DataSet, observedProperty, observedPropertyGroup){



    if(!is.null(observedPropertyGroup)){

      sql <- paste0("Select * from Properties where PropertyGroup = '", observedPropertyGroup, "' COLLATE NOCASE")
      methods = doQueryFromFed(sql)
      #nativeProps <- getLGMethod(methods, mappings)
      outDF <- data.frame(nativeProp=methods$Property, standardProp=methods$Property, propertyType=methods$PropertyType)
    }else{

      bits <- str_split(observedProperty, ';')
      recs <- bits[[1]]

      # meths <- character()
      # mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))
      # for (i in 1:nrow(mappings)) {
      #   if(str_to_upper(mappings$ObservedProperty[i]) %in% str_to_upper(recs) ){
      #     nativeProp <- mappings[i,]
      #     meths <- c(meths, nativeProp$OrigPropertyCode)
      #
      #   }
      # }
      #nativeProps <- unique(meths)


      outDF <- data.frame(nativeProp=character(), standardProp=character(),propertyType=character(), StandardCode=character())
      for (i in 1:length(recs)) {
        sql <- paste0("Select * from Mappings where Dataset = '", DataSet, "' and ObservedProperty = '", recs[i], "' COLLATE NOCASE")
        codes = doQueryFromFed(sql)
        #nativeProps <- c(nativeProps, codes$OrigPropertyCode)
        rdf <- data.frame(nativeProp=codes$OrigPropertyCode, standardProp=codes$ObservedProperty,propertyType=codes$DataType, StandardCode=codes$StandardCode, stringsAsFactors = F)
        rdf$propertyType[rdf$propertyType=='L'] <- 'LaboratoryMeasurement'
        rdf$propertyType[rdf$propertyType=='M'] <- 'FieldMeasurement'
        outDF <- rbind(outDF, rdf)
      }
    }

  return(outDF)
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
      sql <- paste0("Select * from DataSets WHERE Active = 1")
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


# getObservedProperties <- function(verbose=F){
#   sql <- paste0("Select * from LabMethods")
#   methods = doQueryFromFed(sql)
#
#   if(verbose){
#     return(methods)
#   }else{
#     return(methods$LABM_CODE)
#   }
#
# }


getStandardProperties <- function(PropertyGroup=NULL, verbose=F){

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

getAvailableProperties <- function(DataSet=NULL, StandardTypes = 'ALL', PropertyGroup=NULL, verbose=F){


  if(is.null(DataSet)){stop('Value for "DataSet" needs to be suplied')}
  sql <- "SELECT Mappings.Dataset, Mappings.OrigPropertyCode, Mappings.ObservedProperty, Properties.PropertyType, Properties.PropertyGroup, Properties.Description, Properties.TableName, Properties.DataType, Properties.VocabURL FROM Mappings LEFT JOIN Properties ON Mappings.ObservedProperty = Properties.Property COLLATE NOCASE"

  if (!is.null(DataSet) | !is.null(PropertyGroup)) {sql <- paste0(sql," WHERE ")}

  if(!is.null(PropertyGroup)) {sql <- paste0(sql," Properties.PropertyGroup = '", PropertyGroup, "' COLLATE NOCASE ")}

  if(!is.null(DataSet))
  {
    if(!is.null(PropertyGroup)){sql <- paste0(sql," AND ")}
    sql <- paste0(sql," Mappings.Dataset = '", DataSet, "' COLLATE NOCASE ")
  }



  sql <- paste0(sql, " ;")
  print(sql)
  props = doQueryFromFed(sql)



  if(str_to_upper(StandardTypes) == 'STANDARD')
  {
    idxs <- which(!is.na(props$PropertyType))
    propsOut <- props[idxs,]
  }else if(str_to_upper(StandardTypes) == 'NONSTANDARD'){
    idxs <- which(is.na(props$PropertyType))
    propsOut <- props[idxs,]
  }else{
    propsOut <- props
  }
  print(head((props)))

  if(verbose){
    return(propsOut)
  }else{
    return(propsOut$ObservedProperty)
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
  }else{
    bits <- str_split(ObserverdProperties, ';')
    ps <- bits[[1]]
  }
  return(ps)
}

getDataQualityDescriptions <- function(){
  sql <- paste0("select * from QualCodes")
  quals <- doQueryFromFed(sql)
  return(quals)
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


getPropertyType <- function(PropCode){
   sql <- paste0("Select * from Properties where Property = '", PropCode, "' COLLATE NOCASE")
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




