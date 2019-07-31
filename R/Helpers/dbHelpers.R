library(DBI)
library(RSQLite)


getEndPointURL <- function(DataStoreName){

  DataStoreName <- 'TERNSoilDB'
    sql <- paste0("Select * from DataStores where Name = '", DataStoreName, "' COLLATE NOCASE")
    store = doQueryFromFed(sql)
    url <- store$Location
    return(url)
}

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



getProviders <- function( usr='Public', pwd='Public'){
  #getProviders <- function(activeOnly=T, usr='Public', pwd='Public'){


  if(usr=='Public'){
    #if(activeOnly){
      sql <- paste0("Select * from Providers where Active = 1 and Availability = 'Public'")
    #
    orgs = doQueryFromFed(sql)
    return(orgs)

  }else{

    sql <- paste0("SELECT * FROM AuthUsers WHERE usrID = '", usr, "'")
    idRec <- doQueryFromFed(sql)

    if(nrow(idRec) != 1){stop('Incorrect user name or password - username actually')}

    cusr <- as.character(idRec$usrID[1])
    cpwd <- as.character(idRec$Pwd[1])
    cgrp <- as.character(idRec$GroupName[1])

    sql <- paste0("SELECT * FROM AuthAccess WHERE GroupName = '", cgrp, "'")
    accessRecs <- doQueryFromFed(sql)


    accessList <- accessRecs$access

    if(pwd == cpwd){

      if(usr == 'Admin'){
        sql <- paste0("Select * from Providers WHERE Active = 1")
        orgs = doQueryFromFed(sql)
        return(orgs)

      }else if('All' %in% accessList){

        #sql <- "SELECT * FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID"
        #orgs = doQueryFromFed(sql)

        return(NULL)
      }
      else {

        sql <- paste0("Select * from Providers
        WHERE Active = 1 and ( Availability = 'Public' or ( Availability == 'Private' and OrgName IN ( SELECT access FROM AuthAccess WHERE GroupName = '", cgrp, "')))")
        orgs = doQueryFromFed(sql)

        return(orgs)
      }
      stop('Login failed')
    }
    else{
      stop('Incorrect user name or password')
    }
    stop('Login failed')
  }
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
  sql <- paste0("Select distinct PropertyGroup, PropertyType from Properties COLLATE NOCASE")
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




blankResponseDF <- function(){

  outDF <- na.omit(data.frame(Organisation=NULL, Dataset=character(), Observation_ID=character(), SampleID=character(), SampleDate=character() ,
                              Longitude=numeric() , Latitude= numeric(),
                              UpperDepth=numeric() , LowerDepth=numeric() , PropertyType=character(), ObservedProperty=character(), Value=numeric(),
                              Units= character(), Quality=integer()))
}

generateResponseDF <- function(provider, dataset, observation_ID, sampleID, date, longitude, latitude, upperDepth, lowerDepth, dataType, observedProp, value, units, qualityCode ){

  outDF <- na.omit(data.frame(Provider=provider, Dataset=dataset, Observation_ID=observation_ID, SampleID=sampleID , SampleDate=date ,
                              Longitude=longitude, Latitude=latitude ,
                              UpperDepth=upperDepth, LowerDepth=lowerDepth, PropertyType=dataType, ObservedProperty=observedProp,
                              Value=value , Units=units, Quality=qualityCode))
  oOutDF <- outDF[order(outDF$Observation_ID, outDF$Dataset, outDF$UpperDepth, outDF$SampleID),]
  return(oOutDF)
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




