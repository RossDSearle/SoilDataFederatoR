library(stringr)

projectRoot <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR'

machineName <- as.character(Sys.info()['nodename'])

#* @apiTitle Soil Federator Web API
#* @apiDescription These services allow <b>unified</b> and <b>standardised</b> access to a range of disparate soil database systems.<br><br> More detail about the Soils Federation Service can be found <a href='http://esoil.io/FederatedServices/FedeartedSesnsorsHelpPage.html' > here </a>

#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req){



  logentry <- paste0(as.character(Sys.time()), ",",
       machineName, ",",
       req$REQUEST_METHOD, req$PATH_INFO, ",",
       str_replace_all( req$HTTP_USER_AGENT, ",", ""), ",",
       req$QUERY_STRING, ",",
       req$REMOTE_ADDR
      )

  dt <- format(Sys.time(), "%d-%m-%Y")

  logDir <- paste0(projectRoot, "/WebAPI/Logs")
  if(!dir.exists(logDir)){
     dir.create(logDir , recursive = T)
    }

  logfile <- paste0(projectRoot, "/WebAPI/Logs/SoilFederationAPI_logs_", dt, ".csv")
  if(file.exists(logfile)){
    cat(logentry, '\n', file=logfile, append=T)
  }else{
    hdr <- paste0('System_time,Server,Request_method,HTTP_user_agent,QUERY_STRING,REMOTE_ADDR\n')
    cat(hdr, file=logfile, append=F)
    cat(logentry, '\n', file=logfile, append=T)
  }

  plumber::forward()
}




#* Returns information about the locations of sensors

#* @param pwd (Optional) Password for logging into the system - if not supplied defaults to 'Public'
#* @param usr (Optional) User name for logging into the system - if not supplied defaults to 'Public'
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param numToReturn (Optional) The number of closest sensor locations to be returned. Default = 10
#* @param radius_km (Optional) The radius around the supplied location in which to search.  Default = NULL ie  returns all records
#* @param latitude (Optional) If a location is entered  the sensor locations closest to this location will be returned. If not specified all sensor locations will be returned
#* @param longitude (Optional) If a location is entered  the sensor locations closest to this location will be returned. If not specified all sensor locations will be returned
#* @param bbox (Optional) The  rectangular bounding box of the area to be queried in  the form bl;ll;tl;tr ie bottom latitude, left longitude, top latitude, right longitude
#* @param provider (Optional) fotmat of the response to return. Either json, csv, or xml. Default = json
#* @param observedPropertyGroup (Optional) Filter on a specific type of sensor. Currently supported sensor types are 'Soil-Moisture' and 'Rainfall' - if not supplied all records are returned
#* @param observedProperty (Optional) Filter on a specific type of sensor. Currently supported sensor types are 'Soil-Moisture' and 'Rainfall' - if not supplied all records are returned


#* @get /SoilDataAPI/SoilData
apiGetSoilData<- function(res, usr='Public', pwd='Public', providers=NULL, observedProperty=NULL, observedPropertyGroup=NULL, format='json'){

tryCatch({

        DF <-getSoilData(providers, observedProperty, observedPropertyGroup)

        label <- 'SoilProperty'
        resp <- cerealize(DF, label, format, res)
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}




#* Returns information about the locations of sensors

#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json


#* @get /SoilDataAPI/Providers
apiGetProviders <- function( res, format='json'){

  tryCatch({

    DF <-getProviders(activeOnly = T)
    label <- 'DataProvider'
    resp <- cerealize(DF, label, format, res)
    return(resp)

  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}



  cerealize <- function(DF, label, format, res){
    if(format == 'xml'){

      res$setHeader("Content-Type", "application/xml; charset=utf-8")
      xdoc=xml_new_root(paste0(label, 's'))
      vars_xml <- lapply(purrr::transpose(DF),
                         function(x) {
                           #as_xml_document(list(DataProvider = lapply(x, as.list)))
                           as_xml_document(setNames(list(lapply(x, as.list)), label))
                         })

      for(trial in vars_xml) xml_add_child(xdoc, trial)
      res$body <- as(xdoc, "character")
      return(res)

    }else if(format == 'csv'){
      res$setHeader("content-disposition", "attachment; filename=FederatedSoilData.csv");
      res$setHeader("Content-Type", "application/csv; charset=utf-8")
      res$body <- writecsv(DF)
      return(res)

    }else if(format == 'html'){
      res$setHeader("Content-Type", "text/html ; charset=utf-8")
      res$body <- htmlTable(DF, align = "l", align.header = "l", caption = "Data Providers")
      return(res)

    }else{
      return(DF)
    }


}



#* Returns a listing of the available properties


#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param verbose (Optional) return just the property codes or the full descriptions. Default = True
#* @param PropertyGroup (Optional) return just the properties for a given PropertGroup. Default = All


#* @get /SoilDataAPI/Properties
apiGetPropeties <- function( res,PropertyGroup=NULL, verbose=T, format='json'){

  tryCatch({

    DF <-getProperties(PropertyGroup, verbose)

    if(format == 'xml'){

      res$setHeader("Content-Type", "application/xml; charset=utf-8")
      xdoc=xml_new_root('Properties')
      vars_xml <- lapply(purrr::transpose(DF),
                         function(x) {
                           as_xml_document(list(property = lapply(x, as.list)))
                         })

      for(trial in vars_xml) xml_add_child(xdoc, trial)
      res$body <- as(xdoc, "character")
      return(res)

    }else if(format == 'csv'){
      res$setHeader("content-disposition", "attachment; filename=soilProperties.csv");
      res$setHeader("Content-Type", "application/csv; charset=utf-8")
      res$body <- writecsv(DF)
      return(res)

    }else if(format == 'html'){
      res$setHeader("Content-Type", "text/html ; charset=utf-8")
      res$body <- htmlTable(DF, align = "l", align.header = "l", caption = "Soil Properties")
      return(res)

    }else{
      return(DF)
    }

  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}


#* Returns a listing of the available Property Groups

#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json

#* @get /SoilDataAPI/PropertyGroups
apiGetPropetyGroups <- function( res, verbose=T, format='json'){

  tryCatch({

    DF <-getPropertyGroups(verbose)

    if(format == 'xml'){

      res$setHeader("Content-Type", "application/xml; charset=utf-8")
      xdoc=xml_new_root('PropertyGroups')
      vars_xml <- lapply(purrr::transpose(DF),
                         function(x) {
                           as_xml_document(list(PropertyGroup = lapply(x, as.list)))
                         })

      for(trial in vars_xml) xml_add_child(xdoc, trial)
      res$body <- as(xdoc, "character")
      return(res)

    }else if(format == 'csv'){
      res$setHeader("content-disposition", "attachment; filename=PropertyGroups.csv");
      res$setHeader("Content-Type", "application/csv; charset=utf-8")
      res$body <- writecsv(DF)
      return(res)

    }else if(format == 'html'){
      res$setHeader("Content-Type", "text/html ; charset=utf-8")
      res$body <- htmlTable(DF, align = "l", align.header = "l", caption = "PropertyGroups")
      return(res)

    }else{
      return(DF)
    }

  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}






#######     Some utilities    ###########################


writecsv <- function(DF){

  s <- paste(colnames(DF), collapse = ", ")
  s <- paste(s, '\r\n')
print("here")
  for(i in 1:nrow(DF)){

    for(j in 1:ncol(DF)){
      if(j==1){
        s <- paste0(s, as.character(DF[i,j]))
      }else{
        s <- paste0(s, ',', as.character(DF[i,j]))
      }
    }
    s <- paste(s, '\r\n')
  }
  return(s)
}



writeNestedCSV <- function(nDF){

  s <- paste0('SiteID = ', nDF$SiteID[1], '\n')
  s <- paste0(s, 'SiteName = ', nDF$SiteName[1], '\n')
  s <- paste0(s, 'Provider = ', nDF$Provider[1], '\n')
  s <- paste0(s, 'Backend = ', nDF$Backend[1], '\n')
  s <- paste0(s, 'Access = ', nDF$Access[1], '\n')
  s <- paste0(s, 'Longitude = ', nDF$Longitude[1], '\n')
  s <- paste0(s, 'Latitude = ', nDF$Latitude[1], '\n')

  s <- paste0(s, 'RequestStartDate = ', nDF$RequestStartDate, '\n')
  s <- paste0(s, 'RequestEndDate = ', nDF$RequestEndDate, '\n')
  s <- paste0(s, 'AggregationPeriod = ', nDF$AggregationPeriod, '\n')

  s <- paste0(s, 'DataType = ', nDF$DataType[1], '\n')
  s <- paste0(s, 'Units = ', nDF$Units[1], '\n')
  s <- paste0(s, 'Calibrated = ', nDF$Calibrated[1], '\n')

   colnms <- paste(nDF$DataType[1],'_', nDF$UpperDepthCm,'_', nDF$LowerDepthCm, collapse = ', ', sep='')
   s <- paste0(s,'DateTime,', colnms,  '\n')
   numrows <- nrow(nDF$DataStream[[1]])
   numcols <- length(nDF$DataStream)
   for(i in 1:numrows){
     s <- paste0(s, format(nDF$DataStream[[1]][i,1],"%Y-%m-%dT%H:%M:%S"))
     for(j in 1:numcols){
         s <- paste0(s, ', ', as.character(nDF$DataStream[[j]][i,2]))
     }
     s <- paste(s, '\n')
   }

  return(s)
}

#writeNestedHTML(nDF)

writeNestedHTML <- function(nDF){

  flds <- c( 'SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Longitude', 'Latitude', 'RequestStartDate', 'RequestEndDate', 'AggregationPeriod', 'DataType', 'Units', 'Calibrated')

  htmln = newXMLNode("HTML")
  bodyn <- newXMLNode("BODY", parent = htmln)

  for(i in 1:length(flds)){

    fname <-flds[i]
    n0 <- newXMLNode("p")
    n1 <- newXMLNode("b", newXMLTextNode(paste0(fname, " : ")))
    n2 <- addChildren(n0, kids = c( n1, nDF[fname][[1]][1]))
    addChildren(bodyn, kids=c(n2))

  }

  tablen <- newXMLNode("TABLE", parent = bodyn, attrs = c(border = 1))

  tr <- newXMLNode("tr")
  n1 <- newXMLNode("th", newXMLTextNode(paste0( "DateTime")),parent = tr)
  for (i in 1:nrow(nDF)) {

    n1 <- newXMLNode("th", newXMLTextNode(paste0(nDF$DataType[1],'_', nDF$UpperDepthCm[i],'_', nDF$LowerDepthCm[i])),parent = tr)

  }
  addChildren(tablen,c(tr))


  numrows <- nrow(nDF$DataStream[[1]])
  numcols <- length(nDF$DataStream)
  for(i in 1:numrows){
    dt <- format(nDF$DataStream[[1]][i,1],"%Y-%m-%dT%H:%M:%S")

    tr <- newXMLNode("tr")
    n1 <- newXMLNode("td", newXMLTextNode(paste0( dt)),parent = tr)

    for(j in 1:numcols){
      v <- as.character(nDF$DataStream[[j]][i,2])
      n1 <- newXMLNode("td", newXMLTextNode(paste0( v )),parent = tr)
    }


    addChildren(tablen,c(tr))
  }

  return(htmln)
}




writeNestedXML <- function(nDF){

  topN = newXMLNode("DataStreams")

  for (i in 1:nrow(nDF)) {

    sensor = newXMLNode("Sensor")

    newXMLNode("SensorID", newXMLTextNode(nDF$SiteID[i]), parent = sensor)
    newXMLNode("SiteName", newXMLTextNode(nDF$SiteName[i]), parent = sensor)
    newXMLNode("Provider", newXMLTextNode(nDF$Provider[i]), parent = sensor)
    newXMLNode("Backend", newXMLTextNode(nDF$Backend[i]), parent = sensor)
    newXMLNode("Access", newXMLTextNode(nDF$Access[i]), parent = sensor)
    newXMLNode("Longitude", newXMLTextNode(nDF$Longitude[i]), parent = sensor)
    newXMLNode("Latitude", newXMLTextNode(nDF$Latitude[i]), parent = sensor)

    newXMLNode("SensorID", newXMLTextNode(nDF$SensorID[i]), parent = sensor)
    newXMLNode("SensorName", newXMLTextNode(nDF$SensorName[i]), parent = sensor)

    newXMLNode("UpperDepthCm", newXMLTextNode(nDF$UpperDepthCm [i]), parent = sensor)
    newXMLNode("LowerDepthCm", newXMLTextNode(nDF$LowerDepthCm[i]), parent = sensor)

    newXMLNode("RequestStartDate", newXMLTextNode(nDF$RequestStartDate [i]), parent = sensor)
    newXMLNode("RequestEndDate", newXMLTextNode(nDF$RequestEndDate[i]), parent = sensor)
    newXMLNode("AggregationPeriod", newXMLTextNode(nDF$AggregationPeriod[i]), parent = sensor)

    newXMLNode("DataType", newXMLTextNode(nDF$DataType[i]), parent = sensor)
    newXMLNode("Units", newXMLTextNode(nDF$Units[i]), parent = sensor)
    newXMLNode("Calibrated", newXMLTextNode(nDF$Calibrated[i]), parent = sensor)

    #addChildren(sensor, kids = kidss)

    ds = newXMLNode("DataStream")
    df <- nDF$DataStream[[i]]

    for(j in 1:nrow(df)){
      vals = newXMLNode("Record")
      vkids <- c(newXMLNode("DateTime", newXMLTextNode(format( df$t[j],"%Y-%m-%dT%H:%M:%S")  )),
                 newXMLNode("Value", newXMLTextNode(df$v[j])))
     suppressWarnings(addChildren(vals, kids = vkids))
     suppressWarnings(addChildren(ds, kids = c(vals)))
    }

    addChildren(sensor, c(ds))
    addChildren(topN, kids = c(sensor))
  }
  return(topN)
}




makeDFList <- function(DF) {

  l<- vector("list", ncol(DF)-1)
  for(i in 1:(ncol(DF)-1)){
    nd <-  cbind(DF[1], DF[i+1])
    names(nd) <- c('T', 'V')
    l[[i]] <-nd
  }
  names(l) <- names(DF)[-1]
  l

}
