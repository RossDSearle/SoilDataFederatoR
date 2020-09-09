library(dplyr)
library(lubridate)

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



writeLog <- function(df, usr){

  if(!dir.exists(logDir)){dir.create(logDir)}
  logFile <- paste0(logDir,'/soilDataFederatorUsage.csv')
  if(!file.exists(logFile)){
    cat('DateTime,User,Dataset,Attribute,Count\n', sep = '', file = logFile, append = F)
  }

  sdf <- df %>% group_by(Dataset, ObservedProperty) %>% summarise(n = n())
  odf <- data.frame(DateTime=now(),User=usr,Dataset=sdf$Dataset, Attribute=sdf$ObservedProperty,Count=sdf$n, stringsAsFactors = F)
  write.table(odf, logFile,append = TRUE,sep = ",",col.names = FALSE, row.names = FALSE,  quote = FALSE)
}





