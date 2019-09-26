library(stringr)
library(XML)
library(xml2)
library(htmlTable)

#projectRoot <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR'

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){

  deployDir <-'/srv/plumber/TERNLandscapes/SoilDataFederatoR'
  #server <- 'http://esoil.io'
}else{
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR'
  #server <- '0.0.0.0'
}

source(paste0(deployDir, '/R/Helpers/apiHelpers.R'))
source(paste0(deployDir, '/R/Backends.R'))

#* @apiTitle TERN SoilDataFederatoR Web API
#* @apiDescription These services allow <b>unified</b> and <b>standardised</b> access to a range of disparate soil database systems.<br><br> More detail about the SoilDataFederatoR service can be found <a href='http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/help/index.html' > HERE </a>
#* <h2>API Key Registration</h2>
#* You need to register for an API Key to be able to use the API to access the soil data. You can quickly register <a
#* href="https://shiny.esoil.io/SoilDataFederator/Register/"
#* target="_blank">HERE</a>. Just supply your email address, name and organisation and you are good to go.



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

  logDir <- paste0(deployDir, "/Logs")

  if(!dir.exists(logDir)){
     dir.create(logDir , recursive = T)
    }

  logfile <- paste0(deployDir, "/Logs/SoilFederationAPI_logs_", dt, ".csv")
  try(writeLogEntry(logfile, logentry), silent = TRUE)

  plumber::forward()
}


writeLogEntry <- function(logfile, logentry){

  if(file.exists(logfile)){
    cat(logentry, '\n', file=logfile, append=T)
  }else{
    hdr <- paste0('System_time,Server,Request_method,HTTP_user_agent,QUERY_STRING,REMOTE_ADDR\n')
    cat(hdr, file=logfile, append=F)
    cat(logentry, '\n', file=logfile, append=T)
  }
}





#* Returns information about the data providers known to the SoilDataFederator

#* @param key (Optional)  API key for accessing the API.
#* @param usr (Optional) User name for accessing the API. To register for an API key go to - https://shiny.esoil.io/SoilDataFederator/Register/
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @tag Soil Data Federator
#* @get /SoilDataAPI/Providers
apiGetProviders <- function( res, usr=NULL, key=NULL, format='json'){

  tryCatch({

    DF <- getProviders(usr, key)
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






#* Returns a listing of the available properties


#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param verbose (Optional) return just the property codes or the full descriptions. Default = True
#* @param PropertyGroup (Optional) return just the properties for a given PropertyGroup. Default = All

#* @tag Soil Data Federator
#* @get /SoilDataAPI/Properties
apiGetProperties <- function( res,PropertyGroup=NULL, verbose=T, format='json'){

  tryCatch({

    DF <-getProperties(PropertyGroup, verbose)
    label <- 'Properties'
    resp <- cerealize(DF, label, format, res)
    return(resp)

  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}







#* Returns a listing of the available Property Groups

#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @tag Soil Data Federator
#* @get /SoilDataAPI/PropertyGroups
apiGetPropetyGroups <- function( res, verbose=T, format='json'){

  tryCatch({

    DF <-getPropertyGroups(verbose)
    label <- 'PropetyGroups'
    resp <- cerealize(DF, label, format, res)
    return(resp)

  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}



bob <- function(){
  # just to hold these lines of text as I condn't work out how ot cooment them out in place

  #* @param radius_km (Optional) The radius around the supplied location in which to search.  Default = NULL ie  returns all records
  #* @param latitude (Optional) If a location is entered  the sensor locations closest to this location will be returned. If not specified all sensor locations will be returned
  #* @param longitude (Optional) If a location is entered  the sensor locations closest to this location will be returned. If not specified all sensor locations will be returned
  #* @param bbox (Optional) The  rectangular bounding box of the area to be queried in  the form bl;ll;tl;tr ie bottom latitude, left longitude, top latitude, right longitude

}

#* Returns soil data

#* @param key (Required)  API key for accessing the API.
#* @param usr (Required) User name for accessing the API. To register for an API key go to - https://shiny.esoil.io/SoilDataFederator/Register/ You can use usr=Demo & key=Demo but only the first 5 records will be returned
#* @param numToReturn (Optional) The number of records to be returned. Default = All
#* @param format (Optional) Format of the response to return. Either json, csv, or xml. Default = json
#* @param bbox (Optional) The  rectangular bounding box of the area in the form minx;maxx;miny;maxy - semicolon delimited

#* @param providers (Optional) Filter the data returned to a specific set of Providers. It should be a single Provider code or a semi-colon delimited text string of Provider codes. Default = All providers
#* @param observedPropertyGroup (Optional) Extract data for a defined group of soil properties.
#* @param observedProperty (Optional) Specify the soil data property/s to return. It should be a single observedProperty code or a semi-colon delimited text string of observedProperty codes.

#* @tag Soil Data Federator
#* @get /SoilDataAPI/SoilData
apiGetSoilData<- function(res, usr='Demo', key='Demo', providers=NULL, observedProperty=NULL, observedPropertyGroup=NULL, bbox=NULL, format='json', numToReturn=NULL){

  nrowsToGet <- as.numeric(numToReturn)

  tryCatch({

    if(!is.null(bbox)){
      bits <- str_split(wext, ';')
      l <- as.numeric(bits[[1]][1])
      r <- as.numeric(bits[[1]][2])
      t <- as.numeric(bits[[1]][4])
      b <- as.numeric(bits[[1]][3])
      bboxExt <- extent(l, r, b, t)
    }else{
      bboxExt <- NULL
    }

    DF <-getSoilData(providers, observedProperty, observedPropertyGroup, bboxExt, usr, key)

    if(!is.null(numToReturn)){
      nget <- min(nrowsToGet, nrow(DF))
      print(nget)
      oDF <- DF[1:nget, ]
    }else{
      oDF <- DF
    }

    label <- 'SoilProperty'
    resp <- cerealize(oDF, label, format, res)
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}




#* @assets /srv/plumber/TERNLandscapes/SoilDataFederatoR/R/Docs /help
list()








cerealize <- function(DF, label, format, res){


  if(format == 'xml'){

    res$setHeader("Content-Type", "application/xml; charset=utf-8")
    xmlT <- writexml(DF, label)
    res$body <- xmlT
    return(res)

  }else if(format == 'csv'){
    res$setHeader("content-disposition", paste0("attachment; filename=", label, ".csv"));
    res$setHeader("Content-Type", "application/csv; charset=utf-8")
    res$body <- writecsv(DF)
    return(res)

  }else if(format == 'html'){
    res$setHeader("Content-Type", "text/html ; charset=utf-8")
    res$body <- htmlTable(DF, align = "l", align.header = "l", caption = label)
    return(res)

  }else{
    return(DF)
  }


}



writecsv <- function(DF){

  tc <- textConnection("value_str2", open="w")
  write.table(DF, textConnection("value_str2", open="w"), sep=",", row.names=F, col.names=T)
  value_str2 <- paste0(get("value_str2"), collapse="\n")
  close(tc)
  return(value_str2)

}

writexml <- function(df, label){

  o <- apply(df, 1, DataFrameToXmlwriter, label)
  s <- unlist(o)
  xml <- paste( s, collapse = '')
  xml2 <- str_replace_all(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>\n<', label, 'Records>\n', xml, '</', label, 'Records>'), '&', '')


  #cat(xml2, file='c:/temp/x.xml')
  return(xml2)
}

DataFrameToXmlwriter <- function(x, label){

  v <- paste0('<', label, 'Record>')
  for (i in 1:length(names(x))) {

    v <- paste0(v, '<', names(x)[i], '>', str_replace(x[i], '<', 'less than'), '</', names(x)[i], '> ')
  }
  v <- paste0(v,'</', label, 'Record>\n')

  v2 <- v
  return(v2)
}




