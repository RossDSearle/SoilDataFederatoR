library(stringr)
library(XML)
library(xml2)
library(htmlTable)
library(lubridate)

#To start in supervisorctl on esoil use this - "sudo supervisorctl -c /etc/supervisor/supervisord.conf start plumber_SoilFed"



machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){

  deployDir <-'/srv/plumber/TERNLandscapes/SoilDataFederatoR'
  logDir <- '/mnt/data/APILogs/SoilDataFederator/'
}else{
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR'
  logDir <- 'c:/temp/Logs'
}

source(paste0(deployDir, '/Helpers/apiHelpers.R'))
source(paste0(deployDir, '/Backends.R'))

#* @apiTitle TERN SoilDataFederatoR Web API
#* @apiDescription These services allow <b>unified</b> and <b>standardised</b> access to a range of disparate soil database systems.<br><br> More detail about the SoilDataFederatoR service can be found <a href="https://esoil.io/TERNLandscapes/SoilDataFederatoR/help/SoilDataFederatorHelp.html">HERE</a>
#* <br><br><H2>API Key Registration</H2>
#* You need to register for an API Key to be able to use the API to access the soil data. You can quickly register <a
#* href="https://shiny.esoil.io/SoilDataFederator/Pages/Register/"
#* target="_blank">HERE</a>. Just supply your email address, name and organisation and you are good to go.



#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req, res){

  logentry <- paste0(as.character(now()), ",",
       machineName, ",",
       req$REQUEST_METHOD, req$PATH_INFO, ",",
       str_replace_all( req$HTTP_USER_AGENT, ",", ""), ",",
       req$QUERY_STRING, ",",
       req$REMOTE_ADDR
      )

  dt <- format(Sys.time(), "%d-%m-%Y")

 # logDir <- paste0(deployDir, "/Logs")

  if(!dir.exists(logDir)){
     dir.create(logDir , recursive = T)
    }

  logfile <- paste0(logDir, "/SoilFederationAPI_logs_", dt, ".csv")
  #print(logfile)
  #try(writeLogEntry(logfile, logentry), silent = TRUE)
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





#* Returns information about the DataSets available in the SoilDataFederator

#* @param key (Optional)  API key for accessing the API.
#* @param usr (Optional) User name for accessing the API. To register for an API key go to - https://shiny.esoil.io/SoilDataFederator/Register/
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @tag Soil Data Federator
#* @get /SoilDataAPI/DataSets
apiGetDataSets <- function(req, res, usr=NULL, key=NULL, format='json'){

  tryCatch({
    library(rlang)
   # env_print(req)
   # print(env_get(req, 'QUERY_STRING'))

    DF <- getDataSets(usr, key)

    DF <- within(DF, rm('NativeAPIURL'))

    label <- 'DataSets'
    resp <- cerealize(DF, label, format, res)
    return(resp)

  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}






#* Returns a listing of the Standard properties


#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param verbose (Optional) return just the property codes or the full descriptions. Default = True
#* @param PropertyGroup (Optional) return just the properties for a given PropertyGroup. Default = All

#* @tag Soil Data Federator
#* @get /SoilDataAPI/Properties
apiGetProperties <- function( res,PropertyGroup=NULL, verbose=T, format='json'){

  tryCatch({

    DF <-getStandardProperties(PropertyGroup, verbose)
    label <- 'StandardProperties'
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


#* Returns a listing of the Available properties for a dataset. This is a list of properties known to the SoilDataFederator for a particular data set. It does not mean that there is actual data avialbale for each of the listed properties.


#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param verbose (Optional) return just the property codes or the full descriptions. Default = True
#* @param PropertyGroup (Optional) return just the properties for a given PropertyGroup. Default = All
#* @param StandardTypes (Optional) which properties to return. Options are ony the standard properties (STANDARD), only the non standard properties (NONSTANDARD) or all the properties (ALL). Default = All
#* @param DataSet (Required) return the available properties for a given Dataset Default = All

#* @tag Soil Data Federator
#* @get /SoilDataAPI/AvialableProperties
apiGetAvailableProperties <- function( res, DataSet=NULL, StandardTypes='ALL', PropertyGroup=NULL, verbose=T, format='json'){

  tryCatch({

    DF <-getAvailableProperties(DataSet, StandardTypes, PropertyGroup, verbose)
    print(head((DF)))
    label <- 'AvailableProperties'
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

#* Returns soil observation data

#* @param key (Required)  API key for accessing the API.
#* @param usr (Required) User name for accessing the API. To register for an API key go to - https://shiny.esoil.io/SoilDataFederator/Pages/Register/ You can use usr=Demo & key=Demo but only the first 5 records will be returned
#* @param numToReturn (Optional) The number of records to be returned. Default = All
#* @param format (Optional) Format of the response to return. Either json, csv, or xml. Default = json
#* @param bbox (Optional) The  rectangular bounding box of the area in the form minx;maxx;miny;maxy - semicolon delimited

#* @param DataSet (Optional) Filter the data returned to a specific set of DataSets. It should be a single DataSet code or a semi-colon delimited text string of DataSet codes. Default = All DataSets
#* @param observedPropertyGroup (Optional) Extract data for a defined group of soil properties.
#* @param observedProperty (Optional) Specify the soil data property/s to return. It should be a single observedProperty code or a semi-colon delimited text string of observedProperty codes.

#* @tag Soil Data Federator
#* @get /SoilDataAPI/SoilData
apiGetSoilData<- function(res, usr='Demo', key='Demo', DataSet=NULL, observedProperty=NULL, observedPropertyGroup=NULL, bbox=NULL, format='json', numToReturn=NULL){

  nrowsToGet <- as.numeric(numToReturn)

  tryCatch({

    if(!is.null(bbox)){
      bits <- str_split(bbox, ';')
      l <- as.numeric(bits[[1]][1])
      r <- as.numeric(bits[[1]][2])
      t <- as.numeric(bits[[1]][4])
      b <- as.numeric(bits[[1]][3])
      bboxExt <- extent(l, r, b, t)
    }else{
      bboxExt <- NULL
    }

    DF <-getSoilData(DataSet, observedProperty, observedPropertyGroup, bboxExt, usr, key)

    if(!is.null(numToReturn)){
      nget <- min(nrowsToGet, nrow(DF))
      print(nget)
      oDF <- DF[1:nget, ]
    }else{
      oDF <- DF
    }

    label <- 'SoilProperty'
    writeLog(oDF, usr, logDir )
    resp <- cerealize(oDF, label, format, res)
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}


#* Returns the soil observation locations.

#* @param format (Optional) Format of the response to return. Either json, csv, or xml. Default = json
#* @param bbox (Optional) The  rectangular bounding box of the area in the form minx;maxx;miny;maxy - semicolon delimited

#* @param DataSet (Optional) Filter on a specific DataSet.
#* @tag Soil Data Federator
#* @get /SoilDataAPI/ObservationLocations
apiGetObservationLocations <- function(res, DataSet=NULL, bbox=NULL, format='json', usr='Demo', key='Demo'){

  tryCatch({

    DF <-getSiteLocations(DataSet, bbox)
    #DF = data.frame(c('Hi'))
     label <- 'ObservationLocations'
     print(head(DF))
     resp <- cerealize(DF, label, format, res)
    # return(resp)
  }, error = function(res)
  {
    res$status <- 400 # Bad request
    list(error=jsonlite::unbox(geterrmessage()))

  })
}

#* Returns an image of the soil observation locations.

#* @param bbox (Optional) The  rectangular bounding box of the area in the form minx;maxx;miny;maxy - semicolon delimited
#* @param DataSet (Optional) Filter on a specific DataSet.
#* @png (width = 400, height = 500)
#* @tag Soil Data Federator
#* @get /SoilDataAPI/ObservationLocationsAsMap
apiGetObservationLocationsAsMap <- function(DataSet=NULL, bbox=NULL, usr='Demo', key='Demo'){

  tryCatch({

    DF <-getSiteLocations(DataSet, bbox)
    print(head(DF))
    spp <- plotObservationLocationsImage(DF)
    return(plot(spp))
  }, error = function(res)
  {
    res$status <- 400 # Bad request
    list(error=jsonlite::unbox(geterrmessage()))

  })
}



#* Returns the codes and descriptions for the SoilDataFederator data quality descriptors.

#* @param format (Optional) Format of the response to return. Either json, csv, or xml. Default = json


#* @tag Soil Data Federator
#* @get /SoilDataAPI/DataQualityDescriptions
apiGetDataQualityDescriptions <- function(res, DataSet=NULL, bbox=NULL, format='json', usr='Demo', key='Demo'){

  tryCatch({

    DF <-getDataQualityDescriptions()
    label <- 'DataQualityDescriptions'
    resp <- cerealize(DF, label, format, res)

  }, error = function(res)
  {
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))

  })
}









#* @assets /srv/plumber/TERNLandscapes/SoilDataFederatoR/Docs /help















