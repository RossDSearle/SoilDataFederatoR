library(stringr)
library(xml2)

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

#* @apiTitle TERN Soil Federator Web API
#* @apiDescription These services allow <b>unified</b> and <b>standardised</b> access to a range of disparate soil database systems.<br><br> More detail about the Soils Federation Service can be found <a href='http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/help/index.html' > here </a>

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
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @tag Soil Data Federator
#* @get /SoilDataAPI/Providers
apiGetProviders <- function( res, format='json'){

  tryCatch({

    DF <- getProviders()
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
#* @tag Soil Data Federator
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



bob <- function(){
  # just to hold these lines of text as I condn't work out how ot cooment them out in place

  #* @param radius_km (Optional) The radius around the supplied location in which to search.  Default = NULL ie  returns all records
  #* @param latitude (Optional) If a location is entered  the sensor locations closest to this location will be returned. If not specified all sensor locations will be returned
  #* @param longitude (Optional) If a location is entered  the sensor locations closest to this location will be returned. If not specified all sensor locations will be returned
  #* @param bbox (Optional) The  rectangular bounding box of the area to be queried in  the form bl;ll;tl;tr ie bottom latitude, left longitude, top latitude, right longitude

}

#* Returns soil data

#* @param pwd (Optional) Password for logging into the system - if not supplied defaults to 'Public'
#* @param usr (Optional) User name for logging into the system - if not supplied defaults to 'Public'
#* @param numToReturn (Optional) The number of records to be returned. Default = All
#* @param format (Optional) Format of the response to return. Either json, csv, or xml. Default = json
#* @param providers (Optional) Filter the data returned to a specific set of Providers. It should be a single Provider code or a semi-colon delimited text string of Provider codes. Default = All providers
#* @param observedPropertyGroup (Optional) Extract data for a defined group of soil properties.
#* @param observedProperty (Optional) Specify the soil data property/s to return. It should be a single observedProperty code or a semi-colon delimited text string of observedProperty codes.

#* @tag Soil Data Federator
#* @get /SoilDataAPI/SoilData
apiGetSoilData<- function(res, usr='Public', pwd='Public', providers=NULL, observedProperty=NULL, observedPropertyGroup=NULL, format='json', numToReturn=NULL){

  nrowsToGet <- as.numeric(numToReturn)

  tryCatch({

    DF <-getSoilData(providers, observedProperty, observedPropertyGroup)

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




