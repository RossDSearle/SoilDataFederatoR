library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)
library(RSQLite)
library(DBI)
### This uses the National Soil Site Collation - Searle 2014

#projectRoot <- 'C:/Users/sea084/Dropbox/RossRCode/Git/NSSCapi'
machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  #fedRoot <- ''
  NSSC_dbPath <- '/home/sea084/Data/NSSC_2.0.0.sqlite'
 # fedDB <-  ''
}else{
  #fedRoot <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR'
  NSSC_dbPath <- 'C:/Projects/TernLandscapes/Site Data/NSSC_2.0.0.sqlite'
  #fedDB <-  'C:/R/R-3.6.0/library/SoilDataFederatoR/extdata/soilsFederator.sqlite'
 }

#source(paste0(fedRoot, '/R/Helpers/dbHelpers.R'))




getData_NSSC <- function( DataSet=NULL, observedProperty=NULL, observedPropertyGroup=NULL, key=NULL){

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

  orgs <-  doQueryFromFed(paste0("select * from Providers WHERE OrgName = '", provider, "'"))
  isRestricted <- as.logical( orgs$Restricted)

  acs <- doQueryFromFed(paste0("select * from NSSC_AgencyCodes WHERE OrgName = '", provider, "'"))
  ac <- acs$sCode[1]

  pl <- getPropertiesList(observedProperty, observedPropertyGroup)
  mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))
  nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)

  OK=T
  if(isRestricted){
    OK=F
    if(!is.null(key)){
      if(key=='Tenosol'){
        OK=T
        print("ROSS")
      }
    }
  }

  if(OK){
    if(length(nativeProps) == 0){
      return(blankResponseDF())
    }

    lodfs <- list(length(nativeProps))

    for (i in 1:length(nativeProps)) {

      prop <- nativeProps[i]
      #propType <- Properties[str_to_upper(Properties$Property) == str_to_upper(prop), ]$PropertyType
      propType <- getPropertyType(prop)

      if(propType==PropertyTypes$LaboratoryMeasurement){

        NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
        sql <- paste0("SELECT OBSERVATIONS.agency_code, OBSERVATIONS.proj_code, OBSERVATIONS.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_latitude_GDA94, OBSERVATIONS.o_longitude_GDA94, SAMPLES.samp_no, SAMPLES.samp_upper_depth, SAMPLES.samp_lower_depth, LAB_RESULTS.labm_code, LAB_RESULTS.labr_value
                      FROM OBSERVATIONS INNER JOIN (SAMPLES INNER JOIN LAB_RESULTS ON (SAMPLES.samp_no = LAB_RESULTS.samp_no) AND (SAMPLES.h_no = LAB_RESULTS.h_no) AND (SAMPLES.o_id = LAB_RESULTS.o_id) AND (SAMPLES.s_id = LAB_RESULTS.s_id) AND (SAMPLES.proj_code = LAB_RESULTS.proj_code) AND (SAMPLES.agency_code = LAB_RESULTS.agency_code)) ON (OBSERVATIONS.o_id = SAMPLES.o_id) AND (OBSERVATIONS.s_id = SAMPLES.s_id) AND (OBSERVATIONS.proj_code = SAMPLES.proj_code) AND (OBSERVATIONS.agency_code = SAMPLES.agency_code)
                      WHERE (((LAB_RESULTS.labm_code)='", prop ,"'));")
        fdf = doQuery(NSSC_con, sql)
        dbDisconnect(NSSC_con)

        #fdf <- qres[!(qres$agency_code %in% agencyFilters), ]

        if(nrow(fdf) > 0){

          #propertyType <- getPropertyType(prop)
          #units <- labMethods[str_to_upper(labMethods$LABM_CODE) == str_to_upper(prop), ]$LABM_UNITS
          units <- getUnits(propertyType = propType, prop = prop)

          oOutDF <- generateResponseDF("NSCC", OrgName, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$samp_no , fdf$o_date_desc , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                       fdf$samp_upper_depth , fdf$samp_lower_depth , propType, prop, fdf$labr_value , units, 'Brilliant')
          lodfs[[i]] <- oOutDF
        }
      }else{


        tabName <- as.character(doQueryFromFed(paste0("select PropertyGroup from Properties WHERE Property = '", prop, "'")))
        # tabName <- Properties[str_to_upper(Properties$Property) == str_to_upper(prop), ]$PropertyGroup
        tabLev <- as.numeric(doQueryFromFed(paste0("select Level from TableLevels WHERE TableName = '", tabName, "'")))

        if(tabLev == 4){
          NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
          sqlTemplate <- paste0('SELECT AGENCIES.STATE_CODE, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, HORIZONS.h_no, HORIZONS.h_upper_depth, HORIZONS.h_lower_depth, xxxx.yyyy
                      FROM AGENCIES INNER JOIN (((SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) INNER JOIN HORIZONS ON (OBSERVATIONS.o_id = HORIZONS.o_id) AND (OBSERVATIONS.s_id = HORIZONS.s_id) AND (OBSERVATIONS.proj_code = HORIZONS.proj_code) AND (OBSERVATIONS.agency_code = HORIZONS.agency_code)) INNER JOIN xxxx ON (HORIZONS.h_no = xxxx.h_no) AND (HORIZONS.o_id = xxxx.o_id) AND (HORIZONS.s_id = xxxx.s_id) AND (HORIZONS.proj_code = xxxx.proj_code) AND (HORIZONS.agency_code = xxxx.agency_code)) ON AGENCIES.AGENCY_CODE = SITES.agency_code
                      WHERE (((AGENCIES.STATE_CODE)="zzzz") AND ((xxxx.yyyy) Is Not Null))
                      ORDER BY SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, HORIZONS.h_upper_depth;
                      ')

          sql1 <- str_replace_all(sqlTemplate, 'xxxx', tabName)
          sql2 <- str_replace_all(sql1, 'yyyy', prop)
          sql3 <- str_replace_all(sql2, 'zzzz', stateCode)

          fdf = doQuery(NSSC_con, sql3)
          dbDisconnect(NSSC_con)

          head(fdf)

          oOutDF <- generateResponseDF("NSCC", OrgName, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$h_no , fdf$o_date_desc , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                       fdf$h_upper_depth , fdf$h_lower_depth , propType, prop, fdf[, 12] , 'NA', 'Brilliant')
          lodfs[[i]] <- na.omit(oOutDF)
          #return(oOutDF)
        }else if(tabLev == 3){

          NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
          # Horizons table only
          if(str_to_upper(tabName) == "HORIZONS"){
            sqlTemplate <- paste0('SELECT AGENCIES.STATE_CODE, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, HORIZONS.h_no, HORIZONS.h_upper_depth, HORIZONS.h_lower_depth, xxxx.yyyy
                          FROM AGENCIES INNER JOIN ((SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) INNER JOIN xxxx ON (OBSERVATIONS.o_id = xxxx.o_id) AND (OBSERVATIONS.s_id = xxxx.s_id) AND (OBSERVATIONS.proj_code = xxxx.proj_code) AND (OBSERVATIONS.agency_code = xxxx.agency_code)) ON AGENCIES.AGENCY_CODE = SITES.agency_code
                          WHERE (((AGENCIES.STATE_CODE)="zzzz") AND ((xxxx.yyyy) Is Not Null))
                          ORDER BY SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, HORIZONS.h_upper_depth;')
            sql1 <- str_replace_all(sqlTemplate, 'xxxx', tabName)
            sql2 <- str_replace_all(sql1, 'yyyy', prop)
            sql3 <- str_replace_all(sql2, 'zzzz', stateCode)

            fdf = doQuery(NSSC_con, sql3)
            dbDisconnect(NSSC_con)

            head(fdf)

            oOutDF <- generateResponseDF("NSCC", OrgName, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$h_no , fdf$o_date_desc , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                         fdf$h_upper_depth , fdf$h_lower_depth , propType, prop, fdf[, 12] , 'NA', 'Brilliant')
            lodfs[[i]] <- na.omit(oOutDF)
            #return(na.omit(oOutDF))

          }else{
            # All other tables at this level
            sqlTemplate <- paste0('SELECT AGENCIES.STATE_CODE, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, xxxx.yyyy
           FROM AGENCIES INNER JOIN ((SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) INNER JOIN xxxx ON (OBSERVATIONS.o_id = xxxx.o_id) AND (OBSERVATIONS.s_id = xxxx.s_id) AND (OBSERVATIONS.proj_code = xxxx.proj_code) AND (OBSERVATIONS.agency_code = xxxx.agency_code)) ON AGENCIES.AGENCY_CODE = SITES.agency_code
           WHERE (((AGENCIES.STATE_CODE)="zzzz") AND ((xxxx.yyyy) Is Not Null))
           ORDER BY SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id')
            sql1 <- str_replace_all(sqlTemplate, 'xxxx', tabName)
            sql2 <- str_replace_all(sql1, 'yyyy', prop)
            sql3 <- str_replace_all(sql2, 'zzzz', stateCode)

            fdf = doQuery(NSSC_con, sql3)
            dbDisconnect(NSSC_con)

            head(fdf)

            oOutDF <- generateResponseDF("NSCC", OrgName, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), 1 , fdf$o_date_desc , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                         0 , 0 , propType, prop, fdf[, 9] , 'NA', 'Brilliant')
            lodfs[[i]] <- na.omit(oOutDF)
            #return(na.omit(oOutDF))
          }
        }else if(tabLev == 2){
          # Observations table only
          NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
          sqlTemplate <- paste0('SELECT AGENCIES.STATE_CODE, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, OBSERVATIONS.yyyy
                      FROM AGENCIES INNER JOIN (SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) ON AGENCIES.AGENCY_CODE = SITES.agency_code
                      WHERE (((AGENCIES.STATE_CODE)="zzzz") AND ((OBSERVATIONS.yyyy) Is Not Null))
                      ORDER BY SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id;')

          sql2 <- str_replace_all(sqlTemplate, 'yyyy', prop)
          sql3 <- str_replace_all(sql2, 'zzzz', stateCode)

          fdf = doQuery(NSSC_con, sql3)
          dbDisconnect(NSSC_con)

          head(fdf)

          oOutDF <- generateResponseDF("NSCC", OrgName, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$h_no , fdf$o_date_desc , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                       fdf$h_upper_depth , fdf$h_lower_depth , propType, prop, fdf[, 12] , 'NA', 'Brilliant')
          lodfs[[i]] <- na.omit(oOutDF)


        }else if(tabLev == 1){
          # Sites table  only
          NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
          sqlTemplate <- paste0('SELECT AGENCIES.STATE_CODE, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, OBSERVATIONS.o_date_desc, SITES.yyyy
                      FROM (AGENCIES INNER JOIN SITES ON AGENCIES.AGENCY_CODE = SITES.agency_code) INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)
                      WHERE (((AGENCIES.STATE_CODE)="zzzz") AND ((SITES.yyyy) Is Not Null))
                      ORDER BY SITES.agency_code, SITES.proj_code, SITES.s_id;')


          sql2 <- str_replace_all(sqlTemplate, 'yyyy', prop)
          sql3 <- str_replace_all(sql2, 'zzzz', stateCode)

          fdf = doQuery(NSSC_con, sql3)
          dbDisconnect(NSSC_con)

          head(fdf)

          oOutDF <- generateResponseDF("NSCC", OrgName, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), 1 , fdf$o_date_desc , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                       0 , 0 , propType, prop, fdf[, 9] , 'NA', 'Brilliant')
          lodfs[[i]] <- na.omit(oOutDF)


        }else{
          return(blankResponseDF())
        }
      }
    }

    outDF = as.data.frame(data.table::rbindlist(lodfs))

    return(outDF)

  }
  else{

    r <- character(10)
    r[1] <- 'The data from this provider is restricted. '
    r[2] <- ''
    r[3] <- 'The data is available but first you need to contact the Organisation in question and ask them for permission to access this data.'
    r[4] <- ''
    r[5] <- 'For QLD Government data email : Evan Thomas at evan.thomas@qld.gov.au'
    r[6] <- 'For NT Government data email : Evan Thomas at evan.thomas@qld.gov.au'
    r[7] <- 'For Victorian Government data email : Evan Thomas at evan.thomas@qld.gov.au'
    r[8] <- ''
    r[9] <- 'Once you have recieved these permissions contact Ross Searle ross.searle@csiro.au and he will send you a key to allow access'

    data.frame(r)
    return(r)
    # stop(paste0("The data from this provider is restricted. The data is available but first you need to contact the Organisation in question and ask them for permission to access this data.
    #
    #             For QLD Government data email : Evan Thomas at evan.thomas@qld.gov.au
    #             For NT Government data email : Evan Thomas at evan.thomas@qld.gov.au
    #             For Victorian Government data email : Evan Thomas at evan.thomas@qld.gov.au
    #
    #             "))
  }

}
