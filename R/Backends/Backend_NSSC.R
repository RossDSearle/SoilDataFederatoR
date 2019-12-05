library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)
library(RSQLite)
library(DBI)
### This uses the National Soil Site Collation - Searle 2014





machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  NSSC_dbPath <- '/OSM/CBR/LW_SOILDATAREPO/work/TERNLandscapes/SoilsFederator/HostedDBs/NSSC_2.0.0.sqlite'
}else{
  NSSC_dbPath <- 'C:/Projects/TernLandscapes/Site Data/HostedDBs/NSSC_2.0.0.sqlite'
 }


DataSts <- c( 'NSWGovernment', 'VicGovernment', 'SAGovernment', 'WAGovernment', 'TasGovernment')
code <- c( '1', '2', '4', '5', '6')
NSSCStateCodes = data.frame(DataSts, code, stringsAsFactors = F)



getLocationData_NSSC <- function(DataSet){

  NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)

  OrgName <- getOrgName(DataSet)

  stateCode <- NSSCStateCodes[str_to_upper(NSSCStateCodes$DataSts) == str_to_upper(DataSet), 2]

  NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
  sql <- paste0("SELECT AGENCIES.STATE_CODE, OBSERVATIONS.agency_code, OBSERVATIONS.proj_code, OBSERVATIONS.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_latitude_GDA94, OBSERVATIONS.o_longitude_GDA94
                  FROM (AGENCIES INNER JOIN SITES ON AGENCIES.AGENCY_CODE = SITES.agency_code) INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)
                  WHERE (((AGENCIES.STATE_CODE)='",stateCode, "')); COLLATE NOCASE;")

  fdf = doQuery(NSSC_con, sql)
  dbDisconnect(NSSC_con)

  if(nrow(fdf) > 0){

    day <- str_sub(fdf$o_date_desc, 1,2)
    mnth <- str_sub(fdf$o_date_desc, 3,4)
    yr <- str_sub(fdf$o_date_desc, 5,8)
    oOutDF <-  generateResponseAllLocs( DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$o_longitude_GDA94, fdf$o_latitude_GDA94, paste0(day, '-', mnth, '-', yr,'T00:00:00' ) )
    return(oOutDF)
  }
 }



getData_NSSC <- function( DataSet=NULL, observedProperty=NULL, observedPropertyGroup=NULL, key=NULL){

  OrgName <- getOrgName(DataSet)
  propList <- getPropertiesList(observedProperty, observedPropertyGroup)

  stateCode <- NSSCStateCodes[str_to_upper(NSSCStateCodes$DataSts) == str_to_upper(DataSet), 2]

    if(length(propList) == 0){
      return(blankResponseDF())
    }

  lodfs <- vector("list", length(propList))

    for (i in 1:length(propList)) {

      ObsProp <- propList[i]

      propType <- getPropertyType(ObsProp)

      if(propType==PropertyTypes$LaboratoryMeasurement){

        NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
       sql <- paste0("SELECT AGENCIES.STATE_CODE, OBSERVATIONS.agency_code, OBSERVATIONS.proj_code, OBSERVATIONS.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_latitude_GDA94, OBSERVATIONS.o_longitude_GDA94, SAMPLES.samp_no, SAMPLES.samp_upper_depth, SAMPLES.samp_lower_depth, LAB_RESULTS.labm_code, LAB_RESULTS.labr_value
        FROM AGENCIES INNER JOIN (OBSERVATIONS INNER JOIN (SAMPLES INNER JOIN LAB_RESULTS ON (SAMPLES.agency_code = LAB_RESULTS.agency_code) AND (SAMPLES.proj_code = LAB_RESULTS.proj_code) AND (SAMPLES.s_id = LAB_RESULTS.s_id) AND (SAMPLES.o_id = LAB_RESULTS.o_id) AND (SAMPLES.h_no = LAB_RESULTS.h_no) AND (SAMPLES.samp_no = LAB_RESULTS.samp_no)) ON (OBSERVATIONS.agency_code = SAMPLES.agency_code) AND (OBSERVATIONS.proj_code = SAMPLES.proj_code) AND (OBSERVATIONS.s_id = SAMPLES.s_id) AND (OBSERVATIONS.o_id = SAMPLES.o_id)) ON AGENCIES.AGENCY_CODE = OBSERVATIONS.agency_code
        WHERE (((AGENCIES.STATE_CODE)='",stateCode , "' ) AND ((LAB_RESULTS.labm_code)='", ObsProp ,"')) COLLATE NOCASE;")

        fdf = doQuery(NSSC_con, sql)
        dbDisconnect(NSSC_con)


            if(nrow(fdf) > 0){
              units <- getUnits(propertyType = propType, prop = ObsProp)

              oOutDF <- generateResponseDF(DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$samp_no , fdf$o_date_desc , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                           fdf$samp_upper_depth , fdf$samp_lower_depth , propType, ObsProp, fdf$labr_value , units)
              lodfs[[i]] <- oOutDF
            }else{
              lodfs[[i]] <- blankResponseDF()
            }


      }else{


        tabName <- as.character(doQueryFromFed(paste0("select PropertyGroup from Properties WHERE Property = '", ObsProp, "' COLLATE NOCASE")))
        tabLev <- as.numeric(doQueryFromFed(paste0("select Level from TableLevels WHERE TableName = '", tabName, "' COLLATE NOCASE")))

        if(tabLev == 4){
          NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
          sqlTemplate <- paste0('SELECT AGENCIES.STATE_CODE, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, HORIZONS.h_no, HORIZONS.h_upper_depth, HORIZONS.h_lower_depth, xxxx.yyyy
                      FROM AGENCIES INNER JOIN (((SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) INNER JOIN HORIZONS ON (OBSERVATIONS.o_id = HORIZONS.o_id) AND (OBSERVATIONS.s_id = HORIZONS.s_id) AND (OBSERVATIONS.proj_code = HORIZONS.proj_code) AND (OBSERVATIONS.agency_code = HORIZONS.agency_code)) INNER JOIN xxxx ON (HORIZONS.h_no = xxxx.h_no) AND (HORIZONS.o_id = xxxx.o_id) AND (HORIZONS.s_id = xxxx.s_id) AND (HORIZONS.proj_code = xxxx.proj_code) AND (HORIZONS.agency_code = xxxx.agency_code)) ON AGENCIES.AGENCY_CODE = SITES.agency_code
                      WHERE (((AGENCIES.STATE_CODE)="', stateCode, '") AND ((xxxx.yyyy) Is Not Null))
                      ORDER BY SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, HORIZONS.h_upper_depth COLLATE NOCASE;
                      ')

          sql1 <- str_replace_all(sqlTemplate, 'xxxx', tabName)
          sql2 <- str_replace_all(sql1, 'yyyy', ObsProp)
          sql3 <- str_replace_all(sql2, 'zzzz', stateCode)

          fdf = doQuery(NSSC_con, sql3)
          dbDisconnect(NSSC_con)

          head(fdf)

          oOutDF <- generateResponseDF( DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$h_no , fdf$o_date_desc , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                       fdf$h_upper_depth , fdf$h_lower_depth , propType, ObsProp, fdf[, 12] , 'None')
          lodfs[[i]] <- oOutDF

        }else if(tabLev == 3){

          NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
          # Horizons table only
          if(str_to_upper(tabName) == "HORIZONS"){
            sqlTemplate <- paste0('SELECT AGENCIES.STATE_CODE, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, HORIZONS.h_no, HORIZONS.h_upper_depth, HORIZONS.h_lower_depth, xxxx.yyyy
                          FROM AGENCIES INNER JOIN ((SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) INNER JOIN xxxx ON (OBSERVATIONS.o_id = xxxx.o_id) AND (OBSERVATIONS.s_id = xxxx.s_id) AND (OBSERVATIONS.proj_code = xxxx.proj_code) AND (OBSERVATIONS.agency_code = xxxx.agency_code)) ON AGENCIES.AGENCY_CODE = SITES.agency_code
                          WHERE (((AGENCIES.STATE_CODE)="zzzz") AND ((xxxx.yyyy) Is Not Null))
                          ORDER BY SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, HORIZONS.h_upper_depth COLLATE NOCASE;')
            sql1 <- str_replace_all(sqlTemplate, 'xxxx', tabName)
            sql2 <- str_replace_all(sql1, 'yyyy', ObsProp)
            sql3 <- str_replace_all(sql2, 'zzzz', stateCode)

            fdf = doQuery(NSSC_con, sql3)
            dbDisconnect(NSSC_con)

            head(fdf)

            oOutDF <- generateResponseDF( DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$h_no , fdf$o_date_desc , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                         fdf$h_upper_depth , fdf$h_lower_depth , propType, ObsProp, fdf[, 12] , 'None')
            lodfs[[i]] <- oOutDF

          }else{
            # All other tables at this level
            sqlTemplate <- paste0('SELECT AGENCIES.STATE_CODE, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, xxxx.yyyy
           FROM AGENCIES INNER JOIN ((SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) INNER JOIN xxxx ON (OBSERVATIONS.o_id = xxxx.o_id) AND (OBSERVATIONS.s_id = xxxx.s_id) AND (OBSERVATIONS.proj_code = xxxx.proj_code) AND (OBSERVATIONS.agency_code = xxxx.agency_code)) ON AGENCIES.AGENCY_CODE = SITES.agency_code
           WHERE (((AGENCIES.STATE_CODE)="zzzz") AND ((xxxx.yyyy) Is Not Null))
           ORDER BY SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id COLLATE NOCASE')
            sql1 <- str_replace_all(sqlTemplate, 'xxxx', tabName)
            sql2 <- str_replace_all(sql1, 'yyyy', ObsProp)
            sql3 <- str_replace_all(sql2, 'zzzz', stateCode)

            fdf = doQuery(NSSC_con, sql3)
            dbDisconnect(NSSC_con)

            head(fdf)

            oOutDF <- generateResponseDF( DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), 1 , fdf$o_date_desc , fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 ,
                                          'None' , 'None' , propType, ObsProp, fdf[, 9] , 'None')
            lodfs[[i]] <- oOutDF
          }
        }else if(tabLev == 2){
          # Observations table only
          NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
          sqlTemplate <- paste0('SELECT AGENCIES.STATE_CODE, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, OBSERVATIONS.yyyy
                      FROM AGENCIES INNER JOIN (SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) ON AGENCIES.AGENCY_CODE = SITES.agency_code
                      WHERE (((AGENCIES.STATE_CODE)="zzzz") AND ((OBSERVATIONS.yyyy) Is Not Null))
                      ORDER BY SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id COLLATE NOCASE;')

          sql2 <- str_replace_all(sqlTemplate, 'yyyy', ObsProp)
          sql3 <- str_replace_all(sql2, 'zzzz', stateCode)

          fdf = doQuery(NSSC_con, sql3)
          dbDisconnect(NSSC_con)

          head(fdf)

          oOutDF <- generateResponseDF( DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id),1, fdf$o_date_desc ,
                                        fdf$o_longitude_GDA94, fdf$o_latitude_GDA94, 'None' , 'None' , propType, ObsProp, fdf[, 9] , 'None')
          lodfs[[i]] <- oOutDF


        }else if(tabLev == 1){

          NSSC_con <- dbConnect(RSQLite::SQLite(), NSSC_dbPath)
          sqlTemplate <- paste0('SELECT AGENCIES.STATE_CODE, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, OBSERVATIONS.o_date_desc, SITES.yyyy
                      FROM (AGENCIES INNER JOIN SITES ON AGENCIES.AGENCY_CODE = SITES.agency_code) INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)
                      WHERE (((AGENCIES.STATE_CODE)="zzzz") AND ((SITES.yyyy) Is Not Null))
                      ORDER BY SITES.agency_code, SITES.proj_code, SITES.s_id COLLATE NOCASE;')


          sql2 <- str_replace_all(sqlTemplate, 'yyyy', ObsProp)
          sql3 <- str_replace_all(sql2, 'zzzz', stateCode)

          fdf = doQuery(NSSC_con, sql3)
          dbDisconnect(NSSC_con)

          head(fdf)

          oOutDF <- generateResponseDF( DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), 1 , fdf$o_date_desc ,
                                        fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 , 'None' , 'None' , propType, ObsProp, fdf[, 9] , 'None')
          lodfs[[i]] <- oOutDF


        }else{
          return(blankResponseDF())
        }
      }
    }

    outDF = as.data.frame(data.table::rbindlist(lodfs))

    return(outDF)

  }



