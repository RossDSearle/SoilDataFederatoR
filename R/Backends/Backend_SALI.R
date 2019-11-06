library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)


machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  SALI_dbPath <- '/OSM/CBR/LW_SOILDATAREPO/work/TERNLandscapes/SoilsFederator/HostedDBs/SALI_Morphology.sqlite'
}else{
  SALI_dbPath <- 'C:/Projects/TernLandscapes/Site Data/QLD/SALI_Morphology.sqlite'
}

doQueryFromSALI <- function(sql){
  #print(dbPathSoilsFed)
  conn <- DBI::dbConnect(RSQLite::SQLite(), SALI_dbPath)
  qry <- dbSendQuery(conn, sql)
  res <- dbFetch(qry)
  dbClearResult(qry)
  dbDisconnect(conn)
  return(res)
}

# labresults <- fromJSON(paste0("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?%24filter=ProjectCode%20eq%20'3MC' and labMethodCode eq '3A1'&%24orderby=SiteId"))
# samples <- fromJSON(paste0("https://soil-chem.information.qld.gov.au/odata/Samples?%24filter=ProjectCode%20eq%20'3MC'&%24orderby=SiteId"))
# testvals <- fromJSON(URLencode("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?%24filter=ProjectCode%20eq%20%273MC%27%20and%20LabMethodCode%20eq%20%273A1%27"))
# testvals <- fromJSON(URLencode("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?$filter=ProjectCode eq '3MC' and LabMethodCode eq '3A1'"))
# testvals <- fromJSON(URLencode("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?$filter=LabMethodCode eq '3A1'"))
# testvals <- fromJSON(URLencode("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?$filter=LABP_CODE eq 'PH'"))
# nrow(testvals)
# head(testvals)

getData_QLDGovernment <- function(DataSet, observedProperty, observedPropertyGroup=NULL ){

  OrgName <- getOrgName(DataSet)

  mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))
  nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)

  if(length(nativeProps) == 0){
    return(blankResponseDF())
  }

  lodfs <- list(length(nativeProps))

    for (i in 1:length(nativeProps)) {

      prop <- nativeProps[i]

      propertyType <- getPropertyType(prop)
      units <- getUnits(propertyType = propertyType, prop = prop)

      if(propertyType == 'LaboratoryMeasurement'){

      samples <- fromJSON(paste0("https://soil-chem.information.qld.gov.au/odata/Samples"))

      sd <- fromJSON(URLencode(paste0("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?$filter=LabMethodCode eq '", prop, "'")))
      if(nrow(sd) > 0){
          fdf <- merge(sd, samples,  by=c("projectCode","siteId", "observationNumber", "sampleNumber"), all.x = T)

          oOutDF <- generateResponseDF(OrgName, DataSet, paste0('QLD_', fdf$projectCode , '_', fdf$siteId, '_', fdf$observationNumber ),
                                       fdf$sampleNumber, fdf$analysisDate, fdf$longitude , fdf$latitude,
                                       fdf$upperDepth, fdf$lowerDepth, propertyType, prop, fdf$formattedValue , units)
          lodfs[[i]] <- oOutDF
      }else{
        return(blankResponseDF())
      }

      }else{
  #### extract Morpholgy data
        ObsProp <-  prop
        nativePropRec <- (doQueryFromSALI(paste0("select * from Mappings WHERE SITESfld = '",ObsProp, "' COLLATE NOCASE")))
        nativeProp <- nativePropRec$SALIfld
        nativeTable <- nativePropRec$SALItblName
        tabLev <- as.numeric(doQueryFromSALI(paste0("select Level from TableLevels WHERE TableName = '", nativeTable, "' COLLATE NOCASE")))
        propertyType <- getPropertyType(prop)

        if(tabLev == 4){

          sqlTemplate <- paste0('SELECT SIT.PROJECT_CODE, OBS.SITE_ID, OBS.OBS_NO, OBS.OBS_DATE, OLC.DATUM, OLC.LATITUDE, OLC.LONGITUDE, HOR.HORIZON_NO, HOR.UPPER_DEPTH, HOR.LOWER_DEPTH, xxxx.yyyy
          FROM (((SIT INNER JOIN OBS ON (SIT.SITE_ID = OBS.SITE_ID) AND (SIT.PROJECT_CODE = OBS.PROJECT_CODE)) INNER JOIN HOR ON (OBS.OBS_NO = HOR.OBS_NO) AND (OBS.SITE_ID = HOR.SITE_ID) AND (OBS.PROJECT_CODE = HOR.PROJECT_CODE)) INNER JOIN xxxx ON (HOR.HORIZON_NO = xxxx.HORIZON_NO) AND (HOR.OBS_NO = xxxx.OBS_NO) AND (HOR.SITE_ID = xxxx.SITE_ID) AND (HOR.PROJECT_CODE = xxxx.PROJECT_CODE)) INNER JOIN OLC ON (OBS.OBS_NO = OLC.OBS_NO) AND (OBS.SITE_ID = OLC.SITE_ID) AND (OBS.PROJECT_CODE = OLC.PROJECT_CODE)
          WHERE (((OLC.DATUM)="3")) COLLATE NOCASE;')

          sql1 <- str_replace_all(sqlTemplate, 'xxxx', nativeTable)
          sql2 <- str_replace_all(sql1, 'yyyy', nativeProp)

          fdf =  doQueryFromSALI(sql2)
          head(fdf)

          oOutDF <- generateResponseDF(OrgName, DataSet, paste0( 'QLD_', fdf$PROJECT_CODE, '_', fdf$SITE_ID, '_', fdf$OBS_NO ), fdf$HORIZON_NO , fdf$OBS_DATE , fdf$LONGITUDE, fdf$LATITUDE ,
                                       fdf$UPPER_DEPTH , fdf$LOWER_DEPTH , propertyType, ObsProp, fdf[, 11] , 'NA')
          lodfs[[i]] <- oOutDF

        }else if(tabLev == 3){

          # Horizons table only
          if(str_to_upper(nativeTable) == "HOR"){
            sqlTemplate <- paste0('SELECT SIT.PROJECT_CODE, OBS.SITE_ID, OBS.OBS_NO, OBS.OBS_DATE, OLC.DATUM, OLC.LONGITUDE, OLC.LATITUDE, HOR.HORIZON_NO, HOR.UPPER_DEPTH, HOR.LOWER_DEPTH, xxxx.yyyy
                                    FROM ((SIT INNER JOIN OBS ON (SIT.SITE_ID = OBS.SITE_ID) AND (SIT.PROJECT_CODE = OBS.PROJECT_CODE)) INNER JOIN OLC ON (OBS.OBS_NO = OLC.OBS_NO) AND (OBS.SITE_ID = OLC.SITE_ID) AND (OBS.PROJECT_CODE = OLC.PROJECT_CODE)) INNER JOIN HOR ON (OBS.OBS_NO = HOR.OBS_NO) AND (OBS.SITE_ID = HOR.SITE_ID) AND (OBS.PROJECT_CODE = HOR.PROJECT_CODE)
                                    WHERE (((OLC.DATUM)="3")) COLLATE NOCASE;
                                    ')
            sql1 <- str_replace_all(sqlTemplate, 'xxxx', nativeTable)
            sql2 <- str_replace_all(sql1, 'yyyy', nativeProp)

            fdf =  doQueryFromSALI(sql2)
            head(fdf)

            oOutDF <- generateResponseDF(OrgName, DataSet, paste0( 'QLD_', fdf$PROJECT_CODE, '_', fdf$SITE_ID, '_', fdf$OBS_NO ), fdf$HORIZON_NO , fdf$OBS_DATE , fdf$LONGITUDE, fdf$LATITUDE ,
                                         fdf$UPPER_DEPTH , fdf$LOWER_DEPTH , propertyType, ObsProp, fdf[, 11] , 'NA')
            lodfs[[i]] <- oOutDF

          }else{

            sqlTemplate <- paste0('SELECT SIT.PROJECT_CODE, OBS.SITE_ID, OBS.OBS_NO, OBS.OBS_DATE, OLC.DATUM, OLC.LONGITUDE, OLC.LATITUDE, HOR.HORIZON_NO, xxxx.yyyy, "NA" AS UPPER_DEPTH, "NA" AS LOWER_DEPTH
                                    FROM ((SIT INNER JOIN OBS ON (SIT.SITE_ID = OBS.SITE_ID) AND (SIT.PROJECT_CODE = OBS.PROJECT_CODE)) INNER JOIN OLC ON (OBS.OBS_NO = OLC.OBS_NO) AND (OBS.SITE_ID = OLC.SITE_ID) AND (OBS.PROJECT_CODE = OLC.PROJECT_CODE)) INNER JOIN xxxx ON (OBS.OBS_NO = xxxx.OBS_NO) AND (OBS.SITE_ID = xxxx.SITE_ID) AND (OBS.PROJECT_CODE = xxxx.PROJECT_CODE)
                                    WHERE (((OLC.DATUM)="3")) COLLATE NOCASE')

            sql1 <- str_replace_all(sqlTemplate, 'xxxx', nativeTable)
            sql2 <- str_replace_all(sql1, 'yyyy', nativeProp)

            fdf =  doQueryFromSALI(sql2)
            head(fdf)

            oOutDF <- generateResponseDF(OrgName, DataSet, paste0( 'QLD_', fdf$PROJECT_CODE, '_', fdf$SITE_ID, '_', fdf$OBS_NO ), fdf$HORIZON_NO , fdf$OBS_DATE , fdf$LONGITUDE, fdf$LATITUDE ,
                                         fdf$UPPER_DEPTH , fdf$LOWER_DEPTH , propertyType, ObsProp, fdf[, 11] , 'NA')
            lodfs[[i]] <- oOutDF
          }
        }else if(tabLev == 2){
          # Observations table only

          if(str_to_upper(nativeTable) == "OBS"){
          sqlTemplate <- paste0('SELECT SIT.PROJECT_CODE, OBS.SITE_ID, OBS.OBS_NO, OBS.OBS_DATE, OLC.DATUM, OLC.LONGITUDE, OLC.LATITUDE, xxxx.yyyy
                                  FROM (SIT INNER JOIN OBS ON (SIT.SITE_ID = OBS.SITE_ID) AND (SIT.PROJECT_CODE = OBS.PROJECT_CODE)) INNER JOIN OLC ON (OBS.OBS_NO = OLC.OBS_NO) AND (OBS.SITE_ID = OLC.SITE_ID) AND (OBS.PROJECT_CODE = OLC.PROJECT_CODE)
                                  WHERE (((OLC.DATUM)="3") AND ((xxxx.yyyy) Is Not Null)) COLLATE NOCASE')
          }else{
            sqlTemplate <- paste0('SELECT SIT.PROJECT_CODE, OBS.SITE_ID, OBS.OBS_NO, OBS.OBS_DATE, OLC.DATUM, OLC.LONGITUDE, OLC.LATITUDE, xxxx.yyyy
                                  FROM ((SIT INNER JOIN OBS ON (SIT.SITE_ID = OBS.SITE_ID) AND (SIT.PROJECT_CODE = OBS.PROJECT_CODE)) INNER JOIN OLC ON (OBS.OBS_NO = OLC.OBS_NO) AND (OBS.SITE_ID = OLC.SITE_ID) AND (OBS.PROJECT_CODE = OLC.PROJECT_CODE)) INNER JOIN xxxx ON (OBS.OBS_NO = xxxx.OBS_NO) AND (OBS.SITE_ID = xxxx.SITE_ID) AND (OBS.PROJECT_CODE = xxxx.PROJECT_CODE)
                                  WHERE (((OLC.DATUM)="3") AND ((xxxx.yyyy) Is Not Null)) COLLATE NOCASE;
                                  ')
          }

          sql1 <- str_replace_all(sqlTemplate, 'xxxx', nativeTable)
          sql2 <- str_replace_all(sql1, 'yyyy', nativeProp)

          fdf =  doQueryFromSALI(sql2)

          head(fdf)

          oOutDF <- generateResponseDF(OrgName, DataSet, paste0( 'QLD_', fdf$PROJECT_CODE, '_', fdf$SITE_ID, '_', fdf$OBS_NO ), '1' , fdf$OBS_DATE , fdf$LONGITUDE, fdf$LATITUDE ,
                                       'NA' , 'NA' , propertyType, ObsProp, fdf[, 8] , 'NA')
          lodfs[[i]] <- oOutDF


        }else if(tabLev == 1){

          if(str_to_upper(nativeTable) == "SIT"){
            sqlTemplate <- paste0('SELECT SIT.PROJECT_CODE, OBS.SITE_ID, OBS.OBS_NO, OBS.OBS_DATE, OLC.DATUM, OLC.LONGITUDE, OLC.LATITUDE, xxxx.yyyy
                                    FROM (SIT INNER JOIN OBS ON (SIT.SITE_ID = OBS.SITE_ID) AND (SIT.PROJECT_CODE = OBS.PROJECT_CODE)) INNER JOIN OLC ON (OBS.OBS_NO = OLC.OBS_NO) AND (OBS.SITE_ID = OLC.SITE_ID) AND (OBS.PROJECT_CODE = OLC.PROJECT_CODE)
                                    WHERE (((OLC.DATUM)="3") AND ((SIT.SLOPE_PCT) Is Not Null)) COLLATE NOCASE')
          }else{
            sqlTemplate <- paste0('SELECT SIT.PROJECT_CODE, OBS.SITE_ID, OBS.OBS_NO, OBS.OBS_DATE, OLC.DATUM, OLC.LONGITUDE, OLC.LATITUDE, [xxxx].yyyy
                                    FROM ((SIT INNER JOIN OBS ON (SIT.SITE_ID = OBS.SITE_ID) AND (SIT.PROJECT_CODE = OBS.PROJECT_CODE)) INNER JOIN OLC ON (OBS.OBS_NO = OLC.OBS_NO) AND (OBS.SITE_ID = OLC.SITE_ID) AND (OBS.PROJECT_CODE = OLC.PROJECT_CODE)) INNER JOIN [xxxx] ON (SIT.SITE_ID = [xxxx].SITE_ID) AND (SIT.PROJECT_CODE = [xxxx].PROJECT_CODE)
                                    WHERE (((OLC.DATUM)="3") AND (([xxxx].yyyy) Is Not Null)) COLLATE NOCASE;
                                  ')
          }

          sql1 <- str_replace_all(sqlTemplate, 'xxxx', nativeTable)
          sql2 <- str_replace_all(sql1, 'yyyy', nativeProp)

          fdf =  doQueryFromSALI(sql2)

          head(fdf)

          oOutDF <- generateResponseDF(OrgName, DataSet, paste0( 'QLD_', fdf$PROJECT_CODE, '_', fdf$SITE_ID, '_', fdf$OBS_NO ), '1' , fdf$OBS_DATE , fdf$LONGITUDE, fdf$LATITUDE ,
                                       'NA' , 'NA' , propertyType, ObsProp, fdf[, 8] , 'NA')
          lodfs[[i]] <- oOutDF


        }else{
          return(blankResponseDF())
        }

      }
    }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  return(outDF)
}



