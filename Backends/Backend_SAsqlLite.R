library(DBI)
library(odbc)



machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  TernDBPath <- '/OSM/CBR/LW_SOILDATAREPO/work/TERNLandscapes/SoilsFederator/HostedDBs'
}else{
  TernDBPath <<- 'C:/Projects/TernLandscapes/Site Data/HostedDBs'
}

SA_dbPath <- paste0(TernDBPath, '/SASoils.db')

doSAHostedQuery <- function(sql){
  conn <- DBI::dbConnect(RSQLite::SQLite(), SA_dbPath)
  qry <- dbSendQuery(conn, sql)
  res <- dbFetch(qry)
  dbClearResult(qry)
  dbDisconnect(conn)
  return(res)
}

getLocationData_SADB <- function(DataSet){

  #conSA <- dbConnect(RSQLite::SQLite(), SA_dbPath)

  OrgName <- getOrgName(DataSet)

  sql <- paste0("SELECT observations.site_id, observations.id, observations.obs_id, observations.longitude, observations.latitude, observations.date_described FROM observations;")

  fdf = doSAHostedQuery(sql)

  if(nrow(fdf) > 0){

    rd <- fdf$date_described
    date <- as.Date(rd, format = "%d/%b/%Y")
    odate <- format(date, format="%d-%m-%Y")

    oOutDF <-  generateResponseAllLocs( DataSet, paste0(fdf$site_id , '_', fdf$id , '_', fdf$obs_id), fdf$longitude, fdf$latitude , odate )
    return(oOutDF)
  }
}

getData_SADB <- function(DataSet=NULL, observedProperty=NULL, observedPropertyGroup=NULL){


  OrgName <- getOrgName(DataSet)
  propRecs <- getNativeProperties(DataSet=DataSet, observedProperty, observedPropertyGroup)

  if(nrow(propRecs) == 0){
    return(blankResponseDF())
  }

  lodfs <- vector("list", nrow(propRecs))

  for (i in 1:nrow(propRecs)) {

    nProp <- propRecs$nativeProp[i]
    sProp <- propRecs$standardProp[i]

    propertyType <- propRecs$propertyType[i]
    units <- getUnits(propertyType = propertyType, prop = sProp)
    if(length(units) == 0){units = NA}


    if(propertyType==PropertyTypes$LaboratoryMeasurement){

      sql <- paste0( "SELECT observations.id, observations.site_id, horizons.horizon_id, observations.longitude, observations.latitude, observations.date_described, labresults.upper_depth, labresults.lower_depth, labresults.method_name, labresults.lab_result_value
                      FROM observations INNER JOIN (horizons INNER JOIN labresults ON horizons.horizon_id = labresults.horizon_id) ON observations.id = horizons.id
                      WHERE (((labresults.method_name)='", nProp, "'))
                      ORDER BY observations.id, horizons.horizon_id, labresults.upper_depth;
                      ")

      fdf = doSAHostedQuery(sql)

      if(nrow(fdf) > 0){
        rd <- fdf$date_described
        date <- as.Date(rd, format = "%d/%b/%Y")
        odate <- format(date, format="%d-%m-%Y")
        oOutDF <- generateResponseDF(DataSet, paste0(fdf$id, '_', fdf$site_id), fdf$horizon_id, odate, fdf$longitude, fdf$longitude, (fdf$`upper_depth:1` * 0.01) , (fdf$`lower_depth:1` * 0.01), propertyType, sProp,fdf$lab_result_value, units)
        lodfs[[i]] <- oOutDF
      }else{
        lodfs[[i]] <- blankResponseDF()
      }


    }else{

      tabName <- SAMorphMappings[SAMorphMappings$ObservedProperty == sProp,]$TableName
      tabLev <- as.numeric(SAMorphMappings[SAMorphMappings$ObservedProperty == sProp,]$TabLev)

      if(tabLev == 4){

        sqlTemplate <- paste0("SELECT observations.id, observations.site_id, observations.longitude, observations.latitude, observations.date_described, horizons.horizon_id, horizons.upper_depth, horizons.lower_depth,xxxx.yyyy
        FROM (observations LEFT JOIN horizons ON observations.id = horizons.id) LEFT JOIN xxxx ON horizons.horizon_id = xxxx.horizon_id
        WHERE (((xxxx.yyyy)<>''))
        ORDER BY observations.id, horizons.horizon_id;")

        sql1 <- str_replace_all(sqlTemplate, 'xxxx', tabName)
        sql2 <- str_replace_all(sql1, 'yyyy', nProp)

        fdf = doSAHostedQuery(sql2)
        rd <- fdf$date_described
        date <- as.Date(rd, format = "%d/%b/%Y")
        odate <- format(date, format="%d-%m-%Y")
        oOutDF <- generateResponseDF(DataSet, paste0(fdf$id, '_', fdf$site_id), fdf$horizon_id, odate, fdf$longitude, fdf$longitude, (fdf$upper_depth * 0.01) , (fdf$lower_depth * 0.01), propertyType, sProp, fdf[, 9], units)

        idxs <- which(oOutDF$Value != '')
        lodfs[[i]] <- oOutDF[idxs,]


      }else if(tabLev == 3){


        # Horizons table only

        sqlTemplate <- paste0("SELECT observations.id, observations.site_id, observations.longitude, observations.latitude, observations.date_described, horizons.horizon_id, horizons.upper_depth, horizons.lower_depth, horizons.texture
                                  FROM observations LEFT JOIN horizons ON observations.id = horizons.id
                                  WHERE (((horizons.yyyy)<>''))
                                  ORDER BY observations.id, horizons.horizon_id, horizons.upper_depth;")

          sql2 <- str_replace_all(sqlTemplate, 'yyyy', nProp)

          fdf = doSAHostedQuery(sql2)
          rd <- fdf$date_described
          date <- as.Date(rd, format = "%d/%b/%Y")
          odate <- format(date, format="%d-%m-%Y")
          oOutDF <- generateResponseDF(DataSet, paste0(fdf$id, '_', fdf$site_id), fdf$horizon_id, odate, fdf$longitude, fdf$longitude, (fdf$upper_depth * 0.01) , (fdf$lower_depth * 0.01), propertyType, sProp, fdf[, 9], units)

          idxs <- which(oOutDF$Value != '')
          lodfs[[i]] <- oOutDF[idxs,]


      }else if(tabLev == 2){
        # Observations table only - didn't worry about implementing the landuse table as it is the only other level 2 table

        sqlTemplate <- paste0("SELECT observations.id, observations.site_id, observations.longitude, observations.latitude, observations.date_described, observations.yyyy
                                FROM observations
                                ORDER BY observations.id;
                                ")

        sql2 <- str_replace_all(sqlTemplate, 'yyyy', nProp)

        fdf = doSAHostedQuery(sql2)
        rd <- fdf$date_described
        date <- as.Date(rd, format = "%d/%b/%Y")
        odate <- format(date, format="%d-%m-%Y")
        oOutDF <- generateResponseDF(DataSet, paste0(fdf$id, '_', fdf$site_id), '1', odate, fdf$longitude, fdf$longitude,  'None' , 'None' , propertyType, sProp, fdf[, 6], units)

        idxs <- which(oOutDF$Value != '')
        lodfs[[i]] <- oOutDF[idxs,]

      }else if(tabLev == 1){

        sqlTemplate <- paste0("SELECT observations.id, observations.site_id, observations.longitude, observations.latitude, observations.date_described, sites.yyyy
        FROM sites INNER JOIN observations ON sites.site_id = observations.site_id
        ORDER BY observations.id;;
                                ")

        sql2 <- str_replace_all(sqlTemplate, 'yyyy', nProp)

        fdf = doSAHostedQuery(sql2)
        rd <- fdf$date_described
        date <- as.Date(rd, format = "%d/%b/%Y")
        odate <- format(date, format="%d-%m-%Y")
        oOutDF <- generateResponseDF(DataSet, paste0(fdf$id, '_', fdf$site_id), '1', odate, fdf$longitude, fdf$longitude,  'None' , 'None' , propertyType, sProp, fdf[, 6], units)

        idxs <- which(oOutDF$Value != '')
        lodfs[[i]] <- oOutDF[idxs,]
      }else{
        return(blankResponseDF())
      }
    }

  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  return(outDF)

}




#dput(inDF)

SAMorphMappings <- structure(list(Dataset = c("SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment", "SAGovernment", "SAGovernment", "SAGovernment",
                                              "SAGovernment"), TableName = c("coarsefrags", "coarsefrags",
                                                                             "coarsefrags", "coarsefrags", "horizons", "horizons", "horizons",
                                                                             "horizons", "horizons", "horizons", "horizons", "horizons", "horizons",
                                                                             "horizons", "horizons", "horizons", "horizons", "horizons", "horizons",
                                                                             "horizons", "horizons", "horizons", "horizons", "horizons", "horizons",
                                                                             "landusehistory", "landusehistory", "landusehistory", "landusehistory",
                                                                             "mottles", "mottles", "mottles", "observations", "observations",
                                                                             "observations", "observations", "observations", "observations",
                                                                             "observations", "observations", "observations", "observations",
                                                                             "observations", "segregations", "segregations", "segregations",
                                                                             "segregations", "segregations", "sites", "sites", "sites", "sites",
                                                                             "sites", "sites", "sites", "sites", "sites", "sites", "sites",
                                                                             "sites", "sites", "sites", "sites", "sites"), ObservedProperty = c("CF_NO",
                                                                                                                                                "CF_ABUN", "CF_LITH", "CF_SIZE", "H_NO", "material_no", "H_LOWER_DEPTH",
                                                                                                                                                "H_UPPER_DEPTH", "H_TEXTURE", "H_DESIG_MASTER", "H_BOUND_DISTINCT",
                                                                                                                                                "COL_NO", "COL_HUE_VAL_CHROM", "COL_MOISTURE_STAT", "colour_description",
                                                                                                                                                "PAN_TYPE", "PAN_CEMENTATION", "PAN_STRUCTURE", "H_SOIL_WATER_STAT",
                                                                                                                                                "STRG_CLASS", "H_WATER_REPELLENCE", "ROOT_ABUN", "effervesence",
                                                                                                                                                "carbonate_class", "PH_VALUE", "LUSE_NO", "LAND_USE", "LUSE_DATE",
                                                                                                                                                "end_use_date", "MOTT_NO", "MOTT_TYPE", "MOTT_HUE_VAL_CHROM",
                                                                                                                                                "O_TYPE", "O_NATURE", "O_DESC_BY", "O_DATE_DESC", "date_confidence",
                                                                                                                                                "O_LATITUDE_GDA94", "O_LONGITUDE_GDA94", "location_method", "O_NOTES",
                                                                                                                                                "RO_ABUN", "RO_LITH", "SEG_NO", "SEG_ABUN", "SEG_NATURE", "SEG_FORM",
                                                                                                                                                "SEG_SIZE", "S_TYPE", "site_type_desc", "scs_no", "privacy",
                                                                                                                                                "project", "data_confidence", "reason", "site_contact", "S_NOTES",
                                                                                                                                                "S_MAP_SHEET_NO", "S_RAINFALL", "S_SLOPE", "S_MORPH_TYPE", "S_ELEM_TYPE",
                                                                                                                                                "S_PATT_TYPE", "S_REL_MS_CLASS"), OrigPropertyCode = c("coarse_frag_no",
                                                                                                                                                                                                       "abundance", "lithology", "size", "horizon_no", "material_no",
                                                                                                                                                                                                       "lower_depth", "upper_depth", "texture", "horizon", "boundary_distnctiveness",
                                                                                                                                                                                                       "colour_no", "munsell_colour", "moisture_status", "colour_description",
                                                                                                                                                                                                       "pan_type", "pan_cementation", "pan_structure", "water_status",
                                                                                                                                                                                                       "strength", "water_repellence", "root_abundance", "effervesence",
                                                                                                                                                                                                       "carbonate_class", "ph", "use_no", "use", "use_date", "end_use_date",
                                                                                                                                                                                                       "mottle_no", "mottle_type", "munsell_colour", "obs_type", "nature",
                                                                                                                                                                                                       "described_by", "date_described", "date_confidence", "latitude",
                                                                                                                                                                                                       "longitude", "location_method", "notes", "rock_outcrop_abundance",
                                                                                                                                                                                                       "rock_outcrop_lithology", "seg_no", "abundance", "nature", "form",
                                                                                                                                                                                                       "size", "site_type_id", "site_type_desc", "scs_no", "privacy",
                                                                                                                                                                                                       "project", "data_confidence", "reason", "site_contact", "notes",
                                                                                                                                                                                                       "map_sheet_no", "rainfall", "slope____", "morph_type", "element_type",
                                                                                                                                                                                                       "pattern_type", "pattern_modal_slope_class"), DataType = c("L",
                                                                                                                                                                                                                                                                  "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
                                                                                                                                                                                                                                                                  "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
                                                                                                                                                                                                                                                                  "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
                                                                                                                                                                                                                                                                  "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
                                                                                                                                                                                                                                                                  "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L"), StandardCode = c(TRUE,
                                                                                                                                                                                                                                                                                                                                           TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
                                                                                                                                                                                                                                                                                                                                           TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                                                                                                                                                                                                                                                                                                                                           TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
                                                                                                                                                                                                                                                                                                                                           TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                                                                                                                                                                                                                                                                                                                                           TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
                                                                                                                                                                                                                                                                                                                                           FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                                                                                                                                                                                                                                                                                                                                           TRUE, TRUE), TabLev = c(4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 3L,
                                                                                                                                                                                                                                                                                                                                                                   3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L,
                                                                                                                                                                                                                                                                                                                                                                   2L, 2L, 2L, 4L, 4L, 4L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                                                                                                                                                                                                                                                                                                                                                   2L, 4L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                                                                                                                                                                                                                                                                                                                                   1L, 1L, 1L, 1L, 1L, 1L)), class = "data.frame", row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                 -64L))
