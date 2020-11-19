library(DBI)
library(odbc)



doQuery <- function(con, sql){
  res <- dbSendQuery(con, sql)
  rows <- dbFetch(res)
  dbClearResult(res)
  return(rows)
}


getLocationData_TasGov <- function(DataSet){

  tcon <- DBI::dbConnect(odbc::odbc(),
                         Driver   = "ODBC Driver 17 for SQL Server",
                         Server   = "asris-sql-stage.it.csiro.au\\sql2017",
                         Database = "tasmania_json_services",
                         UID      = 'rosssearle',
                         PWD      = 'Ads@2*&5cv'


  )


  OrgName <- getOrgName(DataSet)

  sql <- paste0("SELECT soil_site.PROJECT, soil_site.SITE_ID, soil_site.EASTING, soil_site.NORTHING, soil_site.NVA_ID, soil_observation.OBSERVATION_DATE
                 FROM soil_site INNER JOIN
                soil_observation ON soil_site.PROJECT = soil_observation.PROJECT AND soil_site.SITE_ID = soil_observation.SITE_ID")

  fdf = doQuery(tcon, sql)

  if(nrow(fdf) > 0){

    rd <- fdf$OBSERVATION_DATE
    date <- as.Date(rd, format = "%d-%b-%Y")
    odate <- format(date, format="%d-%m-%Y")
    c4326 <- projectTasCoords(e=fdf$EASTING, n=fdf$NORTHING)

    oOutDF <-  generateResponseAllLocs( DataSet, paste0(fdf$PROJECT, '_', fdf$SITE_ID), c4326$X, c4326$Y , odate )
    return(oOutDF)
  }
}

getData_TasGov <- function(DataSet=NULL, observedProperty=NULL, observedPropertyGroup=NULL){

  tcon <- DBI::dbConnect(odbc::odbc(),
                         Driver   = "ODBC Driver 17 for SQL Server",
                         Server   = "asris-sql-stage.it.csiro.au\\sql2017",
                         Database = "tasmania_json_services",
                         UID      = 'rosssearle',
                         PWD      = 'Ads@2*&5cv')

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

    sql <- paste0( "SELECT soil_site.PROJECT, soil_site.SITE_ID, soil_site.EASTING, soil_site.NORTHING, soil_observation.OBSERVATION_DATE,  soil_layer.LAYER_NUMBER, soil_layer.UPPER_DEPTH, soil_layer.LOWER_DEPTH, soil_lab_result.LAB_METHOD, soil_lab_result.VALUE
                    FROM soil_site INNER JOIN
                         soil_observation ON soil_site.PROJECT = soil_observation.PROJECT AND soil_site.SITE_ID = soil_observation.SITE_ID INNER JOIN
                         soil_horizon ON soil_observation.SITE_ID = soil_horizon.SITE_ID AND soil_observation.PROJECT = soil_horizon.PROJECT AND soil_observation.OBSERVATION_NUMBER = soil_horizon.OBSERVATION_NUMBER INNER JOIN
                         soil_layer ON soil_horizon.PROJECT = soil_layer.PROJECT AND soil_horizon.SITE_ID = soil_layer.SITE_ID AND soil_horizon.OBSERVATION_NUMBER = soil_layer.OBSERVATION_NUMBER AND
                         soil_horizon.HORIZON_NUMBER = soil_layer.HORIZON_NUMBER INNER JOIN
                         soil_lab_result ON soil_layer.PROJECT = soil_lab_result.PROJECT AND soil_layer.SITE_ID = soil_lab_result.SITE_ID AND soil_layer.OBSERVATION_NUMBER = soil_lab_result.OBSERVATION_NUMBER AND
                         soil_layer.LAYER_NUMBER = soil_lab_result.LAYER_NUMBER
                          WHERE soil_lab_result.LAB_METHOD = '", nProp, "'")

    fdf = doQuery(tcon, sql)



    if(nrow(fdf) > 0){
      fdf <- cleanTasDepths(fdf)

      rd <- fdf$OBSERVATION_DATE
      date <- as.Date(rd, format = "%d-%b-%Y")
      odate <- format(date, format="%d-%m-%Y")
      c4326 <- projectTasCoords(e=fdf$EASTING, n=fdf$NORTHING)
      oOutDF <- generateResponseDF(DataSet, paste0(fdf$PROJECT, '_', fdf$SITE_ID), fdf$LAYER_NUMBER, fdf$LAYER_NUMBER, odate, c4326$X, c4326$Y , (as.numeric(fdf$UPPER_DEPTH) * 0.01) , (as.numeric(fdf$LOWER_DEPTH) * 0.01) , propertyType, sProp,fdf$VALUE , units)
      lodfs[[i]] <- oOutDF
    }else{
      lodfs[[i]] <- blankResponseDF()
    }


  }else{

    tabName <- tasMorphMappings[tasMorphMappings$StdCode == sProp,]$Table
    tabLev <- as.numeric(tasMorphMappings[tasMorphMappings$StdCode == sProp,]$TabLev)

    if(tabLev == 4){

      sqlTemplate <- paste0('SELECT soil_site.PROJECT, soil_site.SITE_ID, soil_horizon.HORIZON_NUMBER, soil_observation.OBSERVATION_DATE, soil_site.EASTING, soil_site.NORTHING, soil_horizon.UPPER_DEPTH, soil_horizon.LOWER_DEPTH, xxxx.yyyy
                            FROM soil_site INNER JOIN
                         soil_observation ON soil_site.PROJECT = soil_observation.PROJECT AND soil_site.SITE_ID = soil_observation.SITE_ID INNER JOIN
                         soil_horizon ON soil_observation.SITE_ID = soil_horizon.SITE_ID AND soil_observation.PROJECT = soil_horizon.PROJECT AND soil_observation.OBSERVATION_NUMBER = soil_horizon.OBSERVATION_NUMBER INNER JOIN
                         xxxx ON soil_horizon.PROJECT = xxxx.PROJECT AND soil_horizon.SITE_ID = xxxx.SITE_ID AND soil_horizon.OBSERVATION_NUMBER = xxxx.OBSERVATION_NUMBER AND soil_horizon.HORIZON_NUMBER = xxxx.HORIZON_NUMBER;
                      ')

      sql1 <- str_replace_all(sqlTemplate, 'xxxx', tabName)
      sql2 <- str_replace_all(sql1, 'yyyy', nProp)

      fdf = doQuery(tcon, sql2)

      fdf <- cleanTasDepths(fdf)

      rd <- fdf$OBSERVATION_DATE
      date <- as.Date(rd, format = "%d-%b-%Y")
      odate <- format(date, format="%d-%m-%Y")
      c4326 <- projectTasCoords(e=fdf$EASTING, n=fdf$NORTHING)

      oOutDF <- generateResponseDF( DataSet, paste0(fdf$PROJECT, '_', fdf$SITE_ID), fdf$HORIZON_NUMBER, 1 , odate,
                                    c4326$X, c4326$Y , (as.numeric(fdf$UPPER_DEPTH) * 0.01) , (as.numeric(fdf$LOWER_DEPTH) * 0.01) , propertyType, sProp, fdf[, 8] , 'None')
      idxs <- which(oOutDF$Value != '')
      lodfs[[i]] <- oOutDF[idxs,]


    }else if(tabLev == 3){


      # Horizons table only
      if(str_to_upper(tabName) == "SOIL_HORIZON"){
        sqlTemplate <- paste0('SELECT soil_site.PROJECT, soil_site.SITE_ID, soil_horizon.HORIZON_NUMBER, soil_observation.OBSERVATION_DATE, soil_site.EASTING, soil_site.NORTHING, soil_horizon.UPPER_DEPTH, soil_horizon.LOWER_DEPTH, soil_horizon.yyyy
                              FROM soil_site INNER JOIN soil_observation ON soil_site.PROJECT = soil_observation.PROJECT AND soil_site.SITE_ID = soil_observation.SITE_ID INNER JOIN
                              soil_horizon ON soil_observation.SITE_ID = soil_horizon.SITE_ID AND soil_observation.PROJECT = soil_horizon.PROJECT AND soil_observation.OBSERVATION_NUMBER = soil_horizon.OBSERVATION_NUMBER')

        sql2 <- str_replace_all(sqlTemplate, 'yyyy', nProp)

        fdf = doQuery(tcon, sql2)


        fdf <- cleanTasDepths(fdf)

        rd <- fdf$OBSERVATION_DATE
        date <- as.Date(rd, format = "%d-%b-%Y")
        odate <- format(date, format="%d-%m-%Y")

        c4326 <- projectTasCoords(e=fdf$EASTING, n=fdf$NORTHING)

        oOutDF <- generateResponseDF( DataSet, paste0(fdf$PROJECT, '_', fdf$SITE_ID), fdf$HORIZON_NUMBER, 1 , odate,
                                      c4326$X, c4326$Y , (as.numeric(fdf$UPPER_DEPTH) * 0.01) , (as.numeric(fdf$LOWER_DEPTH) * 0.01) , propertyType, sProp, fdf[, 8] , 'None')
        idxs <- which(oOutDF$Value != '')
        lodfs[[i]] <- oOutDF[idxs,]

      }else{
        # All other tables at this level

        sqlTemplate <- paste0('SELECT soil_site.PROJECT, soil_site.SITE_ID, soil_horizon.HORIZON_NUMBER, soil_observation.OBSERVATION_DATE, soil_site.EASTING, soil_site.NORTHING, soil_surface_coarse_fragment.yyyy
                        FROM soil_site INNER JOIN
                         soil_observation ON soil_site.PROJECT = soil_observation.PROJECT AND soil_site.SITE_ID = soil_observation.SITE_ID INNER JOIN
                         soil_surface_coarse_fragment ON soil_observation.PROJECT = soil_surface_coarse_fragment.PROJECT AND soil_observation.SITE_ID = soil_surface_coarse_fragment.SITE_ID AND
                         soil_observation.OBSERVATION_NUMBER = soil_surface_coarse_fragment.OBSERVATION_NUMBER')


        sql2 <- str_replace_all(sqlTemplate, 'yyyy', nProp)

        fdf = doQuery(tcon, sql2)
        rd <- fdf$OBSERVATION_DATE
        date <- as.Date(rd, format = "%d-%b-%Y")
        odate <- format(date, format="%d-%m-%Y")
        c4326 <- projectTasCoords(e=fdf$EASTING, n=fdf$NORTHING)

        oOutDF <- generateResponseDF( DataSet, paste0(fdf$PROJECT, '_', fdf$SITE_ID), fdf$HORIZON_NUMBER, 1 , odate, c4326$X, c4326$Y , 'None' , 'None' , propertyType, sProp, fdf[, 6] , 'None')
        idxs <- which(oOutDF$Value != '')
        lodfs[[i]] <- oOutDF[idxs,]
      }
    }else if(tabLev == 2){
      # Observations table only

        sqlTemplate <- 'SELECT soil_site.PROJECT, soil_site.SITE_ID, soil_site.EASTING, soil_site.NORTHING, soil_observation.OBSERVATION_DATE, soil_observation.yyyy
              FROM soil_site INNER JOIN soil_observation ON  soil_site.PROJECT = soil_observation.PROJECT AND soil_site.SITE_ID = soil_observation.SITE_ID
              ORDER BY soil_site.PROJECT, soil_site.SITE_ID;'

      sql2 <- str_replace_all(sqlTemplate, 'yyyy', nProp)

      fdf = doQuery(tcon, sql2)

      rd <- fdf$OBSERVATION_DATE
      date <- as.Date(rd, format = "%d-%b-%Y")
      odate <- format(date, format="%d-%m-%Y")
      c4326 <- projectTasCoords(e=fdf$EASTING, n=fdf$NORTHING)

      oOutDF <- generateResponseDF( DataSet, paste0(fdf$PROJECT, '_', fdf$SITE_ID),'NA', 'NA' , odate,
                                    c4326$X, c4326$Y , 'None' , 'None' , propertyType, sProp, fdf[, 6] , 'None')
      idxs <- which(oOutDF$Value != '')
      lodfs[[i]] <- oOutDF[idxs,]


    }else if(tabLev == 1){

      sqlTemplate <- 'SELECT soil_site.PROJECT, soil_site.SITE_ID, soil_site.EASTING, soil_site.NORTHING, soil_observation.OBSERVATION_DATE, soil_site.yyyy
              FROM soil_site INNER JOIN soil_observation ON  soil_site.PROJECT = soil_observation.PROJECT AND soil_site.SITE_ID = soil_observation.SITE_ID
              ORDER BY soil_site.PROJECT, soil_site.SITE_ID;'

      sql2 <- str_replace_all(sqlTemplate, 'yyyy', nProp)

      fdf = doQuery(tcon, sql2)
      rd <- fdf$OBSERVATION_DATE
      date <- as.Date(rd, format = "%d-%b-%Y")
      odate <- format(date, format="%d-%m-%Y")

      c4326 <- projectTasCoords(e=fdf$EASTING, n=fdf$NORTHING)

      oOutDF <- generateResponseDF( DataSet, paste0(fdf$PROJECT, '_', fdf$SITE_ID),'NA', 'NA' , odate,
                                    c4326$X, c4326$Y , 'None' , 'None' , propertyType, sProp, fdf[, 6] , 'None')
      idxs <- which(oOutDF$Value != '')
      lodfs[[i]] <- oOutDF[idxs,]


    }else{
      return(blankResponseDF())
    }
  }

  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))
  dbDisconnect(tcon)
  return(outDF)

}

cleanTasDepths <- function(fdf){


  suppressWarnings( fdf$UPPER_DEPTH <- as.numeric(fdf$UPPER_DEPTH))
  suppressWarnings( fdf$LOWER_DEPTH<- as.numeric(fdf$LOWER_DEPTH))
  u1 <-  which(is.na(fdf$UPPER_DEPTH))
  u2 <-  which(is.na(fdf$LOWER_DEPTH))
  idxs <- unique(u1, u2)
  if(length(idxs)>0){
  fdf <- fdf[-idxs,]
  }
  return(fdf)
}

projectTasCoords <- function(e,n){
  sdf <- data.frame(e=e, n=n)
  df.SP <- st_as_sf(sdf, coords = c("e", "n"), na.fail=F, remove=F, crs=28355)
  projCoords <- st_transform(df.SP ,  crs = 4326)
  xys <- as.data.frame(st_coordinates(projCoords))
  return(xys)
}




#dput(inDF)

tasMorphMappings <- structure(list(Table = c("soil_coarse_fragment", "soil_coarse_fragment",
                                             "soil_coarse_fragment", "soil_coarse_fragment", "soil_coarse_fragment",
                                             "soil_coarse_fragment", "soil_coarse_fragment", "soil_colour",
                                             "soil_colour", "soil_colour", "soil_crack", "soil_crack", "soil_cutan",
                                             "soil_cutan", "soil_cutan", "soil_cutan", "soil_fabric", "soil_horizon",
                                             "soil_horizon", "soil_horizon", "soil_horizon", "soil_horizon",
                                             "soil_horizon", "soil_horizon", "soil_horizon", "soil_horizon",
                                             "soil_horizon", "soil_horizon", "soil_horizon", "soil_horizon",
                                             "soil_horizon", "soil_horizon", "soil_horizon", "soil_horizon",
                                             "soil_horizon", "soil_horizon", "soil_horizon", "soil_mottle",
                                             "soil_mottle", "soil_mottle", "soil_mottle", "soil_mottle", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_observation", "soil_observation",
                                             "soil_observation", "soil_observation", "soil_pan", "soil_pan",
                                             "soil_pan", "soil_pan", "soil_ph", "soil_ph", "soil_pore", "soil_pore",
                                             "soil_rock_outcrop", "soil_rock_outcrop", "soil_root", "soil_root",
                                             "soil_segregation", "soil_segregation", "soil_segregation", "soil_segregation",
                                             "soil_site", "soil_site", "soil_site", "soil_site", "soil_site",
                                             "soil_site", "soil_site", "soil_site", "soil_site", "soil_site",
                                             "soil_site", "soil_site", "soil_site", "soil_site", "soil_site",
                                             "soil_site", "soil_site", "soil_strength", "soil_strength", "soil_strength",
                                             "soil_structure", "soil_structure", "soil_structure", "soil_surface_coarse_fragment",
                                             "soil_surface_coarse_fragment", "soil_surface_coarse_fragment",
                                             "soil_surface_coarse_fragment"), Fld = c("OBSERVATION_NUMBER",
                                                                                      "ABUNDANCE", "SIZE", "SHAPE", "LITHOLOGY", "DISTRIBUTION", "STRENGTH",
                                                                                      "OBSERVATION_NUMBER", "COLOUR", "MOISTURE_STATUS", "OBSERVATION_NUMBER",
                                                                                      "WIDTH", "OBSERVATION_NUMBER", "TYPE", "ABUNDANCE", "CONTRAST",
                                                                                      "TYPE", "NOTES", "PREFIX", "MASTER_DESIGNATION", "SUB_DIVISION",
                                                                                      "SUFFIX", "UPPER_DEPTH", "LOWER_DEPTH", "BOUNDARY_DISTINCT",
                                                                                      "BOUNDARY_SHAPE", "WATER_STATUS", "VON_POST_HUMIFICATION", "TEXTURE",
                                                                                      "TEXTURE_QUALIFIER_FOR_CLAY_LOAMS_SOILS_AND_COARSER_OR_ORGANIC_RICH_SOILS",
                                                                                      "STICKINESS", "PLASTICITY_TYPE", "PLASTICITY_DEGREE", "DISPERSION",
                                                                                      "ELECTRICAL_CONDUCTIVITY", "CARBONATE_EFFERVESCENCE", "WATER_REPELLENCE",
                                                                                      "TYPE", "ABUNDANCE", "SIZE", "CONTRAST", "COLOUR", "OBSERVER_NAMES",
                                                                                      "OBSERVATION_DATE", "SOIL_OBSERVATION_TYPE", "RUNOFF", "PERMEABILITY",
                                                                                      "DRAINAGE", "ELEVATION", "ELEVATION_METHOD", "LAND_USE", "OBSERVATION_NOTES",
                                                                                      "LAND_CAPABILITY", "LAND_CAPABILITY_CLASS", "LAND_CAPABILITY_SUB_CLASS",
                                                                                      "LAND_CAPABILITY_UNIT", "ASC_ORDER", "ASC_SUB_ORDER", "ASC_GREAT_GROUP",
                                                                                      "ASC_SUB_GROUP", "ASC_CONFIDENCE", "ASC_A_OR_A1_HORIZON_THICKNESS",
                                                                                      "ASC_GRAVEL_OF_SURFACE_AND_A1HORIZON", "ASC_A1_HORIZON_TEXTURE",
                                                                                      "ASC_B_HORIZON_MAXIMUM_TEXTURE", "ASC_CLAY_CONTENT_VERTISOLS_ONLY",
                                                                                      "ASC_SOIL_DEPTH", "ASC_VERSION", "SOIL_NAME", "SLOPE_METHOD",
                                                                                      "ASPECT", "DISTURBANCE_TYPE", "CONDITION", "MICRORELIEF_TYPE",
                                                                                      "MICRORELIEF_PROPORTION", "MICRORELIEF_COMPONENT", "VERTICAL_INTERVAL",
                                                                                      "HORIZONTAL_INTERVAL", "AGGRADATION", "INUNDATION_FREQUENCY",
                                                                                      "INUNDATION_DURATION", "INUNDATION_DEPTH", "WATER_DEPTH", "VEGETATION_WETLAND",
                                                                                      "VEGETATION_RAINFOREST", "VEGETATION_HIGHER_ORDER_CLASSIFICATION",
                                                                                      "VEGETATION_NOTES", "SUBSTRATE_DEPTH", "SUBSTRATE_CONFIDENCE",
                                                                                      "SUBSTRATE_GRAIN_SIZE", "SUBSTRATE_TEXTURE", "SUBSTRATE_STRUCTURE",
                                                                                      "SUBSTRATE_STRENGTH", "SUBSTRATE_LITHOLOGY", "SUBSTRATE_ALTERATION",
                                                                                      "SUBSTRATE_GENETIC_TYPE", "RECORD_STATUS", "CEMENTATION", "TYPE",
                                                                                      "CONTINUITY", "STRUCTURE", "VALUE", "METHOD", "ABUNDANCE", "DIAMETER",
                                                                                      "ABUNDANCE", "LITHOLOGY", "ABUNDANCE", "SIZE", "ABUNDANCE", "NATURE",
                                                                                      "FORM", "SIZE", "SITE_TYPE", "COORDINATE_METHOD", "LOCATION_NOTES",
                                                                                      "NOTES", "NEAREST_TOWN", "RAINFALL", "GEOLOGICAL_MAP_UNIT", "ELEMENT_SLOPE_CLASS",
                                                                                      "ELEMENT_MORPHOLOGICAL_TYPE", "ELEMENT_SLOPE_INCLINATION", "ELEMENT_TYPE",
                                                                                      "GEOMORPHIC_MODE", "GEOMORPHIC_AGENT", "PATTERN_SLOPE_CLASS",
                                                                                      "PATTERN_RELIEF_CLASS", "RELIEF_SLOPE_CLASS", "PATTERN_TYPE",
                                                                                      "HORIZON_NUMBER", "CLASS", "MOISTURE_STATUS", "SIZE", "GRADE",
                                                                                      "TYPE", "ABUNDANCE", "SIZE", "SHAPE", "LITHOLOGY"), StdCode = c("CF_NO",
                                                                                                                                                      "CF_ABUN", "CF_SIZE", "CF_SHAPE", "CF_LITH", "CF_DISTRIBUTION",
                                                                                                                                                      "CF_STRENGTH", "COL_NO", "MOTT_HUE_VAL_CHROM", "COL_MOISTURE_STAT",
                                                                                                                                                      "CRACK_NO", "CRACK_WIDTH", "CUTAN_NO", "CUTAN_TYPE", "CUTAN_ABUN",
                                                                                                                                                      "CUTAN_DISTINCT", "FAB_TYPE", "HORIZON_NOTES", "H_DESIG_NUM_PREF",
                                                                                                                                                      "H_DESIG_MASTER", "H_DESIG_SUBDIV", "H_DESIG_SUFFIX", "H_UPPER_DEPTH",
                                                                                                                                                      "H_LOWER_DEPTH", "H_BOUND_DISTINCT", "H_BOUND_SHAPE", "H_SOIL_WATER_STAT",
                                                                                                                                                      "VON_POST_HUMIFICATION", "H_TEXTURE", "H_TEXTURE_QUAL", "H_STICKINESS",
                                                                                                                                                      "H_PLASTICITY_TYPE", "H_PLASTICITY_DEG", "DISPERSION", "ELECTRICAL_CONDUCTIVITY",
                                                                                                                                                      "H_CARBONATE_EFF", "H_WATER_REPELLENCE", "C_MOTT_TYPE", "N_MOTT_ABUN",
                                                                                                                                                      "N_MOTT_SIZE", "C_CONTRAST", "MOTT_COLOUR", "O_DESC_BY", "O_DATE_DESC",
                                                                                                                                                      "O_TYPE", "O_RUNOFF", "O_PERMEABILITY", "O_DRAINAGE", "O_ELEVATION",
                                                                                                                                                      "O_ELEVATION_EVAL", "O_LAND_USE", "O_NOTES", "LAND_CAPABILITY",
                                                                                                                                                      "LAND_CAPABILITY_CLASS", "LAND_CAPABILITY_SUB_CLASS", "LAND_CAPABILITY_UNIT",
                                                                                                                                                      "O_ASC_ORD", "O_ASC_SUBORD", "O_ASC_GG", "O_ASC_SUBG", "ASC_CONFIDENCE",
                                                                                                                                                      "O_ASC_FAM1", "O_ASC_FAM2", "O_ASC_FAM3", "O_ASC_FAM4", "ASC_CLAY_CONTENT_VERTISOLS_ONLY",
                                                                                                                                                      "O_ASC_FAM5", "O_ASC_TECH_REF", "O_MAP_UNIT_NAME", "S_SLOPE_EVAL",
                                                                                                                                                      "O_ASPECT", "O_SOIL_DISTURB", "SCON_STAT", "MR_TYPE", "MR_PROP_GILGAI",
                                                                                                                                                      "MR_BIOTIC_COMP", "MR_VERTICAL_INT", "MR_HORIZ_INT", "O_AGGRADATION",
                                                                                                                                                      "O_INUND_FREQ", "O_INUND_DUR", "O_INUND_DEPTH", "O_DEPTH_WATER",
                                                                                                                                                      "VEGETATION_WETLAND", "VEGETATION_RAINFOREST", "VEGETATION_HIGHER_ORDER_CLASSIFICATION",
                                                                                                                                                      "VEGETATION_NOTES", "O_SB_DEPTH", "O_SB_CONFIDENCE", "O_SB_GRAIN_SIZE",
                                                                                                                                                      "O_SB_TEXTURE", "O_SB_STRUCTURE", "O_SB_STRENGTH", "O_SB_LITH",
                                                                                                                                                      "O_SB_MASS_ALT", "O_SB_MASS_GEN_TYPE", "RECORD_STATUS", "PAN_CEMENTATION",
                                                                                                                                                      "PAN_TYPE", "PAN_CONTINUITY", "PAN_STRUCTURE", "PH_VALUE", "PH_METHOD",
                                                                                                                                                      "PORE_ABUN", "PORE_DIAMETER", "RO_ABUN", "RO_LITH", "ROOT_ABUN",
                                                                                                                                                      "ROOT_SIZE", "SEG_ABUN", "SEG_NATURE", "SEG_FORM", "SEG_SIZE",
                                                                                                                                                      "S_TYPE", "COORDINATE_METHOD", "LOCATION_NOTES", "S_NOTES", "NEAREST_TOWN",
                                                                                                                                                      "S_RAINFALL", "GEOLOGICAL_MAP_UNIT", "ELEMENT_SLOPE_CLASS", "S_MORPH_TYPE",
                                                                                                                                                      "S_ELEM_INC_SLOPE", "S_ELEM_TYPE", "PGM_MODE", "PGM_AGENT", "S_SLOPE_CLASS",
                                                                                                                                                      "S_RELIEF_CLASS", "S_REL_MS_CLASS", "S_PATT_TYPE", "HORIZON_NUMBER",
                                                                                                                                                      "STRG_CLASS", "SCF_STRENGTH", "STR_PED_SIZE", "C_STR_PED_GRADE",
                                                                                                                                                      "C_STR_PED_TYPE", "SCF_ABUN", "SCF_SIZE", "C_CF_SHAPE", "C_LITHOLOGY"
                                                                                      ), TabLev = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
                                                                                                    4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                                                                                    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 2L, 2L,
                                                                                                    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                                                                                    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                                                                                    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                                                                                    2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 4L,
                                                                                                    4L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                                                                    1L, 1L, 1L, 1L, 1L, 1L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L
                                                                                      )), class = "data.frame", row.names = c(NA, -140L))

