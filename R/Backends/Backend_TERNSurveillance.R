



 propertyType <- 'LabMethod'
 observedProperty <- '4A1'
# 
# propertyType <- 'LabGroup'
# observedProperty <- 'PH'
# 
# propertyType <- 'Morphology'
# observedProperty <- 'h_texture'




getData_TERNSurveillance <- function(propertyType, observedProperty ){
  
  OrgName <- 'TERNSurveillance'
  print(paste0('Extracting data from ', OrgName))

  allInfo <- fromJSON("http://144.6.225.21/ross" )
  allDFs <- allInfo[order(allInfo$site_location_name, allInfo$site_location_visit_id, allInfo$upper_depth),]
    
    df <- data.frame(Organisation='TERN_Surveillance', ObservedProperty=unique(allInfo$observed_property), labMcode='NA')
   
    mappings <- na.omit(read.csv(paste0(projectRoot,'/Mappings/TERNSurveillance_PropertyMappings.csv'), stringsAsFactors = F))
    
    con <- dbConnect(RSQLite::SQLite(), dbPathSoilsFed)
    
    if(str_to_lower(propertyType) ==  str_to_lower(PropertyTypes$LabMethod)){
     
      nativeProp <- mappings[mappings$labMcode == observedProperty, ]
      fdf <- na.omit(allInfo[allInfo$observed_property == nativeProp$ObservedProperty, ])
      
      sql <- paste0("Select * from LabMethods where LABM_CODE = '", observedProperty, "'")
      method = doQuery(con, sql)
      units <- method$LABM_UNITS
    }else if(str_to_lower(propertyType) == str_to_lower(PropertyTypes$LabGroup)){
      sql <- paste0("Select * from LabMethods where LABP_CODE = '", observedProperty, "'")
      methods = doQuery(con, sql)
      nativeProp <- mappings[mappings$labMcode %in% methods$LABM_CODE, ]
      fdf <- na.omit(allInfo[allInfo$observed_property %in% nativeProp$ObservedProperty, ])
    }
    else if(str_to_lower(propertyType) ==  str_to_lower(PropertyTypes$Morphology)){
      nativeProp <- mappings[mappings$labMcode == OP, ]
      fdf <- na.omit(allInfo[allInfo$observed_property == nativeProp$ObservedProperty, ])
      units <- 'None'
    }
    
    dbDisconnect(con)
    
    outDF <- data.frame(Organisation=OrgName, Observation_ID=paste0(fdf$site_location_name), SampleID=fdf$site_location_visit_id, Date=fdf$visit_date, Longitude=fdf$longitude, Latitude= fdf$latitude,
                        UpperDepth=fdf$upper_depth, LowerDepth=fdf$lower_depth, DataType=propertyType, ObservedProperty=observedProperty, Value=fdf$value, Units= units)
    oOutDF <- outDF[order(outDF$Observation_ID, outDF$UpperDepth, outDF$SampleID),]
    head(oOutDF)

return(oOutDF)
}
