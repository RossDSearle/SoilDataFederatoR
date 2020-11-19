library(readxl)
library(stringr)


### Path on esoils is - /usr/local/lib/soils_proxy/html/SoilsFederator/Providers/LawsonGrains

getLGMethod <- function(methods, mappings){
  meths <- character()
  for (i in 1:nrow(mappings)) {
    if(str_to_upper(mappings$ObservedProperty[i]) %in% str_to_upper(methods$Property) ){
      nativeProp <- mappings[i,]
      meths <- c(meths, nativeProp$OrigPropertyCode)
    }
  }
  return(meths)
}


getLocationData_LawsonGrains <- function(DataSet){

  OrgName <- getOrgName(DataSet)

  ep <- getNativeAPIurl(DataSet)

  locsUrl <- paste0(ep, '/all_Locs.xlsx')
  dataUrl <- paste0(ep, '/LawsonSoils.xlsx')

  p1f <- tempfile()
  download.file(locsUrl, p1f, mode="wb", quiet = T)
  locs<- suppressMessages( read_excel(path = p1f))

  p2f <- tempfile()
  download.file(dataUrl, p2f, mode="wb", quiet = T)
  lg <- suppressMessages( read_excel(path = p2f))

    sd <- merge(lg, locs,  by.x=c("Sample No."),by.y=c("Site ID"), all.x = T)

    dfl <- data.frame(paste0(sd$Aggregation , '_', sd$`Sample No.` ), paste0('01-04-', sd$Year ), sd$Lat, sd$Lon )
    colnames(dfl) <- c('ObsID', 'Date',  'Lat', 'Lon')
    df <- distinct(dfl)
    oOutDF <-  generateResponseAllLocs(DataSet, df$ObsID, df$Lon, df$Lat, df$Date )

    unlink(p1f)
    unlink(p2f)

    return(oOutDF)
}

getData_LawsonGrains <- function(DataSet, observedProperty, observedPropertyGroup=NULL ){

  OrgName <- getOrgName(DataSet)

  locsUrl <- 'http://esoil.io/SoilsFederator/Providers/LawsonGrains/all_Locs.xlsx'
  dataUrl <- 'http://esoil.io/SoilsFederator/Providers/LawsonGrains/LawsonSoils.xlsx'

  p1f <- tempfile()
  download.file(locsUrl, p1f, mode="wb", quiet = T)
  locs<- suppressMessages( read_excel(path = p1f))

  p2f <- tempfile()
  download.file(dataUrl, p2f, mode="wb", quiet = T)
  lg <- suppressMessages( read_excel(path = p2f))

  # pl <- getPropertiesList(observedProperty, observedPropertyGroup)
  # mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))
  # nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)

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

    sd <- merge(lg, locs,  by.x=c("Sample No."),by.y=c("Site ID"), all.x = T)

    dc <- unique(sd$Depth)

    sd$LayerID <- NA
    sd$LayerID[sd$Depth == '0 - 10'] <- 1
    sd$LayerID[sd$Depth == '30 - 60'] <- 3
    sd$LayerID[sd$Depth == '10 - 30'] <- 2
    sd$LayerID[sd$Depth == '0- 10'] <- 1

    bits <- str_split(sd$Depth, '-')
    ud <- as.numeric(str_trim(sapply(bits, function (x) x[1])))
    ld <- as.numeric(str_trim(sapply(bits, function (x) x[2])))

    sd$`Lab Number`[is.na(sd$`Lab Number`)] <- "1"

    fdf <- data.frame(sd$Aggregation, sd$Year, sd$`Sample No.`, sd$`Lab Number`, ud/100, ld/100, sd[, nProp], sd$Lat, sd$Lon)
    colnames(fdf) <- c('Aggregation', 'Year',  'SampleNo', 'LabNumber', 'ud', 'ld', 'Value', 'Lat', 'Lon')

    oOutDF <- generateResponseDF(DataSet, paste0(fdf$Aggregation , '_', fdf$SampleNo ), sd$LayerID, fdf$LabNumber, paste0('01-04-', sd$Year ), fdf$Lon, fdf$Lat, fdf$ud, fdf$ld, propertyType, sProp, fdf$Value, units)
    idxs <- which(!is.na(oOutDF$Value))
    lodfs[[i]] <- oOutDF[idxs,]
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  unlink(p1f)
  unlink(p2f)

   return(outDF)
}
