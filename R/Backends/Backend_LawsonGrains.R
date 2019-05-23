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



getData_LawsonGrains <- function( observedProperty=NULL, observedPropertyGroup=NULL ){

  OrgName <- 'LawsonGrains'
  print(paste0('Extracting data from ', OrgName))

  locsUrl <- 'http://esoil.io/SoilsFederator/Providers/LawsonGrains/all_Locs.xlsx'
  dataUrl <- 'http://esoil.io/SoilsFederator/Providers/LawsonGrains/LawsonSoils.xlsx'

  p1f <- tempfile()
  download.file(locsUrl, p1f, mode="wb", quiet = T)
  locs<-read_excel(path = p1f)

  p2f <- tempfile()
  download.file(dataUrl, p2f, mode="wb", quiet = T)
  lg <- read_excel(path = p2f)

  mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))

  nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)


if(length(nativeProp) == 0){
      return(blankResponseDF())
}

  cDF <- blankResponseDF()

  for (i in 1:length(nativeProps)) {

    sd <- merge(lg, locs,  by.x=c("Sample No."),by.y=c("Site ID"), all.x = T)

    bits <- str_split(sd$Depth, '-')
    ud <- as.numeric(str_trim(sapply(bits, function (x) x[1])))
    ld <- as.numeric(str_trim(sapply(bits, function (x) x[2])))

    sd$`Lab Number`[is.na(sd$`Lab Number`)] <- "1"

    fdf <- data.frame(sd$Aggregation, sd$Year, sd$`Sample No.`, sd$`Lab Number`, ud, ld, sd[, nativeProps[i]], sd$Lat, sd$Lon)
    colnames(fdf) <- c('Aggregation', 'Year',  'SampleNo', 'LabNumber', 'ud', 'ld', 'Value', 'Lat', 'Lon')

    propertyType <- getPropertyType(prop)
    units <- getUnits(propertyType = propertyType, prop = prop)

    oOutDF <- generateResponseDF(OrgName, paste0(fdf$Aggregation , '_', fdf$SampleNo ), fdf$LabNumber, fdf$Year, fdf$Lon, fdf$Lat, fdf$ud, fdf$ld, propertyType, prop, fdf$Value, units)
    cDF<- rbind(cDF, oOutDF)
  }

   return(cDF)
}
