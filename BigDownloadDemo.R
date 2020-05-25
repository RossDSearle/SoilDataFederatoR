library(httr)
library(jsonlite)



dfj <- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=6A1;6_DC;6B1;4A1;4B1;3A1;14B1;DIST_TYPE;O_SOIL_DISTURB;O_ASC_ORD&format=json&usr=t.orton@uq.edu.au&key=s4iG4h4Vfw")
write.csv(dfj, 'c:/temp/to.csv')


url <- "https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=6A1&format=json&usr=t.orton@uq.edu.au&key=s4iG4h4Vfw"

props <- c('6A1','6_DC','6B1', '4A1', '4B1', '3A1','14B1','DIST_TYPE','O_SOIL_DISTURB','O_ASC_ORD')
props <- c('6A1','6_DC','6B1')
res <- vector("list", length = length(props))

for(i in 1:length(props)){
  p <- props[i] 
  print(p)
  url <- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=", p ,"&format=json&usr=t.orton@uq.edu.au&key=s4iG4h4Vfw")
  d <- GET(url, timeout(300))
  response <- content(d, "text", encoding = 'UTF-8')
  vals <- fromJSON(response)
  res[[i]] <- vals

}

AllDF = do.call("rbind", res)
write.csv(AllDF, 'c:/temp/tom.csv')


sideBySideDF <- mergeObservedProperties(AllDF)
write.csv(sideBySideDF, 'c:/temp/tom_SByS.csv')

soilDF<-AllDF 

mergeObservedProperties <- function(soilDF){
  
  #if(!isValidSoilDataframe(soilDF)){return(NULL)}
  
  props <- unique(soilDF$ObservedProperty)
  
  lodfs <- vector("list", length(props)-1)
   outdf <-  soilDF[soilDF$ObservedProperty == props[1], ]
   names(outdf)[names(outdf) == "Value"] <- props[1]
   names(outdf)[names(outdf) == "Units"] <- paste0('Units_', props[1])
   outdf <- outdf[, -10]
  outdf <- as.data.frame(outdf, rownames(NULL))[ ,  c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", props[1])]
  
  for(i in 2:length(props)){
    fdf <- as.data.frame(soilDF[soilDF$ObservedProperty == props[i], ])
    jdf <- fdf[, c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", "Value")]
    mdf <- dplyr::full_join(outdf, jdf)
    names(mdf)[names(mdf) == "Value"] <- props[i]
    outdf <- mdf
  }
  
  return(outdf)
}