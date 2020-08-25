library(httr)

url <- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=6A1&format=json&usr=t.orton@uq.edu.au&key=s4iG4h4Vfw")
d <- GET(url, timeout(10000))
response <- content(d, "text", encoding = 'UTF-8')
vals <- fromJSON(response)

df <- getSoilData(DataSets='TERNSurveillance',observedProperty='4A1', usr='ross.searle@csiro.au', key='a')

df <- getSoilData(DataSets=NULL,observedProperty='6A1', usr='ross.searle@csiro.au', key='a', verbose = T)
dft <- getSoilData(DataSets=NULL,observedProperty='6A1', usr='ross.searle@gmail.com', key='s4iG4h4Vfw', verbose = T)

unique(df$Dataset)

getSoilData(DataSets='EastCentral_Australia',observedProperty='6A1', usr='ross.searle@csiro.au', key='a')

oldDF <- read.csv('C:/Temp/dfLong6A1.csv')

oldCnts <- as.data.frame(table(oldDF$Dataset))
allCnts <- as.data.frame(table(df$Dataset))
tomcnts <- as.data.frame(table(df$Dataset))
