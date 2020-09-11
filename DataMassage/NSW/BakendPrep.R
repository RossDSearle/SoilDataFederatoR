library(RSQLite)
library(DBI)
library(stringr)
library(httr)
library(jsonlite)



doQuery <- function(con, sql){
  res <- dbSendQuery(con, sql)
  rows <- dbFetch(res)
  dbClearResult(res)
  return(rows)
}

sendStatement <- function(con, sql){
  rs <- dbSendStatement(con, sql)
  dbHasCompleted(rs)
  nrec <- dbGetRowsAffected(rs)
  dbClearResult(rs)
  return(nrec)
}

getWebData <- function(url){
  resp <- GET(url, timeout = 300)
  response <- content(resp, "text", encoding = 'UTF-8')
  md <- fromJSON(response)
  return(md)
}

write.csv(md, 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/NSW/MorphCodes.csv')



conn <- dbConnect(RSQLite::SQLite(), 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DB/soilsFederator.sqlite', flags = SQLITE_RO)
sql <- "select * from Mappings where Organisation = 'NSWGovernment'"
r <- doQuery(conn, sql)
write.csv(r, 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/NSW/NatsoilMorphCodes.csv')

dbListTables(conn)


df <- getWebData('http://asris-daas02/NatSoil_Services/api/MorphResults?morphology_attribute=s_date_desc&proj_code=CAN')
head(df)
df <- getWebData('http://asris-daas02/NatSoil_restricted_Services/api/MorphResults?morphology_attribute=s_date_desc')
head(df)
df <- getWebData('http://asris-daas02/NatSoil_restricted_Services/api/LabResults?method_code=6B3')
head(df)


df <- getWebData('http://asris-daas02/NatSoil_restricted_Services/api/LabResults?method_code=6B3')
head(df)


df <- getWebData('http://asris-daas02/WA_Services/api/MorphResults?morphology_attribute=s_desc_by')
head(df)

dts <- dplyr::count_(df, vars = c('agency_code', 'project_code', 'feature_id'))
nrow(dts)

df <- getWebData('http://asris-daas02/Vic_Services/api/LabResults?obs_method=HORIZON')
df <- getWebData('http://asris-daas02/Vic_Services/api/LabResults?obs_method=TEXTURE')
df <- getWebData('http://asris-daas02/VIC_Services/api/LabResults?obs_method=15E2_Al')
df <- getWebData('http://asris-daas02/VIC_Services/api/LabResults?obs_method=O_TYPE')



head(df)
df <- getWebData('http://asris-daas02/Vic_Services/api/MorphResults?morphology_attribute=')
head(df)

df <- getWebData('http://asris-daas02/Tas_Services/api/MorphResults?morphology_attribute=s_date_desc')


library(sf)

#unique(df$)
idxs <- which(!is.na(df$site_north))
indf <- df[idxs,]
df.SP <- st_as_sf(indf, coords = c("site_east", "site_north"))
outCoords <- st_coordinates(df.SP)
plot(df.SP[1])

head(indf, 20)
head(outCoords, 20)


df.SP
write_sf(df.SP, 'c:/temp/vic.shp')

df <- getWebData('http://asris-daas02/NSW_Services/api/MorphResults?morphology_attribute=s_date_desc')
head(df)

df2 <- getWebData('http://asris-daas02/NSW_Services/api/MorphResults?morphology_attribute=Colourmoistmunsellcolourchroma')
df3 <- getWebData('http://asris-daas02/NSW_Services/api/MorphResults?morphology_attribute=Stationnumber')
df3 <- getWebData('http://asris-daas02/NSW_Services/api/MorphResults?morphology_attribute=Location')



df <- getWebData('http://asris-daas02/NSW_Services/api/LabResults?method_code=Naswat')
head(df)

Mottlesdominantmunsellcolourchroma
Mottlesdominantmunsellcolourhue
Mottlesdominantmunsellcolourhuevalue
Mottlesdominantmunsellcolourresolveddescription
Mottlesdominantmunsellcolourvalue
Mottlessubdominantmottleabundance



df <- getWebData('http://asris-daas02/TAS_Services/api/MorphResults?morphology_attribute=s_desc_by')
head(df)





nrow(df)
nrow(df2)
nrow(df3)




sampDF <- read.csv('C:/Projects/TernLandscapes/Site Data/NSW/20200214_csvFiles/sa5_smpl_p_tern_hm_200214.csv')
str(sampDF)


cat('labMethod,cnt','\n', sep = '', file='C:/Projects/TernLandscapes/Site Data/NSW/labs.csv', append = F)

for (i in 13:ncol(sampDF)) {
  c <- sampDF[, i]
  if( sum(is.na(c)) == 29263){
  # print(paste0(colnames(sampDF)[i], ' = None'))
  }else{
    c[c==""] <- NA
    print(paste0(colnames(sampDF)[i], ' = ', sum(!is.na(c))))
    cat(colnames(sampDF)[i],',',  sum(!is.na(c)),'\n', sep = '', file='C:/Projects/TernLandscapes/Site Data/NSW/labs.csv', append = T)
  }
}

sampDF <- read.csv('C:/Projects/TernLandscapes/Site Data/NSW/20200214_csvFiles/sa5_lyr_p_tern_hm_191128.csv')


cat('lyrProp,cnt','\n', sep = '', file='C:/Projects/TernLandscapes/Site Data/NSW/layers.csv', append = F)

for (i in 14:ncol(sampDF)) {
  c <- sampDF[, i]
  if( sum(is.na(c)) == 222232){
    # print(paste0(colnames(sampDF)[i], ' = None'))
  }else{
    c[c==""] <- NA
    print(paste0(colnames(sampDF)[i], ' = ', sum(!is.na(c))))
    cat(colnames(sampDF)[i],',',  sum(!is.na(c)),'\n', sep = '', file='C:/Projects/TernLandscapes/Site Data/NSW/layers.csv', append = T)
  }
}



sampDF <- read.csv('C:/Projects/TernLandscapes/Site Data/NSW/20200214_csvFiles/sa5_prf_p_tern_hm_191128.csv')

#cat('lyrProp,cnt','\n', sep = '', file='C:/Projects/TernLandscapes/Site Data/NSW/layers.csv', append = F)

for (i in 14:ncol(sampDF)) {
  c <- sampDF[, i]
  if( sum(is.na(c)) == 49833){
    # print(paste0(colnames(sampDF)[i], ' = None'))
  }else{
    c[c==""] <- NA
    print(paste0(colnames(sampDF)[i], ' = ', sum(!is.na(c))))
    cat(colnames(sampDF)[i],',',  sum(!is.na(c)),'\n', sep = '', file='C:/Projects/TernLandscapes/Site Data/NSW/layers.csv', append = T)
  }
}
