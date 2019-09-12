library(DBI)
library(RSQLite)
library(stringr)


dbPath <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/NT/NT_DB.db'
csvDir <- 'C:/Projects/TernLandscapes/Site Data/NT/NT_SALI Database Export 2 Aug 2019'


con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)


fls <- list.files(csvDir)
tNames <- fls[!grepl('CODE_', fls, ignore.case = T)]
#write.csv(tNames, 'C:/Projects/TernLandscapes/Site Data/NT/table.csv', row.names = F)
#### Manually edit for the required tables

inTables <- read.csv('C:/Projects/TernLandscapes/Site Data/NT/table.csv', header = F)




# for (i in 1:nrow(inTables)) {
#   tname <- str_remove(inTables$V1[i], '_DATA_TABLE.csv')
#   print(tname)
#   inDFName <- paste0(csvDir, '/', inTables$V1[i])
#   inDF <- read.csv(inDFName)
#   dbWriteTable(conn = con, tname, inDF)
#
# }



# Delete all tables
tabs <- dbListTables(con)
for (i in 1:length(tabs)) {
  t <- tabs[i]
  print(t)
  dbRemoveTable(con, t)

}


# create all tables
source('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DataMassage/NT/NT_Create_SQL.R')

res <- dbExecute(con, sql_Create_Survey)
res <- dbExecute(con, sql_Create_SITE)
res <- dbExecute(con, sql_Create_SOIL)
res <- dbExecute(con, sql_Create_HORIZON)
res <- dbExecute(con, sql_Create_CHEMICAL)
res <- dbExecute(con, sql_Create_AGGRADATION)
res <- dbExecute(con, sql_Create_CHEMICAL_METHODS)
res <- dbExecute(con, sql_Create_COARSE_FRAGMENTS)
res <- dbExecute(con, sql_Create_COARSE_FRAGMENTS_LANDFORM)
res <- dbExecute(con, sql_Create_DRAINAGE)
res <- dbExecute(con, sql_Create_EROSION)
res <- dbExecute(con, sql_Create_LANDFORM)
res <- dbExecute(con, sql_Create_LANDS_STAFF)
res <- dbExecute(con, sql_Create_MICRORELIEF)
res <- dbExecute(con, sql_Create_MOTTLE)
res <- dbExecute(con, sql_Create_OBSERVER)
res <- dbExecute(con, sql_Create_PERMEABILITY)
res <- dbExecute(con, sql_Create_ROCK_OUTCROP)
res <- dbExecute(con, sql_Create_SEGREGATIONS)
res <- dbExecute(con, sql_Create_SEGREGATIONS_LANDFORM)
res <- dbExecute(con, sql_Create_STRUCTURE)
res <- dbExecute(con, sql_Create_SUBSTRATE)
res <- dbExecute(con, sql_Create_SURFACE_CONDITION)


# the sites table has the coordinates as WKT text so it bugeers up the table structure due to comma in WKT, so these lines fix that up
sdt <- read.csv('C:/Projects/TernLandscapes/Site Data/NT/NT_SALI Database Export 2 Aug 2019/SITE_DATA_TABLE.csv')
colnames(sdt)
lons <- str_remove( sdt$LANDUSE_LUMP_2007, 'MDSYS.SDO_POINT_TYPE[(]')
fixedSites <- data.frame(sdt[,1:10], DATE_DESCRIBED=sdt[,19],LANDUSE_LUMP_2007=sdt[,20], GDA94_LATITUDE=sdt$GDA94_LATITUDE, GDA94_LONGITUDE=lons, sdt[,23:27])
head(fixedSites)
write.csv(fixedSites,'C:/Projects/TernLandscapes/Site Data/NT/NT_SALI Database Export 2 Aug 2019/SITE_FIXED_DATA_TABLE.csv', row.names = F)

### had to manually fix some of the codes in the Chemical_Methods table so that things will work consistently
### some code below to facilitate this
d <-  dbExecute(con, 'Drop table CHEMICAL_METHODS')
res <- dbExecute(con, sql_Create_CHEMICAL_METHODS)
inDF <- read.csv('C:/Projects/TernLandscapes/Site Data/NT/NT_SALI Database Export 2 Aug 2019/CHEMICAL_METHODS_FIXED_DATA_TABLE.csv')
dbWriteTable(conn = con, tname, inDF, append = T)



#### polulate all the table with data from the CSV files
dbListTables(con)

for (i in 1:nrow(inTables)) {
  tname <- str_remove(inTables$V1[i], '_DATA_TABLE.csv')

  # if(tname == 'SITE'){
  #   inDFName <- paste0('C:/Projects/TernLandscapes/Site Data/NT/NT_SALI Database Export 2 Aug 2019/SITE_FIXED_DATA_TABLE.csv')
  # }else{
  #   inDFName <- paste0(csvDir, '/', inTables$V1[i])
  # }
  # if(tname == 'CHEMICAL_METHODS'){
  #   inDFName <- paste0('C:/Projects/TernLandscapes/Site Data/NT/NT_SALI Database Export 2 Aug 2019/CHEMICAL_METHODS_FIXED_DATA_TABLE.csv')
  # }else{
  #   inDFName <- paste0(csvDir, '/', inTables$V1[i])
  # }

  if(!tname %in% c('SITE', 'CHEMICAL_METHODS')){
    print(tname)
    inDFName <- paste0(csvDir, '/', inTables$V1[i])
    inDF <- read.csv(inDFName, stringsAsFactors = F)
    dbWriteTable(conn = con, tname, inDF, append = T)
  }

}

inDF <- read.csv('C:/Projects/TernLandscapes/Site Data/NT/NT_SALI Database Export 2 Aug 2019/SITE_FIXED_DATA_TABLE.csv', stringsAsFactors = F)
head(inDF)
dbWriteTable(conn = con, 'SITE', inDF, append = T)

inDF <- read.csv('C:/Projects/TernLandscapes/Site Data/NT/NT_SALI Database Export 2 Aug 2019/CHEMICAL_METHODS_FIXED_DATA_TABLE.csv', stringsAsFactors = F)
dbWriteTable(conn = con, 'CHEMICAL_METHODS', inDF, append = T)


dbDisconnect(con)



# Generate the lab methods mappings

meths <- read.csv(paste0('C:/Projects/TernLandscapes/Site Data/NT/NT_SALI Database Export 2 Aug 2019/CHEMICAL_METHODS_FIXED_DATA_TABLE.csv'), stringsAsFactors = F, colClasses=c(rep("character",50)))
labMeths <- data.frame(Organisation=character(), OrigPropertyCode=character(), ObservedProperty=character(), DataType=character())
for (i in 2:ncol(meths)) {

  mCodes <- na.omit(data.frame(Organisation='NTGovernment', OrigPropertyCode=colnames(meths[i]),  ObservedProperty=format(unique(meths[i]), scientific=FALSE), DataType='L'))
  colnames(mCodes)[3] <- 'ObservedProperty'
  mCodes <- mCodes[which(mCodes[,3]  != ""), ]
  labMeths <- rbind(labMeths, mCodes)
}
labMeths

write.csv(labMeths, 'C:/Projects/TernLandscapes/Site Data/NT/NT_Mapping.csv', row.names = F)

labMeths = read.csv('C:/Projects/TernLandscapes/Site Data/NT/NT_Mapping.csv', stringsAsFactors = F)
mapPath = 'C:/R/R-3.6.0/library/SoilDataFederatoR/extdata/soilsFederator.sqlite'
mapcon <- DBI::dbConnect(RSQLite::SQLite(), mapPath)
dbWriteTable(conn = mapcon, 'Mappings', labMeths, append = T)



tabs <- dbListTables(con)
write.csv(tabs, 'C:/Projects/TernLandscapes/Site Data/NT/NT_TableLevels.csv')
tabLevels <- read.csv('C:/Projects/TernLandscapes/Site Data/NT/NT_TableLevels.csv')
mapPath = 'C:/R/R-3.6.0/library/SoilDataFederatoR/extdata/soilsFederator.sqlite'
mapcon <- DBI::dbConnect(RSQLite::SQLite(), mapPath)
dbWriteTable(conn = mapcon, 'TableLevels_NT', tabLevels, append = F, overwrite=T)



