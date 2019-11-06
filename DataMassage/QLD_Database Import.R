library(DBI)
library(RSQLite)

rootDir<- 'C:/Projects/TernLandscapes/Site Data/QLD'



dbPath <- paste0('C:/Projects/TernLandscapes/Site Data/HostedDBs/SALI_Morphology.sqlite')

con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
sitesTablesList <- dbListTables(con)
write.csv(sitesTablesList, paste0(rootDir,'/SALITableNames.csv') )

# remove non relevant tables from the list
sitesTablesList <- read.csv(paste0(rootDir,'/relevantTableList.csv'), stringsAsFactors = F)


skips <- c("PROJECT_CODE","SITE_ID", "CREATED_BY" ,"CREATION_DATE","LAST_UPDATED_BY","LAST_UPDATE_DATE", "CONTEXT_PROJECT_CODE" ,"CONTEXT_SITE_ID")


#### Delete All tables   ######
tbls <- dbListTables(con)
for(i in 1:length(names(ds))){
  print(i)
  tblName <- tbls[i]
  res <- dbExecute(con,  paste0('DROP Table [', tblName, ']') )
}

saliFlds <- data.frame(tbl=character(), fld=character())
### read in the SALI morphology table dump
ds <- readRDS('C:/Projects/TernLandscapes/Site Data/QLD/SALI_SIT-data_20191101.rds')
str(ds)
names(ds)


#####  IMPORTANT ######  Leave existing tables as I have manually set all thl foriegn keys etc - delete the existing records and update

for(i in 1:length(names(ds))){
  print(i)
  tblName <- names(ds)[i]
  tbl <- ds[[tblName]]
  dbWriteTable(con, tblName, tbl, overwrite = F)

  flds <- names(tbl)

  for (j in 1:length(flds))
  {
    fld <- flds[j]

    if(!fld %in% skips){
      saliFlds<-  rbind(saliFlds, data.frame(tblName, fld))
    }
  }
}

 write.csv(saliFlds, paste0(rootDir,'/SALITableList.csv'))


#### Import Mapping table
mappings <- read.csv(paste0(rootDir,'/SALITableList.csv'))
dbWriteTable(con, "Mappings", mappings, overwrite = T)


#### Import Levelstable
levs <- read.csv(paste0(rootDir,'/SALITableNames.csv'))
dbWriteTable(con, "TableLevels", levs, overwrite = T)


ot <- ds$OBS
ot$OBS_DATE <- as.character(ot$OBS_DATE)
dbWriteTable(con, "dtest", ot, overwrite = T)


