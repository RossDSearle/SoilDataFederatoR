library(RODBC)
library(odbc)

odbcListDrivers()

file_path<- 'C:/Projects/TernLandscapes/Site Data/CraigLiddicoat/soil-site-data/Flinders_svy6/susoil.mdb'
#accdb_con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",file_path,";"))

accdb_con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb)};DBQ=",file_path,";"))

pots <- dbGetQuery (accdb_con,"select * from SUOLD_SUPATCH",as.is=FALSE, stringsAsFactors = FALSE)
