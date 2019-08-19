library(DBI)
library(RSQLite)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='soils-discovery'){
  dbPathSoilsFed <- '/srv/plumber/TERNLandscapes/SoilDataFederatoR/DB/soilsFederator.sqlite'
}else{
  dbPathSoilsFed <- system.file("extdata", "soilsFederator.sqlite", package = "SoilDataFederatoR")
}



generateAPIKey <- function(){
  sql <- paste0("Select * from Providers where Active = 1 and Availability = 'Public'")
  #
  orgs = doQueryFromFed(sql)
  return("An email has been sent to you. Please click on the link to confirm your registration")

}
