


machineName <- as.character(Sys.info()['nodename'])

if(machineName=='soils-discovery'){
  dbPathSoilsFed <- '/srv/plumber/TERNLandscapes/SoilDataFederatoR/DB/soilsFederator.sqlite'
}else{
  dbPathSoilsFed <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DB/soilsFederator.sqlite'
}

defaultTimeOut = 300

