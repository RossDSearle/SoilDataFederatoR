


machineName <- as.character(Sys.info()['nodename'])

if(machineName=='soils-discovery'){
  dbPathSoilsFed <- '/srv/DB/SoilDataFederator/soilsFederator.sqlite'
}else{
  dbPathSoilsFed <- 'C:/Users/sea084/OneDrive - CSIRO/ProjectAdmin/SoilDataFederator/DB/soilsFederator.sqlite'
}

defaultTimeOut = 300

