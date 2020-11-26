


machineName <- as.character(Sys.info()['nodename'])

if(machineName=='soils-discovery'){
  dbPathSoilsFed <- '/datasets/work/lw-soildatarepo/work/TERNLandscapes/SoilsFederator/DB/soilsFederator.sqlite'
}else{
  dbPathSoilsFed <- 'C:/Users/sea084/Dropbox/ProjectAdmin/SoilDataFederator/DB/soilsFederator.sqlite'
}

defaultTimeOut = 300

