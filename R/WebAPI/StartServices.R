library(plumber)

devel <- F

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){

  deployDir <-'/srv/plumber/TERNLandscapes/SoilDataFederatoR/R'
  server <- 'http://esoil.io'
}else{
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/R'
  server <- '0.0.0.0'
}

if(devel){
  server <- '0.0.0.0'
}

source(paste0(deployDir, '/Backends.R'))


r <- plumb(paste0(deployDir, "/WebAPI/apiEndPoints.R"))

server <- 'http://esoil.io'
portNum <- 8074
#portNum <- 8079

print(r)

options("plumber.host" = "0.0.0.0")
options("plumber.apiHost" = "0.0.0.0")
r$run(host= server, port=portNum, swagger=TRUE)




