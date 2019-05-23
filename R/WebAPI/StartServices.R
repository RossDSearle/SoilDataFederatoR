library(plumber)

devel <- T

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){

  deployDir <-'/srv/plumber/TERNLandscapes/SoilDataFederatoR/R'
  server <- 'http://esoil.io'
}else{
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR'
  server <- '0.0.0.0'
}

if(devel){
  server <- '0.0.0.0'
}

portNum <- 8074
source(paste0(deployDir, '/Backends.R'))


r <- plumb(paste0(deployDir, "/WebAPI/apiEndPoints.R"))
print(r)
r$run(host = server, port=portNum, swagger = TRUE)








