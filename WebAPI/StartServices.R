library(plumber)

devel <- F

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){

  deployDir <-'/srv/plumber/TERNLandscapes/SoilDataFederatoR'
  #server <- 'https://esoil.io'
  server <- '0.0.0.0'
   #options("plumber.host" = "0.0.0.0")
   # options("plumber.apiHost" = "0.0.0.0")
   #options("plumber.host" = 'esoil.io/TERNLandscapes/SoilDataFederatoR')
   #options("plumber.apiHost" = 'esoil.io/TERNLandscapes/SoilDataFederatoR')
  portNum <- 8074
  #portNum <- 8073
}else{
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR'
  server <- '127.0.0.1'
  options("plumber.host" = "127.0.0.1:3077")
  options("plumber.apiHost" = "127.0.0.1:3077")
  portNum <- 3077
}

# if(devel){
#   server <- '0.0.0.0'
# }

source(paste0(deployDir, '/Backends.R'))


r <- plumb(paste0(deployDir, "/WebAPI/apiEndPoints.R"))

#prf <- plumb(paste0(deployDir, "/WebAPI/apiEndPoints_Development.R"))
#r$mount("/Dev/", prf)


#server <- 'http://esoil.io'
#portNum <- 8079

print(r)


r$run(host= server, port=portNum, swagger=TRUE)




