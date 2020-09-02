library(plumber)

devel <- F

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){

  deployDir <-'/srv/plumber/TERNLandscapes/SoilDataFederatoR/R'
  server <- '0.0.0.0'

  options("plumber.apiScheme" = "https")
  options("plumber.apiPath" ='/TERNLandscapes/SoilDataFederatoR/R')
  options("plumber.apiHost" = 'esoil.io')
  options("plumber.apiURL" = 'https://esoil.io')
  portNum <- 8074
  #portNum <- 8073
  print('######################  I am here   ############################')
}else{
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/R'
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

#server <- 'esoil.io'
#portNum <- 8074

print(r)

print(server)
r$run(host= server, port=portNum, swagger=TRUE)





