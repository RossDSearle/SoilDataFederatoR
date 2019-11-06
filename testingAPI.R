library(jsonlite)


usr <- 'ross.searle@csiro.au'
key <- 'a'

ds <- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/DataSets")
View(ds)

propts <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/Properties')
View(propts)


outLabdfs <- vector("list", length(ds$DataSet))

prop <- '3A1'

for (i in 1:nrow(ds)) {
  dSt <- ds$DataSet[i]

  cat(crayon::blue('\n', dSt,' : ', sep=''))
  r1 <- fromJSON(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=", prop, "&DataSet=", dSt, '&usr=', usr, '&key=', key))

  if(nrow(r1) > 0){
    cat(crayon::green('SUCCESS', sep=''))
    cat(' -  returned ', nrow(r1), ' records', sep='')
  }else{
    cat(crayon::red('POSSIBLE PROBLEM', sep=''))
  }
  outLabdfs[[i]] <- r1
}

outDF = as.data.frame(data.table::rbindlist(outLabdfs, fill=T))
