library(jsonlite)


usr <- 'ross.searle@csiro.au'
key <- 'a'

ds <- fromJSON(paste0('https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets?usr=', usr, '&key=', key))
View(ds)

ds <- fromJSON(paste0('https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets'))
View(ds)

propts <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties')
View(propts)





#### Test Web API soil properties extraction

outLabdfs <- vector("list", length(ds$DataSet))
prop <- '6A1'

for (i in 1:nrow(ds)) {
  dSt <- ds$DataSet[i]
  cat(crayon::blue('\n', dSt,' : ', sep=''))
  r1 <- fromJSON(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", prop , "&DataSet=", dSt, '&usr=', usr, '&key=', key))

  if( length(r1) > 0){
   if( nrow(r1) > 0){
      cat(crayon::green('SUCCESS', sep=''))
      cat(' -  returned ', nrow(r1), ' records', sep='')
      outLabdfs[[i]] <- r1
    }else{
      cat(crayon::red('POSSIBLE PROBLEM - 0 records returned', sep=''))
    }
  }else{
    cat(crayon::red('POSSIBLE PROBLEM - No records returned', sep=''))
  }
}
outDF = as.data.frame(data.table::rbindlist(outLabdfs, fill=T))


#### Test Web API soil locations
outLabdfs <- vector("list", length(ds$DataSet))

for (i in 1:nrow(ds)) {
  dSt <- ds$DataSet[i]
  cat(crayon::blue('\n', dSt,' : ', sep=''))
  r1 <- fromJSON(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/ObservationLocations?DataSet=", dSt, '&usr=', usr, '&key=', key))

  if( length(r1) > 0){
    if( nrow(r1) > 0){
      cat(crayon::green('SUCCESS', sep=''))
      cat(' -  returned ', nrow(r1), ' records', sep='')
      outLabdfs[[i]] <- r1
    }else{
      cat(crayon::red('POSSIBLE PROBLEM - 0 records returned', sep=''))
    }
  }else{
    cat(crayon::red('POSSIBLE PROBLEM - No records returned', sep=''))
  }
}
outDF = as.data.frame(data.table::rbindlist(outLabdfs, fill=T))




#####  Individual tests

r1 <- fromJSON(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", '6Z' , "&DataSet=", 'BM', '&usr=', usr, '&key=', key))







