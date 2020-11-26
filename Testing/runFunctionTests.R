library(stringr)

source('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/Backends.R')


test_Functions <- function(types='ALL', showResults=T, nrows=5, API=F){

  props <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/Testing/testProps.csv')

  if(str_to_upper(types)=='ALL'){
    test_getLabData_Functions(props, showResults, nrows, API)
    test_getMorphData_Functions(props, showResults, nrows, API)
    test_getLocations_Functions(props, showResults, nrows, API)
  }  else if(str_to_upper(types)=='LAB'){
    test_getLabData_Functions(props, showResults, nrows, API)
  }  else if(str_to_upper(types)=='MORPH'){
    test_getMorphData_Functions(props, showResults, nrows, API)
  }else if(str_to_upper(types)=='LOCATIONS'){
    test_getLocations_Functions(props, showResults, nrows, API)
  }
}


test_getMorphData_Functions <- function(props, showResults, nrows, API=F){

     cat(crayon::yellow('\n\nTesting Morphology Data EndPoints', sep=''))
     cat(crayon::yellow('\n=====================================', sep=''))

    ds = props$DataSet

  for (i in 1:length(ds)) {

    dSt <-ds[i]
    OP <- props[i,3]

    cat(crayon::blue('\n', dSt,' : '), crayon::cyan( OP ),' : ', sep='')
    if(is.na(OP))
    {
      cat(crayon::magenta('No props available', sep=''))
    }else{

      if(API){
        url <- paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", OP,"&DataSet=",dSt, "&usr=ross.searle@csiro.au&key=a")
        print(url)
        r1 <- fromJSON(url)
      }else{
        r1 <- getSoilData(DataSets=dSt,observedProperty=OP, usr='ross.searle@csiro.au', key='a', verbose=F)
      }


      if(nrow(r1) > 0){
        cat(crayon::green('SUCCESS', sep=''))
        cat(' -  returned ', nrow(r1), ' records\n', sep='')
        if(showResults){
          print(head(r1, nrows))
        }
      }else{
        cat(crayon::red('POSSIBLE PROBLEM\n', sep=''))        }
    }
  }
}


test_getLabData_Functions <- function(props, showResults, nrows, API){

     cat(crayon::yellow('\n\nTesting Lab Data EndPoints', sep=''))
     cat(crayon::yellow('\n=====================================', sep=''))

  ds = props$DataSet

  for (i in 1:length(ds)) {
    Sys.sleep(2)

    OP <- props[i,2]
    dSt <-ds[i]
    print(dSt)

    cat(crayon::blue('\n', dSt,' : '), crayon::cyan( OP),' : ', sep='')
    if(is.na(OP))
    {
      cat(crayon::magenta('No props available', sep=''))
    }else{

      if(API){
        url <- paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", OP,"&DataSet=",dSt, "&usr=ross.searle@csiro.au&key=a")
        print(url)
        r1 <- fromJSON(url)
      }else{
        r1 <- getSoilData(DataSets=dSt,observedProperty=OP, usr='ross.searle@csiro.au', key='a', verbose=F)
      }


      if(nrow(r1) > 0){
        cat(crayon::green('SUCCESS', sep=''))
        cat(' -  returned ', nrow(r1), ' records\n', sep='')
        if(showResults){
          print(head(r1, nrows))
        }
      }else{
        cat(crayon::red('POSSIBLE PROBLEM\n', sep=''))        }
    }
  }
}

####  Test the getLocations EndPoints
test_getLocations_Functions <- function(props, showResults, nrows, API){

  ds = props$DataSet

    cat(crayon::yellow('\n\nTesting Locations EndPoints', sep=''))
    cat(crayon::yellow('\n=====================================', sep=''))
    for (i in 1:length(ds)) {
      dSt <-ds[i]

      cat(crayon::blue('\n', dSt,' : ', sep=''))

      tryCatch({

        if(API){
          url <- paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/ObservationLocations?DataSet=",dSt, "&usr=ross.searle@csiro.au&key=a")
          print(url)
          r1 <- fromJSON(url)
        }else{
          r2 <- getSiteLocations(DataSets=dSt, usr='ross.searle@csiro.au', key='a')
        }



            if(nrow(r2) > 0){
              cat(crayon::green('SUCCESS', sep=''))
              cat(' -  returned ', nrow(r2), ' records\n', sep='')
              if(showResults){
                print(head(r2, nrows))
              }
            }else{
              cat(crayon::red('POSSIBLE PROBLEM\n', sep=''))
            }
          }, warning = function(w) {
              #cat(crayon::red('WARNING : ', w, sep=''))
            cat(crayon::green('SUCCESS', sep=''))
            cat(' -  returned ', nrow(r2), ' records\n', sep='')
            if(showResults){
              print(head(r2))
            }
          }, error = function(e) {
                cat(crayon::red('ERROR : ', e, '\n', sep=''))
          }, finally = {}
      )

    }
}
