


test_Functions <- function(types, showResults=F){

  props <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/Testing/testProps.csv')

  if(str_to_upper(types)=='ALL'){
    test_getLabData_Functions(props, showResults=T)
    test_getMorphData_Functions(props, showResults)
    test_getLocations_Functions(props, showResults)
  }  else if(str_to_upper(types)=='LAB'){
    test_getLocations_Functions(props, showResults)
  }  else if(str_to_upper(types)=='MORPH'){
    test_getLocations_Functions(props, showResults)
  }else if(str_to_upper(types)=='LOCATIONS'){
    test_getLocations_Functions(props, showResults)
  }
}


test_getMorphData_Functions <- function(props, showResults){

     cat(crayon::yellow('\n\nTesting Morphology Data EndPoints', sep=''))
     cat(crayon::yellow('\n=====================================', sep=''))

    ds = props$DataSet

  for (i in 1:length(ds)) {

    dSt <-ds[i]

    cat(crayon::blue('\n', dSt,' : '), crayon::cyan( OP ),' : ', sep='')
    if(is.na(OP))
    {
      cat(crayon::magenta('No props available', sep=''))
    }else{

      r1 <- getSoilData(DataSets=dSt,observedProperty=OP, usr='ross.searle@csiro.au', key='a', verbose=F)

      if(nrow(r1) > 0){
        cat(crayon::green('SUCCESS', sep=''))
        cat(' -  returned ', nrow(r1), ' records', sep='')
        if(showResults){
          print(head(r1))
        }
      }else{
        cat(crayon::red('POSSIBLE PROBLEM', sep=''))        }
    }
  }
}


test_getLabData_Functions <- function(props, showResults){

     cat(crayon::yellow('\n\nTesting Lab Data EndPoints', sep=''))
     cat(crayon::yellow('\n=====================================', sep=''))

  ds = props$DataSet

  for (i in 1:length(ds)) {

    OP <- props[i,2]
    dSt <-ds[i]

    cat(crayon::blue('\n', dSt,' : '), crayon::cyan( OP),' : ', sep='')
    if(is.na(OP))
    {
      cat(crayon::magenta('No props available', sep=''))
    }else{

      r1 <- getSoilData(DataSets=dSt,observedProperty=OP, usr='ross.searle@csiro.au', key='a', verbose=F)

      if(nrow(r1) > 0){
        cat(crayon::green('SUCCESS', sep=''))
        cat(' -  returned ', nrow(r1), ' records', sep='')
        if(showResults){
          print(head(r1))
        }
      }else{
        cat(crayon::red('POSSIBLE PROBLEM', sep=''))        }
    }
  }
}

####  Test the getLocations EndPoints
test_getLocations_Functions <- function(props, showResults){

  ds = props$DataSet

    cat(crayon::yellow('\n\nTesting Locations EndPoints', sep=''))
    cat(crayon::yellow('\n=====================================', sep=''))
    for (i in 1:length(ds)) {
      dSt <-ds[i]

      cat(crayon::blue('\n', dSt,' : ', sep=''))

      tryCatch({
            r2 <- getSiteLocations(DataSets=dSt, usr='ross.searle@csiro.au', key='a')

            if(nrow(r2) > 0){
              cat(crayon::green('SUCCESS', sep=''))
              cat(' -  returned ', nrow(r2), ' records', sep='')
              if(showResults){
                print(head(r1))
              }
            }else{
              cat(crayon::red('POSSIBLE PROBLEM', sep=''))
            }
          }, warning = function(w) {
              #cat(crayon::red('WARNING : ', w, sep=''))
            cat(crayon::green('SUCCESS', sep=''))
            cat(' -  returned ', nrow(r2), ' records', sep='')
            if(showResults){
              print(head(r1))
            }
          }, error = function(e) {
                cat(crayon::red('ERROR : ', e, sep=''))
          }, finally = {}
      )

    }
}
