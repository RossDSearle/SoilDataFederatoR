library(DBI)
library(RSQLite)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='soils-discovery'){
  dbPathSoilsFed <- '/srv/plumber/TERNLandscapes/SoilDataFederatoR/DB/soilsFederator.sqlite'
}else{
  dbPathSoilsFed <- system.file("extdata", "soilsFederator.sqlite", package = "SoilDataFederatoR")
}




isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

isValidString <- function(x) {
  return(!(is.na(x) || x == '' || is.null(x)))
}

generateEmail <- function(email, firstName, lastName, organisation){

  print(paste0('firstName = ', firstName))
  #key <- makeRandomString(1)
  if(isValidEmail(email)){
    if(isValidString(firstName)){
      if(isValidString(lastName)){
        if(isValidString(organisation)){
          url <- paste0('https://shiny.esoil.io/SoilFederator/Register/?&email=', email, '&firstname=', firstName, '&lastname=', lastName, '&organisation=',organisation )
          email <- paste0('echo "<p> Dear ', firstName, ',</p><p>Thanks for registering with the TERN SoilDataFederator. Click <a href=', url, '> HERE </a> to conform your registration and obtain an API key</p><p>Regards</p><p>The TERN SoilDataFederator" | mail -s "$(echo "SoilDataFederator API Key Request\nContent-Type: text/html")" ross.searle@csiro.au -r no-reply@soils-discovery.csiro.au')
          system(email)
          return("An email has been sent to you. Please click on the link to confirm your registration")
        }else{
          return("The supplied organisation name does not appear to be valid")
        }
      }else{
        return("The supplied last name does not appear to be valid")
      }
    }else{
      return("The supplied first name does not appear to be valid")
    }
  }else{
    return("The supplied email does not appear to be valid")
  }
}
