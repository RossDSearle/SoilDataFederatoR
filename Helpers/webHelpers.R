library(httr)


getWebDataJSON <- function(url){
  resp <- GET(url, timeout = 300)
  response <- content(resp, "text", encoding = 'UTF-8')
  return(response)
}

getWebDataDF <- function(json){
  md <- fromJSON(json)
  return(md)
}


