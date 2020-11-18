library(jsonlite)

# get all the property groups
url <- "http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups"
pg <- fromJSON(url)
pg


# get all the property codes for a group
url <- 'http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=Carbon'
props <- fromJSON(url)

# Get all carbon data by supplying a group name - I have filtered it to just NTGov for demo purposes only. This may timeout if the request returns a lot of data
url <- "http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedPropertyGroup=Carbon&DataSet=NTGovernment"

# Get all carbon data by stepping though each property code
results <- vector(mode = "list", length = nrow(props))
for (i in 1:nrow(props)) {
  url <- paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", props$Property[i] ,"&DataSet=NTGovernment")
  results[[i]] <- fromJSON(url)
}

df <- do.call("rbind", results)


