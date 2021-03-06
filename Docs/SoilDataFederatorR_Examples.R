library(jsonlite)
library(httr)
library(htmltidy)
library(XML)
library(xml2)
devtools::install_github("hrbrmstr/jsonview")
library(jsonview)


# Get the raw json from the Providers endpoint
rawJson <- content(GET('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets'), "text")

# View the raw json data
json_view(rawJson)
json_tree_view(rawJson)

# Get the raw json from the Providers endpoint and create a dataframe - This is probably the best way to use the SoilDataFederator endpoint in R
df <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets')
head(df)

# Get the Providers data as XML
rawXML <- content(GET('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets?format=xml'), "text")

xml_view(rawXML)

# Get the Providers data as XML and create a dataframe
doc<-xmlParse(rawXML)
xmldf <- xmlToDataFrame(nodes = getNodeSet(doc, "//DataSetsRecord"))
head(xmldf)

# Get the Providers data as a CSV file
csvf <- tempfile()
download.file('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets?format=csv', csvf, mode="wb", quiet = T)
df <- read.csv(csvf)
head(df)
unlink(csvf)




# get the soil property groups from the PropertyGroups endpoint
url <- 'http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties'
df <- fromJSON(url)
head(df, 20)



# get the soil property groups from the PropertyGroups endpoint
url <- 'http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups'
df <- fromJSON(url)
head(df)




# get the soil property data from the SoilData endpoint
url <- 'http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=3A1&DataSet=TasGovernment'
df <- fromJSON(url)
head(df)
