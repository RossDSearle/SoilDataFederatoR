# Pull soil texture data down from the soil data federator

library(jsonlite);
#library(leaflet);library(magrittr);library(sp)

ddir<- "Z:/projects/ternlandscapes_2019/soiltexture/data/"

# Check what datasets are available
avail.datasets<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/DataSets")


### Morphology 
# texture class and qualifiers
# search: H_TEXTURE and H_TEXTURE_QUAL
# usr: brendan.malone@csiro.au
# key: djwjgrpt74ld7wm
avail.datasets$DataSet
var.names<- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8")
var.names

for (i in 1:length(avail.datasets$DataSet)){
nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=H_TEXTURE%3BH_TEXTURE_QUAL&DataSet=",avail.datasets$DataSet[i],"&usr=brendan.malone%40csiro.au&key=djwjgrpt74ld7wm")
assign(var.names[i], fromJSON(nm1)) 
print(i)}

# Dont appear to be data for: NatGeoChemicalSurvey

## Save outputs to file (with time stamp)
#var.names<- c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8)
#var.names
#for (i in 1:length(avail.datasets$DataSet)){
#  nm2<- paste0(ddir, "morpolog_texture_",avail.datasets$DataSet[i],"_",Sys.Date(),"_.rds")
#  saveRDS(object = var.names2[i],file = nm2)}


### Lab Data
# texture class and qualifiers
# search: H_TEXTURE and H_TEXTURE_QUAL
# usr: brendan.malone@csiro.au
# key: djwjgrpt74ld7wm
avail.datasets$DataSet
var.names<- c("L1","L2","L3","L4","L5","L6","L7","L8")
var.names

#test<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedPropertyGroup=PSA&DataSet=TasGovernment&usr=brendan.malone%40csiro.au&key=djwjgrpt74ld7wm")

for (i in 1:length(avail.datasets$DataSet)){
  nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedPropertyGroup=PSA&DataSet=",avail.datasets$DataSet[i],"&usr=brendan.malone%40csiro.au&key=djwjgrpt74ld7wm")
  assign(var.names[i], fromJSON(nm1)) 
  print(i)}

# Dont appear to be data for: TERNSurveillance, QLDGovernment, NTGovernment, NatGeoChemicalSurvey

head(L8)



### Soil classes
# search: O_PPF;O_GSG;O_ASC_ORD;O_ASC_SUBORD
# usr: brendan.malone@csiro.au
# key: djwjgrpt74ld7wm
avail.datasets$DataSet
var.names<- c("O1","O2","O3","O4","O5","O6","O7","O8","O9")
var.names

#test<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=O_PPF%3BO_GSG%3BO_ASC_ORD%3BO_ASC_SUBORD&DataSet=NatSoil&numToReturn=10&usr=brendan.malone%40csiro.au&key=djwjgrpt74ld7wm")

for (i in 1:length(avail.datasets$DataSet)){
  nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=O_PPF%3BO_GSG%3BO_ASC_ORD%3BO_ASC_SUBORD&DataSet=",avail.datasets$DataSet[i],"&usr=brendan.malone%40csiro.au&key=djwjgrpt74ld7wm")
  df <- fromJSON(nm1)
  assign(var.names[i], df) 
  print(i)
  print(head(df))}

# Dont appear to be data for: TERNSurveillance, WAGovernment, TasGovernment,SAGovernment, NTGovernment, not retreiving much by way of soil classification NatGeoChemicalSurvey




