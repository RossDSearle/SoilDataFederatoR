library(httr)
library(jsonlite)

# use your owner usr & key
usr <- 'ross.searle@csiro.au'
key <- 'a'

# Get the available properties for a PropertyGroup
propsDF <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=Carbon')
props <-propsDF$Property

# Get the available datasets
datasetsDF <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets')
datasets<-datasetsDF$DataSet




# Iterate Properties
  for(i in 1:length(props)){
    # Make an empty list to put individual results in for this property
      res <- vector("list", length = length(datasets))
      # Iterate Datasets
        for (j in 1:length(datasets)) {
            p <- props[i]
            d <- datasets[j]
            print(paste0(d, ' : ', p))
            url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", p ,"&DataSet=", d, "&format=json&usr=", usr, "&key=", key))
            resp <- GET(url, timeout(300))
            response <- content(resp, "text", encoding = 'UTF-8')
            odf <- fromJSON(response)

            # If there is data returned slot it into the list
              if(is.data.frame(odf)){
                if(nrow(odf)>0){
                  res[[j]] <- odf
                }
              }
      }
    # Merge the dataframes in the list into one dataframe
    outDF = as.data.frame(data.table::rbindlist(res, fill=T))
    # Write it to a csv
    write.csv(outDF, paste0('c:/temp/Carbon/SDF_', p, '.csv'))
  }



