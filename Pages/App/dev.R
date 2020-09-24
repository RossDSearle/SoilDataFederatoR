library(jsonlite)
library(sf)
library(dplyr)
library(colourvalues)

url <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=7A1&DataSet=NatSoil&usr=ross.searle@csiro.au&key=a'
url <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=7A1&usr=ross.searle@csiro.au&key=a'
url <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=CUTAN_TYPE&DataSet=NatSoil&usr=ross.searle@csiro.au&key=a'
url <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=CUTAN_TYPE&usr=ross.searle@csiro.au&key=a'
url <- "https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=7A1&DataSet=NatSoil&usr=Demo&key=Demo"
df <-fromJSON(url)




    which(!grepl('^[0-9][.]','.001'))
    outDF <- outDF[-idxs,]
    outDF$Value <- as.numeric(outDF$Value)

    which(! grepl("^[[:digit:]]+$","0.9933"))


    outDF <- df
    outDF$Value <- as.numeric(outDF$Value)
    idxs <- which(!is.na(outDF$Value))
    outDF <- outDF[idxs,]


jn <-fromJSON(url)
jn <- getWebDataJSON(url)
if(is.null(jn$error)){
  odf <- fromJSON(jn)
}else{
  print('HHHHHHHHHH')
  break
}

if(is.null(df$error)){
  #odf <- fromJSON(jn)
}else{

  break
}



nrow(df)
head(df)
write.csv(df, 'c:/temp/7a1')


idxs <- which(!is.na(df$Longitude))
frows <- as.data.frame(df[idxs,] %>% group_by(Provider, Dataset, Observation_ID) %>% filter(row_number()==1))
df.SP <- st_as_sf(frows, coords = c("Longitude", "Latitude"), crs = 4326, na.fail=F)

cols = colour_values_rgb(df.SP$Value, include_alpha = FALSE,  palette = "viridis", na_colour = "#808080FF") / 255






outdf <- read.csv('c:/temp/7a1')

xmin=113.3;ymin=-43.7;xmax=153.6;ymax=-10.6
outdf <- df[df$Longitude >= xmin & df$Longitude <= xmax & df$Latitude >= ymin & df$Latitude <= ymax, ]
outDF<- outdf


outDF <- outDF[!is.na(outDF$Value),]
nrow(outDF)
outDF <- outDF[outDF$Value != '',]
nrow(outDF)

as.numeric(outDF$Value)

idxs <- which(!is.na(outDF$Longitude))
outDF <- outDF[idxs, ]

idxs <- which(!grepl('^[0-9.]',outDF$Value))
head(outDF)
head(outDF[idxs, ])

t <- outDF[-idxs,]
as.numeric(t$Value)

agg <- count(df, df$Value)
head(agg)

ggplot(df, aes(x=Provider, y=as.numeric(Value))) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5))


boxplot(as.numeric(df$Value)~,data=df, main="Dataaet Boxplots",xlab="DataSet", ylab="Values")


c('Locations', length(unique(df$Observation_ID)))

ggplot(df) + geom_bar(aes(x = Dataset, fill = Value)) + scale_x_discrete( expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

unique(df$Value)

ggplot(df) + geom_bar(aes(x = Value, fill = Value)) + scale_x_discrete( expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))




if(RV$currentdataType=='numeric'){
  s <- summary(as.numeric(df$Value))
  dfs <- data.frame(Statistic=character(), Value=character(), stringsAsFactors = F)

  dfs[1,] <- c('Min Value ',s[1])
  dfs[2,] <- c('1st Quartile ',s[2])
  dfs[3,] <- c('Median ' ,s[3])
  dfs[4,] <- c('Mean ' ,s[4])
  dfs[5,] <- c('3rd Quartile ' ,s[5])
  dfs[6,] <- c('Max value ',s[6])
  dfs[7,] <- c('Locations', length(unique(df$Observation_ID)))
  dfs[8,] <- c('Records', nrow(df))
}else{

  dfs <- data.frame(Categories=character(), Count=character(), stringsAsFactors = F)

  agg <- count(df, Value)
  colnames(agg) <- c('Categories','Count')
  nr <- nrow(agg)
  agg[nr+1, ] <-  c('', '')
  agg[nr+2, ] <-  c('Locations', length(unique(df$Observation_ID)))
  agg[nr+3, ] <-  c('Records', nrow(df))
}
