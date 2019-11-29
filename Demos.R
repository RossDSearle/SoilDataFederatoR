library(rgdal)
library(sf)
library(aqp)
library(leaflet)
library(RColorBrewer)
library(ithir)
library(tidyverse)
library(data.table)
library(jsonlite)

source(paste0('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/R/Backends.R'))

#serverLoc <- 'http://127.0.0.1:6902'
serverLoc  <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/R'

drawLeafletMap <- function(pts, title){

  pal <- colorNumeric(c("red", "green", "blue"), domain = df.SP$Value)

  leaflet() %>%
    addTiles() %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
    addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
    addCircleMarkers(data=df.SP, group='Soil pH', radius = 2, opacity=1,
                     stroke=F,
                     fillOpacity = 1,
                     weight=1,
                     #fillColor = "yellow",
                     color = ~pal(df.SP$Value),
                     popup = paste0("Site ID : ", df.SP$Observation_ID ,
                                    "<br> Attribute : ", df.SP$ObservedProperty ,
                                    "<br> Value : ", df.SP$Value )) %>%
    addLegend("topright", pal = pal, values = df.SP$Value,
              title = "Soil Data",
              #labFormat = labelFormat(prefix = "$"),
              opacity = 1
    ) %>%
    addLayersControl(
      baseGroups = c("Topo","ESRI Aerial"),
      overlayGroups = c("Soil Data"),
      options = layersControlOptions(collapsed = T))
}

drawStaticMap <-  function(pts, title){
  pPath <- paste0('C:/Projects/GIS/National/Australia.shp')
  austBdy <- read_sf(pPath)

  bkd <-  as.numeric(as.character(pts$Value))
  # palt <-brewer.pal(11,"BrBG")
  # cuts = cut(bkd, 4:10)

  par(mar=c(0,0,0,0))
  plot(st_geometry(austBdy), border='black', reset=FALSE, col='beige')

  pal <- colorNumeric(c("red", "green", "blue"), domain = bkd)
  plot(pts["Value"], pch=20, add=T,  pal = sf.colors(7)  )
  legend("topright", legend=paste(seq(4, 10, 1)),fill=sf.colors(7), title = title )

}

blankResponseDF <- function(){

  outDF <- data.frame(Provider=character(), Dataset=character(), Observation_ID=character(), SampleID=character(), SampleDate=character() ,
                      Longitude=numeric() , Latitude= numeric(),
                      UpperDepth=numeric() , LowerDepth=numeric() , PropertyType=character(), ObservedProperty=character(), Value=numeric(),
                      Units= character(), Quality=integer(), stringsAsFactors = F)
}





#############################################################################################
#                                                                                           #
#                                    Start of the Demo                                      #
#                                                                                           #
#############################################################################################

usr <- 'ross.searle@csiro.au'; pwd <- 'a'
prop <- '4A1'


###  From the package directly
df <- getSoilData(DataSets = 'TERNSurveillance', observedProperty=prop, usr='ross.searle@csiro.au', key='a')
df <- getSoilData(providers='CSIRO', observedProperty=prop, usr='ross.searle@csiro.au', key='a')
df <- getSoilData(observedProperty=prop, usr='ross.searle@csiro.au', key='a')



###  From the web API

### get everything in one go
url <- paste0(serverLoc, '/SoilDataAPI/SoilData?observedProperty=', prop, '&usr=', usr, '&key=', pwd)
df <- fromJSON(url)
head(df)
nrow(df)

url <- paste0(serverLoc, '/SoilDataAPI/SoilData?observedProperty=', prop, '&providers=TERNSurveillance', '&usr=', usr, '&key=', pwd)
df2 <- fromJSON(url)
head(df2, 10)

### get from individual providers
url <- paste0(serverLoc, '/SoilDataAPI/Providers')
providerList <- fromJSON(url)
provs <- providerList$OrgName
outdfs <- vector("list", length(provs))

for(i in 1:length(provs)){
  url <- paste0(serverLoc, '/SoilDataAPI/SoilData?observedProperty=', prop, '&providers=', provs[i], '&usr=', usr, '&key=', pwd)
  print(url)
  odf <- fromJSON(url)

  if(is.data.frame(odf))
  {
    outdfs[[i]] <- odf
  }else{
    outdfs[[i]] <- blankResponseDF()
  }
}
df = as.data.frame(data.table::rbindlist(outdfs, fill=T))
nrow(df)

###  Run the data through some Quality filters   #######

str(df)
df$Value <- as.numeric(as.character(df$Value))
df <- df[!is.na(df$Value),]

# Is With Australia Bounding Box
xmin=113.3;ymin=-43.7;xmax=153.6;ymax=-10.6
outdf <- df[df$Longitude >= xmin & df$Longitude <= xmax & df$Latitude >= ymin & df$Latitude <= ymax, ]

# Is within reasonable value range
outdf <- outdf[outdf$Value > 1 & outdf$Value < 12, ]



####  Save the data for a rainy day
write.csv(outdf, 'c:/temp/df.csv')
outdf <- read.csv('c:/temp/df.csv', stringsAsFactors = F)


##### Summarise the data

df <- outdf
s <- summary(df$Value)
s

dfs <- data.frame(label=character(6), other=character(6), stringsAsFactors = F)
dfs[1,] <- c('Min Value ',s[1])
dfs[2,] <- c('1st Quartile ',s[2])
dfs[3,] <- c('Median ' ,s[3])
dfs[4,] <- c('Mean ' ,s[4])
dfs[5,] <- c('3rd Quartile ' ,s[5])
dfs[6,] <- c('Max value ',s[6])
validCnt <- length(which(!is.na(df$Value)))
naCnt <- length(which(is.na(df$Value)))
dfs[7,] <- c('Locations', length(unique(df$Observation_ID)))
dfs[8,] <- c('Records', nrow(df))
dfs[9,] <- c('Valid Vals', validCnt)
dfs[10,] <- c('NA Vals', naCnt)
dfs



hist(df$Value)
d <- density(df$Value)
plot(d, main=paste0("Data Distribution for ", prop))
polygon(d, col="blue", border="blue")

unique(df$Provider)
#df %>% group_by(Provider) %>% summarize(mean = mean(Value),median = median(Value), q10 = quantile(Value, 0.1), q90 = quantile(Value, 0.9))
setDT(df)[ , list(mean = mean(Value) ,median = median(Value), q10 = quantile(Value, 0.1), q90 = quantile(Value, 0.9), Count = length(Value)) , by = .(Provider)]
#boxplot(Value~Provider,data=df, main=paste0('Distributions for ', prop),  xlab="Provider", ylab=prop,las=2)
ggplot(df, aes(x=Provider, y=Value)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5))





#############################################################################################
#                                                                                           #
#                                    Draw some Maps                                         #
#                                                                                           #
#############################################################################################

frows <- as.data.frame(df) %>% group_by(Provider, Dataset,Observation_ID) %>% filter(row_number()==1)
df.SP <- st_as_sf(frows, coords = c("Longitude", "Latitude"), crs = 4326)
nrow(df.SP)


pts <- df.SP
title <- 'Surface pH'

drawStaticMap(pts, title)

drawLeafletMap(pts, title)



#############################################################################################
#                                                                                           #
#                                Some Spling Stuff                                          #
#                                                                                           #
#############################################################################################


pdf <- as.data.frame(df)
profs <- pdf[which(pdf$Provider == 'QLDGovernment'), ]

# change  horizon depth units to 'cm'
profs$UpperDepth   <- as.numeric(as.character(profs$UpperDepth )) * 100
profs$LowerDepth  <- as.numeric(as.character(profs$LowerDepth)) * 100

# initialise the soilProfileCollection
depths(profs) <- Observation_ID  ~ UpperDepth  + LowerDepth


# create site level data
site(profs) <- ~ Longitude
site(profs) <- ~ Latitude

# add spatial conetxt to soilProfileCollection
str(profs)
coordinates(profs) <- ~ Longitude + Latitude
proj4string(profs) <- CRS("+proj=longlat +datum=WGS84")
#plot(profs)
head(profs@horizons)

# select only profiles with 3 or more points for splining purposes
profs$hcnt = profileApply(profs, FUN=nrow)
profs4spline = subsetProfiles(profs, s='hcnt>2')
spls <- ea_spline(profs4spline[1:100], var.name="Value", lam = 0.1, d = t(c(0,5, 15, 30, 60, 100,200)))


saveRDS(spls, 'c:/temp/splines.rds')


######  Draw all splines on 1 plot ########
par(mfrow=c(1,1))
stdDeps <- c('2.5', '10', '22.5', '45', '80', '150')

plot( 0, type="n", main=paste( prop), yaxs = "i", xaxs = "i", xlim = c(2, 10), ylim = rev(range(c(0,200))))


for (i in 1:nrow(spls$harmonised)  )
{
  d = spls$var.1cm[,i]
  lines( d, 1:length(d), type="l")
   stdVals <- c(spls$harmonised$`0-5 cm`[i], spls$harmonised$`5-15 cm`[i], spls$harmonised$`15-30 cm`[i], spls$harmonised$`30-60 cm`[i], spls$harmonised$`60-100 cm`[i], spls$harmonised$`100-200 cm` [i])
   stdPts <- data.frame(stdDeps, stdVals)
   points(stdVals, stdDeps, col='red', pch=19)
}
par(mfrow=c(1,1))


sdf <- spls$harmonised


hdf <- data.frame(id=character(), depth=character(), val=numeric(), stringsAsFactors = F)
for(i in 1:6){
 idf <- data.frame(id=sdf$id , depth=colnames(sdf)[i+1], val=sdf[i+1], stringsAsFactors = F)
 colnames(idf) <- c('id', 'depth', 'val')
 hdf <- rbind(hdf, idf)
}


setDT(hdf)[ , list(mean = mean(val,na.rm=T) ,median = median(val,na.rm=T), q10 = quantile(val, 0.1,na.rm=T), q90 = quantile(val, 0.9,na.rm=T)) , by = .(depth)]
ggplot(hdf, aes(x=depth, y=val)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5))




######  Draw individual splines  ########
par(mar=c(2,2,2,2))
par(mfrow=c(3,4))
stdDeps <- c('2.5', '10', '22.5', '45', '80', '150')

for (i in 1:12 )
{
  d = spls$var.1cm[,i]
  plot( d, 1:length(d), type="n", main=paste( spls$idcol[i]), yaxs = "i", xaxs = "i", xlim = c(2, 10), ylim = rev(range(c(0,200))))
  lines( d, 1:length(d), type="l")
  stdVals <- c(spls$harmonised$`0-5 cm`[i], spls$harmonised$`5-15 cm`[i], spls$harmonised$`15-30 cm`[i], spls$harmonised$`30-60 cm`[i], spls$harmonised$`60-100 cm`[i], spls$harmonised$`100-200 cm` [i])
  stdPts <- data.frame(stdDeps, stdVals)
  points(stdVals, stdDeps, col='red', pch=19)

  rawvals <- spls$obs.preds[spls$obs.preds$Observation_ID  == spls$harmonised$id[i],]

  depths <- data.frame(as.numeric(rawvals$LowerDepth), as.numeric(rawvals$UpperDepth))
  d<- ((depths$as.numeric.rawvals.LowerDepth. - depths$as.numeric.rawvals.UpperDepth.) / 2) + depths$as.numeric.rawvals.UpperDepth.

  rawpts <- data.frame(rawvals$UpperDepth, rawvals$predicted)
  points( rawvals$predicted, d, col='green', pch=19)
}
par(mfrow=c(1,1))


