p <- 'C:/Temp/QLD/SALI_availability_summaries_20191129.rds'

d <- readRDS(p)
d
str(d)
write.csv( d$SIT_LAB_METHODS, 'C:/Temp/QLD/codes.csv')

d$LABDAT_INTEXT

bob <- d$LABDAT_BOTH



samples <- fromJSON(paste0("https://soil-chem.information.qld.gov.au/odata/Samples"))

NBsamples <- samples[samples$bulkFlag=='N',]

prop <- '2Z2_Clay'
url <- URLencode(paste0("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?$filter=LabMethodCode eq '", prop, "'"))
print(url)
sd <- fromJSON(url)

idxr <- duplicated(sd) | duplicated(sd, fromLast = TRUE)
ot <- sd[idxr, ]

sot <- ot[order(ot$DataStore, ot$Dataset, ot$Provider, ot$Observation_ID, ot$UpperDepth, ot$LowerDepth, ot$ObservedProperty, ot$Value, decreasing = FALSE),]
sot

DataSet='NTGovernment'
observedProperty='2Z2_Clay'


df <- getSoilData(DataSets='NTGovernment', observedProperty='4A1', usr='ross.searle@csiro.au', key='a')


df <- getSoilData(DataSets='NTGovernment', observedProperty=NULL, observedPropertyGroup='PSA', usr='ross.searle@csiro.au', key='a')
nrow(df)
which(duplicated(df))

idx <- duplicated(df) | duplicated(df, fromLast = TRUE)
ot <- df[idx, ]
ot

sot <- ot[order(ot$DataStore, ot$Dataset, ot$Provider, ot$Observation_ID, ot$UpperDepth, ot$LowerDepth, ot$ObservedProperty, ot$Value, decreasing = FALSE),]
sot

df[!duplicated(df),]

t1 <- outdf[!duplicated(outdf), c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate","Longitude", "Latitude", "UpperDepth", "LowerDepth", "2Z2_Clay")]
t2 <- jdf[!duplicated(jdf), c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate","Longitude", "Latitude", "UpperDepth", "LowerDepth", "Value")]
ot <- dplyr::full_join(t1, t2)
sot <- ot[order(ot$DataStore, ot$Dataset, ot$Provider, ot$Observation_ID, ot$UpperDepth, ot$LowerDepth, decreasing = FALSE),]
sot


which(duplicated(outdf) )
df[23083:23084,]


df[ duplicated(df),]

write.csv(df, 'c:/temp/dups.csv')

url <- URLencode(paste0("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?$filter=LabMethodCode eq '", '2Z2_Clay', "'"))
print(url)
sd <- fromJSON(url)
nrow(sd)


nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedPropertyGroup=PSA&DataSet=QLDGovernment&usr=brendan.malone%40csiro.au&key=djwjgrpt74ld7wm")

df<- fromJSON(nm1)
nrow(df)
str(df)
write.csv(df, 'c:/temp/texture.csv')

cnts <- df %>% group_by(ObservedProperty) %>% summarise(n())
data.frame(cnts)

mergeObservedProperties(df)


df <- getSoilData(DataSets='QLDGovernment', observedProperty='O_PPF;O_GSG;O_ASC_ORD;O_ASC_SUBORD', usr='ross.searle@csiro.au', key='a')
nrow(df)
write.csv(df, 'c:/temp/class.csv')
O_PPF
O_GSG
O_ASC_TECH_REF
O_ASC_CONF
O_ASC_ORD
O_ASC_SUBORD
O_ASC_GG

vf <- fromJSON('https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=O_PPF%3BO_GSG%3BO_ASC_ORD%3BO_ASC_SUBORD&usr=brendan.malone%40csiro.au&key=djwjgrpt74ld7wm')
write.csv(vf, 'c:/temp/class.csv')
nrow(vf)





