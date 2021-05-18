library(httr)
library(jsonlite)
library(RCurl)
library(data.table)
library(stringr)
library(dplyr)


# \\nexus.csiro.au\Fileservers\fsact01-cdc\LW\Share1\Mud-bu\store1\aclep\Natsoil\sourceData

#1)	Lab Results endpoint (can be filtered on method_code, horizon_num, spec_id, upper_bound_lat, lower_bound_lat, upper_bound_lon and lower_bound_lon. The results can also be PAGED using page_num and page_size)
#3)	Morphology results endpoint (can be filtered on morphology_attribute, agency_code, proj_code, s_id and o_id. The results can also be PAGED using page_num and page_size)

# http://asris-daas02/NT_Services/api/MorphologyMetaData
# http://asris-daas02/NT_Services/api/MorphResults?morphology_attribute=Fieldtextureclayfraction
# http://asris-daas02/NT_Services/api/MorphResults?morphology_attribute=h_upper_depth&page_num=1&page_size=10
#
# Lab Results end points:
# http://asris-daas02/NT_Services/api/LabResults
# http://asris-daas02/NT_Services/api/LabResults?page=1&count=10
# http://asris-daas02/NT_Services/api/LabResults?method_code=15_NR_CA&page_num=1&page_size=10
# http://asris-daas02/NT_Services/api/LabResults?method_code=15_NR_CA&horizon_num=4&page_num=1&page_size=15
# http://asris-daas02/NT_Services/api/LabResults?method_code=15_NR_CA&horizon_num=4&horizon_lower_depth=1&horizon_upper_dept=2&upper_bound_lat=-13&lower_bound_lat=-14.514&upper_bound_lon=131.092&lower_bound_lon=130


getASRISService <- function(Dataset){

  url <- getNativeAPIurl(dataset = Dataset)
  return(url)

  # if(str_to_upper(Dataset)=='NATSOIL'){
  #   return('http://asris-daas02/NatSoil_Services/api')
  # }else if(str_to_upper(Dataset)=='NTGOVERNMENT'){
  #   return('http://asris-daas02/NT_Services/api')
  # }else if(str_to_upper(Dataset)=='WAGOVERNMENT'){
  #   return('http://asris-daas02/WA_Services/api')
  # }  else if(str_to_upper(Dataset)=='SCARP'){
  #   return('http://asris-daas02/NatSoil_restricted_Services/api')
  # }else if(str_to_upper(Dataset)=='VICGOVERNMENT'){
  #     return('http://asris-daas02/VIC_Services/api')
  # }else if(str_to_upper(Dataset)=='NSWGOVERNMENT'){
  #   return('http://asris-daas02/NSW_Services/api')
  # }
}


getLocationData_ASRIS<- function(DataSet){

   if(DataSet=='NatSoil'| DataSet=='SCARP'| DataSet=='NTGovernment'| DataSet=='WAGovernment'){
    odf <- get_NatSoilLocation( DataSet)
    }else if(DataSet=='NSWGovernment'){
      odf <- get_NSWLocation(DataSet)
    }else if(DataSet=='VicGovernment'){
        odf <- get_VicLocation(DataSet)
    }else{odf <- blankResponseDF()}

  return(odf)
}



getData_ASRIS <- function(DataSet=NULL, observedProperty=NULL, observedPropertyGroup=NULL ){

  OrgName <- getOrgName(DataSet)
  propRecs <- getNativeProperties(DataSet=DataSet, observedProperty, observedPropertyGroup)

  if(nrow(propRecs) == 0){
    return(blankResponseDF())
  }

  lodfs <- vector("list", nrow(propRecs))

  for (i in 1:nrow(propRecs)) {

    nProp <- propRecs$nativeProp[i]
    sProp <- propRecs$standardProp[i]

    propertyType <- propRecs$propertyType[i]
    units <- getUnits(propertyType = propertyType, prop = sProp)
    if(length(units) == 0){units = '-'}

    if(propertyType == 'LaboratoryMeasurement'){
      ### Hit the Laboratory endpoint

      if(DataSet=='NatSoil' | DataSet=='SCARP'| DataSet=='NTGovernment' | DataSet=='WAGovernment'){
          odf <- get_NatSoilLab(nProp, DataSet)
      }else if(DataSet=='NSWGovernment'){
        odf <- get_NSWLab(nProp, DataSet)
      }else if(DataSet=='VicGovernment'){
          odf <- get_VicData(nProp, DataSet, propertyType)
      }else{odf <- blankResponseDF()}

    }else{
    ### Hit the morpholgy endpoint

      if(DataSet=='NatSoil' | DataSet=='SCARP'| DataSet=='NTGovernment'| DataSet=='WAGovernment'){

          # if(DataSet=='NTGovernment' )
          # {
          #   if(!is.null(observedProperty)){
          #     if(str_to_upper(observedProperty)=='PH_VALUE'){
          #       odf <- NT_pH_Hack(DataSet)
          #     }
          #   }
          # }else{
            odf <- get_NatSoilMorph(nProp, DataSet)
          #}
      }else if(DataSet=='NSWGovernment'){
        odf <- get_NSWMorph(nProp, DataSet)
      }else if(DataSet=='VicGovernment'){
        odf <- get_VicData(nProp, DataSet, propertyType)
      }else{odf <- blankResponseDF()}


    }

    if(nrow(odf) == 0){
        odf <- blankResponseDF()
    }else{
        odf$ObservedProperty <- sProp
        odf$Units <- units
        lodfs[[i]] <- odf
    }
  }

  outDF = as.data.frame(data.table::rbindlist(lodfs))
  return(outDF)
}


NT_pH_Hack<-function(DataSet){

  tcon <- DBI::dbConnect(odbc::odbc(),
                         Driver   = "ODBC Driver 17 for SQL Server",
                         Server   = "asris-sql-stage.it.csiro.au\\sql2017",
                         Database = "NT_NatSoil",
                         UID      = 'NEXUS\\sea084',
                         PWD      = 'Merv4066',
                         Trusted_Connection='Yes'

  )


 sql<- 'SELECT        dbo.OBSERVATIONS.agency_code, dbo.OBSERVATIONS.proj_code, dbo.OBSERVATIONS.s_id, dbo.OBSERVATIONS.o_id, dbo.HORIZONS.h_no, dbo.OBSERVATIONS.o_latitude_GDA94, dbo.OBSERVATIONS.o_longitude_GDA94,
                         dbo.PHS.ph_value, dbo.PHS.ph_depth, dbo.PHS.ph_method, dbo.SITES.s_date_desc, dbo.HORIZONS.h_upper_depth, dbo.HORIZONS.h_lower_depth
FROM            dbo.SITES INNER JOIN
                         dbo.OBSERVATIONS ON dbo.SITES.agency_code = dbo.OBSERVATIONS.agency_code AND dbo.SITES.proj_code = dbo.OBSERVATIONS.proj_code AND dbo.SITES.s_id = dbo.OBSERVATIONS.s_id INNER JOIN
                         dbo.HORIZONS ON dbo.OBSERVATIONS.agency_code = dbo.HORIZONS.agency_code AND dbo.OBSERVATIONS.proj_code = dbo.HORIZONS.proj_code AND dbo.OBSERVATIONS.s_id = dbo.HORIZONS.s_id AND
                         dbo.OBSERVATIONS.o_id = dbo.HORIZONS.o_id INNER JOIN
                         dbo.PHS ON dbo.HORIZONS.agency_code = dbo.PHS.agency_code AND dbo.HORIZONS.proj_code = dbo.PHS.proj_code AND dbo.HORIZONS.s_id = dbo.PHS.s_id AND dbo.HORIZONS.o_id = dbo.PHS.o_id AND
                         dbo.HORIZONS.h_no = dbo.PHS.h_no'

 fdf = doQuery(tcon, sql)

 if(nrow(fdf) > 0){

   oOutDF <- generateResponseDF(DataSet, paste0(fdf$agency_code, '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id ),
                                fdf$h_no, fdf$h_no, fdf$s_date_desc, fdf$o_longitude_GDA94, fdf$o_latitude_GDA94 , fdf$h_upper_depth, fdf$h_lower_depth , 'FieldMeasurement', 'PH_VALUE',fdf$ph_value , 'NA')
    return(oOutDF)
 }
 return(blankResponseDF())

}



# get_TasLab <- function(nProp, DataSet){
#
#   ep <- getASRISService(DataSet)
#   url <- paste0(ep, '/LabResults?method_code=', paste0(nProp ))
#   fdfRaw <- getWebDataDF(url)
#
#   if(length(fdfRaw)==0){
#     oOutDF <- blankResponseDF()
#   }else{
#     if(nrow(fdfRaw) > 0){
#       fdf <- fdfRaw
#       d <- str_split( fdfRaw$sample_date, ' ')
#       d2 <- sapply(d, function (x) x[1])
#       d3 <- as.Date(d2, format = "%m/%d/%y")
#       outDate <- format(d3, '%d-%m-%Y')
#
#       oOutDF <- generateResponseDF(DataSet, paste0(fdf$agency, '_', fdf$survey_number, '_', fdf$profile_ID, '_1'), '1' , outDate, fdf$longitude, fdf$latitude,
#                                    (fdf$bound_upper * 0.01) , (fdf$bound_lower * 0.01), 'LaboratoryMeasurement', fdf$labm_code, fdf$labr_value , 'NA')
#     }else{
#       oOutDF <- blankResponseDF()
#     }
#   }
#   return(oOutDF)
# }






get_VicData <- function(nProp, DataSet,propertyType){

  ep <- getASRISService(DataSet)

#
#   if(str_to_upper(nProp)=='H_NAME'){
#     fdfRaw <- horizonName_Vic(ep)
#   }else{
    url <- paste0(ep, '/LabResults?obs_method=', paste0(nProp ))
    #print(url)
    fdfRaw <- getWebDataDF(url)
  #}

  if(length(fdfRaw)==0){
    oOutDF <- blankResponseDF()
  }else{
    if(nrow(fdfRaw) > 0){
      fdf <- fdfRaw

      fdf <- projectCoords(fdf)

      d <- str_split( fdf$sample_date, 'T')
      d2 <- sapply(d, function (x) x[1])
      d3 <- as.Date(d2, format = "%Y-%m-%d")
      outDate <- format(d3, '%d-%m-%Y')

      if(nProp=='COL_MUNSELL_COLOUR'){
        fdf$obs_value <- str_remove(fdf$obs_value, '/')
      }
      oOutDF <- generateResponseDF(DataSet, paste0(fdf$agency_code, '_', fdf$project_code, '_', fdf$feature_id, '_',  fdf$obs_no), fdf$sample_no, fdf$sample_no , outDate, fdf$Longitude, fdf$Latitude,
                                   fdf$sample_min_lower , fdf$sample_max_lower, propertyType, nProp, fdf$obs_value , 'NA')
    }else{
      oOutDF <- blankResponseDF()
    }
  }
  return(oOutDF)

}

get_VicLocation <- function(DataSet){

  ep <- getASRISService(Dataset=DataSet)
  OrgName <- getOrgName(DataSet)
  url <- paste0(ep, '/LabResults?obs_method=HORIZON')
  fdf <- getWebDataDF(url)
  fdf <- projectCoords(fdf)

  fdfc <- dplyr::count_(fdf, vars = c('agency_code', 'project_code', 'feature_id', 'Longitude', 'Latitude'))
  fdfc <- fdfc[-6]
  fdfcm <- merge(x=fdfc, y=fdf, by = c('agency_code', 'project_code', 'feature_id'), all.x=T)

  fdfcm <- dplyr::count_(fdfcm, vars = c('agency_code', 'project_code', 'feature_id', 'Longitude.x', 'Latitude.x', 'sample_date'))

d <- str_split( fdfcm$sample_date, 'T')
  d2 <- sapply(d, function (x) x[1])
  d3 <- str_split(d2, '-')
  dy <- sapply(d3, function (x) x[3])
  mn <- sapply(d3, function (x) x[2])
  yr <- sapply(d3, function (x) x[1])
  outDate <- paste0(dy, '-', mn, '-', yr)

  oOutDF <-  generateResponseAllLocs(dataset=DataSet, location_ID=paste0(fdfcm$agency_code, '_', fdfcm$project_code, '_', fdfcm$feature_id, '_1'), longitude=fdfcm$Longitude, latitude=fdfcm$Latitude, date=outDate )

}


VicCRSs <- function(){
  zones <- c("54", "54", "54", "54", "54", "54", "54", "54", "55", "55", "55", "55", "55", "55", "55" )
  datums <- c("ADG66", "AGD66", "AGD84","AGDS84", "GDA94","UTM","WGS84",'Undefined',"ADG66", "AGD66","AGD84","GDA94","UTM","WGS84",'Undefined' )
  cnts <- c( 57, 1116, 34, 1, 628, 40, 191, 2181,  123, 1124, 86, 237, 18, 276, 1669)
  epsgs <- c(20254, 20254, 20354, 20354, 28354, 32754, 32754, 32754, 20255, 20255, 20355, 28355, 32755, 32755, 32755)
  ddf <- data.frame(zones,datums,cnts,epsgs)
  return(ddf)
}

VicUniqueDatums <- function(sdf){

  #dts <- dplyr::count_(sdf, vars = c('site_zone', 'site_datum'))
  dts <- sdf %>% count(site_zone, site_datum)
  dts2 <- dts[-3]
  idxs <- which(dts2$site_zone == "" & dts2$site_datum == "",)
  dts3 <- dts2[-idxs,]
  return(dts3)
}

VicEPSG <- function(availDatums, zone, datum){
  if(zone==''){zone<-'Undefined'}
  if(datum==''){datum<-'Undefined'}
  epsg <- availDatums[availDatums$zones == zone & availDatums$datums ==  datum, ]$epsgs
  return(epsg)
}

projectCoords <- function(df){

  sdf <- df
  sdf$Longitude <- NA
  sdf$Latitude <- NA

  allcrs <- VicCRSs()
  ucrs <- VicUniqueDatums(sdf)

  for (i in 1:nrow(ucrs)) {
    rec <- ucrs[i,]
    if(rec$site_zone != '' & rec$site_zone != 65){
        idxs <- which(sdf$site_zone == rec$site_zone & sdf$site_datum == rec$site_datum)
        indf <- sdf[idxs,]
        epsg <- VicEPSG(availDatums=allcrs, zone=rec$site_zone, datum=rec$site_datum)
        df.SP <- st_as_sf(sdf[idxs,], coords = c("site_east", "site_north"), na.fail=F, remove=F, crs=epsg)
        projCoords <- st_transform(df.SP ,  crs = 4326)
        #plot(st_geometry(projCoords))
        xys <- as.data.frame(st_coordinates(projCoords))

        sdf$Longitude[idxs] <- xys$X
        sdf$Latitude[idxs] <- xys$Y
    }
  }
  return(sdf)
}





get_NSWLocation <- function(Dataset){
  ep <- getASRISService(Dataset=Dataset)
  OrgName <- getOrgName(Dataset)
  url <- paste0(ep, '/MorphResults?morphology_attribute=latitude')
  fdf <- getWebDataDF(url)
  d <- str_split( str_trim(fdf$sample_date), ' ')
  d2 <- sapply(d, function (x) x[1])
  d3 <- as.Date(d2, format = "%m/%d/%y")
  outDate <- format(d3, '%d-%m-%Y')
  oOutDF <-  generateResponseAllLocs(dataset=Dataset, location_ID=paste0(fdf$agency, '_', fdf$survey_number, '_', fdf$profile_ID, '_1'), longitude=fdf$longitude, latitude=fdf$latitude, date=outDate )
  return(oOutDF)
}





get_NSWLab <- function(nProp, DataSet){

  ep <- getASRISService(DataSet)
  #nProp <- 'Location'
  url <- paste0(ep, '/LabResults?method_code=', paste0(nProp ))
  print(url)
  fdfRaw <- getWebDataDF(url)


  if(length(fdfRaw)==0){
    oOutDF <- blankResponseDF()
  }else{
        if(nrow(fdfRaw) > 0){
            fdf <- fdfRaw
            d <- str_split( fdfRaw$sample_date, ' ')
            d2 <- sapply(d, function (x) x[1])
            d3 <- as.Date(d2, format = "%m/%d/%y")
            outDate <- format(d3, '%d-%m-%Y')
            oOutDF <- generateResponseDF(DataSet, paste0(fdf$agency, '_', fdf$survey_number, '_', fdf$profile_ID), fdf$soilprofile_layer_id, fdf$soilprofile_sample_id  , outDate, fdf$longitude, fdf$latitude,
                                         fdf$bound_upper , fdf$bound_lower, 'LaboratoryMeasurement', fdf$labm_code, fdf$labr_value , 'NA')
        }else{
          oOutDF <- blankResponseDF()
        }
  }
  return(oOutDF)
}

get_NSWMorph <- function(nProp, DataSet){

  ep <- getASRISService(DataSet)

  if(str_to_upper(nProp)=='COL_HUE_VAL_CHROM'){

    url <- paste0(ep, '/MorphResults?morphology_attribute=Colourmoistmunsellcolourhue' )
    #print(url)
    fdfhue <- getWebDataDF(url)
    names(fdfhue)[12]<- 'hue'

    url <- paste0(ep, '/MorphResults?morphology_attribute=Colourmoistmunsellcolourhuevalue' )
    fdfhv <- getWebDataDF(url)
    mm1 <- fdfhv[1:12]
    names(mm1)[12]<- 'hv'
    m1 <- merge(fdfhue[1:12], mm1, by=names(fdfhue)[1:11])

    url <- paste0(ep, '/MorphResults?morphology_attribute=Colourmoistmunsellcolourvalue' )
    fdfVal<- getWebDataDF(url)
    mm2 <- fdfVal[1:12]
    names(mm2)[12]<- 'value'
    m2 <- merge(m1, mm2, by=names(m1)[1:11])

    url <- paste0(ep, '/MorphResults?morphology_attribute=Colourmoistmunsellcolourchroma' )
    fdfCrom <- getWebDataDF(url)
    mm3 <- fdfCrom[1:12]
    names(mm3)[12]<- 'chroma'
    m3 <- merge(m2, mm3, by=names(m2)[1:11])
    m3$morphology_attribute_value <- paste0(m3$hv, m3$hue,  m3$value, m3$chroma)
    m4<- m3[-c(12:15)]

    fdfRaw <- m3
    fdfRaw$morphology_attribute <- 'COL_HUE_VAL_CHROM'

  }else if(str_to_upper(nProp) =='H_NAME'){
    fdfRaw <- horizonName_NSW(ep)
  }else {

    url <- paste0(ep, '/MorphResults?morphology_attribute=', nProp )
    fdfRaw <- getWebDataDF(url)
  }



  if(length(fdfRaw)==0){
    oOutDF <- blankResponseDF()
  }else{
    if(nrow(fdfRaw) > 0){
        fdf <- fdfRaw

        d <- str_split( fdfRaw$sample_date, ' ')
        d2 <- sapply(d, function (x) x[1])
        d3 <- as.Date(d2, format = "%m/%d/%y")
        outDate <- format(d3, '%d-%m-%Y')
        oOutDF <- generateResponseDF(DataSet, paste0(fdf$agency, '_', fdf$survey_number, '_', fdf$profile_ID), fdf$soilprofile_layer_id, 'NA' , outDate, fdf$longitude, fdf$latitude,
                                     fdf$bound_upper , fdf$bound_lower, 'FieldMeasurement', fdf$morphology_attribute, fdf$morphology_attribute_value , 'NA')

    }else{
      oOutDF <- blankResponseDF()
    }
  }
  return(oOutDF)
}





get_NatSoilMorph <- function(nProp, DataSet){

  ep <- getASRISService(DataSet)

  if(str_to_upper(nProp)=='H_NAME'){
    fdfRaw <- horizonName_NatSoil(ep)

  }else{
    url <- paste0(ep, '/MorphResults?morphology_attribute=', nProp )
    print(url)
    fdfRaw <- getWebDataDF(url)
  }





  if(length(fdfRaw)==0){
    oOutDF <- blankResponseDF()
  }else{
    if(nrow(fdfRaw) > 0){
      fdf <- fdfRaw

      day <- str_sub(fdf$o_date_desc, 1,2)
      mnth <- str_sub(fdf$o_date_desc, 3,4)
      yr <- str_sub(fdf$o_date_desc, 5,8)
      outDate <- paste0(day, '-', mnth,'-', yr)

      oOutDF <- generateResponseDF(DataSet, paste0(fdf$agency_code , '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$h_no, fdf$samp_no , outDate, fdf$o_longitude_GDA94, fdf$o_latitude_GDA94,
                                   fdf$h_upper_depth , fdf$h_lower_depth,  'FieldMeasurement', fdf$morphology_attribute, fdf$morphology_attribute_value , 'NA')

    }else{
      oOutDF <- blankResponseDF()
    }
  }

  return(oOutDF)
}

get_NatSoilLab <- function(nProp, DataSet){

  ep <- getASRISService(DataSet)
  url <- paste0(ep, '/LabResults?method_code=', paste0(nProp ))
  fdfRaw <- getWebDataDF(url)

  if(length(fdfRaw)==0){
    oOutDF <- blankResponseDF()
  }else{
    if(nrow(fdfRaw) > 0){
      fdf <- fdfRaw

      day <- str_sub(fdf$o_date_desc, 1,2)
      mnth <- str_sub(fdf$o_date_desc, 3,4)
      yr <- str_sub(fdf$o_date_desc, 5,8)
      outDate <- paste0(day, '-', mnth,'-', yr)

     # print(fdf$o_date_desc)

      oOutDF <- generateResponseDF(DataSet, paste0(fdf$agency_code , '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), fdf$h_no, fdf$samp_no , outDate, fdf$o_longitude_GDA94, fdf$o_latitude_GDA94,
                                   fdf$samp_upper_depth , fdf$samp_lower_depth, 'LaboratoryMeasurement', fdf$labm_code, fdf$labr_value , 'NA')
    }else{
      oOutDF <- blankResponseDF()
    }
  }
  return(oOutDF)
}

get_NatSoilLocation <- function(Dataset){
  ep <- getASRISService(Dataset=Dataset)
  OrgName <- getOrgName(Dataset)
  url <- paste0(ep, '/MorphResults?morphology_attribute=s_date_desc')
  fdf <- getWebDataDF(url)
  day <- str_sub(fdf$o_date_desc, 1,2)
  mnth <- str_sub(fdf$o_date_desc, 3,4)
  yr <- str_sub(fdf$o_date_desc, 5,8)
  outDate <- paste0(day, '-', mnth,'-', yr)
  oOutDF <-  generateResponseAllLocs(dataset=Dataset, location_ID=paste0(fdf$agency_code , '_', fdf$proj_code, '_', fdf$s_id, '_', fdf$o_id), longitude=fdf$o_longitude_GDA94, latitude=fdf$o_latitude_GDA94, date=outDate )
  return(oOutDF)
}



horizonName_NatSoil<- function(ep){

  url <- paste0(ep, '/MorphResults?morphology_attribute=h_desig_num_pref')
  fdfpref <- getWebDataDF(url)
  names(fdfpref)[15]<- 'pref'
  mm0 <- fdfpref[-14]

  url <- paste0(ep, '/MorphResults?morphology_attribute=h_desig_master' )
  fdfmaster <- getWebDataDF(url)
  names(fdfmaster)[15]<- 'master'
  mm1 <- fdfmaster[-14]
  m1 <- merge(mm0, mm1, by=names(fdfmaster)[1:13], all=T)
  # print(head(m1))

  url <- paste0(ep, '/MorphResults?morphology_attribute=h_desig_subdiv' )
  fdfsubdiv <- getWebDataDF(url)
  names(fdfsubdiv)[15]<- 'subdiv'
  mm2 <- fdfsubdiv[-14]
  m2 <- merge( m1,mm2, by=names(m1)[1:13], all=T)
  # print(head(m2))

  m2$morphology_attribute_value <- str_remove_all( paste0(m2$pref, m2$master,  m2$subdiv), 'NA')
  m3 <- m2[-c(14:16)]
  # print(head(m3))
  fdfRaw <- m3
  fdfRaw$morphology_attribute <- 'h_name'
  return(fdfRaw)
}


horizonName_NSW <- function(ep){

  url <- paste0(ep, '/MorphResults?morphology_attribute=Statushorizoncodehorizon')
   fdfmaster <- getWebDataDF(url)
   names(fdfmaster)[12]<- 'master'
   mm0 <- fdfmaster[-c(13:14)]

  url <- paste0(ep, '/MorphResults?morphology_attribute=Statushorizoncodesubdivision')
  fdfsubdiv <- getWebDataDF(url)
  names(fdfsubdiv)[12]<- 'subdiv'
  mm1 <- fdfsubdiv[-c(13:14)]
  m1 <- merge(mm0, mm1, by=names(fdfmaster)[1:11], all=T)

  m1$morphology_attribute_value <- paste0(m1$master, m1$subdiv)

  fdfRaw <- m1
  fdfRaw$morphology_attribute <- 'H_NAME'
  return(fdfRaw)
}



