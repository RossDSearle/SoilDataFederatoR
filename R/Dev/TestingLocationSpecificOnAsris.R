
#props <- fromJSON('http://asris-daas02/WA_Services/api/MorphologyMetaData')


#12499

getDataForASite <- function(siteID){

outdf<- data.frame()
for(i in 1:min(100, nrow(props))){

  att <- props[i,]$Morphology_Class_Attribute
  d <- fromJSON(paste0('http://asris-daas02/NatSoil_Services/api/MorphResults?morphology_attribute=', att, '&agency_code=180&proj_code=BASE&s_id=', siteID, '&o_id=1'))

  attdf <- data.frame( h_upper_depth=d$h_upper_depth,
                       h_lower_depth=d$h_lower_depth,
                       morphology=d$morphology,
                       morphology_attribute=d$morphology_attribute,
                       morphology_attribute_value=d$morphology_attribute_value,
                       stringsAsFactors = F)
  outdf <- rbind(outdf, attdf)
  #print(i)
}

d2 <- na.omit(outdf)
d3 <- d2[d2$ morphology_attribute!='', ]
return(d3)
# http://asris-daas02/WA_Services/api/MorphResults?morphology_attribute=h_texture
#
# d <- fromJSON('http://asris-daas02/NatSoil_Services/api/MorphResults?morphology_attribute=h_texture&agency_code=180&proj_code=BASE&s_id=12499&o_id=1')
# nrow(d)


}
