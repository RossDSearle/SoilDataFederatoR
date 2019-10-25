library(httr)
library(jsonlite)
library(RCurl)
library(data.table)


# labresults <- fromJSON(paste0("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?%24filter=ProjectCode%20eq%20'3MC' and labMethodCode eq '3A1'&%24orderby=SiteId"))
# samples <- fromJSON(paste0("https://soil-chem.information.qld.gov.au/odata/Samples?%24filter=ProjectCode%20eq%20'3MC'&%24orderby=SiteId"))
# testvals <- fromJSON(URLencode("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?%24filter=ProjectCode%20eq%20%273MC%27%20and%20LabMethodCode%20eq%20%273A1%27"))
# testvals <- fromJSON(URLencode("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?$filter=ProjectCode eq '3MC' and LabMethodCode eq '3A1'"))
# testvals <- fromJSON(URLencode("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?$filter=LabMethodCode eq '3A1'"))
# testvals <- fromJSON(URLencode("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?$filter=LABP_CODE eq 'PH'"))
# nrow(testvals)
# head(testvals)

getData_QLDGovernment <- function(DataSet, DataStore, observedProperty, observedPropertyGroup=NULL ){

  OrgName <- getOrgName(DataSet)

  samples <- fromJSON(paste0("https://soil-chem.information.qld.gov.au/odata/Samples"))
  mappings <- doQueryFromFed(paste0("Select * from Mappings where Organisation = '", OrgName, "'" ))

  nativeProps <- getNativeProperties(OrgName, mappings, observedProperty, observedPropertyGroup)

  if(length(nativeProps) == 0){
    return(blankResponseDF())
  }

  lodfs <- list(length(nativeProps))

    for (i in 1:length(nativeProps)) {

      prop <- nativeProps[i]

      sd <- fromJSON(URLencode(paste0("https://soil-chem.information.qld.gov.au/odata/SiteLabMethodResults?$filter=LabMethodCode eq '", prop, "'")))
      if(nrow(sd) > 0){
          fdf <- merge(sd, samples,  by=c("projectCode","siteId", "observationNumber", "sampleNumber"), all.x = T)

          propertyType <- getPropertyType(prop)
          units <- getUnits(propertyType = propertyType, prop = prop)

          oOutDF <- generateResponseDF(OrgName, DataSet, paste0(fdf$projectCode , '_', fdf$siteId, '_', fdf$observationNumber ),
                                       fdf$sampleNumber, fdf$analysisDate, fdf$longitude , fdf$latitude,
                                       fdf$upperDepth, fdf$lowerDepth, propertyType, prop, fdf$formattedValue , units)
          lodfs[[i]] <- oOutDF
      }else{
        return(blankResponseDF())
      }
    }

  outDF = as.data.frame(data.table::rbindlist(lodfs))

  return(outDF)
}



