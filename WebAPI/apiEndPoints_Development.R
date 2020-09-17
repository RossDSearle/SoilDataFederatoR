#* Returns all data for a given site.

#* @param format (Optional) Format of the response to return. Either json, csv, or xml. Default = json
#* @param SiteID Site identifier


#* @tag Soil Data Federator
#* @get /getSite
apiGetSiteData <- function(res, SiteID, format='json'){

  tryCatch({

    # DF <- getDataForASite(SiteID)
    # print(DF)
    # label <- 'SiteData'
    # resp <- cerealize(DF, label, format, res)

    resp <- cerealize('This is just a test', label, format, res)

  }, error = function(res)
  {
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))

  })
}
