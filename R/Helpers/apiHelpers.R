



cerealize <- function(DF, label, format, res){
  if(format == 'xml'){

    res$setHeader("Content-Type", "application/xml; charset=utf-8")
    xdoc=xml_new_root(paste0(label, 's'))
    vars_xml <- lapply(purrr::transpose(DF),
                       function(x) {
                         #as_xml_document(list(DataProvider = lapply(x, as.list)))
                         as_xml_document(setNames(list(lapply(x, as.list)), label))
                       })

    for(trial in vars_xml) xml_add_child(xdoc, trial)
    res$body <- as(xdoc, "character")
    return(res)

  }else if(format == 'csv'){
    res$setHeader("content-disposition", "attachment; filename=FederatedSoilData.csv");
    res$setHeader("Content-Type", "application/csv; charset=utf-8")
    res$body <- writecsv(DF)
    return(res)

  }else if(format == 'html'){
    res$setHeader("Content-Type", "text/html ; charset=utf-8")
    res$body <- htmlTable(DF, align = "l", align.header = "l", caption = "Data Providers")
    return(res)

  }else{
    return(DF)
  }


}
