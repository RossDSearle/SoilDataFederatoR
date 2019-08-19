


  sql <- paste0("Select * from Providers where Active = 1 and Availability = 'Public'")
  #
  orgs = doQueryFromFed(sql)
  return(orgs)
