library(shiny)
library(RSQLite)
library(DBI)
library(stringr)
library(shinyalert)

# https://shiny.esoil.io/SoilDataFederator/RegisterConfirm/?&email=ross.searle@gmail.com&firstname=Georgia&lastname=Searle&organisation=Brisbane&tkey=2jpft6WvquTLjwO9XqFD

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='soils-discovery'){
  dbPathSoilsFed <- '/srv/plumber/TERNLandscapes/SoilDataFederatoR/DB/soilsFederator.sqlite'
}else{
  dbPathSoilsFed <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/DB/soilsFederator.sqlite'
}

doQueryFromFed <- function(sql){
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbPathSoilsFed)
  qry <- dbSendQuery(conn, sql)
  res <- dbFetch(qry)
  dbClearResult(qry)
  dbDisconnect(conn)
  return(res)
}

makeRandomString <- function(n=1)
{
  lenght = sample(c(10:20))
  randomString <- c(1:n)
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),lenght, replace=TRUE),collapse="")
  }
  return(randomString)
}

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

isValidString <- function(x) {
  return(!(is.na(x) || x == '' || is.null(x)))
}





confirmUser <- function(email, firstName, lastName, organisation, tKey){

  sqlQry <- paste0("Select * from AuthUsers where usrID = '", email, "' and Pwd = '", tKey, "';")
  print(sqlQry)
  user <- doQueryFromFed(sqlQry)


  if(nrow(user) > 0 ){

    apiKey <- makeRandomString(1)
    sqlUpdate <- paste0("Update AuthUsers SET Pwd = '", apiKey, "' where usrID = '", email, "' and Pwd = '", tKey, "';")

    if(updateKey(sqlUpdate)){
      msg <- paste0("<P>Thanks for registering with the SoilDataFederator Web API</P><P>Your SoilDataFederator API Key is - <span style='color:#FF0000; font-weight:bold;'>", apiKey, "</span></P><P>We just sent you an email containing the API key for your records. Please keep this somewhere safe.</P>")
      emailInfo <- paste0('echo "<p> Dear ', firstName, ',</p><p><br></p><p>Thanks for registering with the TERN SoilDataFederator. </P><P>Your API key is - <span style=color:#FF0000;font-weight:bold;>',  apiKey,'</span></p><p><br></p><p>Regards</p><p>The TERN SoilDataFederator" | mail -s "$(echo "SoilDataFederator API Key Confirmation\nContent-Type: text/html")" ', email,' -r no-reply@soils-discovery.csiro.au')
      system(emailInfo)
      }else{
      msg <- paste0("Oh Dear.... Something went wrong but I don't really know what!")
    }
  }else{
      msg <- paste0("<P>Thanks for trying to register with the SoilDataFederator Web API, but unfortunatley we could not confirm the registration for " , email, ".</P><P> Please contact ross.searle@csiro.au to sort this out.</P><P> Did you click on the link in the email we sent you?<P>")
  }

  return(msg)
}

updateKey <- function(sqlUpdate){
  result <- tryCatch({
    authcon <- dbConnect(RSQLite::SQLite(), dbPathSoilsFed, flags = SQLITE_RW)

    res <- dbSendStatement(authcon, sqlUpdate)
    nr <- dbGetRowsAffected(res)
    print(nr)
    dbDisconnect(authcon)
    return(T)

  }, error = function(err) {
    print(err)
    return(F)
  })
}




ui <- fluidPage(

  useShinyalert(),
  tags$head(
    includeCSS("http://esoil.io/TERNLandscapes/SoilDataFederatoR/help/Templates/styles/stylesV6.css")),

  tags$div(class="container",
           tags$div( class="header",
                     tags$div(class="headertext1", "Soil and Landscape Grid"),
                     tags$div(class="headertext2", "of Australia")),

           tags$div(id="content",

                    uiOutput("P1"),
                    #br(),
                    uiOutput("APIKeyMessage"),
                    br(),br(),br(),
                    tags$table(align="left", width="400px",
                               tags$tr(width = "60%",
                                       tags$td(width = "30%", div(style = "font-size:14px; font-weight:bold; text-align: right;", "Email Address : ")),
                                       tags$td(width = "30%",  uiOutput("T1"))),
                               tags$tr(width = "60%",
                                       tags$td(width = "30%", div(style = "font-size:14px; font-weight:bold; text-align: right;", "First Name : ")),
                                       tags$td(width = "30%", uiOutput("T2"))),
                               tags$tr(width = "60%",
                                       tags$td(width = "30%", div(style = "font-size:14px; font-weight:bold; text-align: right;", "Last Name : ")),
                                       tags$td(width = "30%", uiOutput("T3"))),
                               tags$tr(width = "60%",
                                       tags$td(width = "30%", div(style = "font-size:14px; font-weight:bold; text-align: right;", "Organisation : ")),
                                       tags$td(width = "30%", uiOutput("T4")),
                               tags$tr(width = "60%",
                                       tags$td(width = "30%", HTML("<P></P>"))),
                                       tags$td(width = "30%", "")),
                               tags$tr(width = "60%",
                                       tags$td(width = "30%", ""),
                                       tags$td(width = "30%", "")),
                               tags$tr(width = "60%",
                                       tags$td(width = "30%", HTML("<P>&nbsp</P>")),
                                       tags$td(width = "30%", HTML("&nbsp")))

                    ),



                    uiOutput("P2")
           )
  )


)

server <- function(session, input, output) {

  RV <- reactiveValues()
  RV$email <- NULL
  RV$firstname <- NULL
  RV$lastname <- NULL
  RV$organisation <- NULL
  RV$Result <- NULL
  RV$TmpKey <- NULL

  observe({
    query <- parseQueryString(session$clientData$url_search)
      RV$email <- query[['email']]
      RV$firstname <-  query[['firstname']]
      RV$lastname <-  query[['lastname']]
      RV$organisation <-  query[['organisation']]
      RV$TmpKey <- query[['tkey']]
      print(paste0('Key = ', RV$TmpKey))

      #shinyalert("Debug", paste0(RV$email, RV$firstname, RV$lastname, RV$organisation, RV$TmpKey), type = "info")

      msg <- confirmUser(RV$email, RV$firstname, RV$lastname, RV$organisation, RV$TmpKey)
      RV$Result <- msg

  })





  output$T1<- renderText({
    RV$email
  })

  output$T2<- renderText({
    RV$firstname
  })

  output$T3<- renderText({
    RV$lastname
  })

  output$T4<- renderText({
    RV$organisation
  })


output$APIKeyMessage <- renderText({
  RV$Result
})

  output$P1 <- renderText({

    paste0( '<H1>SoilDataFederator API Confirmation</H1>


')
  })

  output$P2 <- renderText({

    paste0('<br><br> <br><br><p></p>
        <div id="footer">
        <h1 class="textinbar">SLGA Funders<span style="margin-left: 22%;">Partners</span></h1>
        <div id="partners"> <a href="http://www.ga.gov.au/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/AustGovtGSA_stack.gif" alt="Australian Government Geosciences Australia logo" height="59" width="90"></a> <a href="http://www.csiro.au/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/CSIRO.gif" alt="CSIRO logo" height="54" width="55"></a> <a href="http://www.environment.nsw.gov.au/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/NSWOEH.gif" alt="NSW Office of Environment and Heritage logo" height="44" width="120"></a> <a href="http://www.lrm.nt.gov.au/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/NTGovt.gif" alt="Northern Territory Government logo" height="55" width="59"></a> <a href="http://www.qld.gov.au/dsitia/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/QLDGovt.gif" alt="Queensland Government logo" height="69" width="55"></a> <a href="http://www.environment.sa.gov.au/Home" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/SAGovt_DEWNR.gif" alt="South Australian Department of Environment, Water and Natural Resources logo" height="67" width="85"></a> <a href="http://dpipwe.tas.gov.au/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/TasGov.png" style="margin-right: 0%;" alt="Tasmanian Government logo" height="56" width="68"></a> <a href="http://sydney.edu.au/agriculture/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/Uni-of-Sydney.gif" alt="University of Sydney logo" height="44" width="112"></a> <a href="http://www.depi.vic.gov.au/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/VICGovt_DEPI.gif" alt="Victorian Government Department of Environment and Primary Industries logo" height="32" width="137"></a> <a href="https://www.agric.wa.gov.au/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/WAGovt_DAF.gif" alt="Western Australian Government Department of Agriculture and Food logo" height="38" width="168"></a> <a href="http://www.clw.csiro.au/aclep/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/ACLEP.gif" alt="ACLEP logo" height="44" width="165"></a> </div>
        </div>
        <div id="funder">
        <p> <a href="http://www.tern.org.au/" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/TERNLogo.gif" alt="TERN logo" height="83" width="90"></a><a href="https://education.gov.au/national-collaborative-research-infrastructure-strategy-ncris" target="_blank"><img src="http://www.clw.csiro.au/aclep/soilandlandscapegrid/logos/NCRIS.gif" style="margin-left: 8%;" alt="NCRIS logo" height="62" width="130"></a></p>
        <p>TERN is supported by the Australian Government through the National
        Collaborative Research Infrastructure Strategy and the Super Science
        Initiative.</p>
        </div>
        </div>')
  })
}

shinyApp(ui = ui, server = server)
