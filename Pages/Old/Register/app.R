library(shiny)
library(RSQLite)
library(DBI)
library(stringr)




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





addUser <- function(email, firstName, lastName, organisation){

  user <- doQueryFromFed(paste0("Select * from AuthUsers where usrID = '", email, "';"))

  if(nrow(user) > 0 ){
    msg <- paste0("The email address already has an associated API Key.<br> Please contact ross.searle@csiro.au to sort this out.")
  }else{

    apiKey <- makeRandomString(1)
    sqlInsert <- paste0("Insert into AuthUsers ( usrID, FirstName, Surname, Organisation, GroupName, Pwd )
    values ('", email, "', '", firstName ,"', '",lastName, "', '", organisation, "', 'Public',  '", apiKey, "')")



    allGood <- insertUser(sqlInsert)

    if(allGood){

      url <-URLencode(paste0('https://shiny.esoil.io/SoilDataFederator/Pages/RegisterConfirm/?&email=', email, '&firstname=', firstName, '&lastname=', lastName, '&organisation=',organisation, '&tkey=', apiKey ))
      print(url)
      emailInfo <- paste0('echo "<p> Dear ', firstName, ',</p><p><br></p><p>Thanks for registering with the TERN SoilDataFederator. Click <a href=', url, '> HERE </a> to confirm your registration and obtain an API key</p><p><br></p><p>Regards</p><p>The TERN SoilDataFederator" | mail -s "$(echo "SoilDataFederator API Key Request\nContent-Type: text/html")" ', email,' -r no-reply@soils-discovery.csiro.au')
      system(emailInfo)
      msg <- "An email has been sent to you from 'no-reply@soils-discovery@csiro.au. Please click on the link in the email to confirm your registration."


    }else{
      msg <- paste0("There was a problem trying to register " , email, ". Please contact ross.searle@csiro.au to sort this out.")
    }

  }
  return(msg)
}

insertUser <- function(sqlInsert){
  result <- tryCatch({
    authcon <- dbConnect(RSQLite::SQLite(), dbPathSoilsFed, flags = SQLITE_RW)

    res <- dbSendStatement(authcon, sqlInsert)
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


   tags$head(
       includeCSS("http://esoil.io/TERNLandscapes/SoilDataFederatoR/help/Templates/styles/stylesV6.css")),

   tags$div(class="container",
        tags$div( class="header",
            tags$div(class="headertext1", "Soil and Landscape Grid"),
            tags$div(class="headertext2", "of Australia")),

        tags$div(id="content",

            uiOutput("P1"),
            br(),
            tags$table(align="left", width="400px",
              tags$tr(width = "60%",
                      tags$td(width = "30%", div(style = "font-size:14px; font-weight:bold; text-align: right;", "Email Address")),
                      tags$td(width = "30%", textInput(inputId = "txtEmail", label = NULL, width = 200))),
              tags$tr(width = "60%",
                      tags$td(width = "30%", div(style = "font-size:14px; font-weight:bold; text-align: right;", "First Name")),
                      tags$td(width = "30%", textInput(inputId = "txtFirstName", label = NULL, width = 200))),
              tags$tr(width = "60%",
                      tags$td(width = "30%", div(style = "font-size:14px; font-weight:bold; text-align: right;", "Last Name")),
                      tags$td(width = "30%", textInput(inputId = "txtLastName", label = NULL, width = 200))),
              tags$tr(width = "60%",
                      tags$td(width = "30%", div(style = "font-size:14px; font-weight:bold; text-align: right;", "Organisation")),
                      tags$td(width = "30%", textInput(inputId = "txtOrganisation", label = NULL, width = 200))),
              tags$tr(width = "60%",
                      tags$td(width = "30%", HTML("<P></P>")),
                      tags$td(width = "30%", "")),
              tags$tr(width = "60%",
                      tags$td(width = "30%", ""),
                      tags$td(width = "30%", actionButton("doSubmit", "Generate API Key"))),
              tags$tr(width = "60%",
                      tags$td(width = "30%", HTML("<P>&nbsp</P>")),
                      tags$td(width = "30%", HTML("&nbsp"))),
              tags$tr(width = "60%",
                      #tags$td(width = "30%", ""),
                      tags$td(width = "30%", colspan="2", HTML("<P>&nbsp"), div(style = "font-size:14px; font-weight:bold; color:#ebb72f; text-align: center;", uiOutput("SubmitMessage"),  HTML("</P>"))))
              ),

            br(),br(),br(),

            uiOutput("P2")
        )
   )


)

server <- function(session, input, output) {

RV <- reactiveValues()
RV$message <- NULL

    subMsg <- eventReactive(input$doSubmit, {

      msg <- ''
      if(!isValidEmail(input$txtEmail)){
          msg <- "The email you supplied does not seem to be valid."
      }else if(!isValidString(input$txtFirstName)){
          msg <- "The supplied first name does not seem to be valid."
      }else if(!isValidString(input$txtLastName)){
          msg <- "The supplied last name does not appear to be valid."
      }else if(!isValidString(input$txtOrganisation)){
          msg <- "The supplied organisation name does not appear to be valid."
      }else{

        msg <- addUser(input$txtEmail, input$txtFirstName, input$txtLastName, input$txtOrganisation)

      }

      return(msg)
    })


        output$SubmitMessage <- renderText({
          paste0(subMsg())

          })




    output$P1 <- renderText({

        paste0( '<H1>SoilDataFederator API Registration</H1>
       <p> </p>
        <p>Register with the SoilFederator to obtain an API Key to access the soil data. We require this just so we can update you of changes to the system etc. We may contact you once in a while to ask about how you are using the system. </p>
        <p>Fill in your details below, click submit and you will be sent a registration confirmation email to the address supplied.</p>
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
