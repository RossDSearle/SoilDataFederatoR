

library(shiny)
library(RSQLite)
library(DBI)
library(stringr)
library(data.table)
library(tableHTML)
library(htmlTable)



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


ui <- fluidPage(

    titlePanel("SoilDataFederator Datasets"),
    uiOutput("P1")
)

server <- function(input, output) {

    output$P1 <- renderText({
        sql <- paste0('select * from DataSets where Restricted = FALSE')
        df <- doQueryFromFed(sql)
        df$OrgURL <- paste0('<a href=', df$OrgURL ,  ' target="_blank" >', df$OrgURL , '</a>')
        df$ContactEmail <- paste0('<a href=mailto:', df$ContactEmail  ,  '  >', df$ContactEmail , '</a>')


        t <- htmlTable(df, align=paste(rep('l',ncol(df)),collapse=''),
                       align.header=paste(rep('l',ncol(df)),collapse=''),
                        rnames=F, caption='<b>DataSets</b>',
                        css.cell = "padding-left: 1em; padding-right: 1em;",
                       col.rgroup = c('white', 'lightgrey'),
                       css.table = 'width: 90% ;  font-family: "Trebuchet MS", Arial, Helvetica, sans-serif;
  border-collapse: collapse;'

                       )
    })
}

shinyApp(ui = ui, server = server)
