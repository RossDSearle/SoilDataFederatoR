

library(shiny)
library(RSQLite)
library(DBI)
library(stringr)
library(data.table)
library(tableHTML)
library(htmlTable)



dbPathSoilsFed <- '/srv/plumber/TERNLandscapes/SoilDataFederatoR/DB/soilsFederator.sqlite'


doQueryFromFed <- function(sql){

    conn <- DBI::dbConnect(RSQLite::SQLite(), dbPathSoilsFed)
    qry <- dbSendQuery(conn, sql)
    res <- dbFetch(qry)
    dbClearResult(qry)
    dbDisconnect(conn)
    return(res)
}


ui <- fluidPage(

    titlePanel("Soil Fed Docs"),
    uiOutput("P1")
)

server <- function(input, output) {

    output$P1 <- renderText({
        sql <- paste0('select * from Providers')
        df <- doQueryFromFed(sql)
        df$OrgURL <- paste0('<a href=', df$OrgURL ,  ' target="_blank" >', df$OrgURL , '</a>')
        df$ContactEmail <- paste0('<a href=mailto:', df$ContactEmail  ,  '  >', df$ContactEmail , '</a>')


        t <- htmlTable(df, align=paste(rep('l',ncol(df)),collapse=''),
                       align.header=paste(rep('l',ncol(df)),collapse=''),
                        rnames=F, caption='<b>DataStores</b>',
                        css.cell = "padding-left: 1em; padding-right: 1em;",
                       col.rgroup = c('white', 'lightgrey'),
                       css.table = 'width: 90% ;  font-family: "Trebuchet MS", Arial, Helvetica, sans-serif;
  border-collapse: collapse;'

                       )
    })
}

shinyApp(ui = ui, server = server)
