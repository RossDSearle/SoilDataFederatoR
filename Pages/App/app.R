
library(dplyr)
library(shiny)
library(shinythemes)
library(httr)
library(jsonlite)
library(DT)
library(shinyWidgets)
library(rhandsontable)
library(leaflet)
library(leafgl)
library(sf)
library(ggplot2)
library(data.table)
library(colourvalues)

DEBUG <- T

source("helpers.R")

#testdf <- read.csv('c:/temp/df.csv', stringsAsFactors = F)
#serverLoc <- 'http://127.0.0.1:6902'
serverLoc  <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR'

DefPropGrp <- 'Nitrogen'
DefProp <- '7A1'

url <- paste0(serverLoc, '/SoilDataAPI/DataSets')
providerList <- fromJSON(url)

print(getwd())


insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}



getWebDataJSON <- function(url){
  ue <- URLencode(url)
  resp <- GET(ue, timeout = 300)
  response <- content(resp, "text", encoding = 'UTF-8')
  return(response)
}

getWebDataDF <- function(url){
  r <- getWebDataJSON(url)
  md <- fromJSON(r)
  return(md)
}


blankResponseDF <- function(){

  outDF <- data.frame(DataStore=character(), Dataset=character(), Provider=character(), Observation_ID=character(), SampleID=character(), SampleDate=character() ,
                      Longitude=numeric() , Latitude= numeric(),
                      UpperDepth=numeric() , LowerDepth=numeric() , PropertyType=character(), ObservedProperty=character(), Value=numeric(),
                      Units= character(),   QualCollection=integer(), QualSpatialAggregation=integer(), QualManagement=integer(),QualSpatialAccuracy=integer(), stringsAsFactors = F)
}


# Define UI for random distribution app ----
ui <- fluidPage(

  titlePanel(
    tags$head(tags$link( rel="icon", type="image/png", href="barrow50.png", sizes="32x32" ),
              tags$title("SoilDataFederator")
    )),


  tags$style(appCSS),
  theme = shinytheme("flatly"),


  titlePanel(HTML("<img src=HeaderWithTern2.png style='vertical-align: top;'>")),


  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel( width=2,
      fluidRow(textInput('authusr', 'User Name', value='ross.searle@csiro.au' )),
      fluidRow(passwordInput('authkey', 'API key', value='a')),
      fluidRow(selectInput('currentProvider', 'Select a Provider', choices = c('All', providerList$OrgFullName))),
      fluidRow(selectInput('propTypes', 'Select a Provider', choices = c('FieldMeasurement', 'LaboratoryMeasurement'), selected='LaboratoryMeasurement')),
      fluidRow(selectInput('propGrps', 'Property Groups', choices = NULL)),
      fluidRow(selectInput('propObs', 'Observed Property', choices = NULL)),
      tags$br(),
      progressBar(
        id = "pb",
        value = 0,
        total = 100,
        title = "Test",
        display_pct = TRUE
      ),

      # withBusyIndicatorUI(
      fluidRow(column(4, actionButton("getData2","Get Data", class = "btn-success")),
               column(8,downloadButton('downloadData', 'Download Data'))
      )
      #)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Data",
                           #rHandsontableOutput("dataTab")
                          tags$div(DT::dataTableOutput("dataTab"), style = "font-size:80%" )
                  ),

                  tabPanel("Summary", verbatimTextOutput("summary"),
                           htmlOutput("SummaryText"),
                           rHandsontableOutput("summaryTab"),
                           plotOutput('dplot'),
                           plotOutput('bplot'),
                           rHandsontableOutput("perProvTab")
                  ),
                  tabPanel("Map",
                           leafletOutput("sitesMap", width = "800", height = "600")
                  ),
                  tabPanel("Data Contibutors",
                           rHandsontableOutput("contributorsTab")
                  ),
                  tabPanel("API Requests",
                           rHandsontableOutput("apiTab")
                  )
      )

    )
  )
)




server <- function(session, input, output) {

  RV <- reactiveValues()
  RV$currentdata = NULL
  RV$apiDF <- data.frame(Time=as.character(Sys.time()), Request=paste0(serverLoc, '/SoilDataAPI/Providers'), stringsAsFactors = F)


#####  Download Data     ##########
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('TERNSoilsFederator-', input$propObs, '-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(RV$currentdata, con)
    }
  )

  #####  Output Contributors Table    ##########
  output$contributorsTab = renderRHandsontable({
    url <- paste0(serverLoc, '/SoilDataAPI/Providers')
    contribs <- getWebDataDF(url)
    contribs$ContactEmail <- paste0('<a href=mailto:', contribs$ContactEmail, '?subject=Soil%20Data%20on%20TERN-Landscapes%20SoilDataFederator>', contribs$ContactEmail, '</a>'  )
    contribs$OrgURL <- paste0('<a href=', contribs$OrgURL, ' target="_blank">', contribs$OrgURL, '</a>'  )
    rhandsontable(contribs , manualColumnResize = T, readOnly = TRUE, rowHeaders = F) %>%
      hot_col('ContactEmail', renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_col('OrgURL', renderer = htmlwidgets::JS("safeHtmlRenderer"))
  })

  output$apiTab = renderRHandsontable({
    req(RV$apiDF)
    rhandsontable(RV$apiDF , manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
  })

  observe({

    #RV$currentdata <- testdf
  })


#####  Draw Map  ##########
  output$sitesMap <- renderLeaflet({

    req(RV$currentdata )

    df <- as.data.frame(RV$currentdata)
    idxs <- which(!is.na(df$Longitude))
    frows <- as.data.frame(df[idxs,] %>% group_by(Provider, Dataset, Observation_ID) %>% filter(row_number()==1))
    df.SP <- st_as_sf(frows, coords = c("Longitude", "Latitude"), crs = 4326, na.fail=F)

isNum <- all(!is.na(as.numeric(df.SP$Value)))

print(isNum)
    if(isNum){
         pal <- colorNumeric( "viridis", domain = as.numeric(df.SP$Value))
         cols = colour_values_rgb(df.SP$Value, include_alpha = FALSE,  palette = "viridis", na_colour = "#808080FF") / 255

    }


     leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
       addLayersControl(
         baseGroups = c("Topo","ESRI Aerial"),
         overlayGroups = c("Soil Data"),
         options = layersControlOptions(collapsed = T)) -> L1

     if(isNum){
       L1  %>% addGlPoints(data = df.SP, group = 'Soil Data',  fillColor = cols, layerId=paste0(df.SP$Observation_ID)) %>%
      addLegend("topright", pal = pal, values = as.numeric(df.SP$Value),
                title = "Soil Data",
                #labFormat = labelFormat(prefix = "$"),
                opacity = 1
      )
     }else{
       L1  %>% addGlPoints(data = df.SP, group = 'Soil Data', layerId=paste0(df.SP$Observation_ID))
     }


  })


#####  Output Summary text   ##########

  output$SummaryText <- renderText({
    req(input$propObs)
    paste0('<p></p><p style="color:green;font-weight: bold;">Summary Statistics for ',   input$propObs, '</p>' )
  })

#####  Output Summary table   ##########

  output$summaryTab = renderRHandsontable({
    req(RV$currentdata )

    s<- RV$currentdata$Value
    dfs <- data.frame(label=character(6), other=character(6), stringsAsFactors = F)
    dfs[1,] <- c('Min Value ',s[1])
    dfs[2,] <- c('1st Quartile ',s[2])
    dfs[3,] <- c('Median ' ,s[3])
    dfs[4,] <- c('Mean ' ,s[4])
    dfs[5,] <- c('3rd Quartile ' ,s[5])
    dfs[6,] <- c('Max value ',s[6])
    validCnt <- length(which(!is.na(dfs$Value)))
    naCnt <- length(which(is.na(dfs$Value)))
    dfs[7,] <- c('Locations', length(unique(dfs$Observation_ID)))
    dfs[8,] <- c('Records', nrow(dfs))
    dfs[9,] <- c('Valid Vals', validCnt)
    dfs[10,] <- c('NA Vals', naCnt)

    rhandsontable(dfs , manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
  })



  # output$perProvTab = renderRHandsontable({
  #   req(RV$currentdata )
  #   dfs<-setDT(as.data.table(RV$currentdata ))[ , list(mean = mean(Value) ,median = median(Value), q10 = quantile(Value, 0.1), q90 = quantile(Value, 0.9)) , by = .(Provider)]
  #   rhandsontable(dfs , manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
  # })


#####  Output Data Table    ##########
  # output$dataTab = renderRHandsontable({
  #   req(RV$currentdata )
  #   dsub <- RV$currentdata[1:100, ]
  #   rhandsontable(dsub , manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
  # })

  output$dataTab = renderDT({

    req(RV$currentdata )

    datatable(RV$currentdata, filter='none', selection = "none", rownames=F, options = list(searching = FALSE))%>%
      formatStyle(columns = names(RV$currentdata), target = "cell",backgroundColor = "white")

  }
  )

  #####  Update Lists    ##########
  observe({

    req(input$propTypes)
    url <- paste0(serverLoc, '/SoilDataAPI/PropertyGroups')
    RV$apiDF <- insertRow( isolate(RV$apiDF) , c(as.character(Sys.time()), url),1)
    resp <- getWebDataDF(url)
    vals <- resp[resp$PropertyType == input$propTypes, ]
    updateSelectInput(session, "propGrps", choices = sort(vals$PropertyGroup), selected = DefPropGrp)
  })


  observe({

    req(input$propGrps)

    url <- paste0(serverLoc, '/SoilDataAPI/Properties?PropertyGroup=', input$propGrps)
    RV$apiDF <- insertRow( isolate(RV$apiDF) , c(as.character(Sys.time()), url),1)
    resp <- getWebDataDF(url)

    updateSelectInput(session, "propObs", choices = paste0(resp$Property), selected = DefProp)
  })


  observeEvent(input$getData2, {

    if(input$currentProvider == 'All'){
      dSets <- providerList$DataSet
    }
    else{
      dSets = providerList[providerList$OrgFullName == input$currentProvider, 1]
    }

    outdfs <- list(length(dSets))



    for(i in 1:length(dSets)){

      updateProgressBar(
        session = session,
        id = "pb",
        value = i, total = length(dSets),
        title = paste("Getting data from ", dSets[i])
      )

      url <- paste0(serverLoc, '/SoilDataAPI/SoilData?observedProperty=', input$propObs, '&DataSet=', dSets[i], '&usr=', input$authusr, '&key=', input$authkey)
      RV$apiDF <- insertRow( isolate(RV$apiDF) , c(as.character(Sys.time()), url),1)

      odf<-NULL
      if(DEBUG){
        if(dSets[i] == "NatSoil"){
          odf <- getWebDataDF(url)
        }
        }else{
          odf <- getWebDataDF(url)
        }

      if(is.data.frame(odf))
      {
        outdfs[[i]] <- odf
      }else{
        outdfs[[i]] <- blankResponseDF()
      }

    }

    df = as.data.frame(data.table::rbindlist(outdfs, fill=T))


    #df$Value <- as.numeric(as.character(df$Value))
    df <- df[!is.na(df$Value),]

    # Is With Australia Bounding Box
    xmin=113.3;ymin=-43.7;xmax=153.6;ymax=-10.6
    outdf <- df[df$Longitude >= xmin & df$Longitude <= xmax & df$Latitude >= ymin & df$Latitude <= ymax, ]

    # Is within reasonable value range
  #  outDF <- outdf[outdf$Value > 0 & outdf$Value < 12, ]
    outDF<- outdf

    updateProgressBar(
      session = session,
      id = "pb",
      value = 0, total = length(dSets),
      title = paste("Done")
    )

    RV$currentdata <- outDF

  })


  output$bplot<-renderPlot({
    df <- RV$currentdata
    ggplot(df, aes(x=Provider, y=Value)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
  })

  output$dplot<-renderPlot({

    df <- RV$currentdata
    d <- density(df$Value)
    plot(d, main=paste0("Data Distribution for ",  input$propObs))
    polygon(d, col="blue", border="blue")


  })

}

# Create Shiny app ----
shinyApp(ui, server)
