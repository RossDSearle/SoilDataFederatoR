
library(dplyr)
library(shiny)
library(shinyalert)
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


source('SDFconfig.R')
source("helpers.R")




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

  print(url)
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


url <- paste0(serverLoc, '/SoilDataAPI/Properties?verbose=T')
props <- getWebDataDF(url)



# Define UI for random distribution app ----
ui <- fluidPage(

  useShinyalert(),

  titlePanel(
    tags$head(tags$link( rel="icon", type="image/png", href="barrow50.png", sizes="32x32" ),
              tags$title("SoilDataFederator")
    )),


  tags$style(appCSS),
  theme = shinytheme("flatly"),


  titlePanel(HTML("<img src=HeaderWithTern3.png style='vertical-align: top;'>")),


  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel( width=2,

      fluidRow(textInput('authusr', 'User Name', value=DefUser)),
      fluidRow(passwordInput('authkey', 'API key', value=DefKey),
               HTML('<a href="https://shiny.esoil.io/SoilDataFederator/Pages/Register/" target="_blank"> Register for API Key</a>')
               ),
      fluidRow(selectInput('currentProvider', 'Select a Dataset', choices = c('All', DatasetList$DataSet))),
      fluidRow(selectInput('propTypes', 'Select a Data Type', choices = c('FieldMeasurement', 'LaboratoryMeasurement'), selected='LaboratoryMeasurement')),
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
               #verbatimTextOutput("catchButtonClick")
      ),
      fluidRow( uiOutput('PropInfo')
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
                           rHandsontableOutput("summaryTab"),HTML('<BR>,<BR>'),
                           plotOutput('dplot', width = 1000),HTML('<BR>,<BR>'),
                           plotOutput('bplot', width = 700),HTML('<BR>,<BR>'),
                           rHandsontableOutput("perProvTab")
                  ),
                  tabPanel("Map",
                           leafletOutput("sitesMap", width = "800", height = "600")
                  ),
                  tabPanel("Available Datasets",
                           HTML('<BR><BR>'),
                           downloadButton("DownloadDatasetsTable","Download Datasets Table"),
                           HTML('<BR><BR>'),
                           rHandsontableOutput("contributorsTab")
                  ),
                  tabPanel("Available Soil Properties",
                           HTML('<BR>'),
                           rHandsontableOutput("propertiesTab", width = "1200", height = "600")
                  ),
                  tabPanel("API Requests",
                           rHandsontableOutput("apiTab")
                  ),
                  tabPanel("Help", div(style = "valign:top; height: 90vh; overflow-y: auto;",  includeHTML("SoilDataFederatorAppHelp.html")) )
      )

    )
  )
)




server <- function(session, input, output) {


  RV <- reactiveValues()
  RV$currentdata = NULL
  RV$currentdataType = NULL
  RV$apiDF <- data.frame(Time=as.character(Sys.time()), Request=paste0(serverLoc, '/SoilDataAPI/Providers'), stringsAsFactors = F)
  RV$ShowDemoDialog = T


#####  Download Data     ##########
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('TERNSoilDataFederator-', input$propObs, '-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(RV$currentdata, con)
    }
  )

  output$DownloadDatasetsTable <- downloadHandler(
    filename = function() {
      paste('TERNSoilDataFederator-Available_Datasets', '-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
       url <- paste0(serverLoc, '/SoilDataAPI/DataSets?verbose=T')
       ds <- getWebDataDF(url)
      ds$Description <- gsub("\r?\n|\r", " ", ds$Description)
      #ds <- read.csv('C:/Temp/7a1.csv')
      write.csv(ds, con)
    }
  )

  #####  Output Datasets Table    ##########
  output$contributorsTab = renderRHandsontable({
    url <- paste0(serverLoc, '/SoilDataAPI/DataSets?verbose=T')
    contribs <- getWebDataDF(url)

    contribs$ContactEmail <- paste0('<a href=mailto:', contribs$ContactEmail, '?subject=Soil%20Data%20on%20TERN-Landscapes%20SoilDataFederator>', contribs$ContactEmail, '</a>'  )
    contribs$OrgURL <- paste0('<a href=', contribs$OrgURL, ' target="_blank">', contribs$OrgURL, '</a>'  )
    contribs$MetaDataURI <- paste0('<a href=', contribs$MetaDataURI, ' target="_blank">', contribs$MetaDataURI, '</a>'  )
    contribs$Description <- gsub("\r?\n|\r", " ", contribs$Description)
    rhandsontable(contribs , manualColumnResize = T, readOnly = F, rowHeaders = F) %>%
      hot_col('ContactEmail', renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_col('OrgURL', renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_col('MetaDataURI', renderer = htmlwidgets::JS("safeHtmlRenderer"))
  })

  #####  Output Properties Table    ##########
  output$propertiesTab = renderRHandsontable({


     props$VocabURL <- paste0('<a href=', props$VocabURL, ' target="_blank">', props$VocabURL, '</a>'  )
    rhandsontable(props , manualColumnResize = T, readOnly = F, rowHeaders = F) %>%
       hot_col('VocabURL', renderer = htmlwidgets::JS("safeHtmlRenderer"))

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

    if(RV$currentdataType=='numeric'){
         pal <- colorNumeric( "viridis", domain = df.SP$Value)
         cols = colour_values_rgb(df.SP$Value, include_alpha = FALSE,  palette = "viridis", na_colour = "#808080FF") / 255
    }else{
         colCnt <- length(unique(df.SP$Value))
         #pal <- colorFactor(RColorBrewer::brewer.pal(colCnt, 'Spectral'), df.SP$Value)
         pal <- colorFactor("viridis", df.SP$Value)
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

     if(RV$currentdataType=='numeric'){
       L1  %>% addGlPoints(data = df.SP, group = 'Soil Data',  fillColor = cols, layerId=paste0(df.SP$Observation_ID)) %>%
      addLegend("topright", pal = pal, values = as.numeric(df.SP$Value),
                title = "Soil Data",
                #labFormat = labelFormat(prefix = "$"),
                opacity = 1
      )
     }else{
      # L1  %>% addGlPoints(data = df.SP, group = 'Soil Data', layerId=paste0(df.SP$Observation_ID))
       L1  %>% addGlPoints(data = df.SP, group = 'Soil Data',  fillColor = cols, layerId=paste0(df.SP$Observation_ID)) %>%
         addLegend("topright", pal = pal, values = as.numeric(df.SP$Value),
                   title = "Soil Data",
                   #labFormat = labelFormat(prefix = "$"),
                   opacity = 1)
     }


  })


#####  Output Summary text   ##########

  output$SummaryText <- renderText({
    req(input$propObs)
    paste0('<p></p><p style="color:green;font-weight: bold;">Summary Statistics for ',   input$propObs, '</p>' )
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

  output$PropInfo <- renderText({

    print(head(props))
    rec <- props[props$Property == input$propObs, ]

    HTML(paste0('<BR><a href="', rec$VocabURL, '" target="_blank">Show property info for ', input$propObs, '</a>'))

  })

########    Get the data  ######



     observeEvent(input$getData2, {

       StopHere <- F

       print(input$authusr)

      if(input$authusr=='Demo'){
         dSets <- list(length(1))
         dSets[1] <- 'NatSoil'
      }else if(DEBUG){
        dSets <- list(length(1))
        dSets[1] <- 'NatSoil'
      }
      else if(input$currentProvider == 'All'){
        dSets <- providerList$DataSet
       }
      else{
        dSets = providerList[providerList$DataSet== input$currentProvider, 1]
      }

    outdfs <- list(length(dSets))



    rec <- props[props$Property == input$propObs, ]

    if(rec$DataType == 'FLOAT'){
      RV$currentdataType <- 'numeric'
    }else{
      RV$currentdataType <- 'categorical'
    }


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

          jn <- getWebDataDF(url)
          if(is.null(jn$error)){
            odf <- jn
          }else{
            print(odf)
            StopHere=T
            break
          }


      if(is.data.frame(odf))
      {
        outdfs[[i]] <- odf
      }else{
        outdfs[[i]] <- blankResponseDF()
      }

    }

    if(StopHere){
     shinyalert(
                  title = "Houston... we have a problem",
                  text = paste0("The API user and or key is not valid. Your can quickly register to obtain an API key <a href='", shinyLoc, "/Pages/Register/' target='_blank'>here</a>"),
                  html=T
                )
     return()
   }

    df = as.data.frame(data.table::rbindlist(outdfs, fill=T))


    # Is With Australia Bounding Box
    xmin=113.3;ymin=-43.7;xmax=153.6;ymax=-10.6
    outdf <- df[df$Longitude >= xmin & df$Longitude <= xmax & df$Latitude >= ymin & df$Latitude <= ymax, ]

    # Is within reasonable value range
    # outDF <- outdf[outdf$Value > 0 & outdf$Value < 12, ]
    outDF <- outdf

     if(RV$currentdataType=='numeric'){
         # idxs <- which(!grepl('^[0-9.]',outDF$Value))
         # outDF <- outDF[-idxs,]
         outDF$Value <- as.numeric(outDF$Value)
         idxs <- which(!is.na(outDF$Value))
         outDF <- outDF[idxs,]
       }

    updateProgressBar(
      session = session,
      id = "pb",
      value = 0, total = length(dSets),
      title = paste("Done")
    )

    if(RV$ShowDemoDialog & input$authusr=='Demo'){
      RV$ShowDemoDialog = F
      shinyalert(
        title = "I'll just tell you this once",
        text = paste0("Because you are using the 'Demo' API key only 5 records will be returned for your queries. Your can quickly register to obtain an API key <a href='", shinyLoc, "/Pages/Register/' target='_blank'>here</a> so that you will be able to access all of the available data."),
        html=T
      )
    }


    RV$currentdata <- outDF

  })






  #####  Output Summary table   ##########

  output$summaryTab = renderRHandsontable({


    req(RV$currentdata )

    df <- RV$currentdata

    if(RV$currentdataType=='numeric'){

      s <- summary(as.numeric(df$Value))
      dfs <- data.frame(Statistic=character(), Value=character(), stringsAsFactors = F)

      dfs[1,] <- c('Min Value ',s[1])
      dfs[2,] <- c('1st Quartile ',s[2])
      dfs[3,] <- c('Median ' ,s[3])
      dfs[4,] <- c('Mean ' ,s[4])
      dfs[5,] <- c('3rd Quartile ' ,s[5])
      dfs[6,] <- c('Max value ',s[6])
      dfs[7,] <- c('Locations', length(unique(df$Observation_ID)))
      dfs[8,] <- c('Records', nrow(df))

    }else{

      agg <- count(df, Value)
      colnames(agg) <- c('Categories','Count')
      nr <- nrow(agg)
      agg[nr+1, ] <-  c('', '')
      agg[nr+2, ] <-  c('Locations', length(unique(df$Observation_ID)))
      agg[nr+3, ] <-  c('Records', nrow(df))
      dfs <- agg

    }

    rhandsontable(dfs , manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
  })



  ####    Render summary plots #####

  #### Box plots or or stacked bars
  output$bplot<-renderPlot({

    req(RV$currentdata )
    df <- RV$currentdata

    if(RV$currentdataType=='numeric'){

      ggplot(df, aes(x=Dataset, y=Value)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
    }else{
      ggplot(df) + geom_bar(aes(x = Dataset, fill = Value)) + scale_x_discrete( expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
    }
  })


  # #### Denisty or Single bars
  output$dplot<-renderPlot({

    req(RV$currentdata )
    df <- RV$currentdata

    if(RV$currentdataType=='numeric'){
      ggplot(df, aes(x=Value))+ geom_density(color="darkblue", fill="lightblue", adjust = 0.5)+ scale_x_continuous( expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
      }else{
          ggplot(df) + geom_bar(aes(x = Value, fill = Value)) + scale_x_discrete( expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
        }
  })


}

# Create Shiny app ----
shinyApp(ui, server)
