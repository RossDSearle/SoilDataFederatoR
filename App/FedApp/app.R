library(shiny)
library(shinythemes)
library(httr)
library(jsonlite)
library(DT)
library(shinyWidgets)
library(rhandsontable)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(data.table)



source("helpers.R")

#testdf <- read.csv('c:/temp/df.csv', stringsAsFactors = F)
#serverLoc <- 'http://127.0.0.1:6902'
serverLoc  <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/R'

url <- paste0(serverLoc, '/SoilDataAPI/DataSets')
datasetList <- fromJSON(url)
#print(datasetList)

#print(getwd())


insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}


blankResponseDF <- function(){

  outDF <- data.frame(DataStore=character(), Dataset=character(), Provider=character(), Observation_ID=character(), SampleID=character(), SampleDate=character() ,
                      Longitude=numeric() , Latitude= numeric(),
                      UpperDepth=numeric() , LowerDepth=numeric() , PropertyType=character(), ObservedProperty=character(), Value=numeric(),
                      Units= character(),   QualCollection=integer(), QualSpatialAgg=integer(), QualManagement=integer(), stringsAsFactors = F)
}


# Define UI for random distribution app ----
ui <- fluidPage(

  titlePanel(
    tags$head(tags$link( rel="icon", type="image/png", href="barrow50.png", sizes="32x32" ),
              tags$title("SoilDataFederator")
    )),


    tags$style(appCSS),
    theme = shinytheme("flatly"),


   titlePanel(HTML("<img src=Header.PNG style='vertical-align: top;'>")),


    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs
        sidebarPanel(
          wellPanel(
            #tags$style(tableHTML::make_css(list('.well', 'border-width', '10px')), tableHTML::make_css(list('.well', 'border-color', 'red'))),
            fluidRow(textInput('authusr', 'User Name', value='ross.searle@csiro.au' )),
            fluidRow(passwordInput('authkey', 'API key', value='a'),
                     fluidRow(         actionButton("loginBtn","Login", class = "btn-success"))
                     )
            ),
            fluidRow(selectInput('currentDataset', 'Select a DataSet', choices = c('All', datasetList$DataSet))),
            fluidRow(selectInput('propTypes', 'Select a Property Type', choices = c('FieldMeasurement', 'LaboratoryMeasurement'), selected='LaboratoryMeasurement')),
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
                                 rHandsontableOutput("dataTab")
                                 #DT::dataTableOutput("dataTab")
                                 ),

                        tabPanel("Summary", verbatimTextOutput("summary"),
                                 htmlOutput("SummaryText"),
                                 rHandsontableOutput("summaryTab"),
                                 plotOutput('dplot'),
                                 plotOutput('bplot'),
                                 rHandsontableOutput("perProvTab")
                                 ),
                        tabPanel("Map",
                                 leafletOutput("sitesMap", width = "750", height = "550")
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

# Define server logic for random distribution app ----
server <- function(session, input, output) {

    RV <- reactiveValues()
    RV$currentdata = NULL
    RV$apiDF <- data.frame(Time=as.character(Sys.time()), Request=paste0(serverLoc, '/SoilDataAPI/DataSets'), stringsAsFactors = F)

    output$downloadData <- downloadHandler(
        filename = function() {
          paste('TERNSoilsFederator-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          write.csv(RV$currentdata, con)
        }
      )

    output$contributorsTab = renderRHandsontable({
      url <- paste0(serverLoc, '/SoilDataAPI/DataSets')
      contribs <- fromJSON(url)
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

    output$sitesMap <- renderLeaflet({

      req(RV$currentdata )

      df <- as.data.frame(RV$currentdata)
      print(head(df))
      frows <- df %>% group_by(Provider, Dataset, Observation_ID) %>% filter(row_number()==1)
      df.SP <- st_as_sf(frows, coords = c("Longitude", "Latitude"), crs = 4326)

      head(df.SP)

      pal <- colorNumeric(c("red", "green", "blue"), domain = df.SP$Value)

      leaflet() %>%
        addTiles() %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
        addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
        addCircleMarkers(data=df.SP, group='Soil pH', radius = 2, opacity=1,
                         stroke=F,
                         fillOpacity = 1,
                         weight=1,
                         #fillColor = "yellow",
                         color = ~pal(df.SP$Value),
                         popup = paste0("Site ID : ", df.SP$Observation_ID ,
                                        "<br> Attribute : ", df.SP$ObservedProperty ,
                                        "<br> Value : ", df.SP$Value )) %>%
        addLegend("topright", pal = pal, values = df.SP$Value,
                  title = "Soil Data",
                  #labFormat = labelFormat(prefix = "$"),
                  opacity = 1
        ) %>%
        addLayersControl(
          baseGroups = c("Topo","ESRI Aerial"),
          overlayGroups = c("Soil Data"),
          options = layersControlOptions(collapsed = T))
    })

    output$SummaryText <- renderText({
      req(input$propObs)
      paste0('<p></p><p style="color:green;font-weight: bold;">Summary Statistics for ',   input$propObs, '</p>' )
    })

    output$summaryTab = renderRHandsontable({
      req(RV$currentdata )

      s<- summary(RV$currentdata$Value)
      dfs <- data.frame(Statistic=character(6), Value=character(6), stringsAsFactors = F)
      dfs[1,] <- c('Min Value ',s[1])
      dfs[2,] <- c('1st Quartile ',s[2])
      dfs[3,] <- c('Median ' ,s[3])
      dfs[4,] <- c('Mean ' ,s[4])
      dfs[5,] <- c('3rd Quartile ' ,s[5])
      dfs[6,] <- c('Max value ',s[6])
      #validCnt <- length(which(!is.na(dfs$Value)))
      #naCnt <- length(which(is.na(dfs$Value)))
      dfs[7,] <- c('Locations', length(unique(RV$currentdata$Observation_ID)))
      dfs[8,] <- c('Records', nrow(RV$currentdata))
     # dfs[9,] <- c('Valid Vals', validCnt)
     # dfs[10,] <- c('NA Vals', naCnt)

      rhandsontable(dfs , manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
    })

    output$perProvTab = renderRHandsontable({
      req(RV$currentdata )
      dfs<-setDT(as.data.table(RV$currentdata ))[ , list(mean = mean(Value) ,median = median(Value), q10 = quantile(Value, 0.1), q90 = quantile(Value, 0.9)) , by = .(Provider)]
      rhandsontable(dfs , manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
    })


    output$dataTab = renderRHandsontable({
      req(RV$currentdata )
     # dfs<-setDT(as.data.table(RV$currentdata ))[ , list(mean = mean(Value) ,median = median(Value), q10 = quantile(Value, 0.1), q90 = quantile(Value, 0.9)) , by = .(Provider)]

      dsub <- RV$currentdata[1:100, ]
       rhandsontable(dsub , manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
    })

    # output$dataTab = DT::renderDataTable({
    #     RV$currentdata
    # })


    observe({

        req(input$propTypes)
        url <- paste0(serverLoc, '/SoilDataAPI/PropertyGroups')
        RV$apiDF <- insertRow( isolate(RV$apiDF) , c(as.character(Sys.time()), url),1)
        resp <- fromJSON(url)
        vals <- resp[resp$PropertyType == input$propTypes, ]
        updateSelectInput(session, "propGrps", choices = sort(vals$PropertyGroup), selected = 'PH')
    })


    observe({

        req(input$propGrps)

        url <- paste0(serverLoc, '/SoilDataAPI/Properties?PropertyGroup=', input$propGrps)
        RV$apiDF <- insertRow( isolate(RV$apiDF) , c(as.character(Sys.time()), url),1)
        print(url)
        resp <- fromJSON(url)
        print(head(resp))

        updateSelectInput(session, "propObs", choices = paste0(resp$Property), selected = '4A1')
    })


    observeEvent(input$getData2, {

            if(input$currentDataset == 'All'){
                provs <- datasetList$DataSet
            }
            else{
                provs = datasetList[datasetList$DataSet == input$currentDataset, 1]
            }
            #outDF <- data.frame(stringsAsFactors = f)

      print(provs)

            outdfs <- list(length(provs))

            for(i in 1:length(provs)){

              updateProgressBar(
                session = session,
                id = "pb",
                value = i, total = length(provs),
                title = paste("Getting data from ", provs[i])
              )

                url <- paste0(serverLoc, '/SoilDataAPI/SoilData?observedProperty=', input$propObs, '&DataSet=', provs[i], '&usr=', input$authusr, '&key=', input$authkey)
                RV$apiDF <- insertRow( isolate(RV$apiDF) , c(as.character(Sys.time()), url),1)
                 print(url)
                odf <- fromJSON(url)


                if(is.data.frame(odf))
                {
                  outdfs[[i]] <- odf
                }else{
                  outdfs[[i]] <- blankResponseDF()
                }

            }

            df = as.data.frame(data.table::rbindlist(outdfs, fill=T))


            df$Value <- as.numeric(as.character(df$Value))
            print(head(df))
            ids <- which(!is.na(df$Value)  & !is.na(df$Longitude) & !is.na(df$Latitude) & df$Latitude != 'NA')
            print(head(ids))
            df2 <- df[ids,]

            # Is With Australia Bounding Box
            xmin=113.3;ymin=-43.7;xmax=153.6;ymax=-10.6
            outDF <- df2[df2$Longitude >= xmin & df2$Longitude <= xmax & df2$Latitude >= ymin & df2$Latitude <= ymax, ]

            # Is within reasonable value range
           # outDF <- outdf[outdf$Value > 0 & outdf$Value < 12, ]

            updateProgressBar(
              session = session,
              id = "pb",
              value = 0, total = length(provs),
              title = paste("Done")
            )

        RV$currentdata <- outDF
       # print(resp)

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
