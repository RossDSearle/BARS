library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(dygraphs)
library(httr)
library(jsonlite)
library(xts)
library(stringr)
library(raster)
library(rgdal)
library(rhandsontable)

library(shinyjs)
library(shinyalert)
library(shinyBS)
library(fields)
library(lubridate)
library(shinybusy)

#library(flexdashboard)


defWidth = '380px'
loaderTime = 5
numberofDaysSinceToday <- 10

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  rootDir <- '/srv/shiny-server/BARS'
  dataStoreDir <- '/mnt/data/BARS/SoilPropertyPredictions'
  source(paste0( rootDir, '/appUtils.R'))
  
}else{
  rootDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/BARS'
  dataStoreDir <- 'C:/Temp/boorowa_2019/data/processed'
  source(paste0( rootDir, '/appUtils.R'))
}

SenFedServer <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI'

soilDataDF <- read.csv(paste0(dataStoreDir, '/scans_predicted_soil_data.csv'), stringsAsFactors = F)
soilLocs <- soilDataDF[row.names(unique(soilDataDF[,c("locID", "easting", "northing")])), c("locID", "easting", "northing")]
coordinates(soilLocs) <- c("easting", "northing")
proj4string(soilLocs) <- CRS('+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
CRS.new <- CRS("+init=epsg:4326")
spts <- spTransform(soilLocs, CRS.new)

today <- paste0(Sys.Date(), 'T00:00:00')

bdy <- readOGR(paste0( rootDir, '/Data/CoarseBound.shp'))

sdLabels <- c('30 cm', '40 cm','50 cm','60 cm','70 cm','80 cm','90 cm','100 cm')
sdVals <- c('0', '1', '2','3','4','4','6','7')
soilDepthsDF <- data.frame(sdLabels, sdVals, stringsAsFactors = F)


today <- str_replace(str_remove(Sys.Date()-hours(10), ' UTC'), ' ', 'T')

url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=boorowa_aws_148.6887_-34.4732&sensorid=boorowa.environdata.AWS1.TOTAL-Rain-Gauge&sensortype=Rainfall&aggperiod=hours&startdate=',today)
print(url)
response <- GET(url)
stream <- content(response, as="text", encoding	='UTF-8')
ts <- convertJSONtoTS(stream)
TodaysWeather=NULL
TodaysWeather$Rainfall <- sum(ts)
TodaysWeather$MaxRainfall <- max(ts)

url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=boorowa_aws_148.6887_-34.4732&sensorid=boorowa.environdata.AWS1.AVERAGE-Air-Temperature&sensortype=Temperature&aggperiod=none&startdate=',today)
print(url)
response <- GET(url)
stream <- content(response, as="text", encoding	='UTF-8')
ts <- convertJSONtoTS(stream)
TodaysWeather$CurrentTemp <- tail(ts, 1)
TodaysWeather$MinTemp <- min(ts)
TodaysWeather$MaxTemp <- max(ts)


url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=boorowa_aws_148.6887_-34.4732&sensorid=boorowa.environdata.AWS1.AVERAGE-Relative-Humidity&sensortype=Humidity&aggperiod=none&startdate=',today)
print(url)
response <- GET(url)
stream <- content(response, as="text", encoding	='UTF-8')
ts <- convertJSONtoTS(stream)
TodaysWeather$CurrentHumidity <- tail(ts, 1)
TodaysWeather$MinHumidity <- min(ts)
TodaysWeather$MaxHumidity <- max(ts)

url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=boorowa_aws_148.6887_-34.4732&sensorid=boorowa.environdata.AWS1.VWSP-Vector-Wind-Spd&sensortype=Wind-Speed&aggperiod=none&startdate=',today)
print(url)
response <- GET(url)
stream <- content(response, as="text", encoding	='UTF-8')
ts <- convertJSONtoTS(stream)
TodaysWeather$CurrentWindSpeed <- tail(ts, 1)
TodaysWeather$MaxWindSpeed <- max(ts, 1)
TodaysWeather$MinWindSpeed <- min(ts, 1)



shiny::shinyApp(
  ui = f7Page(
    title = "BARS",
    init = f7Init(skin = "auto", theme = "light", filled = T, color = 'lightblue'),
    tags$head(tags$link( rel="icon", type="image/png", href="wheat.png", sizes="32x32" ),
              tags$link( rel="apple-touch-icon", href="apple-touch-icon.png" )
              #tags$title("BCG AgDataShop"),
              #tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
              
              ),
    
    useShinyjs(),
    
    #add_busy_bar(color = "#FF0000", centered = FALSE, height = "18px"),
    #add_busy_spinner(spin = "fading-circle"),
    add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
    
    #title = NULL,
    preloader = F,
    loading_duration = loaderTime,
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "About", side = "left", theme = "dark", effect = "cover",
                
                f7Link(label = "About BARS", src = "https://www.csiro.au/en/Research/AF/Areas/Boorowa-Agricultural-Research-Station", external = TRUE),
                f7Link(label = "About CSIRO Ag & Food", src = "https://www.csiro.au/en/Research/AF", external = TRUE),
                f7Link(label = "About CSIRO", src = "https://www.csiro.au", external = TRUE),
                f7Link(label = "BoM Boowora", src = "http://www.bom.gov.au/places/nsw/boorowa/", external = TRUE)
                
                
                
                )
        #f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
      ),
 
##################################  NAVIGATION BAR   ##################################      
      navbar = f7Navbar(
       # title = shiny::tags$div(style="background-image: url('Logos/HdrBkGrdImage.PNG');", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px"), "Boowora Agricultutral Research Station "),
        title = tags$div( tags$div(style="vertical-align:middle!important; text-align:left!important; display:inline-block;", "Boowora Ag Research Station"), HTML('&nbsp&nbsp&nbsp'), tags$div(style="float: right;", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px", align='right'))),
        hairline = T,
        shadow = T,
        left_panel = T,
        right_panel = F
      ),


##################################  UI - SOIL MOISTURE PROBE MAP  ##################################         
      
      f7Tabs(
        animated = T,
        #swipeable = TRUE,
        f7Tab(
          tabName = "SM Probes",
          icon = f7Icon("layers_fill", old = TRUE),
          active = TRUE,
          f7Float( f7Shadow(
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = "Click on a probe location to display info below. Scroll down to view results.",
                        
                       # f7Select(inputId = 'SMDepth', label = "Select Soil Moisture Depth (cm)", c(30, 40, 50,60,70,80,90,100)),
                       # HTML('<BR>'),
                        leafletOutput("SoilMoistureProbeMap", height = 400 )
                        
                      )
            )
          ), side = "left" ),
          
          
          f7Float(  f7Shadow(
            intensity = 100,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = "Current Soil Water Summary",
                        id = 'swgaugecard',
                        f7Gauge(
                          id = "swTotGauge",
                          type  = "semicircle",
                          value = 0,
                          borderColor = "#2196f3",
                          borderWidth = 40,
                          size = 300,
                          valueFontSize = 30,
                          valueTextColor = "#2196f3",
                          labelText = "Total Soil Water"
                        )
                        
                      ))), side = "left" ),
          
          f7Float(  f7Shadow(
            intensity = 100,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = NULL,
                        
                        dygraphOutput("mositureChart1", width = "350", height = "300px")
                        
                      )
            )
          ), side = "left" ),
          
          f7Float( 
            f7Shadow(
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = 'Soil Water Bucket',
                        plotOutput("bucketPlot")
                      )
            )
          ), side = "left" )
        ),

        
################################## UI - SOIL MOISTURE MAPS   ##################################           
        f7Tab(
          tabName = "SM Maps",
          icon = f7Icon("map", old = F ),
          active = FALSE,
          f7Float( 
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            div( style=paste0("width: ", defWidth ,"; align='left'; vertical-align: middle;"),
                 f7Card(
                   title = NULL,
                   f7DatePicker( "SMmapDate", label='Select Map Date', value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd" ),
                   f7Select(inputId = 'SMDepthList', label = "Soil Depth",  choices =  soilDepthsDF$sdLabels),
                   HTML('<BR>'),
                   #div( style=paste0("width: 100px"),
                   f7Button(inputId = 'drawSMmapbtn', label = "Draw Soil Moisture Map", src = NULL, color = 'green', fill = TRUE, outline = F, shadow = T, rounded = T, size = NULL),
                   #),
                   HTML('<BR>'),
                   f7Progress(id = "pg1", value = 0, color = "blue"),
                   
                   
                   leafletOutput("moistureMap2", height = 400 )
                   
                 )
            )
          )
        ), side = "left"),
        
        
##################################  UI - WEATHER   ##################################          
        f7Tab(
          tabName = "Weather",
          icon = f7Icon("cloud_heavyrain_fill", old = F),
          active = FALSE,
          f7Float(  
            f7Shadow(
            intensity = 10,
            hover = TRUE,
            
            tags$div( style=paste0("width: ", defWidth),
                      
                       f7Card(
                        title = paste0("Todays Weather (", format(Sys.Date(), format="%B %d %Y"), ')' ),
                        
                        verbatimTextOutput("todaysRainfall"),
                        verbatimTextOutput("todaysMaxRainfall"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentTemperature"),
                        verbatimTextOutput("todaysMinTemperature"),
                        verbatimTextOutput("todaysMaxTemperature"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentHumidity"),
                        verbatimTextOutput("todaysMinHumidity"),
                        verbatimTextOutput("todaysMaxHumidity"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentWindspeed"),
                        verbatimTextOutput("todaysMinWindspeed"),
                        verbatimTextOutput("todaysMaxWindspeed"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentWindDirection")
                        # verbatimTextOutput("todaysMinHumidity"),
                        # verbatimTextOutput("todaysMaxHumidity")
                      ))), side = "left"), 
                      
          f7Float(  
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              
              tags$div( style=paste0("width: ", defWidth),           
            f7Card(
              title = "Weather History",
              prettyRadioButtons(
                
                inputId = "WeatherHistoryButtons",
                label = "Variable:",
                
                c("Rainfall" = "Rainfall",
                  "Temperature" = "Temperature",
                  "Humidity" = "Humidity",
                  "Windspeed" = "Wind-Speed"),
                inline = TRUE,
                status = "success",
                animation = "pulse",
                bigger = T
              ),
              dygraphOutput("WeatherHistoryChart", height = "300px")
            )))), side = "left"),
            
            
##################################  UI - SOIL DATA MAP   ##################################             
        
        f7Tab(
          tabName = "Soil Data",
          icon = f7Icon("list_number_rtl", old = F),
          active = FALSE,
          f7Float(
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),  
            f7Card(
              title = NULL,
              fluidRow( f7Select('SoilPropList', "Select soil attribute", choices=c('clay', 'ecec', 'phc', 'soc')),  f7Select('SoilDepthList', "Select depth (cm)", choices=c('d1', 'd2', 'd3', 'd4'))),
              #f7Select('SoilPropList', "Select soil attribute", choices=c('clay', 'ecec', 'phc', 'soc')),
              HTML('<BR>'),
              leafletOutput("soilMap", height = 400),
              rHandsontableOutput('soilDataTable' )
              #tableOutput('soilDataTable' )
            )
            )
          )
          , side = "left")
        )
        
      )
    )
  ),


##################################  SERVER  ##################################   
  server = function(input, output, session) {
    
    session$allowReconnect(TRUE)
    
    RV <- reactiveValues()
    RV$currentTS <- NULL
    RV$currentSite <- NULL
    RV$sensorLocs <- NULL
    RV$currentSoil <- NULL
    RV$SoilMOistureSensors <- NULL
    RV$m <- NULL
    RV$TodaysWeather <- NULL
    RV$HistoricalRainfall <- NULL
    
    
    ##################################  SERVER - GLOBAL PROCESSING   ##################################
    
    acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=8, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
    
    ########  Get sensor locations   ##############
    
    observe({
      url <- paste0(SenFedServer,"/getSensorLocations?sensortype=Soil-Moisture" )
      response <- GET(url)
      stop_for_status(response) # stop if the response is an error
      sensorLocs <- fromJSON(content(response, as="text"))
      bs <- sensorLocs[sensorLocs$SensorGroup == 'Booroowa',]
      RV$sensorLocs <- bs
    })
    
    ##################################  SERVER - WEATHER   ##################################      
    observe(  {
      
     # req(RV$HistoricalRainfall)
      
     # isolate({
      
        
        # today <- str_replace(str_remove(Sys.Date()-hours(10), ' UTC'), ' ', 'T')
      # 
      # url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=boorowa_aws_148.6887_-34.4732&sensorid=boorowa.environdata.AWS1.TOTAL-Rain-Gauge&sensortype=Rainfall&aggperiod=hours&startdate=',today)
      # print(url)
      # response <- GET(url)
      # stream <- content(response, as="text", encoding	='UTF-8')
      # ts <- convertJSONtoTS(stream)
      RV$TodaysWeather$Rainfall <- TodaysWeather$Rainfall
      RV$TodaysWeather$MaxRainfall <- TodaysWeather$MaxRainfall
      # print(ts)
      # 
      # 
      # url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=boorowa_aws_148.6887_-34.4732&sensorid=boorowa.environdata.AWS1.AVERAGE-Air-Temperature&sensortype=Temperature&aggperiod=none&startdate=',today)
      # print(url)
      # response <- GET(url)
      # stream <- content(response, as="text", encoding	='UTF-8')
      # ts <- convertJSONtoTS(stream)
       RV$TodaysWeather$CurrentTemp <- TodaysWeather$CurrentTemp
       RV$TodaysWeather$MinTemp <- TodaysWeather$MinTemp
       RV$TodaysWeather$MaxTemp <- TodaysWeather$MaxTemp
      # 
      # 
      # url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=boorowa_aws_148.6887_-34.4732&sensorid=boorowa.environdata.AWS1.AVERAGE-Relative-Humidity&sensortype=Humidity&aggperiod=none&startdate=',today)
      # print(url)
      # response <- GET(url)
      # stream <- content(response, as="text", encoding	='UTF-8')
      # ts <- convertJSONtoTS(stream)
       RV$TodaysWeather$CurrentHumidity <- TodaysWeather$CurrentHumidity
       RV$TodaysWeather$MinHumidity <- TodaysWeather$MinHumidity
       RV$TodaysWeather$MaxHumidity <- TodaysWeather$MaxHumidity
      # 
      # url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=boorowa_aws_148.6887_-34.4732&sensorid=boorowa.environdata.AWS1.VWSP-Vector-Wind-Spd&sensortype=Wind-Speed&aggperiod=none&startdate=',today)
      # print(url)
      # response <- GET(url)
      # stream <- content(response, as="text", encoding	='UTF-8')
      # ts <- convertJSONtoTS(stream)
       RV$TodaysWeather$CurrentWindSpeed <- TodaysWeather$CurrentWindSpeed
       RV$TodaysWeather$MaxWindSpeed <- TodaysWeather$MaxWindSpeed
       RV$TodaysWeather$MinWindSpeed <- TodaysWeather$MinWindSpeed
      
     # })
      
    })
    
    
    
    output$todaysRainfall <- renderText({paste0('Rainfall : ', RV$TodaysWeather$Rainfall, ' mm') })
    output$todaysMaxRainfall <- renderText({paste0('Maximum Rainfall : ', RV$TodaysWeather$Rainfall, ' mm/hr') })
    
    
    output$todaysCurrentTemperature <- renderText({paste0('Current Temperature : ', RV$TodaysWeather$CurrentTemp,  intToUtf8(176), 'C') })
    output$todaysMaxTemperature <- renderText({ paste0('Maximum Temperature : ', RV$TodaysWeather$MaxTemp,  intToUtf8(176), 'C') })
    output$todaysMinTemperature <- renderText({ paste0('Minimum Temperature : ', RV$TodaysWeather$MinTemp,  intToUtf8(176), 'C') })
    
    output$todaysCurrentHumidity <- renderText({paste0('Current Humidity : ', RV$TodaysWeather$CurrentHumidity, '%') })
    output$todaysMaxHumidity <- renderText({ paste0('Maximum Humidity : ', RV$TodaysWeather$MaxHumidity, '%') })
    output$todaysMinHumidity <- renderText({ paste0('Minimum Humidity : ', RV$TodaysWeather$MinHumidity, '%') })
    
    output$todaysCurrentWindspeed <- renderText({ paste0('Current Wind Speed : ',  RV$TodaysWeather$CurrentWindSpeed , ' km/hr') })
    output$todaysMinWindspeed <- renderText({ paste0('Maximum Wind Speed : ',  RV$TodaysWeather$MaxWindSpeed , ' km/hr') })
    output$todaysMaxWindspeed <- renderText({ paste0('Minimum Wind Speed : ',  RV$TodaysWeather$MinWindSpeed , ' km/hr') })
    
    
    
      #  Render the Chart from a map drill ====

    
    ################## Render the Chart from a map drill  ##################
    output$WeatherHistoryChart <- renderDygraph({
      
      req(input$WeatherHistoryButtons)
      
      d1 <- as.Date(Sys.Date())
      d2 <- d1-numberofDaysSinceToday
      d3 <- paste0(d2, 'T00:00:00')
      
      print(d3)
      
      if(input$WeatherHistoryButtons == 'Rainfall'){
        senid <- 'boorowa.environdata.AWS1.TOTAL-Rain-Gauge'
      }else if(input$WeatherHistoryButtons == 'Temperature'){
        senid <- 'boorowa.environdata.AWS1.AVERAGE-Air-Temperature'
      }else if(input$WeatherHistoryButtons == 'Humidity'){
        senid <- 'boorowa.environdata.AWS1.AVERAGE-Relative-Humidity'
      }else if(input$WeatherHistoryButtons == 'Wind-Speed'){
        senid <- 'boorowa.environdata.AWS1.AVERAGE-Wind-Speed-3m'
      }
      
      
      url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=boorowa_aws_148.6887_-34.4732&sensorid=', senid,'&sensortype=',input$WeatherHistoryButtons ,'&aggperiod=days&startdate=',d3)
      print(url)
      response <- GET(url)
      stream <- content(response, as="text", encoding	='UTF-8')
      ts <- convertJSONtoTS(stream)
      
      RV$HistoricalRainfall <- ts
      
      dygraph(ts ,  main = paste0(''), ylab = '')%>%
        dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 0) %>%
        dyLegend(show = "follow", hideOnMouseOut = T)  %>%
        dyAxis("y",axisLabelWidth = 15)  %>%
        dyRangeSelector()
      
      
      
    }
    )
    
    
    
    
    ##################################  SERVER - SOIL PROBE MAP   ##################################   
    
      #   Generate the soil bucket plot   ====
    
    output$bucketPlot <- renderPlot({
      req( RV$currentTS)
      
      buk <- getBucket("sid", RV$currentTS)
      
      
      sw <- round(RV$currentTS[1,1], digits = 1)
      updateF7Gauge(session, id = 'swTotGauge', value = sw)
      RV$m = sw
      
      plot( 0, type="n", main=paste( ''), 
            xlab='Volumteric Soil Mositure (%)', ylab='Soil Depth (cm)',
            yaxs = "i", xaxs = "i", xlim = c(10, 50), ylim = rev(range(c(0,100))),
            cex.lab = 1.5
      )
      
      polygon(buk$x,buk$y,
              col=c("navajowhite3"),
              border=c("navajowhite3"),
              lwd=1, lty=c("solid"))
      
      polygon(buk$xm, buk$ym,
              col=c("lightskyblue"),
              border=c("lightskyblue"),
              lwd=1, lty=c("solid"))
      
    })
    
    
    ################  Get data from Clicking on a sensor  #################
    observe({
      click<-input$SoilMoistureProbeMap_marker_click
      if(is.null(click))
        return()
     
     # show_modal_spinner()
      
      
      RV$currentTS <- NULL
      
      DataType <- 'Soil-Moisture'
      
      sid <- click$id
      url <- paste0(SenFedServer, "/getSensorDataStreams?siteid=", sid,"&sensortype=", DataType, "&aggperiod=days")
      
      response <- GET(url)
      stop_for_status(response) # stop if the response is an error
      sensorData <- content(response, as="text")
      ts <- convertJSONtoTS(sensorData)
      RV$currentTS <- ts
    })
    
    observeEvent(input$SoilMoistureProbeMap_marker_click, { # update the map markers and view on location selectInput changes
      p <- input$SoilMoistureProbeMap_marker_click
      if(is.null(p))
        return()
      proxy <- leafletProxy("SoilMoistureProbeMap")
      if(p$id=="Selected"){
        proxy %>% removeMarker(layerId="Selected")
      } else {
        proxy %>% acm_defaults(p$lng, p$lat)
      }
      
      #updateF7Card(id = "swgaugecard", session = session)
    })
    
    
    ################## Render the Chart from a map drill  ##################
    output$mositureChart1 <- renderDygraph({
      
      if(!is.null(RV$currentTS)){
        
        isolate({
          
          maxVal <- max(RV$currentTS)
          
          dygraph(RV$currentTS ,  main = paste0('SoilMoisture'), ylab = 'Soil Moisture')%>%
            dyAxis("y", label = '', valueRange = c(10, 40),axisLabelWidth = 15) %>%
            dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 26) %>%
            dyLegend(show = "follow", hideOnMouseOut = T, labelsSeparateLines = T)  %>%
            dyRangeSelector()
          
        })
      }
    })  
    
    
    
    
    
    
    
    #   Soil Moisture Probe Map  ====
    
    output$SoilMoistureProbeMap <- renderLeaflet({
      leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
        setView(lng = 148.692, lat = -34.475, zoom = 14) %>%
        
        # addControlGPS() %>%
        
        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
          overlayGroups = c( "SW Probes"),
          options = layersControlOptions(collapsed = T)
        )
    })
    
    #  Soil Moisture Map proxy
    observe({
      #req(input$SoilPropList)
      sdf <- RV$sensorLocs
      labs <- lapply(seq(nrow(sdf)), function(i) {
        paste0( '<li>Site Name : ', sdf[i, "SiteName"], '</li>',
                '<li>Provider : ', sdf[i, "SensorGroup"], '</li>',
                '<li>Backend : ', sdf[i, "Backend"], '</li>',
                #'<li>Access : ', sdf[i, "Access"], '</li>',
                '<li>Site ID : ', sdf[i, "SiteID"], '</li>')
      })
      
      colCnt <- length(unique(sdf[,input$SensorLabel]))
      colCats <- unique(sdf[,input$SensorLabel])
      colField <- sdf[,input$SensorLabel]
      factpal <-colorFactor(RColorBrewer::brewer.pal(colCnt, 'Spectral'), colField)
      
      proxy <- leafletProxy("SoilMoistureProbeMap", data = RV$sensorLocs)
      proxy %>% clearMarkers()
      proxy %>% clearControls()
      proxy %>% addCircleMarkers(   lng = ~Longitude, lat = ~Latitude,
                                    label = lapply(labs, HTML),
                                    stroke = FALSE,
                                    fillOpacity = 1,
                                    color = 'blue',
                                    radius = 10,
                                    layerId=paste0(sdf$SiteID),
                                    group = "SW Probes" )
    })
    
    
    ##################################  SERVER - SOIL MOISTURE MAPS   ##################################       
    
    output$moistureMap2 <- renderLeaflet({
      
      req(RV$sensorLocs)
      sdf <- RV$sensorLocs
      labs <- lapply(seq(nrow(sdf)), function(i) {
        paste0( '<li>Site Name : ', sdf[i, "SiteName"], '</li>',
                '<li>Provider : ', sdf[i, "SensorGroup"], '</li>',
                '<li>Backend : ', sdf[i, "Backend"], '</li>',
                '<li>Site ID : ', sdf[i, "SiteID"], '</li>')
      })
      
      
      leaflet(sdf) %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
        
        setView(lng = 148.689633, lat = -34.483, zoom = 13) %>%
        
        addCircleMarkers(   lng = ~Longitude, lat = ~Latitude,
                            label = lapply(labs, HTML),
                            stroke = FALSE,
                            fillOpacity = 1,
                            color = 'blue',
                            radius = 5,
                            layerId=paste0(sdf$SiteID),
                            group = "SM Probes" ) %>%
        
        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
          overlayGroups = c("Moisture Map", 'SM Probes'),
          options = layersControlOptions(collapsed = T)
        )
    })
    
    #   Draw the soil moisture maps   ====
    
    observeEvent(input$drawSMmapbtn, {
      
      req(input$drawSMmapbtn)
      
      print(paste0('XXXXXXX  ' , input$SMDepthList))
      depth <- soilDepthsDF[soilDepthsDF$sdLabels==input$SMDepthList, 2]
      
      print(paste0('dl = ', depth))
      
      req( RV$sensorLocs)
      bs <- RV$sensorLocs
      outDf <- data.frame()
      
      itl <- 100/nrow(bs)
      
      
      DataType <- 'Soil-Moisture'
      day <- input$SMmapDate
      
      for(i in 1:nrow(bs)){
        print(i)
        
        itl <- round((100/(nrow(bs) ) * i))
        updateF7Progress(session, id = "pg1", value = itl)
        
        sid <- bs$SiteID[i]
        d1 <- as.Date(day)
        d2 <- d1-numberofDaysSinceToday
        d3 <- paste0(d2, 'T00:00:00')
        
        sens <- RV$SoilMOistureSensors[ RV$SoilMOistureSensors$SiteID == sid, ]
        sens1 <- sens[grepl(paste0('_', depth, '_'), sens$SensorID), ]
        sens2 <- sens1[grepl(paste0('_dielectric_constant'), sens1$SensorID), ]
        
        url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', sid, '&sensortype=Soil-Moisture&sensorid=', sens2$SensorID, '&startdate=', d3)
        
        response <- GET(url)
        stream <- content(response, as="text", encoding	='UTF-8')
        ts <- convertJSONtoTS(stream)
        rec <- tail(ts, 1)
        index(rec)[1]
        df <- data.frame(SiteID=sid, dt= index(rec)[1], y=bs$Latitude[i], x=bs$Longitude[i], SM=rec[1,1])
        colnames(df) <- c('sid', 'dt', 'y', 'x', 'SM')
        outDf <- rbind(outDf,df)
        
      }
      
      ext <- extent(bdy)
      r <- raster(ext, nrows=100, ncols=100)
      
      print(df)
      
      xy <- data.frame(x=outDf$x, y=outDf$y)
      tps <- Tps(xy, outDf$SM, lon.lat = T, lambda=0.01)
      p <- interpolate(r, tps)
      p <- mask(p, bdy)
      
      
      
      crs(p) <- CRS("+init=epsg:4326")
      pal <- colorNumeric(c("brown", "lightgreen",  "darkgreen"), values(p),na.color = "transparent")
      
      updateF7Progress(session, id = "pg1", value = 0)
      
      proxysm <- leafletProxy("moistureMap2", data = RV$sensorLocs)
      #proxysm %>% clearMarkers()
      proxysm %>% clearControls()
      
      proxysm %>% addRasterImage(p, colors = pal, opacity = 0.8 ,  group = "Moisture Map")
      proxysm %>% leaflet::addLegend(pal = pal, values = values(p), title = 'Soil Moisture', position = "bottomleft" )
      
      
    } , ignoreInit = TRUE   )
    
    
    
    
    
    
    
    
    
   
    
    
    ############################   SERVER - SOIL OBSERVATIONS MAP  ##################################

    
    
    output$soilMap <- renderLeaflet({
      
      sdf <- RV$sensorLocs
      labs <- lapply(seq(nrow(sdf)), function(i) {
        paste0( '<li>Site Name : ', sdf[i, "SiteName"], '</li>',
                '<li>Provider : ', sdf[i, "SensorGroup"], '</li>',
                '<li>Backend : ', sdf[i, "Backend"], '</li>',
                #'<li>Access : ', sdf[i, "Access"], '</li>',
                '<li>Site ID : ', sdf[i, "SiteID"], '</li>')
      })
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
        setView(lng = 148.692, lat = -34.475, zoom = 14) %>%
        
        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
          overlayGroups = c("Soil Maps", "Soil Observations"),
          options = layersControlOptions(collapsed = T)
        )
    })
    
    
    #  Soil observations proxy map =====
    observe({
      
      req(input$SoilPropList,input$SoilDepthList)
      sdf <- soilLocs
      labs <- lapply(seq(nrow(sdf@data)), function(i) {
        paste0( '<li>Site Name : ', sdf@data[i, "locID"], '</li>')
        
      })
      
      
      rPath <- paste0(dataStoreDir, '/', input$SoilPropList, '/', input$SoilPropList, '_', input$SoilDepthList, '_50th_percentile.tif' )
      print(rPath)
      r <- raster(rPath)
      crs(r) <- CRS('+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
      pal <- colorNumeric(c("brown", "lightgreen",  "darkgreen"), values(r),na.color = "transparent")
      
      
      print(head(sdf))
      proxy <- leafletProxy("soilMap", data = spts)
      proxy %>% clearMarkers()
      proxy %>% clearControls()
      proxy %>% addRasterImage(r, colors = pal, opacity = 0.8 ,  group = "Soil Maps")
      proxy %>% leaflet::addLegend(pal = pal, values = values(r), title = input$SoilPropList, position = "bottomleft" )
      proxy %>% addCircleMarkers(  # lng = ~easting, lat = ~northing,
        label = lapply(labs, HTML),
        stroke = FALSE,
        fillOpacity = 1,
        color = 'red',
        radius = 8,
        layerId=paste0(sdf$locID),
        group = "Soil Observations" )
    })
    
    ################  Get data from Clicking on a soil observation location  #################
    observe({
      click <-input$soilMap_marker_click
      if(is.null(click))
        return()
      
      sid <- click$id
      RV$currentSoil <- soilDataDF[soilDataDF$locID == sid, c(3, 6,7, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20) ]
      print(RV$currentSoil)
    })
    
    #  Render selected point  ====    
    observeEvent(input$soilMap_marker_click, { # update the map markers and view on location selectInput changes
      p <- input$soilMap_marker_click
      if(is.null(p))
        return()
      proxy <- leafletProxy("soilMap")
      if(p$id=="Selected"){
        proxy %>% removeMarker(layerId="Selected")
      } else {
        proxy %>% acm_defaults(p$lng, p$lat)
      }
    })
    
    
    #   Render the soil data table ====
    output$soilDataTable = renderRHandsontable({
      req(RV$currentSoil)
      if(nrow(RV$currentSoil) > 0){
        rhandsontable(RV$currentSoil,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F)%>%
          hot_table(highlightCol = F, highlightRow = F)
      }else{
        return(NULL)
      }
    })
    
    # output$soilDataTable <- renderTable({
    #   RV$currentSoil
    # }, rownames = F)
    
  }
)









