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




machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  dataStoreDir <- '/mnt/data/BARS/SoilPropertyPredictions'
  source('/srv/shiny-server/BARS/appUtils.R')
}else{
  dataStoreDir <- 'C:/Temp/boorowa_2019/data/processed'
  source('appUtils.R')
}

SenFedServer <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI'

soilDataDF <- read.csv(paste0(dataStoreDir, '/scans_predicted_soil_data.csv'), stringsAsFactors = F)
soilLocs <- soilDataDF[row.names(unique(soilDataDF[,c("locID", "easting", "northing")])), c("locID", "easting", "northing")]
coordinates(soilLocs) <- c("easting", "northing")
proj4string(soilLocs) <- CRS('+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
CRS.new <- CRS("+init=epsg:4326")
spts <- spTransform(soilLocs, CRS.new)



shiny::shinyApp(
  ui = f7Page(
    title = "Boowoora Ag Research Station",
    init = f7Init(skin = "auto", theme = "light"),
    #title = NULL,
    preloader = T,
    loading_duration = 4,
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "Left Panel", side = "left", theme = "light", "Blabla", effect = "cover")
        #f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
      ),
      
      navbar = f7Navbar(
        title = shiny::tags$div(tags$img(src = "Logos/csiro.png", width = "20px", height = "20px"), "Boowoora Ag Research Station "),
        hairline = T,
        shadow = T,
        left_panel = T,
        right_panel = F
      ),
      
     
      
     
      f7Tabs(
        animated = T,
        #swipeable = TRUE,
        f7Tab(
          tabName = "Soil Moisture",
          icon = f7Icon("layers_fill"),
          active = TRUE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = NULL,
              #sliderInput("obs1", "Number of observations", 0, 1000, 500),
              f7Select('SMDepth', "Select Soil Moisture Depth (cm)", c(30, 40, 50,60,70,80,90,100)),
              leafletOutput("moistureMap", height = 400)
              # footer = tagList(
              #   #f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
              #   f7Badge("Badge", color = "green")
              # )
            )

          ), f7Shadow(
            intensity = 100,
            hover = TRUE,
            f7Card(
              title = "Hi there",
              dygraphOutput("mositureChart1", width = "350", height = "300px")

            )

          )
        ),
        f7Tab(
          tabName = "Weather",
          icon = f7Icon("cloud_heavyrain_fill"),
          active = FALSE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Weather",
              prettyRadioButtons(
                "obs2",
                "Distribution type:",
                c("Normal" = "norm",
                  "Uniform" = "unif",
                  "Log-normal" = "lnorm",
                  "Exponential" = "exp"),
                inline = TRUE,
                status = "warning",
                animation = "pulse"
              ),
              plotOutput("distPlot2"),
              footer = tagList(
                f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
                f7Badge("Badge", color = "green")
              )
            )
          )
        ),
        f7Tab(
          tabName = "Soil Data",
          icon = f7Icon("list_number_rtl"),
          active = FALSE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
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
       
      )
    )
  ),
  server = function(input, output) {

    RV <- reactiveValues()
    RV$currentTS <- NULL
    RV$currentSite <- NULL
    RV$sensorLocs <- NULL
    RV$currentSoil <- NULL

    ################  Get data from Clicking on a sensor  #################
    observe({
      click<-input$moistureMap_marker_click
      if(is.null(click))
        return()

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


    ################## Render the Chart from a map drill  ##################
    output$mositureChart1 <- renderDygraph({

      if(!is.null(RV$currentTS)){

        isolate({

          maxVal <- max(RV$currentTS)

          dygraph(RV$currentTS ,  main = paste0('SoilMoisture'), ylab = 'Soil Moisture')%>%
            dyAxis("y", label = 'Soil Moisture', valueRange = c(0, maxVal)) %>%
            dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 26) %>%
            dyLegend(show = "follow", hideOnMouseOut = T, labelsSeparateLines = T)  %>%
            dyRangeSelector()

        })
      }
    })

    observe({
      url <- paste0(SenFedServer,"/getSensorLocations?sensortype=Soil-Moisture" )
      response <- GET(url)
      stop_for_status(response) # stop if the response is an error
      sensorLocs <- fromJSON(content(response, as="text"))
      bs <- sensorLocs[sensorLocs$SensorGroup == 'Booroowa',]
      RV$sensorLocs <- bs
    })


    observe({
      req(input$SoilPropList)
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

      proxy <- leafletProxy("moistureMap", data = RV$sensorLocs)
      proxy %>% clearMarkers()
      proxy %>% clearControls()
      proxy %>% addCircleMarkers(   lng = ~Longitude, lat = ~Latitude,
                                    label = lapply(labs, HTML),
                                    stroke = FALSE,
                                    fillOpacity = 1,
                                    color = factpal(sdf[,input$SensorLabel]),
                                    radius = 15,
                                    layerId=paste0(sdf$SiteID),
                                    group = "Sensors" )
    })

    output$moistureMap <- renderLeaflet({
      leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
        setView(lng = 148.689633, lat = -34.468953, zoom = 14) %>%

        # addControlGPS() %>%

        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
          overlayGroups = c("Moisture Maps", "All Sensors"),
          options = layersControlOptions(collapsed = T)
        )
    })

    
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
        setView(lng = 148.689633, lat = -34.468953, zoom = 14) %>%
        
        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
          overlayGroups = c("Soil Maps", "Soil Observations"),
          options = layersControlOptions(collapsed = T)
        )
    })
    
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
      proxy %>% leaflet::addLegend(pal = pal, values = values(r), title = input$SoilPropList)
      proxy %>% addCircleMarkers(  # lng = ~easting, lat = ~northing,
                                     label = lapply(labs, HTML),
                                    stroke = FALSE,
                                    fillOpacity = 1,
                                    color = 'red',
                                    radius = 8,
                                    layerId=paste0(sdf$locID),
                                    group = "Soil Observations" )
    })
    
    ################  Get data from Clicking on a sensor  #################
    observe({
      click <-input$soilMap_marker_click
      if(is.null(click))
        return()
      
      sid <- click$id
      RV$currentSoil <- soilDataDF[soilDataDF$locID == sid, c(3, 6,7, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20) ]
      print(RV$currentSoil)
    })
    
    acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=8, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
    
    observeEvent(input$soilMap_marker_click, { # update the map markers and view on location selectInput changes
      
      p <- input$soilMap_marker_click
      if(is.null(p))
        return()
      
      
      proxy <- leafletProxy("soilMap")
      
      if(p$id=="Selected"){
        proxy %>% removeMarker(layerId="Selected")
      } else {
        #proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
        proxy %>% acm_defaults(p$lng, p$lat)
      }
    })
    
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