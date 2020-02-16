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

defWidth = '380px'
loaderTime = 1
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


shiny::shinyApp(
  ui = f7Page(
    title = "BARS",
    init = f7Init(skin = "auto", theme = "light", filled = T, color = 'lightblue' ),
    
    useShinyjs(),
    
    #title = NULL,
    preloader = T,
    loading_duration = loaderTime,
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "Left Panel", side = "left", theme = "light", "Blabla", effect = "cover")
        #f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
      ),
      
      navbar = f7Navbar(
        #title = shiny::tags$div(style="background-image: url('Logos/HdrBkGrdImage.PNG');", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px"), "Boowora Agricultutral Research Station "),
        title = shiny::tags$div( "Boowora Ag Research Station ",  tags$img(src = "Logos/csiro.png", width = "40px", height = "40px")),
        hairline = T,
        shadow = T,
        left_panel = T,
        right_panel = F
      ),
      
     
      
     
      f7Tabs(
        animated = T,
        #swipeable = TRUE,
        f7Tab(
          tabName = "SM Probes",
          icon = f7Icon("layers_fill"),
          active = TRUE,
          f7Float( f7Shadow(
            intensity = 10,
            hover = TRUE,
           tags$div( style=paste0("width: ", defWidth),
              f7Card(
              title = NULL,
           
              f7Select(inputId = 'SMDepth', label = "Select Soil Moisture Depth (cm)", c(30, 40, 50,60,70,80,90,100)),
              
              leafletOutput("moistureMap", height = 400 )
              
            )
            )
          ), side = "left" )
          
          , f7Float(  f7Shadow(
            intensity = 100,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
            f7Card(
              title = NULL,
              
              dygraphOutput("mositureChart1", width = "350", height = "300px")

            )
        )
          ), side = "left" )
        ),
        
        f7Tab(
          tabName = "SM Maps",
          icon = f7Icon("map"),
          active = FALSE,
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
  server = function(input, output, session) {

    RV <- reactiveValues()
    RV$currentTS <- NULL
    RV$currentSite <- NULL
    RV$sensorLocs <- NULL
    RV$currentSoil <- NULL
    RV$SoilMOistureSensors <- NULL

    
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
    
   ###################   Draw the soil moisture maps   ##########
     
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
      
      

      proxysm <- leafletProxy("moistureMap2", data = RV$sensorLocs)
      #proxysm %>% clearMarkers()
      proxysm %>% clearControls()
     
      proxysm %>% addRasterImage(p, colors = pal, opacity = 0.8 ,  group = "Moisture Map")
      proxysm %>% leaflet::addLegend(pal = pal, values = values(p), title = 'Soil Moisture', position = "bottomleft" )
      
      
    } , ignoreInit = TRUE   )
    
    
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
            dyAxis("y", label = 'Soil Moisture', valueRange = c(10, 40)) %>%
            dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 26) %>%
            dyLegend(show = "follow", hideOnMouseOut = T, labelsSeparateLines = T)  %>%
            dyRangeSelector()

        })
      }
    })

   
    ########  Get sensor locations   ##############
    
     observe({
      url <- paste0(SenFedServer,"/getSensorLocations?sensortype=Soil-Moisture" )
      response <- GET(url)
      stop_for_status(response) # stop if the response is an error
      sensorLocs <- fromJSON(content(response, as="text"))
      bs <- sensorLocs[sensorLocs$SensorGroup == 'Booroowa',]
      RV$sensorLocs <- bs
    })

    ########  Get Boowora Sensor Info   ##############
    
    observe({
      url <- paste0(SenFedServer,'/getSensorInfo?sensortype=Soil-Moisture')
      response <- GET(url)
      stop_for_status(response) # stop if the response is an error
      sensors3 <- fromJSON(content(response, as="text"))
      
      sensors  <- sensors3[sensors3$SensorGroup == 'Booroowa',]
      RV$SoilMOistureSensors <- sensors
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
