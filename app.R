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


source('appUtils.R')


SenFedServer <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI'
dataStoreDir <- 'C:/Temp/boorowa_2019/data'



shiny::shinyApp(
  ui = f7Page(
    title = "Boowoora Ag Research Station",
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "Left Panel", side = "left", theme = "light", "Blabla", effect = "cover")
        #f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
      ),
      navbar = f7Navbar(
        title = "Boowoora Ag Research Station",
        hairline = T,
        shadow = T,
        left_panel = TRUE,
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
              title = "",
              #sliderInput("obs1", "Number of observations", 0, 1000, 500),
              f7Select('SMDepth', "Select Soil Moisture Depth", c(30, 40, 50,60,70,80,90,100)),
              leafletOutput("moistureMap", height = 500)
              # footer = tagList(
              #   #f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
              #   f7Badge("Badge", color = "green")
              # )
            ),

          ), f7Shadow(
            intensity = 100,
            hover = TRUE,
            f7Card(
              title = "Hi there",
              dygraphOutput("mositureChart1", width = "350", height = "300px")

            ),

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
              title = "Soil Data",
              prettyCheckboxGroup(
                "variable",
                "Variables to show:",
                c("Cylinders" = "cyl",
                  "Transmission" = "am",
                  "Gears" = "gear"),
                inline = TRUE,
                status = "danger",
                animation = "pulse"
              ),
              f7Picker('SoilPropList', "Select soil attribute", choices=c('clay', 'ecec', 'phc', 'soc')),
              leafletOutput("soilMap", height = 500)
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
      req(RV$sensorLocs)
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
      leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
        setView(lng = 148.689633, lat = -34.468953, zoom = 14) %>%
        
        # addControlGPS() %>%
        
        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
          overlayGroups = c("Soil Maps", "Soil Observations"),
          options = layersControlOptions(collapsed = T)
        )
    })
    observe({
      req(RV$sensorLocs)
      sdf <- RV$sensorLocs
      labs <- lapply(seq(nrow(sdf)), function(i) {
        paste0( '<li>Site Name : ', sdf[i, "SiteName"], '</li>',
                '<li>Provider : ', sdf[i, "SensorGroup"], '</li>',
                '<li>Backend : ', sdf[i, "Backend"], '</li>',
                #'<li>Access : ', sdf[i, "Access"], '</li>',
                '<li>Site ID : ', sdf[i, "SiteID"], '</li>')
      })
     
      r <- raster(paste0(dataStoreDir, '/processed/clay/clay_d1_50th_percentile.tif' ))
      crs(r) <- CRS('+init=EPSG:28355')
      pal <- colorNumeric(c("brown", "lightgreen",  "darkgreen"), values(r),na.color = "transparent")
      # colCnt <- length(unique(sdf[,input$SensorLabel]))
      # colCats <- unique(sdf[,input$SensorLabel])
      # colField <- sdf[,input$SensorLabel]
      # factpal <-colorFactor(RColorBrewer::brewer.pal(colCnt, 'Spectral'), colField)
      # 
      proxy <- leafletProxy("soilMap", data = RV$sensorLocs)
      proxy %>% clearMarkers()
      proxy %>% clearControls()
      #addRasterImage(r, colors = pal, opacity = 0.8 ,  group = "Soil Maps")
      proxy %>% addRasterImage(r, colors = pal, opacity = 0.8 ,  group = "Soil Maps")
      proxy %>% addCircleMarkers(   lng = ~Longitude, lat = ~Latitude,
                                    label = lapply(labs, HTML),
                                    stroke = FALSE,
                                    fillOpacity = 1,
                                   # color = factpal(sdf[,input$SensorLabel]),
                                    radius = 10,
                                    layerId=paste0(sdf$SiteID),
                                    group = "Sensors" )
    })
    

  }
)