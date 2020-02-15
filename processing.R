library(httr)
library(fields)
library(gstat)

SenFedServer <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI'

url <- paste0(SenFedServer,"/getSensorLocations?sensortype=Soil-Moisture" )
response <- GET(url)
stop_for_status(response) # stop if the response is an error
sensorLocs <- fromJSON(content(response, as="text"))
bs <- sensorLocs[sensorLocs$SensorGroup == 'Booroowa',]


depth <- 'd1'

DataType <- 'Soil-Moisture'


df <- getSMmap()


tictoc::tic()
getSMmap(depth, day)
tictoc::toc()


depth <- 1
day <- '2020-01-10T00%3A00%3A00'

getSMmap <- function(depth, day, bdy){

    outDf <- data.frame()
    
    for(i in 1:nrow(bs)){
          print(i)
          
          sid <- bs$SiteID[i]
        
          url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorInfo?siteid=', sid)
          response <- GET(url)
          stop_for_status(response) # stop if the response is an error
          sensors <- fromJSON(content(response, as="text"))
        
          sens1 <- sensors[grepl(paste0('_', depth, '_'), sensors$SensorID), ]
          sens2 <- sens1[grepl(paste0('_dielectric_constant'), sens1$SensorID), ]

          d1 <- as.Date(day)
          d2 <- d1-10
          d3 <- paste0(d2, 'T00:00:00')
          
          #url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', sid, '&sensortype=Soil-Moisture&sensorid=', sens2$SensorID, '&startdate=', d3)
          url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', sid, '&sensortype=Soil-Moisture&startdate=2020-01-10T00%3A00%3A00')
          
           response <- GET(url)
          stop_for_status(response) # stop if the response is an error
          stream <- content(response, as="text")
          
          xin <- fromJSON(stream)
          outDF <- data.frame(xin$DataStream[[1]]$t)
          cnames<- c('DateTime', rep('x', nrow(xin)))
          
          for (i in 1:nrow(xin)) {
            d <- xin[i,]
            dd <- d$DataStream
            outDF[, (i+1)] <- dd[[1]]$v
            
            # d$DataType
            #  d$UpperDepthCm
            
            if(is.null(d$UpperDepth[1])){
              suffix = paste0('_x', i)
            }else if(is.na(d$UpperDepth[1])){
              suffix = paste0('_x', i)
            }else if(d$UpperDepth == d$LowerDepth[1]){
              suffix = paste0('_', d$UpperDepth[1])
            }else{
              suffix =  paste0('_', d$UpperDepth[1], '_', d$LowerDepth[1])
            }
            cnames[i+1] <- c(paste0(d$DataType[1],suffix))
          }
          colnames(outDF) <- cnames
          
          ts <- outDF
          
          tail(ts)
          rec <- tail(ts, 1)
            index(rec)[1]
          df <- data.frame(SiteID=sid, dt= index(rec)[1], y=bs$Latitude[i], x=bs$Longitude[i], SM=rec[1,1])
          outDf <- rbind(outDf,df)
            
    }
    
    ext <- extent(bdy)
    r <- raster(ext, nrows=100, ncols=100)
    
    xy <- data.frame(x=outDf$x, y=outDf$y)
    tps <- Tps(xy, outDf$SM, lon.lat = T, lambda=0.01)
    p <- interpolate(p, tps)
    p <- mask(p, bdy)
    
    return(p)

}



d1 <- as.Date('2020-01-10T00:00:00')
d2 <- d1-10
d3 <- paste0(d2, 'T00:00:00')

getSMmap()


urls <- vector('list', 21)
length(urls)

getSMmapPara <- function(){
  
  outDf <- data.frame()
  
  sid <- bs$SiteID
  
  for(i in 1:nrow(bs)){
    print(i)
    
    sid <- bs$SiteID[i]
    
    url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorInfo?siteid=', sid)
    response <- GET(url)
    stop_for_status(response) # stop if the response is an error
    sensors <- fromJSON(content(response, as="text"))
    
    sensID <- sensors$SensorID[1]
    #url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', sid, '&sensortype=Soil-Moisture&sensorid=', sensID, '&startdate=2020-01-10T00%3A00%3A00')
    url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', sid, '&sensortype=Soil-Moisture&startdate=2020-01-10T00%3A00%3A00')
    
    urls[[i]] <- url
    
    response <- GET(url)
    stop_for_status(response) # stop if the response is an error
    stream <- content(response, as="text")
    ts <- convertJSONtoTS(stream)
    tail(ts)
    rec <- tail(ts, 1)
    index(rec)[1]
    df <- data.frame(SiteID=sid, dt= index(rec)[1], y=bs$Latitude[i], x=bs$Longitude[i], SM=rec[1,1])
    outDf <- rbind(outDf,df)
    
  }
  
  ext <- extent(bdy)
  r <- raster(ext, nrows=100, ncols=100)
  
  xy <- data.frame(x=outDf$x, y=outDf$y)
  tps <- Tps(xy, outDf$SM, lon.lat = T, lambda=0.01)
  p <- interpolate(p, tps)
  p <- mask(p, bdy)
  
  return(p)
  
}


tictoc::tic()
dataStreamsDF <- synchronise(async_map(urls, getURLAsync, .limit = 10))
unlist(dataStreamsDF)
smDF <- as.data.frame(data.table::rbindlist(dataStreamsDF))
tictoc::toc()

getURLAsync <- function(url){
  
  response <- GET(url)
  stop_for_status(response) # stop if the response is an error
  stream <- suppressMessages (content(response, as="text"))
  ts <- convertJSONtoTS(stream)
  tail(ts)
  rec <- tail(ts, 1)
  df <- data.frame(SiteID=sid, dt= index(rec)[1], y=bs$Latitude[i], x=bs$Longitude[i], SM=rec[1,1])
  return(df)
}


write.csv(smDF, 'c:/temp/urls.csv')




