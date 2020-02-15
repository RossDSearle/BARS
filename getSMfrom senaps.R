
url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorInfo')
response <- GET(url)
stop_for_status(response) # stop if the response is an error
sensors <- fromJSON(content(response, as="text"))


write.csv(sensors, 'c:/temp/sensors.csv')


sensors <- read.csv('c:/temp/sensors.csv')
sens1 <- sensors[grepl(paste0('_', depth, '_'), sensors$SensorID), ]
sens2 <- sens1[grepl(paste0('_dielectric_constant'), sens1$SensorID), ]

urls <- paste0("https://senaps.io/api/sensor/v2/observations?streamid=", sens2$SensorID , "&start=2020-02-01T00:00:00")


tictoc::tic()
dataStreamsDF <- synchronise(async_map(urls, getURLAsync, .limit = 40))
dataStreamsDF <- synchronise(async_map(urls, getURLAsync, .limit = 10))
unlist(dataStreamsDF)
smDF <- as.data.frame(data.table::rbindlist(dataStreamsDF))
tictoc::toc()

getURLAsync <- function(url){
  
 paras <- param_get(url)
  
  
  credentials <- authenticate('ross.searle@csiro.au', 'QBP')
  response <- GET(url, credentials)
  
  stop_for_status(response) # stop if the response is an error
  response_data <- fromJSON(content(response, as="text"))

  if(response_data$count == 0){
   return(NULL)
  }
  
  ds1 <- str_replace(response_data$results$t, 'T', ' ')
  ds2 <- str_replace(ds1, '.000Z', ' ')
  ndf <- data.frame(paras$streamid, ds2, response_data$results$v, stringsAsFactors = F)
  
  colnames(ndf)<- c('StreamID', 'theDate', 'Values')
  
  return(tail(ndf,1))
}




obs <- fromJSON(obssJ)

https://senaps.io/api/sensor/v2/observations?streamid=hussat.boorowa-soil-moisture.terrasonde.142.sensor_3_dielectric_constant&start=2020-02-01T00%3A00%3A00