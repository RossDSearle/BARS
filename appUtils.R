


convertJSONtoTS <- function(resp){

  xin <- fromJSON(resp)
  outDF <- data.frame(xin$DataStream[[1]]$t)
  cnames<- c('DateTime', rep('x', nrow(xin)))

  for (i in 1:nrow(xin)) {

    outDF <- cbind(outDF, xin$DataStream[[i]]$v)
    cnames[i+1] <-  paste0(xin$DataType[[i]], "_", xin$UpperDepth[[i]])

  }
  colnames(outDF) <- cnames

  ds <- na.omit(outDF)
  d <- as.POSIXct(str_trim(ds$DateTime) , format = "%Y-%m-%d %H:%M:%S")
  ts <- xts(x=ds[,-1], unique = FALSE, order.by=d, tzone =  Sys.getenv("TZ"))



  return(ts)
}