# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function(req) {
  req$x
}

createModel <- function(input){
  #require(dplyr)
  #require(jsonlite)
  require(prophet)

  #df <- jsonlite::unserializeJSON(req$data)
  #df$ds <- as.Date(df$ds, req$dateFormat)
  newdata <- if(is.character(input) && file.exists(input)){
    read.csv(input)
  } else {
    as.data.frame(input)
  }

  m <- prophet::prophet(newdata)
  #jsonM <- jsonlite::serializeJSON(m)
  #jsonM
  future <- make_future_dataframe(m, periods = 365)

  forecast <- predict(m, future)

  forecast <- forecast[which(weekdays(as.Date(forecast$ds, format = "%m/%d/%Y"))

                             %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
  forecast
}

predictit <- function(input){


  newdata <- if(is.character(input) && file.exists(input)){
    read.csv(input)
  } else {
    as.data.frame(input)
  }

  input$ds <- as.Date(input$ds, "%Y-%m-%d")

  result.stl <- forecastStl(rates, n.ahead = 90)

  result.stl
}

## Forecast with STL model
forecastStl <- function(x, n.ahead=30){
  myTs <- ts(x$y, start=1, frequency=256)
  fit.stl <- stl(myTs, s.window=256)
  sts <- fit.stl$time.series
  trend <- sts[,"trend"]
  fore <- forecast(fit.stl, h=n.ahead, level=95)
  plot(fore)
  pred <- fore$mean
  upper <- fore$upper
  lower <- fore$lower
  output <- data.frame(actual = c(x$y, rep(NA, n.ahead)),
                       trend = c(trend, rep(NA, n.ahead)),
                       #pred = c(trend, pred),
                       pred = c(rep(NA, nrow(x)), pred),
                       lower = c(rep(NA, nrow(x)), lower),
                       upper = c(rep(NA, nrow(x)), upper),
                       date = c(x$Date, max(x$Date) + (1:n.ahead))
  )
  return(output)
}


