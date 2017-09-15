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

prophetFileForecast<- function(input,steps){

  newdata <- read.csv(input)

  m <- prophet(newdata)

  future <- make_future_dataframe(m, periods = future)

  forecast <- predict(m, future)

  forecast <- forecast[which(weekdays(as.Date(forecast$ds, format = "%m/%d/%Y"))

                             %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
  forecast
}

fbprophet <- function(input, steps=5 ,frequency='d', remove_weekends=FALSE){

  newdata <- if(is.character(input) && file.exists(input)){
    read.csv(input)
    } else {
       as.data.frame(input)
    }

  #adapt input dataset
  names(newdata)[names(newdata)=="date"] <- "ds"
  names(newdata)[names(newdata)=="value"] <- "y"

  m <- prophet(newdata)

  future <- make_future_dataframe(m, periods = steps, freq = frequency )

  forecast <- predict(m, future)

  if(remove_weekends==TRUE){

  forecast <- forecast[which(weekdays(as.Date(forecast$ds, format = "%m/%d/%Y"))

                             %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]

  }

  #adapt returning dataset
  df = forecast[c("ds","yhat","yhat_lower","yhat_upper")]
  names(df)[names(df)=="ds"] <- "date"
  names(df)[names(df)=="yhat"] <- "value_pred"
  names(df)[names(df)=="yhat_lower"] <- "value_pred_min"
  names(df)[names(df)=="yhat_upper"] <- "value_pred_max"
  df
}

testFunction <- function(input,steps,frequency,remove_weekends){

  newdata <- as.data.frame(input)

  #adapt input dataset
  names(newdata)[names(newdata)=="date"] <- "ds"

  m <- prophet(newdata)

  future <- make_future_dataframe(m, periods = steps, freq = frequency )

  forecast <- predict(m, future)

  if(exists("remove_weekends") && remove_weekends==TRUE){

    forecast <- forecast[which(weekdays(as.Date(forecast$ds, format = "%m/%d/%Y"))

                               %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]

  }

  #adapt returning dataset
  df = forecast[c("ds","yhat","yhat_lower","yhat_upper")]
  names(df)[names(df)=="ds"] <- "date"
  names(df)[names(df)=="yhat"] <- "ypred"
  names(df)[names(df)=="yhat_lower"] <- "ypred_min"
  names(df)[names(df)=="yhat_upper"] <- "ypred_max"
  df
}

stlJsonForecast <- function(input){


  newdata <- as.data.frame(input$dataset)

  newdata$date <- as.Date(input$ds, "%Y-%m-%d")

  result.stl <- forecastStl(rates, n.ahead = input$steps)

  result.stl
}

## Forecast with STL model
forecastStl <- function(x, n.ahead=300){
  x$ds <- as.Date(x$ds,"%d.%m.%Y")
  x <- x[order(x$ds),]


  myTs <- ts(x$y, start=1, frequency=256)
  fit.stl <- stl(myTs, s.window=256)
  #fit.stl <- nnetar(myTs)
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
                       date = c(x$ds, max(x$ds) + (1:n.ahead))
  )
  return(output)
}


