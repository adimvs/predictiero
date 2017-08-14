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
