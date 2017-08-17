# flib.R
library(jsonlite)
library(prophet)

#* @post /ondemand
predictie <- function(req, dateFormat, inputData){
  #df <- fromJSON(req$postBody)
  m <- prophet(inputData)
  future <- make_future_dataframe(m, periods = 31,include_history = FALSE)
  forecast <- predict(m, future)
  forecast <- forecast[which(weekdays(as.Date(forecast$ds, format = "%m/%d/%Y"))
                             %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
  forecast
}

#* @get /model
createModel <- function(){
  df <- read.csv('C:\\Users\\adria\\Downloads\\CURSZ_small.csv', header=T)
  df$ds <- as.Date(df$ds, "%d.%m.%Y")
  m <- prophet(df)
  jsonM <- serializeJSON(m)
  write(jsonM, "C:\\Users\\adria\\Downloads\\model.json")

}

#* @get /apply
predictie <- function(){
  jsonM <- readLines("C:\\Users\\adria\\Downloads\\model.json")
  m <- unserializeJSON(jsonM)
  future <- make_future_dataframe(m, periods = 31,include_history = FALSE)
  forecast <- predict(m, future)
  forecast <- forecast[which(weekdays(as.Date(forecast$ds, format = "%m/%d/%Y"))
                             %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
  forecast
}
