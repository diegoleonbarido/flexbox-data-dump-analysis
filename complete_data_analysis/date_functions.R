#Date functions

#Breaks down date into minutes, hours, days and onths
date.vars <-
  function(data_input) {
    data_input$minute <- minute(data_input$time_stamp.y)
    data_input$hour<- hour(data_input$time_stamp.y)
    data_input$day<- day(data_input$time_stamp.y)
    data_input$month<- month(data_input$time_stamp.y)
    return(data_input)
  }

#Breaks down date into seconds, minutes, hours, days and onths
date.vars.simple <-
  function(data_input) {
    data_input$second <- second(data_input$time_stamp)
    data_input$minute <- minute(data_input$time_stamp)
    data_input$hour<- hour(data_input$time_stamp)
    data_input$day<- day(data_input$time_stamp)
    data_input$month<- month(data_input$time_stamp)
    return(data_input)
  }


#Creates the time stamp
date.data.frame <- function(ambient,inside,refrigerator,switch,house) {
  
  ambient$datetime_rgx <- gsub("\\..*","",ambient$datetime)
  inside$datetime_rgx <- gsub("\\..*","", inside$datetime)
  refrigerator$datetime_rgx <- gsub("\\..*","", refrigerator$datetime)
  switch$datetime_rgx <- gsub("\\..*","", switch$datetime)
  house$datetime_rgx <- gsub("\\..*","", house$datetime)
  
  #Dates with times 
  ambient$datetime_rgx_new <- paste(ambient$datetime_rgx,"-0000",sep=" ")
  ambient$time_stamp <- strptime(ambient$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
  
  inside$datetime_rgx_new <- paste(inside$datetime_rgx,"-0000",sep=" ")
  inside$time_stamp <- strptime(inside$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
  
  refrigerator$datetime_rgx_new <- paste(refrigerator$datetime_rgx,"-0000",sep=" ")
  refrigerator$time_stamp <- strptime(refrigerator$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
  
  switch$datetime_rgx_new <- paste(switch$datetime_rgx,"-0000",sep=" ")
  switch$time_stamp <- strptime(switch$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
  
  house$datetime_rgx_new <- paste(house$datetime_rgx,"-0000",sep=" ")
  house$time_stamp <- strptime(house$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
  
  
  #Just dates (day without time)
  ambient$date <- as.Date(ambient$time_stamp)
  inside$date <- as.Date(inside$time_stamp)
  switch$date <- as.Date(switch$time_stamp)
  house$date <- as.Date(house$time_stamp)
  refrigerator$date <- as.Date(refrigerator$time_stamp)
  
return(list(ambient,inside,switch,house,refrigerator))
}

#Creates a time vector with all time stamps
time.vector.seconds <- function(data.vector){
  
z <- seq(as.POSIXct(head(data.vector$date,1)), as.POSIXct(tail(data.vector$date,1)), by = "1 sec")
some.data.frame<- as.data.frame(c(1:length(z))) %>% mutate(id.time.stamp =c(1:length(z))) %>% select(id.time.stamp)
some.data.frame$time_stamp <-  as.POSIXct(z)

return(some.data.frame)
}






