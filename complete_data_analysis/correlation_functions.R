#Correlation functions

#Breaks down date into seconds, minutes, hours and 
date.vars <-
  function(data_input) {
    data_input$minute <- minute(data_input$time_stamp.y)
    data_input$hour<- hour(data_input$time_stamp.y)
    data_input$day<- day(data_input$time_stamp.y)
    data_input$month<- month(data_input$time_stamp.y)
    return(data_input)
  }
