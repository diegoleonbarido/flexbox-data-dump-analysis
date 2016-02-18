#Daily plots


#Hourly aggregation of daily plots
hourly.plotting <- function(data){

  data <- data[!duplicated(data$time_stamp),]
  data <- date.vars.simple(data) 
  
  #Creating the unique ID and merging with the original data set so that all unique hours
  

#Comment out for now
#house_Power_sum <- aggregate(house_analysis$house_Power,list(house_analysis$hour,house_analysis$date),FUN=sum) %>% mutate(hour = Group.1,date=Group.2,power=x) %>% select(hour,date, power)
return(house_analysis)
}



#Second-by-second aggregation of daily plots
second.plotting <- function(data){
  
  data <- data[!duplicated(data$time_stamp),]
  data <- date.vars.simple(data) %>% select(time_stamp,house_Power,house_Energy,second,minute,hour)
  
  #Creating the unique ID and merging with the original data set so that all pair 'hour - minute' values have a unique id
  unique_day_combo <- unique(data[c("second","hour", "minute")])
  unique_day_combo_ordered <- unique_day_combo[order(unique_day_combo$hour,unique_day_combo$minute,unique_day_combo$second),]
  unique_day_combo_ordered$id <- c(1:length(unique_day_combo_ordered$hour))
  unique_day_merge <-  join(data,unique_day_combo_ordered,type='left',match='all')
  
  return(unique_day_merge)
}



