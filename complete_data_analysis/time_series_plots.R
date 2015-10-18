############# Plotting a subset of the data for the last seven days


#Function to plot the time series data for non-clean data, takesin ambient, inside, refrigerator, switch and house data
time.series.sevendays <- function(ambient,inside,refrigerator,switch,house) {

  #Keeping only a subset of the data for each variable and plotting it 
  end_date_ambient <- unique(tail(ambient$date))
  plot_date_ambient <- end_date_ambient - 7 
  
  end_date_inside <- unique(tail(inside$date))
  plot_date_inside <- end_date_inside - 7 
  
  end_date_refrigerator <- unique(tail(refrigerator$date))
  plot_date_refrigerator <- end_date_refrigerator - 7 
  
  end_date_switch <- unique(tail(switch$date))
  plot_date_switch <- end_date_switch - 7 
  
  end_date_house <- unique(tail(house$date))
  plot_date_house<- end_date_house - 7 
    
   #Unique time_stamps
   ambient_unique_timestamps <- ambient[!duplicated(ambient$time_stamp),]
   inside_unique_timestamps <- inside[!duplicated(inside$time_stamp),]
   refrigerator_unique_timestamps <- refrigerator[!duplicated(refrigerator$time_stamp),]
   switch_unique_timestamps <- switch[!duplicated(switch$time_stamp),]
   house_unique_timestamps <- house[!duplicated(house$time_stamp),]
   
  
    #Subset data
    ambient_subset <- subset(ambient_unique_timestamps,ambient_unique_timestamps$date > plot_date_ambient & ambient_unique_timestamps$ambient_temp>20)
    inside_subset <- subset(inside_unique_timestamps,inside_unique_timestamps$date > plot_date_inside)
    refrigerator_subset <- subset(refrigerator_unique_timestamps,refrigerator_unique_timestamps$date > plot_date_refrigerator)
    switch_subset <- subset(switch_unique_timestamps ,switch_unique_timestamps $date > plot_date_switch)
    house_subset <- subset(house_unique_timestamps,house_unique_timestamps$date > plot_date_house)
    
return(list(ambient_subset,inside_subset,refrigerator_subset,switch_subset,house_subset))
    
}




#Function to plot the time series data for CLEAN DATA, takesin ambient, inside, refrigerator, switch and house data
time.series.sevendays.clean <- function(ambient,inside,refrigerator,switch,house) {
  
  #Keeping only a subset of the data for each variable and plotting it 
  end_date_ambient <- unique(tail(ambient$date))
  plot_date_ambient <- end_date_ambient - 7 
  
  end_date_inside <- unique(tail(inside$date))
  plot_date_inside <- end_date_inside - 7 
  
  end_date_refrigerator <- unique(tail(refrigerator$date))
  plot_date_refrigerator <- end_date_refrigerator - 7 
  
  end_date_switch <- unique(tail(switch$date))
  plot_date_switch <- end_date_switch - 7 
  
  house <- subset(house, house$house_Power < quantile(house$house_Power,.99,na.rm=TRUE) )
  end_date_house <- unique(tail(house$date))
  plot_date_house<- end_date_house - 7 
  
  #Unique time_stamps
  ambient_unique_timestamps <- ambient[!duplicated(ambient$time_stamp),]
  inside_unique_timestamps <- inside[!duplicated(inside$time_stamp),]
  refrigerator_unique_timestamps <- refrigerator[!duplicated(refrigerator$time_stamp),]
  switch_unique_timestamps <- switch[!duplicated(switch$time_stamp),]
  house_unique_timestamps <- house[!duplicated(house$time_stamp),]
  
  
  #Subset data
  ambient_subset <- subset(ambient_unique_timestamps,ambient_unique_timestamps$date > plot_date_ambient & ambient$ambient_temp>20)
  inside_subset <- subset(inside_unique_timestamps,inside_unique_timestamps$date > plot_date_inside)
  refrigerator_subset <- subset(refrigerator_unique_timestamps,refrigerator_unique_timestamps$date > plot_date_refrigerator)
  switch_subset <- subset(switch_unique_timestamps,switch_unique_timestamps$date > plot_date_switch)
  house_subset <- subset(house_unique_timestamps,house_unique_timestamps$date > plot_date_house)
  
  return(list(ambient_subset,inside_subset,refrigerator_subset,switch_subset,house_subset))
  
}





