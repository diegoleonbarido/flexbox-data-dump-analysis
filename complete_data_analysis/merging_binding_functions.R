################## Merging functions
#Includes
# 1) Non-clean data merging data for individual houses
# 2) Clean-data merging functions for individual houses
# 3) Non-clean data for ALL HOUSES

################## 1) Non-clean data Merging functions for Individal Houses

# NOTE: Does not include the subset for cleaning data 
merge.bind <- function(ambient,refrigerator,inside,house,switch) {
  
  #### Merging ambient and data together
  
  small_df_ambient_power <- merge(ambient,refrigerator,by='datetime_rgx')
  small_df_ambient_inside <- merge(ambient, inside, by='datetime_rgx')
  small_df_ambient_housepower <- merge(ambient,house,by='datetime_rgx')
  small_df_ambient_switch <- merge(ambient,switch,by='datetime_rgx')
  
  
  #Unique time_stamps
  small_df_ambient_power <- small_df_ambient_power[!duplicated(small_df_ambient_power$time_stamp.y),]
  small_df_ambient_inside <- small_df_ambient_inside[!duplicated(small_df_ambient_inside$time_stamp.y),]
  small_df_ambient_housepower <- small_df_ambient_housepower[!duplicated(small_df_ambient_housepower$time_stamp.y),]
  small_df_ambient_switch <- small_df_ambient_switch[!duplicated(small_df_ambient_switch$time_stamp.y),]
  
  
  #### Keeping some vars
  
  vars <- c('time_stamp.y','active_pwr3','ambient_temp')
  some_vars_ambient_power <- small_df_ambient_power[vars]
  vars <- c('time_stamp.y','fridge_temp1','ambient_temp')
  some_vars_ambient_inside <- small_df_ambient_inside[vars]
  vars <- c('time_stamp.y','house_Energy','ambient_temp') #Before I was using power, now I'm using energy
  some_vars_ambient_housepower <- small_df_ambient_housepower[vars]
  vars <- c('time_stamp.y','value','ambient_temp')
  some_vars_ambient_switch <- small_df_ambient_switch[vars]
  
  
  
  ###### Adding date variables
  
  some_vars_ambient_power <- date.vars(some_vars_ambient_power)
  some_vars_ambient_inside <- date.vars(some_vars_ambient_inside)
  some_vars_ambient_housepower <- date.vars(some_vars_ambient_housepower)
  some_vars_ambient_switch <- date.vars(some_vars_ambient_switch)
  
  
  
  ###### Grouping before analysis
  
  #Keep vars
  vars <- c('x')
  
  #Ambient and Inside Temperature
  mean_ambient_i <- aggregate(some_vars_ambient_inside$ambient_temp,list(some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
  mean_ambient_vars_i <- mean_ambient_i %>% mutate(average_hr_ambient = x,hour=Group.1) %>% select(hour,average_hr_ambient) 
  
  mean_inside <-  aggregate(some_vars_ambient_inside$fridge_temp1,list(some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
  mean_inside_vars <- mean_inside %>% mutate(average_hr_inside = x,hour=Group.1) %>% select(hour,average_hr_inside) 
  
  #Ambient and Fridge Power
  mean_ambient_p <- aggregate(some_vars_ambient_power$ambient_temp,list(some_vars_ambient_power$hour,some_vars_ambient_power$day,some_vars_ambient_power$month),FUN=mean)
  mean_ambient_vars_p <- mean_ambient_p %>% mutate(average_hr_ambient = x,hour=Group.1) %>% select(hour,average_hr_ambient) 
  
  mean_power <-  aggregate(some_vars_ambient_power$active_pwr3,list(some_vars_ambient_power$hour,some_vars_ambient_power$day,some_vars_ambient_power$month),FUN=mean)
  mean_power_vars <- mean_power %>% mutate(average_hr_power = x,hour=Group.1) %>% select(hour,average_hr_power) 
  
  
  #Ambient and House Power 
  mean_ambient_hp <- aggregate(some_vars_ambient_housepower$ambient_temp,list(some_vars_ambient_housepower$hour,some_vars_ambient_housepower$day,some_vars_ambient_housepower$month),FUN=mean)
  mean_ambient_vars_hp <- mean_ambient_hp %>% mutate(average_hr_ambient = x,hour=Group.1) %>% select(hour,average_hr_ambient) 
  
  mean_hpower <-  aggregate(some_vars_ambient_housepower$house_Power,list(some_vars_ambient_housepower$hour,some_vars_ambient_housepower$day,some_vars_ambient_housepower$month),FUN=mean)
  mean_hpower_vars <- mean_hpower %>% mutate(average_hr_hpower = x,hour=Group.1) %>% select(hour,average_hr_hpower) 
  
  
  #Ambient and switch
  mean_ambient_switch <- aggregate(some_vars_ambient_switch$ambient_temp,list(some_vars_ambient_switch$hour,some_vars_ambient_switch$day,some_vars_ambient_switch$month),FUN=mean)
  mean_ambient_vars_switch <- mean_ambient_switch %>% mutate(average_hr_ambient = x,hour=Group.1) %>% select(hour,average_hr_ambient) 
  
  mean_switch <-  aggregate(some_vars_ambient_switch$value,list(some_vars_ambient_switch$hour,some_vars_ambient_switch$day,some_vars_ambient_switch$month),FUN=sum)
  mean_switch_vars <- mean_switch %>% mutate(sum_opennings = x,hour=Group.1) %>% select(hour,sum_opennings) 
  
  #Binding
  ambient_inside <- cbind(mean_inside_vars,mean_ambient_vars_i)
  ambient_power <- cbind(mean_ambient_vars_p,mean_power_vars)
  ambient_hpower <- cbind(mean_ambient_vars_hp,mean_hpower_vars)
  ambient_switch <- cbind(mean_ambient_vars_switch,mean_switch_vars)
  
  return(list(ambient_inside,ambient_power,ambient_hpower,ambient_switch))
  
}







################## 2) Clean data Merging functions for Individal Houses


# NOTE1: Includes the subset for clean data
# NOTE2: Includes the integral to calculate energy not the sum
merge.bind.clean <- function(ambient,refrigerator,inside,house,switch) {
  
  ### Keeping only unique dates
  ambient <- ambient[!duplicated(ambient$time_stamp),]
  inside <- inside[!duplicated(inside$time_stamp),]
  refrigerator <- refrigerator[!duplicated(refrigerator$time_stamp),]
  switch <- switch[!duplicated(switch$time_stamp),]
  house <- house[!duplicated(house$time_stamp),]
  
  #### Merging ambient and data together
  small_df_ambient_power <- merge(ambient,refrigerator,by='datetime_rgx')
  small_df_ambient_inside <- merge(ambient, inside, by='datetime_rgx')
  small_df_ambient_housepower <- merge(ambient,house,by='datetime_rgx')
  small_df_ambient_switch <- merge(ambient,switch,by='datetime_rgx')
  
  
  #### Keeping some vars
  vars <- c('time_stamp.y','active_pwr3','ambient_temp')
  some_vars_ambient_power <- small_df_ambient_power[vars]
  vars <- c('time_stamp.y','fridge_temp1','ambient_temp')
  some_vars_ambient_inside <- small_df_ambient_inside[vars]
  vars <- c('time_stamp.y','house_Power','ambient_temp')
  some_vars_ambient_housepower <- small_df_ambient_housepower[vars]
  vars <- c('time_stamp.y','value','ambient_temp')
  some_vars_ambient_switch <- small_df_ambient_switch[vars]
  
  
  ###### Adding date variables
  
  some_vars_ambient_power <- date.vars(some_vars_ambient_power)
  some_vars_ambient_inside <- date.vars(some_vars_ambient_inside)
  some_vars_ambient_housepower <- date.vars(some_vars_ambient_housepower)
  some_vars_ambient_switch <- date.vars(some_vars_ambient_switch)
  
  
  ###### Subset for clean data
  some_vars_ambient_housepower <- subset(some_vars_ambient_housepower, some_vars_ambient_housepower$house_Power < quantile(some_vars_ambient_housepower$house_Power,.99,na.rm=TRUE) )
  
  
  ###### Grouping before analysis
  
  #Keep vars
  vars <- c('x')
  
  #Ambient and Inside Temperature
  mean_ambient_i <- aggregate(some_vars_ambient_inside$ambient_temp,list(some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
  mean_ambient_vars_i <- mean_ambient_i %>% mutate(average_hr_ambient = x,hour=Group.1) %>% select(hour,average_hr_ambient) 
  
  mean_inside <-  aggregate(some_vars_ambient_inside$fridge_temp1,list(some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
  mean_inside_vars <- mean_inside %>% mutate(average_hr_inside = x,hour=Group.1) %>% select(hour,average_hr_inside) 
  
  #Ambient and Fridge Power
  mean_ambient_p <- aggregate(some_vars_ambient_power$ambient_temp,list(some_vars_ambient_power$hour,some_vars_ambient_power$day,some_vars_ambient_power$month),FUN=mean)
  mean_ambient_vars_p <- mean_ambient_p %>% mutate(average_hr_ambient = x,hour=Group.1) %>% select(hour,average_hr_ambient) 
  
  mean_power <-  aggregate(some_vars_ambient_power$active_pwr3,list(some_vars_ambient_power$hour,some_vars_ambient_power$day,some_vars_ambient_power$month),FUN=mean)
  mean_power_vars <- mean_power %>% mutate(average_hr_power = x,hour=Group.1) %>% select(hour,average_hr_power) 
  
  #Ambient and House Power 
  mean_ambient_hp <- aggregate(some_vars_ambient_housepower$ambient_temp,list(some_vars_ambient_housepower$hour,some_vars_ambient_housepower$day,some_vars_ambient_housepower$month),FUN=mean)
  mean_ambient_vars_hp <- mean_ambient_hp %>% mutate(average_hr_ambient = x,hour=Group.1) %>% select(hour,average_hr_ambient) 
  
  mean_hpower <-  aggregate(some_vars_ambient_housepower$house_Power,list(some_vars_ambient_housepower$hour,some_vars_ambient_housepower$day,some_vars_ambient_housepower$month),FUN=mean)
  mean_hpower_vars <- mean_hpower %>% mutate(average_hr_hpower = x,hour=Group.1) %>% select(hour,average_hr_hpower) 
  
  #Ambient and House Power   some_vars_ambient_switch
  mean_ambient_switch <- aggregate(some_vars_ambient_switch$ambient_temp,list(some_vars_ambient_switch$hour,some_vars_ambient_switch$day,some_vars_ambient_switch$month),FUN=mean)
  mean_ambient_vars_switch <- mean_ambient_switch %>% mutate(average_hr_ambient = x,hour=Group.1) %>% select(hour,average_hr_ambient) 
  
  mean_switch <-  aggregate(some_vars_ambient_switch$value,list(some_vars_ambient_switch$hour,some_vars_ambient_switch$day,some_vars_ambient_switch$month),FUN=sum)
  mean_switch_vars <- mean_switch %>% mutate(sum_opennings = x,hour=Group.1) %>% select(hour,sum_opennings) 
  
  #Binding
  ambient_inside <- cbind(mean_inside_vars,mean_ambient_vars_i)
  ambient_power <- cbind(mean_ambient_vars_p,mean_power_vars)
  ambient_hpower <- cbind(mean_ambient_vars_hp,mean_hpower_vars)
  ambient_switch <- cbind(mean_ambient_vars_switch,mean_switch_vars)
  
  return(list(ambient_inside,ambient_power,ambient_hpower,ambient_switch))
  
}











################## 3) Non-clean data Merging functions for ALL HOUSES

# NOTE: Does not include the subset for cleaning data 
merge.bind.all <- function(ambient,refrigerator,inside,house,switch) {
  
  #Merging ambient and data together
  small_df_ambient_power <- merge(ambient,refrigerator,by=c('house.id','datetime_rgx')) %>% select(house.id,date.y,time_stamp.y,ambient_temp,active_pwr3)
  small_df_ambient_inside <- merge(ambient, inside, by=c('house.id','datetime_rgx')) %>% select(house.id,date.y,time_stamp.y,ambient_temp,fridge_temp1,fridge_temp2)
  small_df_ambient_housepower <- merge(ambient,house,by=c('house.id','datetime_rgx')) %>% select(house.id,date.y,time_stamp.y,ambient_temp,house_Power,house_Energy)
  small_df_ambient_switch <- merge(ambient,switch,by=c('house.id','datetime_rgx')) %>% select(house.id,date.y,time_stamp.y,ambient_temp,open)
  
  #Unique time_stamps
  small_df_ambient_power <- unique(small_df_ambient_power[c("house.id", "date.y","time_stamp.y","ambient_temp","active_pwr3")])
  small_df_ambient_inside <- unique(small_df_ambient_inside[c("house.id", "date.y","time_stamp.y","ambient_temp","fridge_temp1","fridge_temp2")])
  small_df_ambient_housepower <- unique(small_df_ambient_housepower[c("house.id", "date.y","time_stamp.y","ambient_temp","house_Power","house_Energy")])
  small_df_ambient_switch <- unique(small_df_ambient_switch[c("house.id", "date.y","time_stamp.y","ambient_temp","open")])
  
  #Adding date variables
  some_vars_ambient_power <- date.vars(small_df_ambient_power)
  some_vars_ambient_inside <- date.vars(small_df_ambient_inside)
  some_vars_ambient_housepower <- date.vars(small_df_ambient_housepower)
  some_vars_ambient_switch <- date.vars(small_df_ambient_switch)
  
  #Ambient and Inside Temperature 1 (NOTE: Need to add TEMP1)
  mean_ambient_i <- aggregate(some_vars_ambient_inside$ambient_temp,list(some_vars_ambient_inside$house.id,some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
  mean_ambient_vars_i <- mean_ambient_i %>% mutate(house.id = Group.1, average_hr_ambient = x, hour=Group.2) %>% select(house.id,hour,average_hr_ambient) 
  
  mean_inside <-  aggregate(some_vars_ambient_inside$fridge_temp1,list(some_vars_ambient_inside$house.id,some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
  mean_inside_vars <- mean_inside %>% mutate(house.id=Group.1,average_hr_inside = x,hour=Group.2) %>% select(house.id,hour,average_hr_inside) 
  
  #Ambient and Fridge Power
  mean_ambient_p <- aggregate(some_vars_ambient_power$ambient_temp,list(some_vars_ambient_power$house.id,some_vars_ambient_power$hour,some_vars_ambient_power$day,some_vars_ambient_power$month),FUN=mean)
  mean_ambient_vars_p <- mean_ambient_p %>% mutate(house.id=Group.1,average_hr_ambient = x,hour=Group.2) %>% select(house.id,hour,average_hr_ambient) 
  
  mean_power <-  aggregate(some_vars_ambient_power$active_pwr3,list(some_vars_ambient_power$house.id,some_vars_ambient_power$hour,some_vars_ambient_power$day,some_vars_ambient_power$month),FUN=mean)
  mean_power_vars <- mean_power %>% mutate(house.id=Group.1,average_hr_power = x,hour=Group.2) %>% select(house.id,hour,average_hr_power) 
  
  #Ambient and House Power 
  
  mean_ambient_hp <- aggregate(some_vars_ambient_housepower$ambient_temp,list(some_vars_ambient_housepower$house.id,some_vars_ambient_housepower$hour,some_vars_ambient_housepower$day,some_vars_ambient_housepower$month),FUN=mean)
  mean_ambient_vars_hp <- mean_ambient_hp %>% mutate(house.id=Group.1,average_hr_ambient = x,hour=Group.2) %>% select(house.id,hour,average_hr_ambient) 
  
  mean_hpower <-  aggregate(some_vars_ambient_housepower$house_Power,list(some_vars_ambient_housepower$house.id,some_vars_ambient_housepower$hour,some_vars_ambient_housepower$day,some_vars_ambient_housepower$month),FUN=mean)
  mean_hpower_vars <- mean_hpower %>% mutate(house.id=Group.1,average_hr_hpower = x,hour=Group.2) %>% select(house.id,hour,average_hr_hpower) 
  
  #Ambient and Switch
  mean_ambient_switch <- aggregate(some_vars_ambient_switch$ambient_temp,list(some_vars_ambient_switch$house.id,some_vars_ambient_switch$hour,some_vars_ambient_switch$day,some_vars_ambient_switch$month),FUN=mean)
  mean_ambient_vars_switch <- mean_ambient_switch %>% mutate(house.id=Group.1,average_hr_ambient = x,hour=Group.2) %>% select(hour,average_hr_ambient) 
  
  some_vars_ambient_switch$value <- ifelse(some_vars_ambient_switch$open=='True',1,0)
  mean_switch <-  aggregate(some_vars_ambient_switch$value,list(some_vars_ambient_switch$house.id,some_vars_ambient_switch$hour,some_vars_ambient_switch$day,some_vars_ambient_switch$month),FUN=sum)
  mean_switch_vars <- mean_switch %>% mutate(house.id=Group.1,sum_openings = x,hour=Group.1) %>% select(house.id,hour,sum_openings) 
  
  #Binding
  ambient_inside <- cbind(mean_inside_vars,mean_ambient_vars_i)
  ambient_power <- cbind(mean_ambient_vars_p,mean_power_vars)
  ambient_hpower <- cbind(mean_ambient_vars_hp,mean_hpower_vars)
  ambient_switch <- cbind(mean_ambient_vars_switch,mean_switch_vars)
  
  return(list(ambient_inside,ambient_power,ambient_hpower,ambient_switch))
}





################## 4) Clean data Merging functions for ALL HOUSES

# NOTE: Does not include the subset for cleaning data 
merge.bind.all.clean <- function(ambient,refrigerator,inside,house,switch) {
  
  ambient <- subset(ambient,ambient$ambient_temp > 20)
  refrigerator <- subset(refrigerator,refrigerator$active_pwr3 < 650)
  inside <- subset(inside,inside$fridge_temp1 <40) #Need to subset inside2 at some point
  
  #Merging ambient and data together
  small_df_ambient_power <- merge(ambient,refrigerator,by=c('house.id','datetime_rgx')) %>% select(house.id,date.y,time_stamp.y,ambient_temp,active_pwr3,energy_sum3)
  small_df_ambient_inside <- merge(ambient, inside, by=c('house.id','datetime_rgx')) %>% select(house.id,date.y,time_stamp.y,ambient_temp,fridge_temp1,fridge_temp2)
  small_df_ambient_housepower <- merge(ambient,house,by=c('house.id','datetime_rgx')) %>% select(house.id,date.y,time_stamp.y,ambient_temp,house_Power,house_Energy)
  small_df_ambient_switch <- merge(ambient,switch,by=c('house.id','datetime_rgx')) %>% select(house.id,date.y,time_stamp.y,ambient_temp,open)
  
  #Unique time_stamps
  small_df_ambient_power <- unique(small_df_ambient_power[c("house.id", "date.y","time_stamp.y","ambient_temp","active_pwr3","energy_sum3")])
  small_df_ambient_inside <- unique(small_df_ambient_inside[c("house.id", "date.y","time_stamp.y","ambient_temp","fridge_temp1","fridge_temp2")])
  small_df_ambient_housepower <- unique(small_df_ambient_housepower[c("house.id", "date.y","time_stamp.y","ambient_temp","house_Power","house_Energy")])
  small_df_ambient_switch <- unique(small_df_ambient_switch[c("house.id", "date.y","time_stamp.y","ambient_temp","open")])
  
  #Adding date variables
  some_vars_ambient_power <- date.vars(small_df_ambient_power)
  some_vars_ambient_inside <- date.vars(small_df_ambient_inside)
  some_vars_ambient_housepower <- date.vars(small_df_ambient_housepower)
  some_vars_ambient_switch <- date.vars(small_df_ambient_switch)
  
  #Ambient and Inside Temperature 1 (NOTE: Need to add TEMP1)
  mean_ambient_i <- aggregate(some_vars_ambient_inside$ambient_temp,list(some_vars_ambient_inside$house.id,some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
  mean_ambient_vars_i <- mean_ambient_i %>% mutate(house.id = Group.1, average_hr_ambient = x, hour=Group.2) %>% select(house.id,hour,average_hr_ambient) 
  
  mean_inside <-  aggregate(some_vars_ambient_inside$fridge_temp1,list(some_vars_ambient_inside$house.id,some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
  mean_inside_vars <- mean_inside %>% mutate(house.id=Group.1,average_hr_inside = x,hour=Group.2) %>% select(house.id,hour,average_hr_inside) 
  
  #Ambient and Fridge Power
  fridge.hour.energy <- fenergy.consumption.hour(some_vars_ambient_power)
  
  mean_ambient_p <- aggregate(some_vars_ambient_power$ambient_temp,list(some_vars_ambient_power$house.id,some_vars_ambient_power$hour,some_vars_ambient_power$day,some_vars_ambient_power$month),FUN=mean)
  mean_ambient_vars_p <- mean_ambient_p %>% mutate(house.id=Group.1,average_hr_ambient = x,hour=Group.2) %>% select(house.id,hour,average_hr_ambient) 
  
  # *** Previously used code before it was changed for the lines above ***
  #mean_power <-  aggregate(some_vars_ambient_power$active_pwr3,list(some_vars_ambient_power$house.id,some_vars_ambient_power$hour,some_vars_ambient_power$day,some_vars_ambient_power$month),FUN=mean)
  #mean_power_vars <- mean_power %>% mutate(house.id=Group.1,average_hr_power = x,hour=Group.2) %>% select(house.id,hour,average_hr_power) 
  # ***
  
  #Ambient and House Power: The mean or median doesn't need to be taken here 
  # Note that the dataframe below returns a list so we take only the first item in the list.
  hour.energy.list <- henergy.consumption.hour(some_vars_ambient_housepower)
  hour.energy <- hour.energy.list[[1]]
  norm.energy <- hour.energy.list[[2]]
  
  # The two lines below could be used to calcualte mean or median
  #mean_hpower_vars <- aggregate(mean_hpower$energy,list(energy.hourly.consumption$house.id,energy.hourly.consumption$hour),FUN=mean)  %>% mutate(house.id=Group.1,hour=Group.2,average_hr_hpower = x) %>% select(house.id,hour,average_hr_hpower) 
  #median_hpower_vars <- aggregate(energy.hourly.consumption$energy,list(energy.hourly.consumption$house.id,energy.hourly.consumption$hour),FUN=median) %>% mutate(house.id=Group.1,hour=Group.2,median_hr_hpower = x) %>% select(house.id,hour,median_hr_hpower) 
  
  mean_ambient_hp <- aggregate(some_vars_ambient_housepower$ambient_temp,list(some_vars_ambient_housepower$house.id,some_vars_ambient_housepower$hour,some_vars_ambient_housepower$day,some_vars_ambient_housepower$month),FUN=mean)
  mean_ambient_vars_hp <- mean_ambient_hp %>% mutate(house.id=Group.1,average_hr_ambient = x,hour=Group.2) %>% select(house.id,hour,average_hr_ambient) 
  
  # *** Previously used code before it was changed for the lines above ***
  #mean_hpower <-  aggregate(some_vars_ambient_housepower$house_Power,list(some_vars_ambient_housepower$house.id,some_vars_ambient_housepower$hour,some_vars_ambient_housepower$day,some_vars_ambient_housepower$month),FUN=mean)
  #mean_hpower_vars <- mean_hpower %>% mutate(house.id=Group.1,average_hr_hpower = x,hour=Group.2) %>% select(house.id,hour,average_hr_hpower) 
  # ***
  
  #Ambient and Switch
  mean_ambient_switch <- aggregate(some_vars_ambient_switch$ambient_temp,list(some_vars_ambient_switch$house.id,some_vars_ambient_switch$hour,some_vars_ambient_switch$day,some_vars_ambient_switch$month),FUN=mean)
  mean_ambient_vars_switch <- mean_ambient_switch %>% mutate(house.id=Group.1,average_hr_ambient = x,hour=Group.2) %>% select(hour,average_hr_ambient) 
  
  some_vars_ambient_switch$value <- ifelse(some_vars_ambient_switch$open=='True',1,0)
  mean_switch <-  aggregate(some_vars_ambient_switch$value,list(some_vars_ambient_switch$house.id,some_vars_ambient_switch$hour,some_vars_ambient_switch$day,some_vars_ambient_switch$month),FUN=sum)
  mean_switch_vars <- mean_switch %>% mutate(house.id=Group.1,sum_openings = x,hour=Group.1) %>% select(house.id,hour,sum_openings) 
  
  #Binding
  ambient_inside <- cbind(mean_inside_vars,mean_ambient_vars_i)
  ambient_power <- join(fridge.hour.energy,mean_ambient_vars_p,by=c("house.id","hour"),type="left",match = "first") %>% select(house.id,hour,fridge.energy,average_hr_ambient)
  #ambient_power <- cbind(mean_ambient_vars_p,mean_power_vars) With the previous code we did a bind, now we are doing a join because it required a different step
  ambient_hpower <- join(hour.energy,mean_ambient_vars_hp, by = c("house.id","hour"), type = "left", match = "first") %>% select(house.id,hour,energy,average_hr_ambient)
  #ambient_hpower <- cbind(mean_ambient_vars_hp,mean_hpower_vars) With the previous code we did a bind, now we are doing a join because it required a different step
  ambient_switch <- cbind(mean_ambient_vars_switch,mean_switch_vars)
  
  return(list(ambient_inside,ambient_power,ambient_hpower,ambient_switch,norm.energy))
}




