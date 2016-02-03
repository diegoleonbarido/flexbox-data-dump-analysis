################## Merging functions

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
vars <- c('time_stamp.y','temp1','ambient_temp')
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

###### Grouping before analysis

#Keep vars
vars <- c('x')

#Ambient and Inside Temperature
mean_ambient_i <- aggregate(some_vars_ambient_inside$ambient_temp,list(some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
mean_ambient_vars_i <- mean_ambient_i %>% mutate(average_hr_ambient = x,hour=Group.1) %>% select(hour,average_hr_ambient) 

mean_inside <-  aggregate(some_vars_ambient_inside$temp1,list(some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
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
  vars <- c('time_stamp.y','temp1','ambient_temp')
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
  
  mean_inside <-  aggregate(some_vars_ambient_inside$temp1,list(some_vars_ambient_inside$hour,some_vars_ambient_inside$day,some_vars_ambient_inside$month),FUN=mean)
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


