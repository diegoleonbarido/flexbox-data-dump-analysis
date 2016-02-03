############ Script to Analyze data from A1
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(xtable)
library(grid)
library(Hmisc)

#Function libraries
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/date_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/merging_binding.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/read_data.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/time_series_plots.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/hourly_plots.R')



###### NOTE
# Section 1: Reading data
# Section 2: Non clean data plots
# Section 3: Clean data plots
# Section 4: Daily Plots



###############  Section 1: Reading data
####             Reading data - specify the number of the dump and the flexbox ID
data_list <- read.data('DUMP1','A1') 

inside <- data_list[[1]]
ambient <- data_list[[2]]
refrigerator <- data_list[[3]]
switch <- data_list[[4]]
house <- data_list[[5]]


############  Converting dates and using the date function
####          Converts dates
dates_data_frame <- date.data.frame(ambient,inside,refrigerator,switch,house) 

#Extract the data frames from the list
ambient <- dates_data_frame[[1]]
inside <- dates_data_frame[[2]]
switch <- dates_data_frame[[3]]
house <- dates_data_frame[[4]]
refrigerator <- dates_data_frame[[5]]

############  Printing warnings on the number of days we have logged so far
#print(date.check(refrigerator,inside,house,ambient))

table.table <- xtable(date.check(refrigerator,inside,house,ambient))
print(table.table)


############ Small changes to data

# Dividing my 1000 to have reasonable values
inside$temp1 <- inside$fridge_temp1/1000
inside$temp2 <- inside$fridge_temp2/1000

# Opening and closing for the SWITCH
switch$value <- ifelse(switch$open=='True',1,0)


############################### Section 2: NON CLEAN DATA PLOTS 
# NOTE: NON Clean data the time series and correlation plots below depict data with gross outliers and poor measurements

###### Time series plots of the last seven days

time.series.data <- time.series.sevendays(ambient,inside,refrigerator,switch,house)

ambient.plot <- ggplot(time.series.data[[1]],aes(x=time_stamp, y=ambient_temp)) + geom_point(size=1) + ggtitle(expression(paste("Ambient Temperature",degree,"C"))) + xlab('Date') + ylab(expression(paste(degree,"C"))) + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))
inside.temp.plot <- ggplot() + geom_point(data=time.series.data[[2]],aes(x=time_stamp, y=temp1),color='black',size=1) + geom_point(data=time.series.data[[2]],aes(x=time_stamp, y=temp2),color='red',size=1) +  ggtitle(expression(paste("Inside Temperature",degree,"C")))  + xlab('Date') + ylab(expression(paste(degree,"C"))) + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))   
refrigerator.plot <- ggplot() + geom_line(data=time.series.data[[3]],aes(x=time_stamp, y=active_pwr3),color='black',size=1) + ggtitle(expression(paste("Refrigerator (W)",degree,"C"))) + xlab('Date') + ylab(expression(paste("W"))) + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))
switch.plot <- ggplot() + geom_point(data=time.series.data[[4]],aes(x=time_stamp, y=open),color='black',size=1) + ggtitle(expression(paste("Door Openings"))) +  ylab(expression(paste("N.Openings"))) + xlab('Date') + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))
house.plot <- ggplot() + geom_line(data=time.series.data[[5]],aes(x=time_stamp, y=house_Power),color='black',size=1) + ggtitle(expression(paste("House (W)"))) + xlab('Date') + ylab("W") + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))

grid.arrange(ambient.plot,inside.temp.plot,refrigerator.plot,switch.plot,house.plot,ncol=1,nrow=5)

###### Merging and binding data before plotting
# Note: Returns data frames ready to be plotted. All values are combined with ambient temperature. For example, ambient temperature and power
# ambient temperature and fridge energy, household energy, etc. 

merged_data_list <- merge.bind(ambient,refrigerator,inside,house,switch) 

##### Correlation Plots 

plot1 <- ggplot(merged_data_list[[1]],aes(x=average_hr_ambient, y=average_hr_inside,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Average Inside Fridge Temperature") + ylab(expression(paste("Inside Temperature",degree,"C"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1))
plot2 <- ggplot(merged_data_list[[2]],aes(x=average_hr_ambient, y=average_hr_power,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Fridge Energy Consumption") + ylab(expression(paste("Energy (Wh)"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1))
plot3 <- ggplot(merged_data_list[[3]],aes(x=average_hr_ambient, y=average_hr_hpower,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Household Energy Consumption") + ylab(expression(paste("Energy (Wh)"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1))
plot4 <- ggplot(merged_data_list[[4]],aes(x=average_hr_ambient, y=sum_opennings,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Door Openings ") + ylab(expression(paste("No. Door Openings"))) + xlab(expression(paste("Ambient Temperature",degree,"C")))

grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2)










############################### Section 3. !! CLEAN DATA PLOTS !!
# NOTE1: Clean data plots after removing outliers
# NOTE2: Uses area under curve and not sum to calculate values

###### Time series plots of the last seven days


time.series.data.clean <- time.series.sevendays.clean(ambient,inside,refrigerator,switch,house)

ambient.plot.clean <- ggplot(time.series.data.clean[[1]],aes(x=time_stamp, y=ambient_temp)) + geom_point(size=1) + ggtitle(expression(paste("Ambient Temperature",degree,"C"))) + xlab('Date') + ylab(expression(paste(degree,"C"))) + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))
inside.temp.plot.clean <- ggplot() + geom_point(data=time.series.data.clean[[2]],aes(x=time_stamp, y=temp1),color='black',size=1) + geom_point(data=time.series.data[[2]],aes(x=time_stamp, y=temp2),color='red',size=1) +  ggtitle(expression(paste("Inside Temperature",degree,"C")))  + xlab('Date') + ylab(expression(paste(degree,"C"))) + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))   
refrigerator.plot.clean <- ggplot() + geom_line(data=time.series.data.clean[[3]],aes(x=time_stamp, y=active_pwr3),color='black',size=1) + ggtitle(expression(paste("Refrigerator (W)",degree,"C"))) + xlab('Date') + ylab(expression(paste("W"))) + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))
switch.plot.clean <- ggplot() + geom_point(data=time.series.data.clean[[4]],aes(x=time_stamp, y=open),color='black',size=1) + ggtitle(expression(paste("Door Openings"))) +  ylab(expression(paste("N.Openings"))) + xlab('Date') + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))
house.plot.clean <- ggplot() + geom_line(data=time.series.data.clean[[5]],aes(x=time_stamp, y=house_Power),color='black',size=1) + ggtitle(expression(paste("House (W)"))) + xlab('Date') + ylab("W") + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))

grid.arrange(ambient.plot.clean,inside.temp.plot.clean,refrigerator.plot.clean,switch.plot.clean,house.plot.clean,ncol=1,nrow=5)

###### Merging and binding data before plotting
# Note: Returns data frames ready to be plotted. All values are combined with ambient temperature. For example, ambient temperature and power
# ambient temperature and fridge energy, household energy, etc. 

merged.data.list.clean <- merge.bind.clean(ambient,refrigerator,inside,house,switch) 

##### Correlation Plots 

plot1 <- ggplot(merged.data.list.clean[[1]],aes(x=average_hr_ambient, y=average_hr_inside,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Average Inside Fridge Temperature") + ylab(expression(paste("Inside Temperature",degree,"C"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1))
plot2 <- ggplot(merged.data.list.clean[[2]],aes(x=average_hr_ambient, y=average_hr_power,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Fridge Energy Consumption") + ylab(expression(paste("Energy (Wh)"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1))
plot3 <- ggplot(merged.data.list.clean[[3]],aes(x=average_hr_ambient, y=average_hr_hpower,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Household Energy Consumption") + ylab(expression(paste("Energy (Wh)"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1))
plot4 <- ggplot(merged.data.list.clean[[4]],aes(x=average_hr_ambient, y=sum_opennings,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Door Openings ") + ylab(expression(paste("No. Door Openings"))) + xlab(expression(paste("Ambient Temperature",degree,"C")))

grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2)






##################  4. Daily Plots

#Quick data clean based on the 99th percentile and zero values

house_energy_clean <- clean.data.percentile(house) #gets rid of 99th percentile
house_energy_clean_zero <- clean.data.percentile.zero(house) #gets rid of 99th percentile and zero values

#Creating a data vector with all possible points in the vector
time.vector.seconds <- time.vector.seconds(house_energy_clean_zero)

#Merging data before proceeding with the analysis
house_energy_clean_zero$time_stamp <- as.POSIXct(house_energy_clean_zero$time_stamp)
data.seconds <- merge(time.vector.seconds,house_energy_clean_zero,by=('time_stamp'),all=TRUE) %>% select(time_stamp,id,house_Power,house_Energy) 

###### Second by Second Analysis

#Sensor Analysis: Missing Data
  #How many zeros did we read from the data that we actually read?
    num.zeros <- subset(house, house$house_Power == 0) 
    (length(num.zeros$house_Power)/(length(house$house_Power)))*100 #Percentage of zeros that we read, when we read data
  #How many missing values do we have after merging with the data vector?
    data.seconds$house_Power[is.na(data.seconds$house_Power)] <- 0
    num.zeros.complete <- subset(data.seconds, data.seconds$house_Power == 0) 
    (length(num.zeros.complete$house_Power)/(length(data.seconds$house_Power))) #Percentage of zeros that we read, when we read data
    
  

#Overlayed second plots
second.analysis <- second.plotting(data.seconds) 
#Adding Zeros so we can plot the data
second.analysis$house_Power[is.na(second.analysis$house_Power)] <- 0
ggplot(data=second.analysis,aes(id,house_Power)) + geom_point(alpha=0.05,colour='blue') + labs(title="Daily Energy Consumption") + ylab("Power (KW)") + xlab("Minute") 

#Second-By-Second Plots Below 1
second.analysis.one <- subset(second.analysis,second.analysis$house_Power<1)
ggplot(data=second.analysis.one,aes(id,house_Power)) + geom_point(alpha=0.05,colour='blue') + labs(title="Daily Energy Consumption") + ylab("Power (KW)") + xlab("Minute") 




#Hourly Plots
hour.analysis <- hourly.plotting(data.seconds)
ggplot(data=hour.analysis,aes(hour,house_Power)) + geom_point(alpha=0.01,colour='blue') + labs(title="Daily Energy Consumption") + ylab("Power (KW)") + xlab("Hour") 

    #Hourly Plots Below 1
    hour.analysis.one <- subset(hour.analysis,data$house_Power<1)
    ggplot(data=hour.analysis.one,aes(hour,house_Power)) + geom_point(alpha=0.01,colour='blue') + labs(title="Daily Energy Consumption") + ylab("Power (KW)") + xlab("Hour") 






###############
#Just one day
one_day <- subset(house_energy_clean_zero, house_energy_clean_zero$date == '2015-08-02' & house_energy_clean_zero$house_Power < 1000)


plot(1:length(one_day$house_Power),one_day$house_Power)
trapz(as.numeric(just_vals$id),as.numeric(just_vals$house_Power))

plot(1:length(just_vals$house_Power),just_vals$house_Power)

sum(just_vals$house_Power,na.rm=TRUE)

caca <- subset(one_day,one_day$house_Power != 0)
plot(1:length(caca$house_Power),caca$house_Power)

caca$second_lag <- lag(as.integer(caca$second), -1, na.pad = TRUE)
################






