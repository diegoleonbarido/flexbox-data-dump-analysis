########### Purpose: This code puts together a variety of functions from the analysis. 


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
library(e1071)



#Function libraries
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/date_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/merging_binding_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/read_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/time_series_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/hourly_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_consumption_functions.R')



###### NOTE
# Section 1: Reading data
# Section 2: Non clean data plots
# Section 3: Clean data plots
# Section 4: Daily Plots
# Section 5: BoxPLot Distributions
# Section 6: Normalization and Clustering (including week of the day analysis)       




###############  Section 1: Reading data
#### Reading Data: 1) Provide a list of houses, and 2) Specify the number of the dump
### NOTE: A27,A28,A29 causes problems

flexlist <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15','A16','A17','A18','A19','A20','A21','A22','A23','A24','A25')
data.list.houses <- read.data.all('DUMP1',flexlist) 

inside <- data.list.houses[[1]]
ambient <- data.list.houses[[2]]
refrigerator <- data.list.houses[[3]]
switch <- data.list.houses[[4]]
house <- data.list.houses[[5]]


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
#Note this can create a report by house -- so one must specify houseid to get different tables for different houses
#print(date.check(refrigerator,inside,house,ambient))

table.table <- xtable(date.check(refrigerator,inside,house,ambient))
print(table.table)


############ Small changes to data

# Opening and closing for the SWITCH
switch$value <- ifelse(switch$open=='True',1,0)





############################### Section 2: NON CLEAN DATA PLOTS 
# NOTE1: NON Clean data the time series and correlation plots below depict data with gross outliers and poor measurements
# NOTE2: One must specify house plots if you want to visualize specific house plots


###### Correlation Plots: Merging and binding data before plotting
# Note: Returns data frames ready to be plotted. All values are combined with ambient temperature. For example, ambient temperature and power
# ambient temperature and fridge energy, household energy, etc. 

merged.all.list <- merge.bind.all(ambient,refrigerator,inside,house,switch) 

##### Correlation Plots 

plot1 <- ggplot(merged.all.list[[1]],aes(x=average_hr_ambient, y=average_hr_inside,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Average Inside Fridge Temperature") + ylab(expression(paste("Inside Temperature",degree,"C"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1))
plot2 <- ggplot(merged.all.list[[2]],aes(x=average_hr_ambient, y=average_hr_power,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Fridge Energy Consumption") + ylab(expression(paste("Energy (Wh)"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1))
plot3 <- ggplot(merged.all.list[[3]],aes(x=average_hr_ambient, y=average_hr_hpower,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Household Energy Consumption") + ylab(expression(paste("Energy (Wh)"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1))
plot4 <- ggplot(merged.all.list[[4]],aes(x=average_hr_ambient, y=sum_openings,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Door Openings ") + ylab(expression(paste("No. Door Openings"))) + xlab(expression(paste("Ambient Temperature",degree,"C")))

grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2)





############################### Section 3. !! CLEAN DATA PLOTS !!
# NOTE1: Clean data plots after removing outliers
# NOTE2: Uses area under curve and not sum to calculate values



merged.all.data.list.clean <- merge.bind.all.clean (ambient,refrigerator,inside,house,switch) 

##### Correlation Plots 

all.plot1 <- ggplot(merged.all.data.list.clean[[1]],aes(x=average_hr_ambient, y=average_hr_inside,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Average Inside Fridge Temperature") + ylab(expression(paste("Inside Temperature",degree,"C"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1)) + theme(panel.background = element_blank())
all.plot2 <- ggplot(merged.all.data.list.clean[[2]],aes(x=average_hr_ambient, y=fridge.energy,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Fridge Energy Consumption") + ylab(expression(paste("Energy (Wh)"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1)) + theme(panel.background = element_blank())
all.plot3 <- ggplot(merged.all.data.list.clean[[3]],aes(x=average_hr_ambient, y=energy,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Household Energy Consumption") + ylab(expression(paste("Energy (Wh)"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1)) + theme(panel.background = element_blank())
all.plot4 <- ggplot(merged.all.data.list.clean[[4]],aes(x=average_hr_ambient, y=sum_openings,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Door Openings ") + ylab(expression(paste("No. Door Openings"))) + xlab(expression(paste("Ambient Temperature",degree,"C"))) + theme(panel.background = element_blank())

grid.arrange(all.plot1,all.plot2,all.plot3,all.plot4,ncol=2,nrow=2)


##### Descriptive stats for each of the variables thare are plotted

inside.temp <- merged.all.data.list.clean[[1]]
inside.ener <- merged.all.data.list.clean[[2]]
house.ener <-  merged.all.data.list.clean[[3]]
doors <- merged.all.data.list.clean[[4]]

#Cluster Correlation Statistics
cor.test(inside.temp$average_hr_inside,inside.temp$average_hr_ambient,method="pearson") #Inside temp and Ambient temp
cor.test(inside.ener$average_hr_ambient,inside.ener$fridge.energy) #Inside temp and Ambient temp
cor.test(house.ener$average_hr_ambient,house.ener$energy,method='spearman') #Inside temp and Ambient temp
cor.test(doors$average_hr_ambient,doors$sum_openings,method='spearman') #Inside temp and Ambient temp

#Micro-enterprise level Correlation Statistics
micro.ids.list <- c()
corr.list <- c()
var.list <- c()
count <- 0

for (i in 1:4) {
  data.corr <- merged.all.data.list.clean[[i]]
  unique.microlist <- unique(data.corr$house.id)
  iter.vars <- c('average_hr_inside','fridge.energy','energy','sum_openings')
  
  for (j in 1:length(unique.microlist)) {
    count <- count +1
    micro.corr <- subset(data.corr,data.corr$house.id == unique.microlist[j])
    
    iter.var <- micro.corr[iter.vars[i]]
    names(iter.var)[1] <- 'itervar'
    
    corr.list[count] <- cor.test(micro.corr$average_hr_ambient,iter.var$itervar,method="spearman")$estimate
    var.list[count] <- iter.vars[i]
    micro.ids.list[count] <- unique.microlist[j]
  }
}

corr.microids <- as.data.frame(cbind(corr.list,var.list,micro.ids.list))

#Preparing the box plot
corr.microids$var.list <- ifelse(corr.microids$var.list=='average_hr_inside','Inside Temperature',ifelse(corr.microids$var.list=='energy','Total Energy',ifelse(corr.microids$var.list=='fridge.energy','Fridge Energy','Door Openings')))
corr.microids$size.var <- ifelse(abs(as.numeric(as.character(corr.microids$corr.list))) > 0 & abs(as.numeric(as.character(corr.microids$corr.list))) <= 0.19,5,ifelse(abs(as.numeric(as.character(corr.microids$corr.list))) >.19 & abs(as.numeric(as.character(corr.microids$corr.list))) <= 0.39,10,ifelse(abs(as.numeric(as.character(corr.microids$corr.list))) >0.39 & abs(as.numeric(as.character(corr.microids$corr.list))) <= 0.59,15,ifelse(abs(as.numeric(as.character(corr.microids$corr.list))) >0.59 & abs(as.numeric(as.character(corr.microids$corr.list))) <= 0.79,30,35))))
corr.microids$strength.corr <- ifelse(abs(as.numeric(as.character(corr.microids$corr.list))) > 0 & abs(as.numeric(as.character(corr.microids$corr.list))) <= 0.19,'very weak',ifelse(abs(as.numeric(as.character(corr.microids$corr.list))) >.19 & abs(as.numeric(as.character(corr.microids$corr.list))) <= 0.39,'weak',ifelse(abs(as.numeric(as.character(corr.microids$corr.list))) >0.39 & abs(as.numeric(as.character(corr.microids$corr.list))) <= 0.59,'moderate',ifelse(abs(as.numeric(as.character(corr.microids$corr.list))) >0.59 & abs(as.numeric(as.character(corr.microids$corr.list))) <= 0.79,'strong','very strong'))))
corr.microids$unit <- 1


ggplot(corr.microids,aes(y=as.numeric(as.character(corr.list)),x=var.list)) + 
  geom_point(aes(fill=micro.ids.list,size=size.var),pch=21,show_guide = FALSE) +
  geom_boxplot(fill = NA, size=0.5) + 
  xlab('Variable') + ylab('Correlation') +
  ylim(-1,1) + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))

#Counting the strength of correlation
sum.strength.corr <- subset(corr.microids,corr.microids$var.list=='Fridge Energy') 
sum.strength.corr <- aggregate(sum.strength.corr$unit,by=list(sum.strength.corr$strength.corr),FUN=sum)









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
second.analysis.one <- subset(second.analysis,second.analysis$house_Power<2)
ggplot(data=second.analysis.one,aes(id,house_Power)) + geom_point(alpha=0.05,colour='blue') + labs(title="Daily Energy Consumption") + ylab("Power (KW)") + xlab("Minute") 



#NOTE: NEED DO FIX THIS FUNCTION
#Hourly Plots #NOTE: Need to fix this function before using
hour.analysis <- hourly.plotting(data.seconds)
ggplot(data=hour.analysis,aes(hour,house_Power)) + geom_point(alpha=0.01,colour='blue') + labs(title="Daily Energy Consumption") + ylab("Power (KW)") + xlab("Hour") 

#Hourly Plots Below 1
hour.analysis.one <- subset(hour.analysis,data$house_Power<2)
ggplot(data=hour.analysis.one,aes(hour,house_Power)) + geom_point(alpha=0.01,colour='blue') + labs(title="Daily Energy Consumption") + ylab("Power (KW)") + xlab("Hour") 


#NOTE: THIS ANALYSIS NEEDS TO BE EXPANDED
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







########### Section 5: BoxPLot Distributions

#Note that these distributions should be done for micro-Enterprises and for houses

#Distribution of Measured Parameters



#Boxplot Ambient Temperature without the botoom Removing 1st Percentile
ambient_sub <- subset(ambient,ambient$ambient_temp > quantile(ambient$ambient_temp,c(0.01)))
bamb <- ggplot(ambient_sub, aes(house.id,ambient_sub$ambient_temp )) + geom_boxplot(outlier.colour = "red", outlier.size = 1) + ylab(expression(paste("Ambient Temperature",degree,"C")))

#Boxplot Inside Temp
inside_sub <- subset(inside,inside$fridge_temp1 > quantile(inside$fridge_temp1,c(0.01),na.rm=TRUE))
binside <- ggplot(inside_sub, aes(house.id,inside_sub$fridge_temp1 ))+ geom_boxplot(outlier.colour = "red", outlier.size = 1) + ylab(expression(paste("Inside Temperature",degree,"C")))

#Boxplot for Fridge Power
bfridge <- ggplot(refrigerator, aes(house.id,refrigerator$active_pwr3 ))+ geom_boxplot(outlier.colour = "red", outlier.size = 2) + ylab(expression('Power'))
                                                                                                                                        
#Boxplot for House Power
house_sub <- subset(house, house$house_Power<1000)
bhouse <- ggplot(house_sub, aes(house.id,house_sub$house_Power ))+ geom_boxplot(outlier.colour = "red", outlier.size = 1) + ylab('Power')
                                                                                                                                        
#Boxplot for 
grid.arrange(bamb,binside,bfridge,bhouse,nrow=2,ncol=2)
                                                                                                                                        
                

    



                                                                                                                    
###################################################
###################################################
###################################################

########### Section 6: Normalization and Clustering

merged.all.data.list.clean <- merge.bind.all.clean(ambient,refrigerator,inside,house,switch) 

  ############# Clustering by Load Shape

  # Grouping 
  ehouse.norm <- merged.all.data.list.clean[[5]]
  norm.mean <- aggregate(ehouse.norm$norm.energy,list(ehouse.norm$house.id,ehouse.norm$hour),FUN=mean) %>% mutate(house.id=Group.1,hour=Group.2,norm.energy=x) %>% select(house.id,hour,norm.energy)
  norm.median <- aggregate(ehouse.norm$norm.energy,list(ehouse.norm$house.id,ehouse.norm$hour),FUN=median) %>% mutate(house.id=Group.1,hour=Group.2,norm.energy=x) %>% select(house.id,hour,norm.energy)
  
  unique.norms <- unique(norm.median$house.id)

  #For loop to create all plots
  # NOTE: When you run these plots just make sure that you are using the correct data set and the correct path (norm.median, norm.median)
  plot_list = list()
  for (i in 1:length(unique.norms)) {
    
    a.whatev.median <- subset(norm.median,norm.median$house.id==unique.norms[i])
    p <- ggplot(a.whatev.median,aes(x=hour,y=norm.energy)) + geom_line()
    plot_list[[i]] = p
  }
  
  # Save plots to jpeg making a separate file for each plot.
  for (i in 1:length(unique.norms)) {
    
    plot.var = unique.norms[i]
    name <- substring(plot.var,1,35)
    mypath <- file.path("/Users/Diego/Desktop/Projects/Enabling Micro-level Demand Response in Resource Constrained Environments/Figures/median",paste(name,".jpg",sep = ""))
    jpeg(file=mypath)
    print(plot_list[[i]])
    dev.off()
  }
  
  #### Plotting load shapes together
  
  #### Mean:
  
  # bumps throughout, higher in the middle of the day: A1, A15, A20, A21
  # one big bump middle of the day: A2, A6, A10, A16, A19
  # increasing throughout the day: A3, A5, A7, A8, A9, A11, A13, A17, A22, A24, A25
  # one big bump early morning, rise at night: A4, A12, A23
  # one bump in the middle of the day, one bump in the evening: A14
  
  norm.median$load.shape.type <- ifelse(norm.median$house.id=='A1' | norm.median$house.id=='A15' | norm.median$house.id=='A20' | norm.median$house.id=='A21','Bumps Throughout',ifelse(norm.median$house.id=='A2' | norm.median$house.id=='A6' | norm.median$house.id=='A10' | norm.median$house.id=='A16' | norm.median$house.id=='A19','Middle of the Day',ifelse(norm.median$house.id=='A3' | norm.median$house.id=='A5' | norm.median$house.id=='A7' | norm.median$house.id=='A8' | norm.median$house.id=='A9' | norm.median$house.id=='A11' | norm.median$house.id=='A13' | norm.median$house.id=='A17'  | norm.median$house.id=='A22'  | norm.median$house.id=='A24'  | norm.median$house.id=='A25','Increasing',ifelse(norm.median$house.id=='A4' | norm.median$house.id=='A12' | norm.median$house.id=='A23','Early Morning','Two Peaks'))))
  norm.median.plot <- aggregate(norm.median$norm.energy,list(norm.median$hour,norm.median$load.shape.type),FUN=mean) %>% mutate(hour=Group.1,Load.Type=Group.2,Mean.Energy=x) %>% select(hour,Load.Type,Mean.Energy)
  ggplot(norm.median.plot,aes(x=hour, y=Mean.Energy,colour=Load.Type)) + geom_line(size=0.9,alpha=0.5) + theme(panel.background = element_blank()) + xlab('Hour') +ylab('Normalized Energy Consumption') +  theme(legend.position="none")
                                                                                                                                                                                                                         
  
  ### Median:
  
  # bumps throughout, higher in the middle of the day: A15, A21
  # one bump in the middle of the day, one bump in the evening: A1, A14, A20
  # one big bump middle of the day: A2, A6, A10, A16, A19
  # increasing throughout the day: A3, A5, A7, A8, A9, A11, A13, A17, A22, A24, A25
  # one big bump early morning, rise at night: A4, A12, A23
  
  norm.median$load.shape.type <- ifelse(norm.median$house.id=='A15' | norm.median$house.id=='A21','Bumps Throughout',ifelse(norm.median$house.id=='A2' | norm.median$house.id=='A6' | norm.median$house.id=='A10' | norm.median$house.id=='A16' | norm.median$house.id=='A19','Middle of the Day',ifelse(norm.median$house.id=='A3' | norm.median$house.id=='A5' | norm.median$house.id=='A7' | norm.median$house.id=='A8' | norm.median$house.id=='A9' | norm.median$house.id=='A11' | norm.median$house.id=='A13' | norm.median$house.id=='A17'  | norm.median$house.id=='A22'  | norm.median$house.id=='A24'  | norm.median$house.id=='A25','Increasing',ifelse(norm.median$house.id=='A4' | norm.median$house.id=='A12' | norm.median$house.id=='A23','Early Morning','Two Peaks'))))
  
  norm.median.plot <- aggregate(norm.median$norm.energy,list(norm.median$hour,norm.median$load.shape.type),FUN=mean) %>% mutate(hour=Group.1,Load.Type=Group.2,Mean.Energy=x) %>% select(hour,Load.Type,Mean.Energy)
  ggplot(norm.median.plot,aes(x=hour, y=Mean.Energy,colour=Load.Type)) + geom_line(size=0.9,alpha=0.5) + theme(panel.background = element_blank()) + xlab('Hour') +ylab('Normalized Energy Consumption') +  theme(legend.position="none")
  


  ############# Week of the day analysis
  #Clustering was done on 5 minute intervals
  
  day.energy <- eday.analysis(house)
  
    # Weekday or weekend?
    day.energy$type.day <- ifelse(day.energy$day.id=='Saturday' | day.energy$day.id=='Sunday','Weekend','Weekday')
    # Do weekdays or weekends matter for energy consumption?
    mean.day <- aggregate(day.energy$energy,by=list(day.energy$day.id),FUN=mean)
    median.day <- aggregate(day.energy$energy,by=list(day.energy$day.id),FUN=median)
    mean.type.day <- aggregate(day.energy$energy,by=list(day.energy$type.day),FUN=mean)
    median.type.day <- aggregate(day.energy$energy,by=list(day.energy$type.day),FUN=median)
    
  
  
  
  
  

                                                                                                                                        
                                                                                                                                        
                                                                                                                                        
                                                                                                                                        
                                                                                                                                        
                                                                                                                                        
                                                                                                                                        
                                                                                                                                        
                                                                                                                                        