library(data.table)
library(lubridate)

setwd('/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/DUMPS')


############  Reading the data in - simply change the file name to analyze another data


ambient <- read.csv('flxbxA17.8.05.2015.sqld_ambient_table.csv')
inside <- read.csv('flxbxA17.8.05.2015.sqld_inside_table.csv')
refrigerator <- read.csv('flxbxA17.8.05.2015.sqld_mfi_table.csv')
switch <- read.csv('flxbxA17.8.05.2015.sqld_switch_table.csv')
house <- read.csv('flxbxA17.8.05.2015.sqld_zwave_table.csv')


############  Data preparation

#Turning the time zone into Nicaragua time

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

#Check Unique dates
unique(ambient$date)
unique(inside$date )
unique(switch$date)
unique(house$date)
unique(refrigerator$date)


#Dividing my 1000 to have reasonable values
inside$temp1 <- inside$fridge_temp1/1000
inside$temp2 <- inside$fridge_temp2/1000

#Calculating power for the fridge
refrigerator$power <- refrigerator$vrms3*refrigerator$i_rms3






############# Plotting an Entire Week

#Plotting Ambient Temperature

par(mfrow=c(5,1),mai=c(0.6,0.6,0.1,0.1))

plot(ambient$time_stamp,ambient$ambient_temp,xlab='Day',ylab='Ambient (C)',cex=0.1)

plot(inside$time_stamp,inside$temp1, xlab='Day',ylab='Inside T1 (C)',cex=0.1,ylim=c(-10,40))
par(new=TRUE)
plot(inside$time_stamp,inside$temp2, xlab='Day',ylab='Inside T2 (C)',cex=0.1, col='red',ylim=c(-10,40))

plot(refrigerator$time_stamp,refrigerator$power,xlab='Day',ylab='Fridge (W)',cex=0.2,pch=19,ylim=c(0,800))
lines(refrigerator$time_stamp, refrigerator$power)

plot(switch$time_stamp,switch$open,xlab='Day',ylab='Openings')

plot(house$time_stamp,house$house_Power,xlab='Day',ylab='House (W)',cex=0.1)
lines(house$time_stamp, house$house_Power)



#PLotting power and temperature only
par(mfrow=c(2,1),mai=c(0.6,0.6,0.1,0.1))

plot(inside$time_stamp,inside$temp1, xlab='Day',ylab='Inside T1 (C)',cex=0.1,ylim=c(-10,40))
par(new=TRUE)
plot(inside$time_stamp,inside$temp2, xlab='Day',ylab='Inside T2 (C)',cex=0.1, col='red',ylim=c(-10,40))

plot(refrigerator$time_stamp,refrigerator$power,xlab='Day',ylab='Fridge (W)',cex=0.2,pch=19,ylim=c(0,800))
lines(refrigerator$time_stamp, refrigerator$power)



########### Ploting a one day subset

#Creating a subet of only one day to plot

day_ambient <- subset(ambient,ambient$date == '2015-07-19')
day_inside <- subset(inside, inside$date == '2015-07-19')
day_refrigerator <- subset(refrigerator, refrigerator$date == '2015-07-19')
day_switch <- subset(switch, switch$date == '2015-07-19')
day_house <- subset(house, house$date == '2015-07-19')


par(mfrow=c(5,1),mai=c(0.6,0.6,0.1,0.1))

plot(day_ambient$time_stamp, day_ambient$ambient_temp,xlab='Day',ylab='Ambient (C)',cex=0.1)

plot(day_inside$time_stamp, day_inside$temp1, xlab='Day',ylab='Inside T1 (C)',cex=0.1,ylim=c(-10,20))
par(new=TRUE)
plot(day_inside$time_stamp, day_inside$temp2, xlab='Day',ylab='Inside T2 (C)',cex=0.1, col='red',ylim=c(-10,20))

plot(day_refrigerator$time_stamp, day_refrigerator$power,xlab='Day',ylab='Fridge (W)',cex=0.2,pch=19)
lines(day_refrigerator$time_stamp, day_refrigerator$power)

plot(day_switch$time_stamp, day_switch$open,xlab='Day',ylab='Openings')

plot(day_house$time_stamp, day_house$house_Power,xlab='Day',ylab='House (W)',cex=0.1)
lines(day_house$time_stamp, day_house$house_Power)





########################################
########### 5 Hourly reality checks 

#Midnight to 5 am
five_house <- with(day_house, day_house[hour(day_house$time_stamp) >= 0 & hour(day_house$time_stamp)< 5,])
five_ambient <- with(day_ambient, day_ambient[hour(day_ambient$time_stamp) >= 0 & hour(day_ambient$time_stamp)< 5,])
five_inside <- with(day_inside, day_inside[hour(day_inside$time_stamp) >= 0 & hour(day_inside$time_stamp)< 5,])
five_refrigerator <- with(day_refrigerator, day_refrigerator[hour(day_refrigerator$time_stamp) >= 0 & hour(day_refrigerator$time_stamp)< 5,])
five_switch <- with(day_switch, day_switch[hour(day_switch$time_stamp) >= 0 & hour(day_switch$time_stamp)< 5,])


#Plot Begins
par(mfrow=c(5,1),mai=c(0.6,0.6,0.1,0.1))
plot(five_ambient$time_stamp, five_ambient$ambient_temp,xlab='Day',ylab='Ambient (C)',cex=0.1,ylim=c(25,40))
plot(five_inside$time_stamp, five_inside$temp1, xlab='Day',ylab='Inside T1 (C)',cex=0.1, ylim=c(-10,20))
par(new=TRUE)
plot(five_inside$time_stamp, five_inside$temp2, xlab='Day',ylab='Inside T2 (C)',cex=0.1,ylim=c(-10,20), col='red')
plot(five_refrigerator$time_stamp, five_refrigerator$power,ylim=c(100,180),xlab='Day',ylab='Fridge (W)',cex=0.2,pch=19)
lines(five_refrigerator$time_stamp, five_refrigerator$power)
plot(five_switch$time_stamp, five_switch$open,ylim=c(0,1),xlab='Day',ylab='Openings')
plot(five_house$time_stamp, five_house$house_Power,xlab='Day',ylab='House (W)',cex=0.1,ylim=c(0,2000))
lines(five_house$time_stamp, five_house$house_Power)



#6 am  to noon
six_house <- with(day_house, day_house[hour(day_house$time_stamp) >= 6 & hour(day_house$time_stamp)< 12,])
six_ambient <- with(day_ambient, day_ambient[hour(day_ambient$time_stamp) >= 6 & hour(day_ambient$time_stamp)< 12,])
six_inside <- with(day_inside, day_inside[hour(day_inside$time_stamp) >= 6 & hour(day_inside$time_stamp)< 12,])
six_refrigerator <- with(day_refrigerator, day_refrigerator[hour(day_refrigerator$time_stamp) >= 6 & hour(day_refrigerator$time_stamp)< 12,])
six_switch <- with(day_switch, day_switch[hour(day_switch$time_stamp) >= 6 & hour(day_switch$time_stamp)< 12,])

#Plot Begins

par(mfrow=c(5,1),mai=c(0.6,0.6,0.1,0.1))
plot(six_ambient$time_stamp, six_ambient$ambient_temp,xlab='Day',ylab='Ambient (C)',cex=0.1,ylim=c(25,40))
plot(six_inside$time_stamp, six_inside$temp1, xlab='Day',ylab='Inside T1 (C)',cex=0.1, ylim=c(-10,20))
par(new=TRUE)
plot(six_inside$time_stamp, six_inside$temp2, xlab='Day',ylab='Inside T2 (C)',cex=0.1,ylim=c(-10,20), col='red')
plot(six_refrigerator$time_stamp, six_refrigerator$power,ylim=c(100,180),xlab='Day',ylab='Fridge (W)',cex=0.2,pch=19)
lines(six_refrigerator$time_stamp, six_refrigerator$power)
plot(six_switch$time_stamp, six_switch$open,ylim=c(0,1),xlab='Day',ylab='Openings')
plot(six_house$time_stamp, six_house$house_Power,xlab='Day',ylab='Ambient (C)',cex=0.1,ylim=c(0,2000))
lines(six_house$time_stamp, six_house$house_Power)


#1 pm to 6 pm 
one_house <- with(day_house, day_house[hour(day_house$time_stamp) >= 13 & hour(day_house$time_stamp)< 18,])
one_ambient <- with(day_ambient, day_ambient[hour(day_ambient$time_stamp) >= 13 & hour(day_ambient$time_stamp)< 18,])
one_inside <- with(day_inside, day_inside[hour(day_inside$time_stamp) >= 13 & hour(day_inside$time_stamp)< 18,])
one_refrigerator <- with(day_refrigerator, day_refrigerator[hour(day_refrigerator$time_stamp) >= 13 & hour(day_refrigerator$time_stamp)< 18,])
one_switch <- with(day_switch, day_switch[hour(day_switch$time_stamp) >= 13 & hour(day_switch$time_stamp)< 18,])

#Plot Begins

par(mfrow=c(5,1),mai=c(0.6,0.6,0.1,0.1))
plot(one_ambient$time_stamp, one_ambient$ambient_temp,xlab='Day',ylab='Ambient (C)',cex=0.1,ylim=c(25,40))
plot(one_inside$time_stamp, one_inside$temp1, xlab='Day',ylab='Inside T1 (C)',cex=0.1, ylim=c(-10,20))
par(new=TRUE)
plot(one_inside$time_stamp, one_inside$temp2, xlab='Day',ylab='Inside T2 (C)',cex=0.1,ylim=c(-10,20), col='red')
plot(one_refrigerator$time_stamp, one_refrigerator$power,ylim=c(100,180),xlab='Day',ylab='Fridge (W)',cex=0.2,pch=19)
lines(one_refrigerator$time_stamp, one_refrigerator$power)
plot(one_switch$time_stamp, one_switch$open,ylim=c(0,1),xlab='Day',ylab='Openings')
plot(one_house$time_stamp, one_house$house_Power,xlab='Day',ylab='Ambient (C)',cex=0.1,ylim=c(0,2000))
lines(one_house$time_stamp, one_house$house_Power)



#1 pm to 6 pm 
one_house <- with(day_house, day_house[hour(day_house$time_stamp) >= 13 & hour(day_house$time_stamp)< 18,])
one_ambient <- with(day_ambient, day_ambient[hour(day_ambient$time_stamp) >= 13 & hour(day_ambient$time_stamp)< 18,])
one_inside <- with(day_inside, day_inside[hour(day_inside$time_stamp) >= 13 & hour(day_inside$time_stamp)< 18,])
one_refrigerator <- with(day_refrigerator, day_refrigerator[hour(day_refrigerator$time_stamp) >= 13 & hour(day_refrigerator$time_stamp)< 18,])
one_switch <- with(day_switch, day_switch[hour(day_switch$time_stamp) >= 13 & hour(day_switch$time_stamp)< 18,])

#Plot Begins

par(mfrow=c(5,1),mai=c(0.6,0.6,0.1,0.1))
plot(one_ambient$time_stamp, one_ambient$ambient_temp,xlab='Day',ylab='Ambient (C)',cex=0.1,ylim=c(25,40))
plot(one_inside$time_stamp, one_inside$temp1, xlab='Day',ylab='Inside T1 (C)',cex=0.1, ylim=c(-10,20))
par(new=TRUE)
plot(one_inside$time_stamp, one_inside$temp2, xlab='Day',ylab='Inside T2 (C)',cex=0.1,ylim=c(-10,20), col='red')
plot(one_refrigerator$time_stamp, one_refrigerator$power,ylim=c(100,180),xlab='Day',ylab='Fridge (W)',cex=0.2,pch=19)
lines(one_refrigerator$time_stamp, one_refrigerator$power)
plot(one_switch$time_stamp, one_switch$open,ylim=c(0,1),xlab='Day',ylab='Openings')
plot(one_house$time_stamp, one_house$house_Power,xlab='Day',ylab='Ambient (C)',cex=0.1,ylim=c(0,2000))
lines(one_house$time_stamp, one_house$house_Power)



#7 pm to 12 pm 
seven_house <- with(day_house, day_house[hour(day_house$time_stamp) >= 19 & hour(day_house$time_stamp)< 23,])
seven_ambient <- with(day_ambient, day_ambient[hour(day_ambient$time_stamp) >= 19 & hour(day_ambient$time_stamp)< 23,])
seven_inside <- with(day_inside, day_inside[hour(day_inside$time_stamp) >= 19 & hour(day_inside$time_stamp)< 23,])
seven_refrigerator <- with(day_refrigerator, day_refrigerator[hour(day_refrigerator$time_stamp) >= 19 & hour(day_refrigerator$time_stamp)< 23,])
seven_switch <- with(day_switch, day_switch[hour(day_switch$time_stamp) >= 19 & hour(day_switch$time_stamp)< 23,])

#Plot Begins

par(mfrow=c(5,1),mai=c(0.6,0.6,0.1,0.1))
plot(seven_ambient$time_stamp, seven_ambient$ambient_temp,xlab='Day',ylab='Ambient (C)',cex=0.1,ylim=c(25,40))
plot(seven_inside$time_stamp, seven_inside$temp1, xlab='Day',ylab='Inside T1 (C)',cex=0.1, ylim=c(-10,20))
par(new=TRUE)
plot(seven_inside$time_stamp, seven_inside$temp2, xlab='Day',ylab='Inside T2 (C)',cex=0.1,ylim=c(-10,20), col='red')
plot(seven_refrigerator$time_stamp, seven_refrigerator$power,ylim=c(100,180),xlab='Day',ylab='Fridge (W)',cex=0.2,pch=19)
lines(seven_refrigerator$time_stamp, seven_refrigerator$power)
plot(seven_switch$time_stamp, seven_switch$open,ylim=c(0,1),xlab='Day',ylab='Openings')
plot(seven_house$time_stamp, seven_house$house_Power,xlab='Day',ylab='Ambient (C)',cex=0.1,ylim=c(0,2000))
lines(seven_house$time_stamp, seven_house$house_Power)






