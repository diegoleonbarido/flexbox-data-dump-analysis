##### Created: July 28th 2015
##### Creator: Diego Ponce de Leon Barido
##### 


library(data.table)
library(lubridate)


############ Reading in the data
setwd('/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/DUMPS')
house <- read.csv('flxbxA3.7.29.2015.sqld_zwave_table.csv')



############  Data preparation

#Turning the time zone into Nicaragua time

house$datetime_rgx <- gsub("\\..*","", house$datetime)
house$datetime_rgx_new <- paste(house$datetime_rgx,"-0000",sep=" ")
house$time_stamp <- strptime(house$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")

#Just dates (day without time)
house$date <- as.Date(house$time_stamp)

############  Plots

#Total household consumption
plot(house$time_stamp,house$house_Power,xlab='Day',ylab='House (W)',cex=0.1)
lines(house$time_stamp, house$house_Power)

#Voltage
plot(house$time_stamp,house$house_Voltage,xlab='Day',ylab='House (W)',cex=0.1)
lines(house$time_stamp, house$house_Voltage)

#Current
plot(house$time_stamp,house$house_Current,xlab='Day',ylab='House (W)',cex=0.1)
lines(house$time_stamp, house$house_Current)

########### Ploting a one day subset

day_house <- subset(house, house$date == '2015-07-29')

#Total household consumption
plot(day_house$time_stamp, day_house$house_Power,xlab='Day',ylab='House (W)',cex=0.1)
lines(day_house$time_stamp, day_house$house_Power)

#Voltage
plot(day_house$time_stamp, day_house$house_Voltage,xlab='Day',ylab='House (W)',cex=0.1)
lines(day_house$time_stamp, day_house$house_Voltage)

#Current
plot(day_house$time_stamp, day_house$house_Current,xlab='Day',ylab='House (W)',cex=0.1)
lines(day_house$time_stamp, day_house$house_Current)


########### Ploting NILM Hour

nilm_hour <- with(day_house, day_house[hour(time_stamp) >= 10 & hour(time_stamp) < 11, ])

#Total household consumption
plot(nilm_hour$time_stamp, nilm_hour$house_Power,xlab='Day',ylab='House (W)',cex=0.1)
lines(day_house$time_stamp, day_house$house_Power)

#Voltage
plot(nilm_hour$time_stamp, nilm_hour$house_Voltage,xlab='Day',ylab='House (W)',cex=0.1)
lines(nilm_hour$time_stamp, nilm_hour$house_Voltage)

#Current
plot(nilm_hour$time_stamp, nilm_hour$house_Current,xlab='Day',ylab='House (W)',cex=0.1)
lines(nilm_hour$time_stamp, nilm_hour$house_Current)





