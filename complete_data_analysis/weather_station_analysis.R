###### Analyzing temperature data from weather station and ambient temperature

# Libraries and functions

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


source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/date_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/merging_binding_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/read_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/time_series_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/hourly_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_consumption_functions.R')

### Read data

# Weather Station
setwd('/Users/Diego/Desktop')
weewx <- read.csv("weewx_table.csv")

# FlexList
flexlist <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15','A16','A17','A18','A19','A20','A21','A22','A23','A24','A25','A27','A28','A29')
data.list.houses <- read.data.all('DUMP1',flexlist) 

dates_data_frame <- date.data.frame(data.list.houses[[1]],data.list.houses[[2]],data.list.houses[[3]],data.list.houses[[4]],data.list.houses[[5]]) 
ambient <- dates_data_frame[[2]]



# Fixing dates before beginning merge

weewx$time_stamp <- as.POSIXct(weewx$dateTime, origin="1970-01-01")
weewx <- date.vars.simple(weewx)
weewx$temp <- as.numeric(as.character(weewx$inTemp))

ambient <- date.vars.simple(ambient)

# Calculating the 

weewx.df <- aggregate(weewx$temp,by=list(weewx$hour),FUN=mean,na.rm=TRUE)
weewx.df$Group.2 <- 'Weather Station'
colnames(weewx.df)[1] <- 'hour'
colnames(weewx.df)[2] <- 'temp'
colnames(weewx.df)[3] <- 'id'

weewx.df$temp <- (weewx.df$temp - 32)/1.8


ambient.df <- aggregate(ambient$ambient_temp,by=list(ambient$hour,ambient$house.id),FUN=mean,na.rm=TRUE)
colnames(ambient.df)[1] <- 'hour'
colnames(ambient.df)[2] <- 'id'
colnames(ambient.df)[3]<- 'temp'

ambient.df <- ambient.df[,c('hour','temp','id')]

weather.ambient <- rbind(weewx.df,ambient.df)
weather.ambient$Group <- ifelse(weather.ambient$id=='Weather Station','Weather Station','House Ambient Sensor')
weather.ambient$ball.size <- ifelse(weather.ambient$id=='Weather Station',15,5)

ggplot(data = weather.ambient, aes(x = hour, y = temp, colour = Group)) + geom_point(aes(size = ball.size)) + xlab('Hour') + ylab(expression(paste("Temperature",degree,"C"))) + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + guides(size = FALSE,fill=guide_legend(title=NULL)) + scale_shape_discrete(name  ="Group") + theme(legend.position="none")


plot(weewx.df$Group.1,weewx.df$x)


ambient.weather <- merge(ambient,weewx,by=c('date','hour','minute'),all.x=TRUE)

#Keeping only the complete cases on the variables of interest
completeVec <- ambient.weather[complete.cases(ambient.weather[,c('date','hour','minute','ambient_temp','house.id','outTemp')]),]
complete.vars <- completeVec[,c('date','hour','minute','house.id','ambient_temp','humidity','inTemp','inHumidity')]
complete.vars$wtemp_c <- (as.numeric(as.character(complete.vars$inTemp))-32)/1.8
complete.vars$whumid_c <- (as.numeric(as.character(complete.vars$inHumidity))-32)/1.8

#Complete Cases
ops <- subset(complete.vars,complete.vars$date == '2015-07-23' & complete.vars$house.id == 'A1')


plot(1:length(ops$date),ops$ambient_temp)



plot(1:length(complete.vars$date),complete.vars$wtemp_c)






