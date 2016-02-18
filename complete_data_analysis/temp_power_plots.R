###### Purpose

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
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/merging_binding_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/read_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/time_series_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/hourly_plots_functions.R')


###### NOTE
# Section 1: Reading data
# Section 2: Non clean data plots
# Section 3: Clean data plots
# Section 4: Daily Plots
# Section 5: Boxplot Distributions



flexlist <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15','A16','A17','A18','A19','A20','A21','A22','A23','A24','A25')
plot_list = list()
for (i in 1:length(flexlist)) {
  
  flexid.var <- flexlist[i]  
  data_list <- read.data('DUMP1',flexid.var)  #Read data for each of these ones
  
  inside <- data_list[[1]]
  refrigerator <- data_list[[3]]
  
  #### Converts dates
  dates_data_frame <- date.data.frame.simple(inside,refrigerator) 
  
  #Extract the data frames from the list
  inside <- dates_data_frame[[1]]
  refrigerator <- dates_data_frame[[2]]
  
  ###### Time series plots of the last seven days
  power.temp <- time.series.power.temp(inside,refrigerator)
  
  inside.temp.plot <- ggplot() + geom_point(data=power.temp[[1]],aes(x=time_stamp, y=fridge_temp1),color='black',size=1) + geom_point(data=power.temp[[1]],aes(x=time_stamp, y=fridge_temp2),color='red',size=1) +  ggtitle(expression(paste("Inside Temperature",degree,"C",flex.list[i])))  + xlab('Date') + ylab(expression(paste(degree,"C"))) + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))   
  refrigerator.plot <- ggplot() + geom_line(data=power.temp[[2]],aes(x=time_stamp, y=active_pwr3),color='black',size=1) + ggtitle(expression(paste("Refrigerator (W)",flex.list[i]))) + xlab('Date') + ylab(expression(paste("W"))) + theme(plot.title=element_text(size=8),axis.title=element_text(size=8),plot.margin= unit(c(0,1,0,1),units='cm'))
  
  combined.plot <- grid.arrange(inside.temp.plot,refrigerator.plot,ncol=1,nrow=2)
  
  plot_list[[i]] = combined.plot
}

apple <- plot_list[[2]]

grid.draw(apple)


# Save plots to jpeg making a separate file for each plot.
for (i in 1:length(flexlist)) {
  
  name <- flexlist[i]
  mypath <- file.path("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/initial_exploratory_results/UNITS/temp_power_all_units/",paste(name,".jpg",sep = ""))
  jpeg(file=mypath)
  grid.draw(plot_list[[i]])
  dev.off()
}




