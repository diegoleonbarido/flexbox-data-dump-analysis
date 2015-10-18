############ Script to Analyze data from A1


library(data.table)
library(lubridate)
library(ggplot2)
library(plyr)
library(gridExtra)
setwd('/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/')



####### Reading Data and Merging

a1_fridge <- read.csv('DUMPS/flxbxA1.8.03.2015.sqld_mfi_table.csv')
a2_fridge <- read.csv('DUMPS/flxbxA2.7.29.2015.sqld_mfi_table.csv')
a3_fridge <- read.csv('DUMPS/flxbxA3.7.29.2015.sqld_mfi_table.csv')
a4_fridge <- read.csv('DUMPS/flxbxA4.7.29.2015.sqld_mfi_table.csv')
a5_fridge <- read.csv('DUMPS/flxbxA5.7.29.2015.sqld_mfi_table.csv')
a6_fridge <- read.csv('DUMPS/flxbxA6.7.29.2015.sqld_mfi_table.csv')
a7_fridge <- read.csv('DUMPS/flxbxA7.7.29.2015.sqld_mfi_table.csv')
a8_fridge <- read.csv('DUMPS/flxbxA8.8.03.2015.sqld_mfi_table.csv')
a9_fridge <- read.csv('DUMPS/flxbxA9.7.31.2015.sqld_mfi_table.csv')
a10_fridge <- read.csv('DUMPS/flxbxA10.8.03.2015.sqld_mfi_table.csv')

a11_fridge <- read.csv('DUMPS/flxbxA11.7.29.2015.sqld_mfi_table.csv')
a12_fridge <- read.csv('DUMPS/flxbxA12.7.29.2015.sqld_mfi_table.csv')
a13_fridge <- read.csv('DUMPS/flxbxA13.7.31.2015.sqld_mfi_table.csv')
a14_fridge <- read.csv('DUMPS/flxbxA14.7.30.sqld_mfi_table.csv')
a15_fridge <- read.csv('DUMPS/flxbxA15.7.30.2015.sqld_mfi_table.csv')
a16_fridge <- read.csv('DUMPS/flxbxA16.7.30.2015.sqld_mfi_table.csv')
a17_fridge <- read.csv('DUMPS/flxbxA17.7.30.2015.sqld_mfi_table.csv')
a18_fridge <- read.csv('DUMPS/flxbxA18.7.30.2015.sqld_mfi_table.csv')
a19_fridge <- read.csv('DUMPS/flxbxA19.8.03.2015.sqld_mfi_table.csv')
a20_fridge <- read.csv('DUMPS/flxbxA20.8.03.2015.sqld_mfi_table.csv')

a21_fridge <- read.csv('DUMPS/flxbxA21.7.30.2015.sqld_mfi_table.csv')
a22_fridge <- read.csv('DUMPS/flxbxA22.7.30.2015.sqld_mfi_table.csv')
a23_fridge <- read.csv('DUMPS/flxbxA23.7.30.2015.sqld_mfi_table.csv')
a24_fridge <- read.csv('DUMPS/flxbxA24.8.03.2015.sqld_mfi_table.csv')
a25_fridge <- read.csv('DUMPS/flxbxA25.8.03.2015.sqld_mfi_table.csv')
a26_fridge <- read.csv('DUMPS/flxbxA26.7.31.2015.sqld_mfi_table.csv')
a27_fridge <- read.csv('DUMPS/flxbxA27.7.31.2015.sqld_mfi_table.csv')
a28_fridge <- read.csv('DUMPS/flxbxA28.7.29.2015.sqld_mfi_table.csv')
a29_fridge <- read.csv('DUMPS/flxbxA29.7.29.2015.sqld_mfi_table.csv')

# Adding an identifier

a1_fridge$FLEXBOXID <- 'A1'
a2_fridge$FLEXBOXID <- 'A2'
a3_fridge$FLEXBOXID <- 'A3'
a4_fridge$FLEXBOXID <- 'A4'
a5_fridge$FLEXBOXID <- 'A5'
a6_fridge$FLEXBOXID <- 'A6'
a7_fridge$FLEXBOXID <- 'A7'
a8_fridge$FLEXBOXID <- 'A8'
a9_fridge$FLEXBOXID <- 'A9'

a10_fridge$FLEXBOXID <- 'A10'
a11_fridge$FLEXBOXID <- 'A11'
a12_fridge$FLEXBOXID <- 'A12'
a13_fridge$FLEXBOXID <- 'A13'
a14_fridge$FLEXBOXID <- 'A14'
a15_fridge$FLEXBOXID <- 'A15'
a16_fridge$FLEXBOXID <- 'A16'
a17_fridge$FLEXBOXID <- 'A17'
a18_fridge$FLEXBOXID <- 'A18'
a19_fridge$FLEXBOXID <- 'A19'

a20_fridge$FLEXBOXID <- 'A20'
a21_fridge$FLEXBOXID <- 'A21'
a22_fridge$FLEXBOXID <- 'A22'
a23_fridge$FLEXBOXID <- 'A23'
a24_fridge$FLEXBOXID <- 'A24'
a25_fridge$FLEXBOXID <- 'A25'
a26_fridge$FLEXBOXID <- 'A26'
a27_fridge$FLEXBOXID <- 'A7'
a28_fridge$FLEXBOXID <- 'A28'
a29_fridge$FLEXBOXID <- 'A29'

all_fridge_data <- rbind(a1_fridge,a2_fridge,a3_fridge,a4_fridge,a5_fridge,a6_fridge,a7_fridge,a8_fridge,a9_fridge,a10_fridge,a11_fridge,a12_fridge,a13_fridge,a14_fridge,a15_fridge,a16_fridge,a17_fridge,a18_fridge,a19_fridge,a20_fridge,a21_fridge,a22_fridge,a23_fridge,a24_fridge,a25_fridge,a26_fridge,a27_fridge,a28_fridge,a29_fridge)

#### Bringing in the survey data
survey_data <- read.csv('all_data/survey_data_comlpete.csv')
names(survey_data)[names(survey_data)=="flexbox_id"] <- "FLEXBOXID"

#Keep Vars
vars <- c('FLEXBOXID','tipo_encuesta')
survey_data_vars <- survey_data[vars]

### Merging Sensor and Survey Data

complete_data <- merge(all_fridge_data,survey_data_vars,by=c("FLEXBOXID"))

#### #Creating a time stamp

complete_data$datetime_rgx <- gsub("\\..*","", complete_data$datetime)
complete_data$datetime_rgx_new <- paste(complete_data$datetime_rgx,"-0000",sep=" ")
complete_data$time_stamp <- strptime(complete_data$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
complete_data$date <- as.Date(complete_data$time_stamp)

#### #Calculating power for the fridge
complete_data$power <- complete_data$vrms3*complete_data$i_rms3

###  #House Data 
house_data <- subset(complete_data,complete_data$tipo_encuesta == 'casa')
me_data <- subset(complete_data,complete_data$tipo_encuesta == 'micro_empresa')


############
# Plotting 
############

#Densiy plot
house_density <- ggplot(data=house_data, aes(x=power, fill=FLEXBOXID)) + geom_density(alpha=.3) + xlim(0, 500) +  labs(title = "Household Power Consumption (Density)")
me_density <- ggplot(data=me_data, aes(x=power, fill=FLEXBOXID)) + geom_density(alpha=.3) + xlim(0, 1250) + labs(title = "Micro-Enterprise Power Consumption (Density)")
grid.arrange(house_density,me_density)

#Power Consumption over time
house_trends <- ggplot(data=house_data, aes(x=time_stamp, y=power, group=FLEXBOXID, colour=FLEXBOXID)) + geom_line(size=1.5,alpha=0.6) +  geom_point(size=3, fill="white") + labs(title = "Household Power Consumption")
me_trends <- ggplot(data=me_data, aes(x=time_stamp, y=power, group=FLEXBOXID, colour=FLEXBOXID)) + geom_line(size=1.5,alpha=0.6) +  geom_point(size=3, fill="white") + labs(title = "Micro-Enterprise Power Consumption")
grid.arrange(house_trends,me_trends)




