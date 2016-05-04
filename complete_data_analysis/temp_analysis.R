############ Script to Analyze data from A1


library(data.table)
library(lubridate)
library(ggplot2)
library(plyr)
library(gridExtra)
setwd('/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/')



####### Reading Data and Merging

a1_temp <- read.csv('DUMPS/DUMP1/flxbxA1.8.03.2015.sqld_inside_table.csv')
a2_temp <- read.csv('DUMPS/DUMP1/flxbxA2.7.29.2015.sqld_inside_table.csv')
a3_temp <- read.csv('DUMPS/DUMP1/flxbxA3.7.29.2015.sqld_inside_table.csv')
a4_temp <- read.csv('DUMPS/DUMP1/flxbxA4.7.29.2015.sqld_inside_table.csv')
a5_temp <- read.csv('DUMPS/DUMP1/flxbxA5.7.29.2015.sqld_inside_table.csv')
a6_temp <- read.csv('DUMPS/DUMP1/flxbxA6.7.29.2015.sqld_inside_table.csv')
a7_temp <- read.csv('DUMPS/DUMP1/flxbxA7.7.29.2015.sqld_inside_table.csv')
a8_temp <- read.csv('DUMPS/DUMP1/flxbxA8.8.03.2015.sqld_inside_table.csv')
a9_temp <- read.csv('DUMPS/DUMP1/flxbxA9.7.31.2015.sqld_inside_table.csv')
a10_temp <- read.csv('DUMPS/DUMP1/flxbxA10.8.03.2015.sqld_inside_table.csv')

a11_temp <- read.csv('DUMPS/DUMP1/flxbxA11.7.29.2015.sqld_inside_table.csv')
a12_temp <- read.csv('DUMPS/DUMP1/flxbxA12.7.29.2015.sqld_inside_table.csv')
a13_temp <- read.csv('DUMPS/DUMP1/flxbxA13.7.31.2015.sqld_inside_table.csv')
a14_temp <- read.csv('DUMPS/DUMP1/flxbxA14.7.30.sqld_inside_table.csv')
a15_temp <- read.csv('DUMPS/DUMP1/flxbxA15.7.30.2015.sqld_inside_table.csv')
a16_temp <- read.csv('DUMPS/DUMP1/flxbxA16.7.30.2015.sqld_inside_table.csv')
a17_temp <- read.csv('DUMPS/DUMP1/flxbxA17.7.30.2015.sqld_inside_table.csv')
a18_temp <- read.csv('DUMPS/DUMP1/flxbxA18.7.30.2015.sqld_inside_table.csv')
a19_temp <- read.csv('DUMPS/DUMP1/flxbxA19.8.03.2015.sqld_inside_table.csv')
a20_temp <- read.csv('DUMPS/DUMP1/flxbxA20.8.03.2015.sqld_inside_table.csv')

a21_temp <- read.csv('DUMPS/DUMP1/flxbxA21.7.30.2015.sqld_inside_table.csv')
a22_temp <- read.csv('DUMPS/DUMP1/flxbxA22.7.30.2015.sqld_inside_table.csv')
a23_temp <- read.csv('DUMPS/DUMP1/flxbxA23.7.30.2015.sqld_inside_table.csv')
a24_temp <- read.csv('DUMPS/DUMP1/flxbxA24.8.03.2015.sqld_inside_table.csv')
a25_temp <- read.csv('DUMPS/DUMP1/flxbxA25.8.03.2015.sqld_inside_table.csv')
a26_temp <- read.csv('DUMPS/DUMP1/flxbxA26.7.31.2015.sqld_inside_table.csv')
a27_temp <- read.csv('DUMPS/DUMP1/flxbxA27.7.31.2015.sqld_inside_table.csv')
a28_temp <- read.csv('DUMPS/DUMP1/flxbxA28.7.29.2015.sqld_inside_table.csv')
a29_temp <- read.csv('DUMPS/DUMP1/flxbxA29.7.29.2015.sqld_inside_table.csv')

# Adding an identifier

a1_temp$FLEXBOXID <- 'A1'
a2_temp$FLEXBOXID <- 'A2'
a3_temp$FLEXBOXID <- 'A3'
a4_temp$FLEXBOXID <- 'A4'
a5_temp$FLEXBOXID <- 'A5'
a6_temp$FLEXBOXID <- 'A6'
a7_temp$FLEXBOXID <- 'A7'
a8_temp$FLEXBOXID <- 'A8'
a9_temp$FLEXBOXID <- 'A9'

a10_temp$FLEXBOXID <- 'A10'
a11_temp$FLEXBOXID <- 'A11'
a12_temp$FLEXBOXID <- 'A12'
a13_temp$FLEXBOXID <- 'A13'
a14_temp$FLEXBOXID <- 'A14'
a15_temp$FLEXBOXID <- 'A15'
a16_temp$FLEXBOXID <- 'A16'
a17_temp$FLEXBOXID <- 'A17'
a18_temp$FLEXBOXID <- 'A18'
a19_temp$FLEXBOXID <- 'A19'

a20_temp$FLEXBOXID <- 'A20'
a21_temp$FLEXBOXID <- 'A21'
a22_temp$FLEXBOXID <- 'A22'
a23_temp$FLEXBOXID <- 'A23'
a24_temp$FLEXBOXID <- 'A24'
a25_temp$FLEXBOXID <- 'A25'
a26_temp$FLEXBOXID <- 'A26'
a27_temp$FLEXBOXID <- 'A7'
a28_temp$FLEXBOXID <- 'A28'
a29_temp$FLEXBOXID <- 'A29'

all_temp_data <- rbind(a1_temp,a2_temp,a3_temp)
                       ,a4_temp,a5_temp,a6_temp,a7_temp,a8_temp,a9_temp,a10_temp,a11_temp,a12_temp,a13_temp,a14_temp,a15_temp,a16_temp,a17_temp,a18_temp,a19_temp,a20_temp,a21_temp,a22_temp,a23_temp,a24_temp,a25_temp,a26_temp,a27_temp,a28_temp,a29_temp)

#### Bringing in the survey data
survey_data <- read.csv('all_data/all_survey_data_complete.csv')
names(survey_data)[names(survey_data)=="flexbox_id"] <- "FLEXBOXID"

#Keep Vars
vars <- c('FLEXBOXID','tipo_encuesta')
survey_data_vars <- survey_data[vars]

### Merging Sensor and Survey Data

complete_data <- merge(all_temp_data,survey_data_vars,by=c("FLEXBOXID"))

#### #Creating a time stamp

complete_data$datetime_rgx <- gsub("\\..*","", complete_data$datetime)
complete_data$datetime_rgx_new <- paste(complete_data$datetime_rgx,"-0000",sep=" ")
complete_data$time_stamp <- strptime(complete_data$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
complete_data$date <- as.Date(complete_data$time_stamp)

###  #House and Micro-Enterprise Data 
house_temp_data <- subset(complete_data,complete_data$tipo_encuesta == 'casa')
me_temp_data <- subset(complete_data,complete_data$tipo_encuesta == 'micro_empresa')


############
# Plotting 
############




#Densiy plot
#NOTE: You can change the 'aes(x=fridge_temp1)' to 'aes(x=fridge_temp2)'
house_temp_density <- ggplot(data=house_temp_data, aes(x=fridge_temp2, fill=FLEXBOXID)) + geom_density(alpha=.3) + xlim(-25000,35000) + labs(title = "Household Fridge Temp *C (Density)")
me_temp_density <- ggplot(data=me_temp_data, aes(x=fridge_temp2, fill=FLEXBOXID)) + geom_density(alpha=.3) +  xlim(-25000, 35000) + labs(title = "Micro-Enterprise  Fridge Temp *C (Density)")
grid.arrange(house_temp_density,me_temp_density)







