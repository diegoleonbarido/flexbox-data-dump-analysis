############ Script to Analyze data from A1


library(data.table)
library(lubridate)
library(ggplot2)
library(plyr)
library(gridExtra)
setwd('/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/')



####### Reading Data and Merging

a1_amb <- read.csv('DUMPS/flxbxA1.8.03.2015.sqld_ambient_table.csv')
a2_amb <- read.csv('DUMPS/flxbxA2.7.29.2015.sqld_ambient_table.csv')
a3_amb <- read.csv('DUMPS/flxbxA3.7.29.2015.sqld_ambient_table.csv')
a4_amb <- read.csv('DUMPS/flxbxA4.7.29.2015.sqld_ambient_table.csv')
a5_amb <- read.csv('DUMPS/flxbxA5.7.29.2015.sqld_ambient_table.csv')
a6_amb <- read.csv('DUMPS/flxbxA6.7.29.2015.sqld_ambient_table.csv')
a7_amb <- read.csv('DUMPS/flxbxA7.7.29.2015.sqld_ambient_table.csv')
a8_amb <- read.csv('DUMPS/flxbxA8.8.03.2015.sqld_ambient_table.csv')
a9_amb <- read.csv('DUMPS/flxbxA9.7.31.2015.sqld_ambient_table.csv')
a10_amb <- read.csv('DUMPS/flxbxA10.8.03.2015.sqld_ambient_table.csv')

a11_amb <- read.csv('DUMPS/flxbxA11.7.29.2015.sqld_ambient_table.csv')
a12_amb <- read.csv('DUMPS/flxbxA12.7.29.2015.sqld_ambient_table.csv')
a13_amb <- read.csv('DUMPS/flxbxA13.7.31.2015.sqld_ambient_table.csv')
a14_amb <- read.csv('DUMPS/flxbxA14.7.30.sqld_ambient_table.csv')
a15_amb <- read.csv('DUMPS/flxbxA15.7.30.2015.sqld_ambient_table.csv')
a16_amb <- read.csv('DUMPS/flxbxA16.7.30.2015.sqld_ambient_table.csv')
a17_amb <- read.csv('DUMPS/flxbxA17.7.30.2015.sqld_ambient_table.csv')
a18_amb <- read.csv('DUMPS/flxbxA18.7.30.2015.sqld_ambient_table.csv')
a19_amb <- read.csv('DUMPS/flxbxA19.8.03.2015.sqld_ambient_table.csv')
a20_amb <- read.csv('DUMPS/flxbxA20.8.03.2015.sqld_ambient_table.csv')

a21_amb <- read.csv('DUMPS/flxbxA21.7.30.2015.sqld_ambient_table.csv')
a22_amb <- read.csv('DUMPS/flxbxA22.7.30.2015.sqld_ambient_table.csv')
a23_amb <- read.csv('DUMPS/flxbxA23.7.30.2015.sqld_ambient_table.csv')
a24_amb <- read.csv('DUMPS/flxbxA24.8.03.2015.sqld_ambient_table.csv')
a25_amb <- read.csv('DUMPS/flxbxA25.8.03.2015.sqld_ambient_table.csv')
a26_amb <- read.csv('DUMPS/flxbxA26.7.31.2015.sqld_ambient_table.csv')
a27_amb <- read.csv('DUMPS/flxbxA27.7.31.2015.sqld_ambient_table.csv')
a28_amb <- read.csv('DUMPS/flxbxA28.7.29.2015.sqld_ambient_table.csv')
a29_amb <- read.csv('DUMPS/flxbxA29.7.29.2015.sqld_ambient_table.csv')

# Adding an identifier

a1_amb$FLEXBOXID <- 'A1'
a2_amb$FLEXBOXID <- 'A2'
a3_amb$FLEXBOXID <- 'A3'
a4_amb$FLEXBOXID <- 'A4'
a5_amb$FLEXBOXID <- 'A5'
a6_amb$FLEXBOXID <- 'A6'
a7_amb$FLEXBOXID <- 'A7'
a8_amb$FLEXBOXID <- 'A8'
a9_amb$FLEXBOXID <- 'A9'

a10_amb$FLEXBOXID <- 'A10'
a11_amb$FLEXBOXID <- 'A11'
a12_amb$FLEXBOXID <- 'A12'
a13_amb$FLEXBOXID <- 'A13'
a14_amb$FLEXBOXID <- 'A14'
a15_amb$FLEXBOXID <- 'A15'
a16_amb$FLEXBOXID <- 'A16'
a17_amb$FLEXBOXID <- 'A17'
a18_amb$FLEXBOXID <- 'A18'
a19_amb$FLEXBOXID <- 'A19'

a20_amb$FLEXBOXID <- 'A20'
a21_amb$FLEXBOXID <- 'A21'
a22_amb$FLEXBOXID <- 'A22'
a23_amb$FLEXBOXID <- 'A23'
a24_amb$FLEXBOXID <- 'A24'
a25_amb$FLEXBOXID <- 'A25'
a26_amb$FLEXBOXID <- 'A26'
a27_amb$FLEXBOXID <- 'A7'
a28_amb$FLEXBOXID <- 'A28'
a29_amb$FLEXBOXID <- 'A29'

all_amb_data <- rbind(a1_amb,a2_amb,a3_amb,a4_amb,a5_amb,a6_amb,a7_amb,a8_amb,a9_amb,a10_amb,a11_amb,a12_amb,a13_amb,a14_amb,a15_amb,a16_amb,a17_amb,a18_amb,a19_amb,a20_amb,a21_amb,a22_amb,a23_amb,a24_amb,a25_amb,a26_amb,a27_amb,a28_amb,a29_amb)

#### Bringing in the survey data
survey_data <- read.csv('all_data/all_survey_data_complete.csv')
names(survey_data)[names(survey_data)=="flexbox_id"] <- "FLEXBOXID"

#Keep Vars
vars <- c('FLEXBOXID','tipo_encuesta')
survey_data_vars <- survey_data[vars]

### Merging Sensor and Survey Data

complete_data <- merge(all_amb_data,survey_data_vars,by=c("FLEXBOXID"))

#### #Creating a time stamp

complete_data$datetime_rgx <- gsub("\\..*","", complete_data$datetime)
complete_data$datetime_rgx_new <- paste(complete_data$datetime_rgx,"-0000",sep=" ")
complete_data$time_stamp <- strptime(complete_data$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
complete_data$date <- as.Date(complete_data$time_stamp)

###  #House and Micro-Enterprise Data 
house_amb_data <- subset(complete_data,complete_data$tipo_encuesta == 'casa')
me_amb_data <- subset(complete_data,complete_data$tipo_encuesta == 'micro_empresa')


############
# Plotting 
############

#Densiy plot
#NOTE: You can change the 'aes(x=fridge_temp1)' to 'aes(x=fridge_temp2)'
house_amb_density <- ggplot(data=house_amb_data, aes(x=ambient_temp, fill=FLEXBOXID)) + geom_density(alpha=.3) + xlim(20,45) + labs(title = "Household Ambient Temp *C (Density)")
me_amb_density <- ggplot(data=me_amb_data, aes(x=ambient_temp, fill=FLEXBOXID)) + geom_density(alpha=.3) +  xlim(20, 45) + labs(title = "Micro-Enterprise  Ambient Temp *C (Density)")
grid.arrange(house_amb_density,me_amb_density)





