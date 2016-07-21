library(RPostgreSQL)
library(DBI)
library(sqldf)
library(RSQLite)
library(RMySQL)
library(rPython)
library(lubridate)
import(pR)
library(tidyr)
library(zoo)
library(data.table)

devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
library(lubridate)


########## Local Connection to the Server

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv = PostgreSQL(), user = "niuera_analyzer", dbname = "niuera_real_time_db", host = "localhost",  password = "flexbox")

access.database <- dbSendQuery(con,statement=paste("SELECT * from cndc_20sec_table"));
real_time_data <- fetch(access.database,n=-1)

as.data.table.dt <- data.table(real_time_data)



########### Reading 1 min, 5 min, 10 min and 15 min resampled packages from python









########## Importing to Python


python.load("/Users/Diego/Desktop/read_data_dr.py")

data <- read.csv("/Users/Diego/Desktop/time_series_data.csv")

sopita$date <- as.Date(sopita$datetime)

sopita$timestamp <- sopita$datetime
sopita$count <- sopita$load_all
sopita$date <- as.Date(sopita$timestamp)
sopita$hour <- hour(sopita$timestamp)
sopita$minute <- minute(sopita$timestamp)
sopita$date_char <- as.character(sopita$date)


sopita_keep <- sopita[,c("count","date_char","date","hour","minute")]

sopita_aggregate <- na.omit(aggregate(sopita_keep$count,by=list(sopita_keep$date_char,sopita_keep$hour,sopita_keep$minute),FUN=mean,na.rm=TRUE))
sopita_aggregate_order <- sopita_aggregate[order(sopita_aggregate$Group.1,sopita_aggregate$Group.2,sopita_aggregate$Group.3),]
names(sopita_aggregate_order)[1] <- "date"
names(sopita_aggregate_order)[2] <- "hour"
names(sopita_aggregate_order)[3] <- "minute"
names(sopita_aggregate_order)[4] <- "count"

#Create Time Stamp
sopita_aggregate_order$time_stamp_char1 <- paste(sopita_aggregate_order$hour,sopita_aggregate_order$minute,sep=":")
sopita_aggregate_order$time_stamp_char2 <- paste(sopita_aggregate_order$date,sopita_aggregate_order$time_stamp_char1,sep=" ")
sopita_aggregate_order$timestamp <-  strptime(sopita_aggregate_order$time_stamp_char2,"%Y-%m-%d %H:%M")

sopita_dr <- sopita_aggregate_order[,c('timestamp','count')] 

# Complete time sequences
timestamps <- seq(ISOdate(2016,6,9,0), ISOdate(2016,7,14,22,29),by = "min")
df <- data.frame(matrix(ncol = 1, nrow = 51750))
df$matrix.ncol...1..nrow...51750. <- timestamps
names(df) <- "timestamp"


merge_saving <- merge(sopita_dr,df,by=c("timestamp"),all=T)
date_subset <- subset(merge_saving,as.Date(merge_saving$timestamp)>"2016-06-09")


sopita_dr$date.time <- with(sopita_dr,as.POSIXct(sopita_dr$timestamp,format="%Y-%m-%d %H:%M"))
full.time    <- with(sopita_dr,seq(date.time[1],tail(date.time,1),by=60))

df.zoo <- zoo(sopita_dr[,2],sopita_dr$date.time)        # convert to zoo object
result <- na.approx(df.zoo,xout=full.time)  # interpolate; result is also a zoo object
head(result)

zooToDf <- function(z) {
  df <- as.data.frame(z) 
  df$Date <- time(z) #create a Date column
  rownames(df) <- NULL #so row names not filled with dates
  df <- df[,c(ncol(df), 1:(ncol(df)-1))] #reorder columns so Date first
  return(df)
}

result_dr <- zooToDf(result)
names(result_dr)[1] <- "timestamp"
names(result_dr)[2] <- "count"

res = AnomalyDetectionTs(result_dr, max_anoms=0.10, direction='both', plot=TRUE,longterm=TRUE)
res$plot




########################################






###### Day Subset of the Data #####

analyze <- subset(sopita,sopita$date == "2016-07-12")
analyze$timestamp <- analyze$datetime
analyze$count <- analyze$load_all
analyze$date <- as.Date(analyze$timestamp)
analyze$hour <- hour(analyze$timestamp)
analyze$minute <- minute(analyze$timestamp)

analyze_aggregate <- aggregate(analyze$count,by=list(analyze$date,analyze$hour,analyze$minute),FUN=mean)
analyze_aggregate$time_stamp_char1 <- paste(analyze_aggregate$Group.2,analyze_aggregate$Group.3,sep=":")
analyze_aggregate$time_stamp_char2 <- paste(analyze_aggregate$Group.1,analyze_aggregate$time_stamp_char1,sep=" ")
analyze_aggregate$timestamp <-  strptime(analyze_aggregate$time_stamp_char2,"%Y-%m-%d %H:%M")

keep <- analyze_aggregate[,c('timestamp','x')] 
names(keep)[2] <- "count"
new <- na.omit(keep)

######

res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE,longterm=TRUE)
res$plot

res = AnomalyDetectionTS(new, max_anoms=0.02, direction='both', plot=TRUE, longterm=TRUE)
res$plot

AnomalyDetectionVec(views$count, max_anoms=0.05, direction='both', plot=TRUE, period=7)

res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
res$plot


