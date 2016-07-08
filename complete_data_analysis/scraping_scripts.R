# First we have to install the package

get.data <- function(days.list) {

library(XML)
library(timeDate)

#Creating a variable called url
base_url <- "http://www.cndc.org.ni/consultas/reportesDiarios/postDespacho/consultaPostdespacho.php?fecha="

scrape_list <- format(days.list, format="%d/%m/%Y")

get_day_data <- function(date) {
	
	print(paste("proceessing", date))
	#Getting the first part of the URL
	url <- paste(base_url,date, sep = "")
	this_day <- readHTMLTable(url)
	this_day <- this_day$GeneracionXAgente
	this_day <- this_day[3:nrow(this_day),]

	#Renaming the columns of our data
	colnames(this_day) <- c("hora","ABR","PCG1","PCG2","PCG3", "PCG4", "PCG5", "PCG6", "PCG7", "PCG8", "PCG9", "PHC1", "PHC2", "AMY1", "AMY2", "PBP", "CEN", "EEC", "EEC20", "EOL", "PNI1", "PNI2","MTL" ,"GSR", "HEM","HPA1", "HPA2", "PHD","MTR", "NSL","PCA1","PCA2","PCF1", "PCF2", "PEN3","PEN4","PHL1","PHL2","PLB1", "PLB2", "PMG3", "PMG4", "PMG5","PMT1", "PMT2","PMPT3","TPC" ,"LNI-L9040", "SND-L9090","AMY-L9030","TCPI-L9150","Demanda")
	
	this_day$date <- date
	return(this_day)
}

#dates <- scrape_list[1:10]
dates<- scrape_list
data <- NULL
for(i in dates) {
	df <- get_day_data(i)
	data <- rbind(data, df)
}


#data$hora <- as.numeric(data$hora)
#date
d2 <- apply(data[,1:ncol(data)-1], 2, function(x) as.numeric(x))
data2 <- cbind(data.frame(d2), data$date)

#mdata <- melt(data2, id=c("hora","data$date")) 
#names(mdata)[2] <- "date"
#names(mdata)[3] <- "plant"
#names(mdata)[4] <- "generation"


#Creating labels for all plants
#mdata$plant <- as.character(mdata$plant)

data2$Viento <- data2$AMY1 + data2$AMY2 + data2$ABR + data2$EOL + data2$PBP
data2$Biomasa <- data2$MTR + data2$NSL + data2$MTL
data2$Geotermia <- data2$PEN3 + data2$PEN4 + data2$PMT1 + data2$PMT2 + data2$PMPT3 
data2$Hidroelectrica <- data2$PCA1 + data2$PCA2 + data2$PCF1 + data2$PCF2 + data2$HPA1 + data2$HPA2 + data2$HEM + data2$PHD + data2$PHL1 + data2$PHL2
data2$Interconexion <- data2$LNI.L9040 + data2$SND.L9090 + data2$AMY.L9030 + data2$TCPI.L9150
data2$Petroleo <- data2$PCG1 + data2$PCG2 + data2$PCG3 + data2$PCG4 + data2$PCG5 + data2$PCG6 + data2$PCG7 + data2$PCG8 + data2$PCG9 + data2$PHC1 + data2$PHC2 + data2$CEN + data2$EEC20 + data2$EEC + data2$TPC + data2$PLB1 + data2$PLB2 + data2$PMG3 + data2$PMG4 + data2$PMG5  + data2$PNI1 + data2$PNI2 + data2$GSR
data2$sum_resources <- data2$Viento + data2$Biomasa + data2$Geotermia + data2$Hidroelectrica + data2$Petroleo


# Some Analysis

# Data frame for Grid Plot 1
data2$NetDemanda <- data2$Demanda - data2$Viento

d_netd_wind <- aggregate(cbind(data2$Demanda,data2$Viento,data2$NetDemanda),by=list(data2$hora),FUN=mean,na.rm=TRUE)
names(d_netd_wind)[1] <- "Hora"
names(d_netd_wind)[2] <- "Demanda"
names(d_netd_wind)[3] <- "Viento"
names(d_netd_wind)[4] <- "Net-Demanda"

melted_dfwind <- melt(d_netd_wind, id=c("Hora"))  %>% mutate(resource=variable,generation=value) %>% select(Hora,resource,generation)

#Data frame for Grid Plot 2

barinfo <- data2[,c('hora','Viento','Biomasa','Geotermia','Hidroelectrica','Interconexion','Petroleo','sum_resources')]
barinfomelt <- melt(barinfo,id=c('hora'))
barinfomelt2 <- aggregate(barinfomelt$value,by=list(barinfomelt$variable),FUN=sum) %>% mutate(resource=Group.1,sum=x) %>% select(resource,sum)

barinfomelt2$pct <- NA

barinfomelt2$pct[1] <- (barinfomelt2$sum[1]/ barinfomelt2$sum[7])*100
barinfomelt2$pct[2] <- (barinfomelt2$sum[2]/ barinfomelt2$sum[7])*100
barinfomelt2$pct[3] <- (barinfomelt2$sum[3]/ barinfomelt2$sum[7])*100
barinfomelt2$pct[4] <- (barinfomelt2$sum[4]/ barinfomelt2$sum[7])*100
barinfomelt2$pct[5] <- (abs(barinfomelt2$sum[5])/ barinfomelt2$sum[7])*100
barinfomelt2$pct[6] <- (barinfomelt2$sum[6]/ barinfomelt2$sum[7])*100

percent_resources <- subset(barinfomelt2,barinfomelt2$resource != "sum_resources")
percent_resources$resources <- "Recurso"

return(list(melted_dfwind,percent_resources))
}

