library(data.table)  # faster fread() and better weekdays()
library(dplyr)       # consistent data.frame operations
library(purrr)       # consistent & safe list/vector munging
library(tidyr)       # consistent data.frame cleaning
library(lubridate)   # date manipulation
library(countrycode) # turn country codes into pretty names
library(ggplot2)     # base plots are for Coursera professors
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(knitr)       # kable : prettier data.frame output
library(plyr)
library(reshape2)
library(viridis)

#####



###### 1 Temperature deadband

###### Bringing in the survey data
survey_data <- read.csv('Data/Nicaragua/implementation_nicaragua/all_data/all_survey_data_complete.csv')
names(survey_data)[names(survey_data)=="flexbox_id"] <- "house.id"

#Subset data

survey_data_vars <- survey_data[,c('house.id','tipo_encuesta')]
complete_data <- join(inside,survey_data_vars,by=c("house.id"),type='left')

house_temp_data <- subset(complete_data,complete_data$tipo_encuesta == 'casa')
me_temp_data <- subset(complete_data,complete_data$tipo_encuesta == 'micro_empresa')

# Plotting 

sub_house <- subset(house_temp_data,house_temp_data$fridge_temp1 < 25)
sub_house$fridge_temp1 <- sub_house$fridge_temp1*1000

sub_me <- subset(me_temp_data,me_temp_data$fridge_temp1 < 25)
sub_me$fridge_temp1 <- sub_me$fridge_temp1*1000

house_temp_density <- ggplot(data=sub_house, aes(x=fridge_temp1, fill=house.id)) + geom_density(alpha=.3) +  xlim(0, 40)+ labs(title = "Household") + xlab(expression(paste("Inside Temperature",degree,"C"))) + theme(panel.background = element_blank()) + theme(legend.position="none")
me_temp_density <- ggplot(data=sub_me, aes(x=fridge_temp1, fill=house.id)) + geom_density(alpha=.3) +   xlim(-20, 40) + labs(title = "Micro-Enterprise") + xlab(expression(paste("Inside Temperature",degree,"C"))) + theme(panel.background = element_blank()) + theme(legend.position="none")
grid.arrange(house_temp_density,me_temp_density)












#1 Fridge Energy Hour



f.energy.hour <- function(data_input) {
  
  #Getting rid of zeros and NaNs  
  data_input <- date.vars.simple(data_input)
  unique.data.input <-  unique(data_input[c("house.id","date","hour","energy_sum3")])
  unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  
  
  #Making sure that we are keeping non-negative and non-outlier data
  unique.microids <- unique(unique.data.input$house.id)
  microids.df <- list()
  
  #Getting rid of outlier data
  for (i in 1:length(unique.microids)) {
    iterate.df <- subset(unique.data.input,unique.data.input$house.id == unique.microids[i])
    iterate.df$lagged <-  lag(iterate.df$energy_sum3,k=1)
    iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
    iterate.df$diff <- iterate.df$energy_sum3 - iterate.df$lagged
    microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
  }
  
  microid.data.input <- rbind(microids.df[[1]],microids.df[[2]],microids.df[[3]],microids.df[[4]],microids.df[[5]],microids.df[[6]],microids.df[[7]],microids.df[[8]],microids.df[[9]],microids.df[[10]],microids.df[[11]],microids.df[[12]],microids.df[[13]],microids.df[[14]],microids.df[[15]],microids.df[[16]],microids.df[[17]],microids.df[[18]],microids.df[[19]],microids.df[[20]],microids.df[[21]],microids.df[[22]],microids.df[[23]],microids.df[[24]],microids.df[[25]],microids.df[[26]],microids.df[[27]],microids.df[[28]])
  
  #Setting unique combinations and values so that we can iterate
  unique.combinations <- unique(microid.data.input[c("house.id","date","hour")])
  unique.microids <- unique(unique.combinations$house.id)
  unique.subset <- c()
  count <- 0

  for (i in 1:length(unique.microids)) {
    
    subset.microids.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
    unique.dateys <- unique(subset.microids.df$date)
    
    for (j in 1:length(unique.dateys)) {
      
      subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date == unique.dateys[j])
      unique.hours <- unique(subset.hours.df$hour)
      
      for (k in 1:length(unique.hours)) {
        count <- count + 1
        subset.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date == unique.dateys[j]  & microid.data.input$hour == unique.hours[k])
        subset.df$energy_sum3 <- subset.df$energy_sum3/1000
        unique.subset[count] <- tail(subset.df$energy_sum3,1) - head(subset.df$energy_sum3,1)
        
      }
    }
  }
  
  fenergy.hourly.consumption <- cbind(unique.combinations,unique.subset) %>% mutate(fridge.energy = unique.subset) %>% select(house.id,date,hour,fridge.energy)
  fenergy.hourly.consumption <- subset(fenergy.hourly.consumption,fenergy.hourly.consumption$fridge.energy >= 0)
  return(fenergy.hourly.consumption)
}


#Normalize data
unique.microids <- unique(fenergy.hourly.consumption$house.id)
fridge.energy.hour <- fenergy.hourly.consumption

for (i in 1:length(unique.microids)){
  
  edata.df <- data.table(fridge.energy.hour)[house.id==unique.microids[i]]
  fridge.energy.mean <- data.table(edata.df)[, list(freq=.N, tot=mean(fridge.energy,na.rm=TRUE)), by=hour]
  fridge.energy.mean$e.norm <- (fridge.energy.mean$tot - min(fridge.energy.mean$tot))/(max(fridge.energy.mean$tot)-min(fridge.energy.mean$tot))
  fridge.energy.mean$house.id <- unique.microids[i]
  
  if (i==1){
    fridge.energy.mean.df <- fridge.energy.mean
  } else {
    fridge.energy.mean.df <- rbind(fridge.energy.mean.df,fridge.energy.mean)
  }
}

fridge.df <- as.data.frame(fridge.energy.mean.df)

fridge.df <- fridge.df[,c("hour","house.id","e.norm")]

fridge.df$id.house <- as.numeric(gsub("^.*?A","",fridge.df$house.id))

  


#Heat Map Palette
e.hour <- ggplot(fridge.df,aes(x=id.house,y=hour,fill=e.norm))
e.hour <- e.hour + geom_tile(color="white",size=0.1)
e.hour <- e.hour + scale_fill_viridis(name="Energy Consumption", label=comma,na.value="white")
e.hour <- e.hour + coord_equal()
e.hour <- e.hour + labs(x="Micro ID", y="Hour of the Day", title="Fridge Energy Consumption by Hour (Normalized 0-1 by Micro ID )")
e.hour <- e.hour + theme(plot.title=element_text(hjust=0))
e.hour <- e.hour + theme(axis.ticks=element_blank())
e.hour <- e.hour + theme(axis.text=element_text(size=9))
e.hour <- e.hour + theme(legend.title=element_text(size=9))
e.hour <- e.hour + theme(legend.text=element_text(size=8)) + theme(panel.background = element_blank())
e.hour 












##### Thermal parameters



# Creating Time DataFrame
time.sec <- list(0:59)[[1]]
time.minute <- list(0:59)[[1]]
time.hour <- list(0:23)[[1]]

hour <- rep(time.hour, each=3600)
minute <- rep(time.minute,times=24,each=60)
second <- rep(time.sec,times=1440)

time.df <- as.data.frame(cbind(hour,minute,second)) 
time.df$time.id <- 1:86400

power.limits <- c(0,0)

#
# Intuitive A1 A14
# Weird A5 A7 A8 A9 A13 
# Non Intuitive but similar A2 A3 A4 A6 A9 A11 A12 A15


# Initializing lists and data 
data_input <- as.data.table(date.vars.simple(refrigerator))
data_inside <- as.data.table(date.vars.simple(inside))
unique.microids <- unique(data_input$house.id)
unique.microids <- c('A1')
 
i <- 1
j <-6

for (i in 1:length(unique.microids)) {
  date.dt <- data_input[house.id==unique.microids[i]] #Subsetting with data table
  date.inside <- data_inside[house.id==unique.microids[i]]
  unique.dates <- unique(date.dt$date)
  
  for (j in 1:length(unique.dates)) {

    date.dt <- data_input[house.id==unique.microids[i] & date==unique.dates[j]] #Subsetting with data table
    date.inside <-  data_inside[house.id==unique.microids[i] & date==unique.dates[j]]
    par(mfrow=c(2,1)) 
    plot(1: length(date.inside$fridge_temp1),date.inside$fridge_temp1)
    plot(1:length(date.dt$active_pwr3),date.dt$active_pwr3)
    par(mfrow=c(1,1))
    #lines(1:length(date.dt$active_pwr3),date.dt$active_pwr3)
    
    data.analysis <- join(date.dt,time.df,by=c('hour','minute','second'),type="right",match="first") 
    data.analysis.inside <- join(date.inside,time.df,by=c('hour','minute','second'),type="right",match="first")
    
    energy.sub <- date.dt %>% select(active_pwr3,second,minute,hour)
    energy.sub <- as.data.frame(energy.sub)
    energy.sub <- unique(energy.sub[c("active_pwr3", "second","minute","hour")])
    inside.sub <- date.inside %>% select(fridge_temp1,second,minute, hour)
    inside.sub <- as.data.frame(inside.sub)
    inside.sub <- unique(inside.sub[c("fridge_temp1", "second","minute","hour")])
    
    
    data.cop <- join(energy.sub,inside.sub,by=c('hour','minute'),type='left',match='first')
    data.cop.time <- join(data.cop,time.df,by=c('hour','minute','second'),type="left",match="first")

    
    
    par(mfrow=c(2,1)) 
    plot(1:length(data.cop$fridge_temp1),data.cop$fridge_temp1)
    plot(1:length(data.cop$active_pwr3),data.cop$active_pwr3)
    #lines(data.analysis$time.id,data.analysis$active_pwr3)
    

    # Quick sum by hour to calculate number of hours that it's off 
    times.off <- data.table(data.analysis)[, list(freq=.N, tot=sum(active_pwr3,na.rm=TRUE)), by=hour] %>% subset(tot <=0) %>% mutate(count.id =1)
    hours.off <- sum(times.off$count.id)
    list.hours <- as.data.frame(times.off$hour) %>% mutate(house.id=unique.microids[i],date=unique.dates[j])
    
    # Complete cases
    completeVec <- complete.cases(data.analysis[, "active_pwr3"])
    data <- as.data.table(data.analysis[completeVec, ])
    
    data$lagged <- lag(data$active_pwr3,k=1)
    data$diff <- data$active_pwr3 - data$lagged
    plot(1:length(data$active_pwr3), data$active_pwr3)
    #lines(data.analysis$time.id,data.analysis$active_pwr3)
    
    #plot(1:length(data$diff), data$diff)
    #lines(data.analysis$time.id,data.analysis$active_pwr3)
    
    if (unique.microids[i] == 'A1' | unique.microids[i] == 'A5' | unique.microids[i] == 'A7' | unique.microids[i] == 'A8' | unique.microids[i] == 'A9' | unique.microids[i] == 'A14' | unique.microids[i] == 'A18' | unique.microids[i] == 'A22' | unique.microids[i] == 'A25') {
    
    if (unique.microids[i] == 'A1'| unique.microids[i] == 'A5' | unique.microids[i] == 'A8' | unique.microids[i] == 'A22') {
      data <- data[active_pwr3 < 200]  
    } else {
    # Quantiles
    quantile.data <- quantile(data$diff, c(.002, .999),na.rm=TRUE) 
    subset.data <- data[diff>quantile.data[2] | data$diff < quantile.data[1]]
    subset.data$lag.time <- lag(subset.data$time.id,k=1) # Lagging data
    #plot(1:length(subset.data$active_pwr3), subset.data$active_pwr3)
    #plot(1:length(subset.data$diff), subset.data$diff)
    #lines(subset.data$time.id,data.analysis$active_pwr3)
    
    subset.data$compressor.on <- subset.data$time.id - subset.data$lag.time #Creating time diff variable
    compressoron.df <- subset.data[diff<0]  %>% mutate(compressor.on=compressor.on/60) #Compressor on data.table
    compressoroff.df <- subset.data[diff>0]  %>% mutate(compressor.off=compressor.on/60) #Compressor off data.table
    
    compressor.analysis <- cbind(compressoron.df[, .(hour, compressor.on)],compressoroff.df[,.(hour,compressor.off)]) %>%  mutate(duty.cycle = (compressor.on/(compressor.on + compressor.off))) 
    compressor.analysis <- compressor.analysis[complete.cases(compressor.analysis),] 
    }
    } else {
      
      # i = 2
      compressor.on.secs <- subset(data,data$active_pwr3>150)
      compressor.on.secs$unit <- 1
      duty.cycle.a2 <- sum(compressor.on.secs$unit)/(dim(data)[1])
      
      # i = 3
      compressor.on.secs <- subset(data,data$active_pwr3>75)
      compressor.on.secs$unit <- 1
      duty.cycle.a3 <- sum(compressor.on.secs$unit)/(dim(data)[1])
      
      # i = 4
      compressor.on.secs <- subset(data,data$active_pwr3>200)
      compressor.on.secs$unit <- 1
      duty.cycle.a4 <- sum(compressor.on.secs$unit)/(dim(data)[1])
      
      # i = 6
      compressor.on.secs <- subset(data,data$active_pwr3>100)
      compressor.on.secs$unit <- 1
      duty.cycle.a6 <- sum(compressor.on.secs$unit)/(dim(data)[1])
      
      # i = 10
      plot(data$time.id,data$active_pwr3)
      first.time <- data[active_pwr3 <200 & time.id < 40000]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      second.time <- data[active_pwr3 <100 & time.id >40000]
      length.second <- tail(second.time$time.id,1) - head(second.time$time.id,1)
      compressor.on.secs <- data[active_pwr3 >200]
      compressor.on.secs <- tail(compressor.on.secs$time.id,1) - head(compressor.on.secs$time.id,1)
      duty.cycle.a10 <- compressor.on.secs/(length.first+length.second+compressor.on.secs)
      
      # i = 11
      plot(data$time.id,data$active_pwr3)
      first.time <- data[active_pwr3 <400 & time.id < 60000]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      second.time <- data[active_pwr3 <400 & time.id >60000]
      length.second <- tail(second.time$time.id,1) - head(second.time$time.id,1)
      compressor.on.secs <- data[active_pwr3 >200]
      compressor.on.secs <- tail(compressor.on.secs$time.id,1) - head(compressor.on.secs$time.id,1)
      duty.cycle.a11 <- compressor.on.secs/(length.first+length.second+compressor.on.secs)
      
      
      # i = 12
      plot(data$time.id,data$active_pwr3)
      first.time <- data[active_pwr3 > 100 & time.id < 20000]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      second.time <- data[active_pwr3 >100 & time.id >60000]
      length.second <- tail(second.time$time.id,1) - head(second.time$time.id,1)
      compressor.on.secs <- data[active_pwr3 <100]
      compressor.on.secs <- tail(compressor.on.secs$time.id,1) - head(compressor.on.secs$time.id,1)
      duty.cycle.a12 <- (length.first+length.second)/(length.first+length.second+compressor.on.secs)
      
      # i = 13
      plot(data$time.id,data$active_pwr3)
      first.time <- data[time.id < 20000]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      second.time <- data[time.id >60000]
      length.second <- tail(second.time$time.id,1) - head(second.time$time.id,1)
      compressor.on.secs <- data[time.id >20000 & time.id < 60000]
      compressor.on.secs <- tail(compressor.on.secs$time.id,1) - head(compressor.on.secs$time.id,1)
      duty.cycle.a13 <- (length.first+length.second)/(length.first+length.second+compressor.on.secs)
      
      # i = 15
      compressor.on.secs <- subset(data,data$active_pwr3>100)
      compressor.on.secs$unit <- 1
      duty.cycle.a15 <- sum(compressor.on.secs$unit)/(dim(data)[1])
      
      # i = 16
      plot(data$time.id,data$active_pwr3)
      first.time <- data[active_pwr3 <100 & time.id <20000]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      second.time <- data[active_pwr3 <100 & time.id >6000]
      length.second <- tail(second.time$time.id,1) - head(second.time$time.id,1)
      compressor.on.secs <- data[active_pwr3 >200]
      compressor.on.secs <- tail(compressor.on.secs$time.id,1) - head(compressor.on.secs$time.id,1)
      duty.cycle.a16 <- compressor.on.secs/(length.first+length.second+compressor.on.secs)
      
      # i = 17
      plot(data$time.id,data$active_pwr3)
      first.time <- data[time.id >20000 & time.id <80000 & active_pwr3<100]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      com.1 <- data[active_pwr3 >100 & time.id <20000]
      comp.1 <- tail(com.1$time.id,1) - head(com.1$time.id,1)
      com.2 <- data[active_pwr3 >100 & time.id >70000]
      comp.2 <- tail(com.2$time.id,1) - head(com.2$time.id,1)
      duty.cycle.a17 <- (comp.1+comp.2)/(length.first+comp.1+comp.2)
      
      # i = 19
      plot(data$time.id,data$active_pwr3)
      first.time <- data[time.id >200 & time.id <30000]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      second.time <- data[time.id >200 & time.id <55000]
      length.second<- tail(second.time$time.id,1) - head(second.time$time.id,1)
      
      com.1 <- data[active_pwr3 >200]
      comp.1 <- tail(com.1$time.id,1) - head(com.1$time.id,1)
      duty.cycle.a19 <- (comp.1)/(length.first + length.second + comp.1)
      
      # i = 20
      plot(data$time.id,data$active_pwr3)
      first.time <- data[active_pwr3 >50 & time.id <50000]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      second.time <- data[time.id >50 & time.id >60000]
      length.second<- tail(second.time$time.id,1) - head(second.time$time.id,1)
      
      com.1 <- data[active_pwr3 < 50]
      comp.1 <- tail(com.1$time.id,1) - head(com.1$time.id,1)
      duty.cycle.a20 <- (length.first+length.second)/(length.first+length.second+comp.1)
      
      # i = 21
      plot(data$time.id,data$active_pwr3)
      first.time <- data[active_pwr3 >100 & time.id <70000]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      second.time <- data[active_pwr3 >100 & time.id >70000]
      length.second<- tail(second.time$time.id,1) - head(second.time$time.id,1)
      
      com.1 <- data[active_pwr3 < 50]
      comp.1 <- tail(com.1$time.id,1) - head(com.1$time.id,1)
      duty.cycle.a21 <- (length.first+length.second)/(length.first+length.second+comp.1)
      
      
      # i = 24
      plot(data$time.id,data$active_pwr3)
      first.time <- data[active_pwr3 <150 & time.id <40000]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      second.time <- data[active_pwr3 <150 & time.id >60000]
      length.second<- tail(second.time$time.id,1) - head(second.time$time.id,1)
      
      com.1 <- data[active_pwr3 > 150]
      comp.1 <- tail(com.1$time.id,1) - head(com.1$time.id,1)
      duty.cycle.a24 <- (comp.1)/(length.first+length.second+comp.1)
      
      # i = 27
      plot(data$time.id,data$active_pwr3)
      first.time <- data[active_pwr3 >200 & time.id <40000]
      length.first <- tail(first.time$time.id,1) - head(first.time$time.id,1)
      second.time <- data[active_pwr3 >200 & time.id >30000]
      length.second<- tail(second.time$time.id,1) - head(second.time$time.id,1)
      
      com.1 <- data[active_pwr3 < 150]
      comp.1 <- tail(com.1$time.id,1) - head(com.1$time.id,1)
      duty.cycle.a27 <- (length.first+length.second)/(length.first+length.second+comp.1)
      
      
    } 
  
    if (j==1){
      list.hours.df <- list.hours
      compressor.analysis.df <- compressor.analysis
    } else {
      list.hours.df <- rbind(list.hours.df,list.hours)
      compressor.analysis.df <- rbind(compressor.analysis.df,compressor.analysis)
      }
  }
}


  duty.cycle.a5 <- compressor.analysis.df[, list(dut.av=mean(duty.cycle,na.rm=TRUE)), by=hour]
  dut.cy.order.a5 <- duty.cycle.a5[order(duty.cycle.a5$hour),]
  
  duty.cycle.a7 <- compressor.analysis.df[, list(dut.av=mean(duty.cycle,na.rm=TRUE)), by=hour]
  dut.cy.order.a7 <- duty.cycle.a7[order(duty.cycle.a7$hour),]
  
  duty.cycle.a8 <- compressor.analysis.df[, list(dut.av=mean(duty.cycle,na.rm=TRUE)), by=hour]
  dut.cy.order.a8 <- duty.cycle.a8[order(duty.cycle.a8$hour),]
  
  duty.cycle.a9 <- compressor.analysis.df[, list(dut.av=mean(duty.cycle,na.rm=TRUE)), by=hour]
  dut.cy.order.a9 <- duty.cycle.a9[order(duty.cycle.a9$hour),]
  
  duty.cycle.a14 <- compressor.analysis.df[, list(dut.av=mean(duty.cycle,na.rm=TRUE)), by=hour]
  dut.cy.order.a14 <- duty.cycle.a14[order(duty.cycle.a14$hour),]
  
  duty.cycle.a18 <- compressor.analysis.df[, list(dut.av=mean(duty.cycle,na.rm=TRUE)), by=hour]
  dut.cy.order.a18 <- duty.cycle.a18[order(duty.cycle.a18$hour),]
  
  duty.cycle.a22 <- compressor.analysis.df[, list(dut.av=mean(duty.cycle,na.rm=TRUE)), by=hour]
  dut.cy.order.a22 <- duty.cycle.a22[order(duty.cycle.a22$hour),]
  
  duty.cycle.a25 <- compressor.analysis.df[, list(dut.av=mean(duty.cycle,na.rm=TRUE)), by=hour]
  dut.cy.order.a25 <- duty.cycle.a25[order(duty.cycle.a25$hour),]
  

  
  #A18 is wrong


  
  
  #dut.cy <- compressor.analysis.df[, list(dut.av=mean(duty.cycle,na.rm=TRUE)), by=hour] 
  dut.cy.order <- dut.cy[order(dut.cy$hour),]
  dut.cy.order$duty.a2 <- duty.cycle.a2
  dut.cy.order$duty.a3 <- duty.cycle.a3
  dut.cy.order$duty.a4 <- duty.cycle.a4
  dut.cy.order <- join(dut.cy.order,dut.cy.order.a5,by=c('hour'))
  dut.cy.order$duty.a6 <- duty.cycle.a6
  dut.cy.order <- join(dut.cy.order,dut.cy.order.a7,by=c('hour')) 
  dut.cy.order <- join(dut.cy.order,dut.cy.order.a8,by=c('hour')) 
  dut.cy.order <- join(dut.cy.order,dut.cy.order.a9,by=c('hour')) 
  dut.cy.order$duty.a10 <- duty.cycle.a10
  dut.cy.order$duty.a11 <- duty.cycle.a11
  dut.cy.order$duty.a12 <- duty.cycle.a12
  dut.cy.order$duty.a13 <- duty.cycle.a13
  dut.cy.order <- join(dut.cy.order,dut.cy.order.a14,by=c('hour')) 
  dut.cy.order$duty.a15 <- duty.cycle.a15
  dut.cy.order$duty.a16 <- duty.cycle.a16
  dut.cy.order$duty.a17 <- duty.cycle.a17
  dut.cy.order <- join(dut.cy.order,dut.cy.order.a18,by=c('hour')) 
  dut.cy.order$duty.a19 <- duty.cycle.a19
  dut.cy.order$duty.a20 <- duty.cycle.a20
  dut.cy.order$duty.a21 <- duty.cycle.a21
  dut.cy.order<- join(dut.cy.order,dut.cy.order.a22,by=c('hour')) 
  dut.cy.order$duty.a23 <- 1
  dut.cy.order$duty.a24 <- duty.cycle.a24
  dut.cy.order<- join(dut.cy.order,dut.cy.order.a25,by=c('hour')) 
  dut.cy.order$duty.a27 <- duty.cycle.a27
  dut.cy.order$duty.a28 <- 1
  dut.cy.order$duty.a29 <- 1
  
  
  

var.names <- c("dut.av","duty.a2" , "duty.a3",  "duty.a4" , "dut.av" ,  "duty.a6" , "dut.av" ,  "dut.av",   "dut.av", "duty.a10" ,"duty.a11", "duty.a12" ,"duty.a13", "dut.av" ,  "duty.a15", "duty.a16", "duty.a17" ,"dut.av" ,  "duty.a19", "duty.a20" ,"duty.a21", "dut.av"  , "duty.a23", "duty.a24" ,"dut.av" ,  "duty.a27" ,"duty.a28", "duty.a29")
duty_subset <- as.data.frame(dut.cy.order)[,var.names]

duty.names <- dut.cy.order

names(duty.names)[2] <- "A1"
names(duty.names)[3] <- "A2"
names(duty.names)[4] <- "A3"
names(duty.names)[5] <- "A4"
names(duty.names)[6] <- "A5"
names(duty.names)[7] <- "A6"
names(duty.names)[8] <- "A7"
names(duty.names)[9] <- "A8"
names(duty.names)[10] <- "A9"
names(duty.names)[11] <- "A10"
names(duty.names)[12] <- "A11"
names(duty.names)[13] <- "A12"
names(duty.names)[14] <- "A13"
names(duty.names)[15] <- "A14"
names(duty.names)[16] <- "A15"
names(duty.names)[17] <- "A16"
names(duty.names)[18] <- "A17"
names(duty.names)[19] <- "A18"
names(duty.names)[20] <- "A19"
names(duty.names)[21] <- "A20"
names(duty.names)[22] <- "A21"
names(duty.names)[23] <- "A22"
names(duty.names)[24] <- "A23"
names(duty.names)[25] <- "A24"
names(duty.names)[26] <- "A25"
names(duty.names)[27] <- "A27"
names(duty.names)[28] <- "A28"
names(duty.names)[29] <- "A29"

data.melt <- melt(duty.names,id.vars='hour',measure.vars=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23","A24","A25","A27","A28","A29"))

#Heat Map Palette

gg <- ggplot(data.melt,aes(x=variable,y=hour,fill=value))
gg <- gg + geom_tile(color="white",size=0.1)
gg <- gg + scale_fill_viridis(name="Duty Cycle", label=comma)
gg <- gg + coord_equal()
gg <- gg + labs(x="Micro ID", y="Hour of the Day", title="Duty Cycle by Hour of Day")
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=9))
gg <- gg + theme(legend.title=element_text(size=9))
gg <- gg + theme(legend.text=element_text(size=8)) 
gg + theme(panel.background = element_blank())


#######################################################


# 3 Coefficient of Performance 


# Initializing lists and data 

data_input <- as.data.table(date.vars.simple(refrigerator))
data_inside <- as.data.table(date.vars.simple(inside))
unique.microids <- unique(data_input$house.id)
unique.microids <- c('A29')

i <- 1
j <- 6


#for (i in 1:length(unique.microids)) {
  date.dt <- data_input[house.id==unique.microids[i]] #Subsetting with data table
  date.inside <- data_inside[house.id==unique.microids[i]]
  unique.dates <- unique(date.dt$date)
  
  #for (j in 1:length(unique.dates)) {
    
    date.dt <- data_input[house.id==unique.microids[i] & date==unique.dates[j]] #Subsetting with data table
    date.inside <-  data_inside[house.id==unique.microids[i] & date==unique.dates[j]]
    par(mfrow=c(2,1)) 
    plot(1: length(date.inside$inside_temp1),date.inside$inside_temp1)
    plot(1:length(date.dt$active_pwr3),date.dt$active_pwr3)
    par(mfrow=c(1,1))
    #lines(1:length(date.dt$active_pwr3),date.dt$active_pwr3)
    
    data.analysis <- join(date.dt,time.df,by=c('hour','minute','second'),type="right",match="first") 
    data.analysis.inside <- join(date.inside,time.df,by=c('hour','minute','second'),type="right",match="first")
    
    energy.sub <- date.dt %>% select(active_pwr3,energy_sum3,second,minute,hour)
    energy.sub <- as.data.frame(energy.sub)
    energy.sub <- unique(energy.sub[c("active_pwr3","energy_sum3","second","minute","hour")])
    inside.sub <- date.inside %>% select(inside_temp1,second,minute, hour)
    inside.sub <- as.data.frame(inside.sub)
    inside.sub <- unique(inside.sub[c("inside_temp1", "second","minute","hour")])
    
    
    data.cop <- join(energy.sub,inside.sub,by=c('hour','minute'),type='left',match='first')
    data.cop.time <- join(data.cop,time.df,by=c('hour','minute','second'),type="left",match="first")
    
    data.cop.time$lagged <- lag(data.cop.time$active_pwr3,k=1)
    data.cop.time$diff <- data.cop.time$active_pwr3 - data.cop.time$lagged

    
    
    par(mfrow=c(2,1)) 
    plot(1:length(data.cop.time$inside_temp1),data.cop.time$inside_temp1)
    plot(1:length(data.cop.time$active_pwr3),data.cop.time$active_pwr3)
    #lines(data.analysis$time.id,data.analysis$active_pwr3)
    
    
    if (unique.microids[i] == 'A1' | unique.microids[i] == 'A5' | unique.microids[i] == 'A7' | unique.microids[i] == 'A8' | unique.microids[i] == 'A9' | unique.microids[i] == 'A14' | unique.microids[i] == 'A18' | unique.microids[i] == 'A22' | unique.microids[i] == 'A25') {
      
      if (unique.microids[i] == 'A1'| unique.microids[i] == 'A5' | unique.microids[i] == 'A8' | unique.microids[i] == 'A22') {
        data <- data.table(data.cop.time)[active_pwr3 < 200 & inside_temp1 < 30]  
        par(mfrow=c(1,1))
        par(mfrow=c(2,1)) 
        plot(1:length(data$inside_temp1), data$inside_temp1)
        plot(1:length(data$active_pwr3), data$active_pwr3)
      } else {
        # Quantiles
       
        
        if (subset.data$house.id == 'A2' | subset.data$house.id == 'A3' | subset.data$house.id == 'A4') {
          
          #A1
          quantile.data <- quantile(data$diff, c(.019, .99),na.rm=TRUE) 
          subset.data <- data[diff>quantile.data[2] | data$diff < quantile.data[1]]
          
          plot(1:length(subset.data$inside_temp1), subset.data$inside_temp1)
          plot(1:length(subset.data$diff), subset.data$diff)
          
          sub2 <- subset.data[abs(diff) > 100]
          plot(1:length(sub2$diff), sub2$diff)
          
          sub2$temp.lagged <- lag(sub2$inside_temp1,k=1)
          sub2$temp.diff <- sub2$inside_temp1 - sub2$temp.lagged
          
          sub2$e.lagged <- lag(sub2$energy_sum3,k=1)
          sub2$e.diff <- sub2$energy_sum3 - sub2$e.lagged
          
          keep.cop <- subset(sub2,sub2$temp.diff<0)
          
          keep.cop$cop <- ((1000  * 0.8  * 4200 * abs(keep.cop$temp.diff) * 0.25) + (1.225*0.8*1000* abs(keep.cop$temp.diff) * 0.75))/(keep.cop$e.diff*3600)
          
          a1.cop <- keep.cop[,list(hour,cop)]
          
          cop.df = data.frame(matrix(vector(), 24, 1))
          cop.df$X1 <- 0:23
          names(cop.df)[2] <- "hour"
          cop.df <- join(cop.df,a1.cop,by=c('hour'),type='left',match='first') %>% mutate(A1=cop) %>% select(hour,A1)
          
          
          
          #A2
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          plot(data.cop.time$new.id,data.cop.time$inside_temp1)
          subset.data <- subset(data.cop.time, data.cop.time$new.id > 550 & data.cop.time$new.id <930)
          plot(subset.data$new.id,subset.data$inside_temp1)
          
          cop.a2<- ((1000  * 0.8  * 4200 * abs(head(subset.data$inside_temp1,1) - 0) * 0.25) + (1.225*0.8*1000* abs(head(subset.data$inside_temp1,1) - 0) * 0.75))/((tail(subset.data$energy_sum3,1)- head(subset.data$energy_sum3,1)) *3600)
          
          cop.a2.df <-  data.frame(matrix(vector(), 5, 1))
          names(cop.a2.df)[1] <- "hour"
          cop.a2.df$hour <- 8:12
          cop.a2.df$A2 <- cop.a2
          
          #A3
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          plot(data.cop.time$new.id,data.cop.time$inside_temp1)
          subset.data <- subset(data.cop.time, data.cop.time$new.id >= 500 & data.cop.time$new.id <1050)
          plot(subset.data$new.id,subset.data$inside_temp1)
          
          cop.a3 <- ((1000  * 0.8  * 4200 * abs(head(subset.data$inside_temp1,1) - 0) * 0.25) + (1.225*0.8*1000* abs(head(subset.data$inside_temp1,1) - 0) * 0.75))/((tail(subset.data$energy_sum3,1)- head(subset.data$energy_sum3,1)) *3600)
          cop.a3.df <-  data.frame(matrix(vector(), 11, 1))
          names(cop.a3.df)[1] <- "hour"
          cop.a3.df$hour <- 10:20
          cop.a3.df$A3 <- cop.a3
          
          #A5
          data$new.id <- 1:length(data$diff)
          
          data1 <- subset(data,data$new.id < 400)
          quantile.data1 <- quantile(data1$diff, c(.019, .99),na.rm=TRUE) 
          subset.data1 <- data1[diff>quantile.data1[2] | data1$diff < quantile.data1[1]]
          
          subset.data1$temp.lagged <- lag(subset.data1$inside_temp1,k=1)
          subset.data1$temp.diff <- subset.data1$inside_temp1 - subset.data1$temp.lagged
          
          subset.data1$e.lagged <- lag(subset.data1$energy_sum3,k=1)
          subset.data1$e.diff <- subset.data1$energy_sum3 - subset.data1$e.lagged
          
          keep.cop1 <- subset(subset.data1,subset.data1$e.diff>1)
          keep.cop1 <- subset(keep.cop1,keep.cop1$temp.diff<0)
          
          keep.cop1$cop <- ((1000  * 0.8  * 4200 * abs(keep.cop1$temp.diff) * 0.25) + (1.225*0.8*1000* abs(keep.cop1$temp.diff) * 0.75))/(keep.cop1$e.diff*3600)
          keep.cop1 <- aggregate(keep.cop1$cop,by=list(keep.cop1$hour),FUN=mean) %>% mutate(hour=Group.1,cop=x) %>% select(hour,cop)
          
          data2 <- subset(data,data$new.id >= 650 & data$new.id <= 710)
          plot(1:length(data2$inside_temp1),data2$inside_temp1)
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1)-tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1)-tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a5.d1 <-  data.frame(matrix(vector(), 3, 1))
          names(cop.a5.d1)[1] <- "hour"
          cop.a5.d1$hour <- 11:13
          cop.a5.d1$cop <- keep.cop.d2
          
          data3 <- subset(data,data$new.id >= 730 & data$new.id <= 980)
          plot(1:length(data3$inside_temp1),data3$inside_temp1)
          keep.cop.d3 <- ((1000  * 0.8  * 4200 * abs(head(data3$inside_temp1,1)-tail(data3$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data3$inside_temp1,1)-tail(data3$inside_temp1,1)) * 0.75))/((tail(data3$energy_sum3,1)-head(data3$energy_sum3,1))*3600)
          
          cop.a5.d2 <-  data.frame(matrix(vector(), 5, 1))
          names(cop.a5.d2)[1] <- "hour"
          cop.a5.d2$hour <- 14:18
          cop.a5.d2$cop <- keep.cop.d3
          
          data4 <- subset(data,data$new.id >= 1100)
          plot(1:length(data4$inside_temp1),data4$inside_temp1)
          keep.cop.d4 <- ((1000  * 0.8  * 4200 * abs(head(data4$inside_temp1,1)-tail(data4$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data4$inside_temp1,1)-tail(data4$inside_temp1,1)) * 0.75))/((tail(data4$energy_sum3,1)-head(data4$energy_sum3,1))*3600)
          
          cop.a5.d3 <-  data.frame(matrix(vector(), 4, 1))
          names(cop.a5.d3)[1] <- "hour"
          cop.a5.d3$hour <- 20:23
          cop.a5.d3$cop <- keep.cop.d4
          
          cop.a5.df <- rbind(keep.cop1,cop.a5.d1,cop.a5.d2,cop.a5.d3) %>% mutate(A5=cop) %>% select(hour,A5)
          
          #A6
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          plot(data.cop.time$new.id,data.cop.time$inside_temp1)
          subset.data <- subset(data.cop.time, data.cop.time$new.id <490)
          plot(subset.data$new.id,subset.data$inside_temp1)
          
          cop.a6 <- ((1000  * 0.8  * 4200 * abs(head(subset.data$inside_temp1,1) - 0) * 0.25) + (1.225*0.8*1000* abs(head(subset.data$inside_temp1,1) - 0) * 0.75))/((tail(subset.data$energy_sum3,1)- head(subset.data$energy_sum3,1)) *3600)
          
          cop.a6.df <-  data.frame(matrix(vector(), 11, 1))
          names(cop.a6.df)[1] <- "hour"
          cop.a6.df$hour <- 6:16
          cop.a6.df$A6 <-  cop.a6         
          
          
          #A7
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          plot(data.cop.time$new.id,data.cop.time$inside_temp1)

          cop.a7<- ((1000  * 0.8  * 4200 * abs(head(data.cop.time$inside_temp1,1) - 0) * 0.25) + (1.225*0.8*1000* abs(head(data.cop.time$inside_temp1,1) - 0) * 0.75))/((tail(data.cop.time$energy_sum3,1)- head(data.cop.time$energy_sum3,1)) *3600)
          
          cop.a7.df <-  data.frame(matrix(vector(), 9, 1))
          names(cop.a7.df)[1] <- "hour"
          cop.a7.df$hour <- 15:23
          cop.a7.df$A7 <- cop.a7
          
          #A8
          data$new.id <- 1:length(data$diff)
          
          #Fix Energy Columm
          norm.data <- subset(data,data$new.id <= 319)
          fix.data <- subset(data,data$new.id > 319) %>% mutate(energy_sum3 = energy_sum3 + 4847.500)
          data <- rbind(norm.data,fix.data)
          
          data1 <- subset(data,data$new.id > 250 & data$new.id <= 400)
          plot(1:length(data1$inside_temp1),data1$inside_temp1)

          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1)-tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1)-tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a8.d1 <-  data.frame(matrix(vector(), 4, 1))
          names(cop.a8.d1)[1] <- "hour"
          cop.a8.d1$hour <- 5:8
          cop.a8.d1$A8 <- keep.cop.d1
          
          data2 <- subset(data,data$new.id >= 710 & data$new.id <= 780)
          plot(1:length(data2$inside_temp1),data2$inside_temp1)
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1)-tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1)-tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a8.d2 <-  data.frame(matrix(vector(), 2, 1))
          names(cop.a8.d2)[1] <- "hour"
          cop.a8.d2$hour <- 12:13
          cop.a8.d2$A8 <- keep.cop.d2
          
          data3 <- subset(data,data$new.id >= 1100)
          plot(1:length(data3$inside_temp1),data3$inside_temp1)
          keep.cop.d3 <- ((1000  * 0.8  * 4200 * abs(head(data3$inside_temp1,1)-tail(data3$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data3$inside_temp1,1)-tail(data3$inside_temp1,1)) * 0.75))/((tail(data3$energy_sum3,1)-head(data3$energy_sum3,1))*3600)
          
          cop.a8.d3 <-  data.frame(matrix(vector(), 4, 1))
          names(cop.a8.d3)[1] <- "hour"
          cop.a8.d3$hour <- 20:23
          cop.a8.d3$A8 <- keep.cop.d3
          
          cop.a8.df <- rbind(cop.a8.d1,cop.a8.d2,cop.a8.d3)
          
   
          #A9
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          plot(data.cop.time$new.id,data.cop.time$inside_temp1)
          
          #Fix Energy Columm
          norm.data <- subset(data.cop.time,data.cop.time$new.id <= 299)
          fix.data <- subset(data.cop.time,data.cop.time$new.id > 299) %>% mutate(energy_sum3 = energy_sum3 + 12292.19)
          data <- rbind(norm.data,fix.data)
          
          data1 <- subset(data,data$new.id < 350)
          plot(1:length(data1$inside_temp1),data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1)-tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1)-tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a9.d1 <-  data.frame(matrix(vector(), 8, 1))
          names(cop.a9.d1)[1] <- "hour"
          cop.a9.d1$hour <- 0:7
          cop.a9.d1$A9 <- keep.cop.d1
          
          data2 <- subset(data,data$new.id > 1050)
          plot(1:length(data2$inside_temp1),data2$inside_temp1)
          
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1)-tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1)-tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a9.d2 <-  data.frame(matrix(vector(), 4, 1))
          names(cop.a9.d2)[1] <- "hour"
          cop.a9.d2$hour <- 20:23
          cop.a9.d2$A9 <- keep.cop.d2
          
          cop.a9.df <- rbind(cop.a9.d1,cop.a9.d2)
          
          #A10
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          plot(data.cop.time$new.id,data.cop.time$inside_temp1)
          
          data1 <- subset(data.cop.time,data.cop.time$new.id > 350 & data.cop.time$new.id < 800)
          plot(1:length(data1$inside_temp1),data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1)-tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1)-tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a10.df <-  data.frame(matrix(vector(), 4, 1))
          names(cop.a10.df)[1] <- "hour"
          cop.a10.df$hour <- 6:9
          cop.a10.df$A10 <- keep.cop.d1
          
          #A11
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          plot(data.cop.time$new.id,data.cop.time$inside_temp1)
          
          data1 <- subset(data.cop.time,data.cop.time$new.id > 300 & data.cop.time$new.id <= 2500)
          plot(1:length(data1$inside_temp1),data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1)-tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1)-tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a11.df <-  data.frame(matrix(vector(), 14, 1))
          names(cop.a11.df)[1] <- "hour"
          cop.a11.df$hour <- 7:20
          cop.a11.df$A11 <- keep.cop.d1
          
          #A12
          data$new.id <- 1:length(data$hour)
          temp <- rbind(subset(data,data$new.id > 900),subset(data,data$new.id<200))
          plot(1:length(temp$inside_temp1),temp$inside_temp1)
          
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(temp$inside_temp1,1)+tail(temp$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1)+tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          cop.a12.df <-  data.frame(matrix(vector(), 11, 1))
          names(cop.a12.df)[1] <- "hour"
          cop.a12.df$hour <- c(0,1,2,3,4,19,20,21,22,23,24)
          cop.a12.df$A12 <- keep.cop.d1
          
          #A14
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
        
          data1 <- subset(data.cop.time,data.cop.time$new.id >=280 & data.cop.time$new.id <= 600)
          plot(data1$new.id,data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1)-tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1)-tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a14.d1 <-  data.frame(matrix(vector(), 3, 1))
          names(cop.a14.d1)[1] <- "hour"
          cop.a14.d1$hour <- 4:6
          cop.a14.d1$A14 <- keep.cop.d1
          
          data2 <- subset(data.cop.time,data.cop.time$new.id >=2000 & data.cop.time$new.id <= 2430)
          plot(data2$new.id,data2$inside_temp1)
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1)-tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1)-tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a14.d2 <-  data.frame(matrix(vector(), 3, 1))
          names(cop.a14.d2)[1] <- "hour"
          cop.a14.d2$hour <- 15:17
          cop.a14.d2$A14 <- keep.cop.d2
          
          cop.a14.df <- rbind(cop.a14.d1,cop.a14.d2)
          
          #A15
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data1 <- subset(data.cop.time,data.cop.time$new.id < 870)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(-4.625 + tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(-4.625+tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a15.d1 <-  data.frame(matrix(vector(), 6, 1))
          names(cop.a15.d1)[1] <- "hour"
          cop.a15.d1$hour <- 0:5
          cop.a15.d1$A15 <- keep.cop.d1
          
          data2 <- subset(data.cop.time,data.cop.time$new.id >= 1300 & data.cop.time$new.id <= 3300)
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1)+tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1)+tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a15.d2 <-  data.frame(matrix(vector(), 11, 1))
          names(cop.a15.d2)[1] <- "hour"
          cop.a15.d2$hour <- 7:17
          cop.a15.d2$A15 <- keep.cop.d2
          
          data3 <- subset(data.cop.time,data.cop.time$new.id >= 3750)
          keep.cop.d3 <- ((1000  * 0.8  * 4200 * abs(head(data3$inside_temp1,1)+tail(data3$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data3$inside_temp1,1)+tail(data3$inside_temp1,1)) * 0.75))/((tail(data3$energy_sum3,1)-head(data3$energy_sum3,1))*3600)
          
          cop.a15.d3 <-  data.frame(matrix(vector(), 5, 1))
          names(cop.a15.d3)[1] <- "hour"
          cop.a15.d3$hour <- 19:23
          cop.a15.d3$A15 <- keep.cop.d3
          
          cop.a15.df <- rbind(cop.a15.d1,cop.a15.d2,cop.a15.d3)
          
          #A16
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data1 <- subset(data.cop.time,data.cop.time$new.id >= 300 & data.cop.time$new.id <= 1600 )
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1) -tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a16.d1 <-  data.frame(matrix(vector(), 9, 1))
          names(cop.a16.d1)[1] <- "hour"
          cop.a16.d1$hour <- 6:14
          cop.a16.d1$A16 <- keep.cop.d1
          
          data2 <- subset(data.cop.time,data.cop.time$new.id >= 1700 & data.cop.time$new.id <= 1880 )
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1) - tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1) -tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a16.d2 <-  data.frame(matrix(vector(), 2, 1))
          names(cop.a16.d2)[1] <- "hour"
          cop.a16.d2$hour <- 15:16
          cop.a16.d2$A16 <- keep.cop.d2
          
          
          cop.a16.df <- rbind(cop.a16.d1,cop.a16.d2)
          
          #A17
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data1 <- subset(data.cop.time,data.cop.time$new.id <= 570)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) + tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1) + tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a17.df <-  data.frame(matrix(vector(), 9, 1))
          names(cop.a17.df)[1] <- "hour"
          cop.a17.df$hour <- 0:8
          cop.a17.df$A17 <- keep.cop.d1
          
          #A18
          data$new.id <- 1:length(data$hour)
          data1 <- subset(data,data$new.id >= 350 & data$new.id <= 1050)
          data1 <- subset(data,data$new.id >= 350 & data$new.id <= 1050)
          #plot(data1$new.id,data1$active_pwr3)
          #plot(data1$new.id,data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a18.df <-  data.frame(matrix(vector(), 14, 1))
          names(cop.a17.df)[1] <- "hour"
          cop.a18.df$hour <- 7:20
          cop.a18.df$A18 <- keep.cop.d1

          #A19
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data1 <- subset(data.cop.time,data.cop.time$new.id >= 300 & data.cop.time$new.id <= 500 )
          #plot(data1$new.id,data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1) -tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a19.d1 <-  data.frame(matrix(vector(), 4, 1))
          names(cop.a19.d1)[1] <- "hour"
          cop.a19.d1$hour <- 5:8
          cop.a19.d1$A19 <- keep.cop.d1
          
          data2 <- subset(data.cop.time,data.cop.time$new.id > 550 & data.cop.time$new.id <= 950 )
          #plot(data2$new.id,data2$inside_temp1)
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1) - tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1) -tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a19.d2 <-  data.frame(matrix(vector(), 4, 1))
          names(cop.a19.d2)[1] <- "hour"
          cop.a19.d2$hour <- 8:11
          cop.a19.d2$A19 <- keep.cop.d2
          
          cop.a19.df <- rbind(cop.a19.d1,cop.a19.d2)
          
          #A20
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data1 <- subset(data.cop.time,data.cop.time$new.id <= 900)
          plot(data1$new.id,data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.25) - (1.225*0.8*1000* abs(head(data1$inside_temp1,1) + tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a20.df <-  data.frame(matrix(vector(), 8, 1))
          names(cop.a20.df)[1] <- "hour"
          cop.a20.df$hour <- 0:7
          cop.a20.df$A20 <- keep.cop.d1
          
          #A21
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data1 <- subset(data.cop.time,data.cop.time$new.id <= 830)
          plot(data1$new.id,data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.25) - (1.225*0.8*1000* abs(head(data1$inside_temp1,1) + tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a21.df <-  data.frame(matrix(vector(), 16, 1))
          names(cop.a21.df)[1] <- "hour"
          cop.a21.df$hour <- 0:15
          cop.a21.df$A21 <- keep.cop.d1
          
          #A22
          data$new.id <- 1:length(data$hour)
          data1 <- subset(data,data$new.id <= 500 )
          #plot(data1$new.id,data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1) -tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a22.d1 <-  data.frame(matrix(vector(), 9, 1))
          names(cop.a22.d1)[1] <- "hour"
          cop.a22.d1$hour <- 0:8
          cop.a22.d1$A22 <- keep.cop.d1
          
          data2 <- subset(data,data$new.id > 830)
          plot(data2$new.id,data2$inside_temp1)
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1) - tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1) -tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a22.d2 <-  data.frame(matrix(vector(), 11, 1))
          names(cop.a22.d2)[1] <- "hour"
          cop.a22.d2$hour <- 13:23
          cop.a22.d2$A22 <- keep.cop.d2
          
          cop.a22.df <- rbind(cop.a22.d1,cop.a22.d2)
          names(cop.a22.df)[2] <- "A22"
          
          
          #A23
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data1 <- subset(data.cop.time,data.cop.time$new.id <= 740)
          #plot(data1$new.id,data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1) -tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a23.d1 <-  data.frame(matrix(vector(), 4, 1))
          names(cop.a23.d1)[1] <- "hour"
          cop.a23.d1$hour <- 6:9
          cop.a23.d1$A23 <- keep.cop.d1
          
          data2 <- subset(data.cop.time,data.cop.time$new.id >= 1890)
          plot(data2$new.id,data2$inside_temp1)
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1) - tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1) -tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a23.d2 <-  data.frame(matrix(vector(), 4, 1))
          names(cop.a23.d2)[1] <- "hour"
          cop.a23.d2$hour <- 15:18
          cop.a23.d2$A23 <- keep.cop.d2
          
          cop.a23.df <- rbind(cop.a23.d1,cop.a23.d2)
          
          #A24
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data1 <- subset(data.cop.time,data.cop.time$new.id > 500 & data.cop.time$new.id <= 1750)
          plot(data1$new.id,data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.25) - (1.225*0.8*1000* abs(head(data1$inside_temp1,1) + tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a24.df <-  data.frame(matrix(vector(),11, 1))
          names(cop.a24.df)[1] <- "hour"
          cop.a24.df$hour <- 9:19
          cop.a24.df$A24 <- keep.cop.d1
          
          #A25
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data1 <- subset(data.cop.time,data.cop.time$new.id <= 350 )
          plot(data1$new.id,data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1) -tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a25.d1 <-  data.frame(matrix(vector(), 7, 1))
          names(cop.a25.d1)[1] <- "hour"
          cop.a25.d1$hour <- 0:6
          cop.a25.d1$A25 <- keep.cop.d1
          
          data2 <- subset(data.cop.time,data.cop.time$new.id >= 410 & data.cop.time$new.id <= 790)
          plot(data2$new.id,data2$inside_temp1)
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1) - tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1) -tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a25.d2 <-  data.frame(matrix(vector(), 8, 1))
          names(cop.a25.d2)[1] <- "hour"
          cop.a25.d2$hour <- 6:13
          cop.a25.d2$A25 <- keep.cop.d2
          
          cop.a25.df <- rbind(cop.a25.d1,cop.a25.d2)
          
          #A27
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data1 <- subset(data.cop.time,data.cop.time$active_pwr3 > 150)
          #plot(data1$new.id,data1$active_pwr3)
          data <- rbind(subset(data1,data1$new.id>400),subset(data1,data1$new.id <400))
          data$new.id <- 1:length(data$energy_sum3)
          
          plot(data$new.id,data$active_pwr3)
          plot(data$new.id,data$energy_sum3)
          
          sub1 <- subset(data,data$new.id <  766)
          #plot(1:length(sub1$new.id),sub1$energy_sum3)
          sub2 <- subset(data,data$new.id >  766)
          plot(1:length(sub2$new.id),sub2$energy_sum3)
          diff <- tail(sub1$energy_sum3,1) - head(sub2$energy_sum3,1)
          sub2$sumsum <- sub2$energy_sum3  + diff
          sub2$energy_sum3 <- sub2$sumsum
          sub2 <- sub2[,c("hour","minute","second","active_pwr3","energy_sum3","inside_temp1","time.id","lagged","diff","new.id")]
          datasub <- rbind(sub1,sub2)
          plot(1:length(datasub$new.id),datasub$energy_sum3)
          
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(datasub$inside_temp1,1) - tail(datasub$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(datasub$inside_temp1,1) - tail(datasub$inside_temp1,1)) * 0.75))/((tail(datasub$energy_sum3,1)-head(datasub$energy_sum3,1))*3600)
          
          cop.a27.df <-  data.frame(matrix(vector(),6, 1))
          names(cop.a27.df)[1] <- "hour"
          cop.a27.df$hour <- 1:6
          cop.a27.df$A27 <- keep.cop.d1
          
          #A28
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data.cop.time$inside_temp1,1) - tail(data.cop.time$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data.cop.time$inside_temp1,1) - tail(data.cop.time$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a28.df <-  data.frame(matrix(vector(),5, 1))
          names(cop.a28.df)[1] <- "hour"
          cop.a28.df$hour <- 7:11
          cop.a28.df$A28 <- keep.cop.d1
          
          #A29
          data.cop.time$new.id <- 1:length(data.cop.time$hour)
          data.cop.time <- data.cop.time[complete.cases(data.cop.time),]
          plot(data.cop.time$new.id,data.cop.time$active_pwr3)
          data1 <- subset(data.cop.time,data.cop.time$new.id <= 1050 )
          plot(data1$new.id,data1$inside_temp1)
          keep.cop.d1 <- ((1000  * 0.8  * 4200 * abs(head(data1$inside_temp1,1) - tail(data1$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data1$inside_temp1,1) -tail(data1$inside_temp1,1)) * 0.75))/((tail(data1$energy_sum3,1)-head(data1$energy_sum3,1))*3600)
          
          cop.a29.d1 <-  data.frame(matrix(vector(), 5, 1))
          names(cop.a29.d1)[1] <- "hour"
          cop.a29.d1$hour <- 1:5
          cop.a29.d1$A29 <- keep.cop.d1
          
          data2 <- subset(data.cop.time,data.cop.time$new.id >= 2000)
          plot(data2$new.id,data2$inside_temp1)
          keep.cop.d2 <- ((1000  * 0.8  * 4200 * abs(head(data2$inside_temp1,1) - tail(data2$inside_temp1,1)) * 0.25) + (1.225*0.8*1000* abs(head(data2$inside_temp1,1) -tail(data2$inside_temp1,1)) * 0.75))/((tail(data2$energy_sum3,1)-head(data2$energy_sum3,1))*3600)
          
          cop.a29.d2 <-  data.frame(matrix(vector(), 15, 1))
          names(cop.a29.d2)[1] <- "hour"
          cop.a29.d2$hour <- 9:23
          cop.a29.d2$A29 <- keep.cop.d2
          
          cop.a29.df <- rbind(cop.a29.d1,cop.a29.d2)
          
          
          
          
          
          
          
          
        }
      

        
        #Adding COPS
        cop.df <- join(cop.df,cop.a2.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a3.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a4.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a5.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a6.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a7.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a8.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a9.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a10.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a11.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a12.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a14.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a15.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a16.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a17.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a18.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a19.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a20.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a21.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a22.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a23.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a24.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a25.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a27.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a28.df,by=c('hour'),type='left',match='first')
        cop.df <- join(cop.df,cop.a29.df,by=c('hour'),type='left',match='first')
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        setwd('/Users/Diego/Desktop')
        write.csv(cop.df,'cop_df.csv')

      }
    }
    }

cop.df.keep <- cop.df[,c("hour","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23","A24","A25","A27","A28","A29")]

cop.melt <- melt(cop.df.keep,id.vars='hour')


cop.melt$id.house <- as.numeric(gsub("^.*?A","",cop.melt$variable))



#Heat Map Palette

gg <- ggplot(cop.melt,aes(x=id.house,y=hour,fill=value))
gg <- gg + geom_tile(color="white",size=0.1)
gg <- gg + scale_fill_viridis(name="EPI", label=comma,na.value="white")
gg <- gg + coord_equal()
gg <- gg + labs(x="Micro ID", y="Hour of the Day", title="Efficiency Performance Index by Hour of Day")
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=9))
gg <- gg + theme(legend.title=element_text(size=9))
gg <- gg + theme(legend.text=element_text(size=8)) + theme(panel.background = element_blank())
gg 







