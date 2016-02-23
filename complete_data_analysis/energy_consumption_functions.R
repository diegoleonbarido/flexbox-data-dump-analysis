#Purpose: Functions to calculate energy consumption for a variety 

#NOTE1: Note that there is a problem beccause micro-level id A18 for some reason is not reading any data. Need
# to check the data input for A18.

#NOTE2: Must create flexible code so that we can incorpotate a different number of houses that might or might
# not have data as we go. This has to be incorporated in the lines that bind all the data frames together after 
# iterating over them.

# Sections:
  # 1. HOUSEHOLD ENERGY CONSUMPTION PER HOUR: CLUSTER
  # 2. HOUSEHOLD ENERGY CONSUMPTION PER HOUR: INDIVIDUAL HOUSE ANALYSIS
  # 3. FRIDGE ENERGY CONSUMPTION PER HOUR: INDIVIDUAL HOUSES
  # 4. DAY OF THE WEEK ANALYSIS: CLUSTER
  # 4.1 DAY OF THE WEEK ANALYSIS: INDIVIDUAL HOUSE ANALYSIS
  # 5. OPOWER NEIGHBOUR COMPARISON

  # 4. HOUSEHOLD ENERGY CONSUMPTION EVERY FEW MINUTES




################# 1.  HOUSEHOLD ENERGY CONSUMPTION PER HOUR: CLUSTER 

henergy.consumption.hour <- function(data_input) {
   
#Getting rid of zeros and NaNs  
   unique.data.input <-  unique(data_input[c("house.id","date.y","hour","minute","second","house_Energy")])
        unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
        unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
        
#Making sure that we are keeping non-negative and non-outlier data
    unique.microids <- unique(unique.data.input$house.id)
    microids.df <- list()
    
    #Getting rid of outliers    
    for (i in 1:length(unique.microids)) {
        iterate.df <- subset(unique.data.input,unique.data.input$house.id == unique.microids[i])
        iterate.df$lagged <-  lag(iterate.df$house_Energy,k=1)
        iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
        iterate.df$diff <- iterate.df$house_Energy - iterate.df$lagged
        microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
    }
    
    microid.data.input <- rbind(microids.df[[1]],microids.df[[2]],microids.df[[3]],microids.df[[4]],microids.df[[5]],microids.df[[6]],microids.df[[7]],microids.df[[8]],microids.df[[9]],microids.df[[10]],microids.df[[11]],microids.df[[12]],microids.df[[13]],microids.df[[14]],microids.df[[15]],microids.df[[16]],microids.df[[17]],microids.df[[18]],microids.df[[19]],microids.df[[20]],microids.df[[21]],microids.df[[22]],microids.df[[23]],microids.df[[24]],microids.df[[25]])
    
#Setting unique combinations and values so that we can iterate
   unique.combinations <- unique(microid.data.input[c("house.id","date.y","hour")])
   unique.microids <- unique(unique.combinations$house.id)
   unique.subset <- c()
   count <- 0
   
   for (i in 1:length(unique.microids)) {
     
     subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
     unique.dateys <- unique(subset.hours.df$date.y)
     
     for (j in 1:length(unique.dateys)) {
       
       subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date.y == unique.dateys[j])
       unique.hours <- unique(subset.hours.df$hour)
       
       for (k in 1:length(unique.hours)) {
         count <- count + 1
         subset.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date.y == unique.dateys[j]  & microid.data.input$hour == unique.hours[k])
         subset.df <- subset.df[order(subset.df$date.y,subset.df$hour,subset.df$minute,subset.df$second),]
         unique.subset[count] <- tail(subset.df$house_Energy,1) - head(subset.df$house_Energy,1)
     
       }
     }
   }
   
  # This is the first data frame that is returned; it includes energy consumption by hour. After wards, the data is normalized so that the load shapes can be compared.
  energy.hourly.consumption <- cbind(unique.combinations,unique.subset) %>% mutate(energy = unique.subset) %>% select(house.id,date.y,hour,energy)
  energy.hourly.consumption <- subset(energy.hourly.consumption, energy.hourly.consumption$energy >=0)
  
  #Normalized data for each micro.level id
  ehouse.list <- list()
  for (a in 1:length(unique.microids)){
    norm.henergy.microid <- subset(energy.hourly.consumption,energy.hourly.consumption$house.id==unique.microids[a])
    norm.henergy.microid$norm.energy <- (norm.henergy.microid$energy - min(norm.henergy.microid$energy))/(max(norm.henergy.microid$energy)-min(norm.henergy.microid$energy))
    ehouse.list[[a]] <- norm.henergy.microid
  }
  
  ehouse.normalized <- rbind(ehouse.list[[1]],ehouse.list[[2]],ehouse.list[[3]],ehouse.list[[4]],ehouse.list[[5]],ehouse.list[[6]],ehouse.list[[7]],ehouse.list[[8]],ehouse.list[[9]],ehouse.list[[10]],ehouse.list[[11]],ehouse.list[[12]],ehouse.list[[13]],ehouse.list[[14]],ehouse.list[[15]],ehouse.list[[16]],ehouse.list[[17]],ehouse.list[[18]],ehouse.list[[19]],ehouse.list[[20]],ehouse.list[[21]],ehouse.list[[22]],ehouse.list[[23]],ehouse.list[[24]])
  
  return(list(energy.hourly.consumption,ehouse.normalized))
  
}
  


#################  2. HOUSEHOLD ENERGY CONSUMPTION PER HOUR: INDIVIDUAL HOUSE   ---- input is house 

henergy.consumption.hour.house <- function(data_input) {
  
  #Getting rid of zeros and NaNs  
  unique.data.input <- date.vars.simple(date.data.frames.simple.v2(data_input))
  unique.data.input <-  unique(unique.data.input[c("house.id","date","hour","minute","second","house_Energy")])
  unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  
  #Making sure that we are keeping non-negative and non-outlier data
  unique.microids <- unique(unique.data.input$house.id)
  microids.df <- list()
  
  #Getting rid of outliers    
  for (i in 1:length(unique.microids)) {
    iterate.df <- subset(unique.data.input,unique.data.input$house.id == unique.microids[i])
    iterate.df$lagged <-  lag(iterate.df$house_Energy,k=1)
    iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
    iterate.df$diff <- iterate.df$house_Energy - iterate.df$lagged
    microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
  }
  
  microid.data.input <- microids.df[[1]]
  
  #Setting unique combinations and values so that we can iterate
  unique.combinations <- unique(microid.data.input[c("house.id","date","hour")])
  unique.microids <- unique(unique.combinations$house.id)
  unique.subset <- c()
  count <- 0
  
  for (i in 1:length(unique.microids)) {
    
    subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
    unique.dateys <- unique(subset.hours.df$date)
    
    for (j in 1:length(unique.dateys)) {
      
      subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date == unique.dateys[j])
      unique.hours <- unique(subset.hours.df$hour)
      
      for (k in 1:length(unique.hours)) {
        count <- count + 1
        subset.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date == unique.dateys[j]  & microid.data.input$hour == unique.hours[k])
        subset.df <- subset.df[order(subset.df$date,subset.df$hour,subset.df$minute,subset.df$second),]
        unique.subset[count] <- tail(subset.df$house_Energy,1) - head(subset.df$house_Energy,1)
        
      }
    }
  }
  
  # This is the first data frame that is returned; it includes energy consumption by hour. After wards, the data is normalized so that the load shapes can be compared.
  energy.hourly.consumption <- cbind(unique.combinations,unique.subset) %>% mutate(energy = unique.subset) %>% select(house.id,date,hour,energy)
  energy.hourly.consumption <- subset(energy.hourly.consumption, energy.hourly.consumption$energy >=0)
  
  #Normalized data for each micro.level id
  ehouse.list <- list()
  for (a in 1:length(unique.microids)){
    norm.henergy.microid <- subset(energy.hourly.consumption,energy.hourly.consumption$house.id==unique.microids[a])
    norm.henergy.microid$norm.energy <- (norm.henergy.microid$energy - min(norm.henergy.microid$energy))/(max(norm.henergy.microid$energy)-min(norm.henergy.microid$energy))
    ehouse.list[[a]] <- norm.henergy.microid
  }
  
  ehouse.normalized <- ehouse.list[[1]]
  
  return(ehouse.normalized)
  
}







################# 3. FRIDGE ENERGY CONSUMPTION PER HOUR
  
fenergy.consumption.hour <- function(data_input) {

#Getting rid of zeros and NaNs  

unique.data.input <-  unique(data_input[c("house.id","date.y","hour","energy_sum3")])
  unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  
  
#Making sure that we are keeping non-negative and non-outlier data
  unique.microids <- unique(unique.data.input$house.id)
  microids.df <- list()
  
  for (i in 1:length(unique.microids)) {
    iterate.df <- subset(unique.data.input,unique.data.input$house.id == unique.microids[i])
    iterate.df$lagged <-  lag(iterate.df$energy_sum3,k=1)
    iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
    iterate.df$diff <- iterate.df$energy_sum3 - iterate.df$lagged
    microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
  }
  
  microid.data.input <- rbind(microids.df[[1]],microids.df[[2]],microids.df[[3]],microids.df[[4]],microids.df[[5]],microids.df[[6]],microids.df[[7]],microids.df[[8]],microids.df[[9]],microids.df[[10]],microids.df[[11]],microids.df[[12]],microids.df[[13]],microids.df[[14]],microids.df[[15]],microids.df[[16]],microids.df[[17]],microids.df[[18]],microids.df[[19]],microids.df[[20]],microids.df[[21]],microids.df[[22]],microids.df[[23]],microids.df[[24]],microids.df[[25]])
  
#Setting unique combinations and values so that we can iterate
  unique.combinations <- unique(microid.data.input[c("house.id","date.y","hour")])
  unique.microids <- unique(unique.combinations$house.id)
  unique.subset <- c()
  count <- 0
  
  for (i in 1:length(unique.microids)) {
    
    subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
    unique.dateys <- unique(subset.hours.df$date.y)
    
    for (j in 1:length(unique.dateys)) {
      
      subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date.y == unique.dateys[j])
      unique.hours <- unique(subset.hours.df$hour)
      
      for (k in 1:length(unique.hours)) {
        count <- count + 1
        subset.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date.y == unique.dateys[j]  & microid.data.input$hour == unique.hours[k])
        subset.df$energy_sum3 <- subset.df$energy_sum3/1000
        unique.subset[count] <- tail(subset.df$energy_sum3,1) - head(subset.df$energy_sum3,1)
        
      }
    }
  }
  
  fenergy.hourly.consumption <- cbind(unique.combinations,unique.subset) %>% mutate(fridge.energy = unique.subset) %>% select(house.id,date.y,hour,fridge.energy)
  fenergy.hourly.consumption <- subset(fenergy.hourly.consumption,fenergy.hourly.consumption$fridge.energy >= 0)
  return(fenergy.hourly.consumption)
}





################## 4. Day of the week analysis (CLUSTER)

eday.analysis <- function(data_input) {

data_input <- date.vars.simple(data_input)  
data_input$day.type <- weekdays(data_input$date)
  
#Getting rid of zeros and NaNs  
unique.data.input <-  unique(data_input[c("house.id","date","day.type","hour","minute","second","house_Energy")])
    unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
    unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs

#Making sure that we are keeping non-negative and non-outlier data
unique.microids <- unique(unique.data.input$house.id)
microids.df <- list()

#Getting rid of outliers    
for (i in 1:length(unique.microids)) {
  iterate.df <- subset(unique.data.input,unique.data.input$house.id == unique.microids[i])
  iterate.df$lagged <-  lag(iterate.df$house_Energy,k=1)
  iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
  iterate.df$diff <- iterate.df$house_Energy - iterate.df$lagged
  microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
}

microid.data.input <- rbind(microids.df[[1]],microids.df[[2]],microids.df[[3]],microids.df[[4]],microids.df[[5]],microids.df[[6]],microids.df[[7]],microids.df[[8]],microids.df[[9]],microids.df[[10]],microids.df[[11]],microids.df[[12]],microids.df[[13]],microids.df[[14]],microids.df[[15]],microids.df[[16]],microids.df[[17]],microids.df[[18]],microids.df[[19]],microids.df[[20]],microids.df[[21]],microids.df[[22]],microids.df[[23]],microids.df[[24]])


#Setting unique combinations of days and energy so that we can iterate over them
unique.combinations <- unique(microid.data.input[c("house.id","date")])
unique.microids <- unique(unique.combinations$house.id)
unique.subset <- c()
count <- 0

for (i in 1:length(unique.microids)) {
  
  subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
  unique.dateys <- unique(subset.hours.df$date)
  
  for (j in 1:length(unique.dateys)) {
    
    count <- count + 1
    subset.dates.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date == unique.dateys[j])
    subset.dates.df <- subset.dates.df[order(subset.dates.df$date,subset.dates.df$hour,subset.dates.df$minute,subset.dates.df$second),]
    unique.subset[count] <- tail(subset.dates.df$house_Energy,1) - head(subset.dates.df$house_Energy,1)
  }
}

# This is the first data frame that is returned; it includes energy consumption by hour. After wards, the data is normalized so that the load shapes can be compared.
energy.daily.consumption <- cbind(unique.combinations,unique.subset) %>% mutate(energy = unique.subset,day.id=weekdays(date)) %>% select(house.id,date,energy,day.id)
energy.daily.consumption <- subset(energy.daily.consumption, energy.daily.consumption$energy >=0)
energy.daily.consumption$day.id.num <- ifelse(energy.daily.consumption$day.id=='Monday',1,ifelse(energy.daily.consumption$day.id=='Tuesday',2,ifelse(energy.daily.consumption$day.id=='Wednesday',3,ifelse(energy.daily.consumption$day.id=='Thursday',4,ifelse(energy.daily.consumption$day.id=='Friday',5,ifelse(energy.daily.consumption$day.id=='Saturday',6,7))))))

#Normalized data for each micro.level id
ehouse.list <- list()
for (a in 1:length(unique.microids)){
  norm.henergy.microid <- subset(energy.daily.consumption,energy.daily.consumption$house.id==unique.microids[a])
  norm.henergy.microid$norm.energy <- (norm.henergy.microid$energy - min(norm.henergy.microid$energy))/(max(norm.henergy.microid$energy)-min(norm.henergy.microid$energy))
  ehouse.list[[a]] <- norm.henergy.microid
}

edaily.normalized <- rbind(ehouse.list[[1]],ehouse.list[[2]],ehouse.list[[3]],ehouse.list[[4]],ehouse.list[[5]],ehouse.list[[6]],ehouse.list[[7]],ehouse.list[[8]],ehouse.list[[9]],ehouse.list[[10]],ehouse.list[[11]],ehouse.list[[12]],ehouse.list[[13]],ehouse.list[[14]],ehouse.list[[15]],ehouse.list[[16]],ehouse.list[[17]],ehouse.list[[18]],ehouse.list[[19]],ehouse.list[[20]],ehouse.list[[21]],ehouse.list[[22]],ehouse.list[[23]])
return(edaily.normalized)
}





########### 4.1 Day of the week analysis (INDIVIDUAL HOUSE ANALYSIS)

eday.analysis.house <- function(data_input) {
  
  data_input <- date.vars.simple(data_input)  
  data_input$day.type <- weekdays(data_input$date)
  
  #Getting rid of zeros and NaNs  
  unique.data.input <-  unique(data_input[c("house.id","date","day.type","hour","minute","second","house_Energy")])
  unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  
  #Making sure that we are keeping non-negative and non-outlier data
  unique.microids <- unique(unique.data.input$house.id)
  microids.df <- list()
  
  #Getting rid of outliers    
  for (i in 1:length(unique.microids)) {
    iterate.df <- subset(unique.data.input,unique.data.input$house.id == unique.microids[i])
    iterate.df$lagged <-  lag(iterate.df$house_Energy,k=1)
    iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
    iterate.df$diff <- iterate.df$house_Energy - iterate.df$lagged
    microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
  }
  
  microid.data.input <-  microids.df[[1]]
  
  #Setting unique combinations of days and energy so that we can iterate over them
  unique.combinations <- unique(microid.data.input[c("house.id","date")])
  unique.microids <- unique(unique.combinations$house.id)
  unique.subset <- c()
  count <- 0
  
  for (i in 1:length(unique.microids)) {
    
    subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
    unique.dateys <- unique(subset.hours.df$date)
    
    for (j in 1:length(unique.dateys)) {
      
      count <- count + 1
      subset.dates.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date == unique.dateys[j])
      subset.dates.df <- subset.dates.df[order(subset.dates.df$date,subset.dates.df$hour,subset.dates.df$minute,subset.dates.df$second),]
      unique.subset[count] <- tail(subset.dates.df$house_Energy,1) - head(subset.dates.df$house_Energy,1)
    }
  }
  
  # This is the first data frame that is returned; it includes energy consumption by hour. After wards, the data is normalized so that the load shapes can be compared.
  energy.daily.consumption <- cbind(unique.combinations,unique.subset) %>% mutate(energy = unique.subset,day.id=weekdays(date)) %>% select(house.id,date,energy,day.id)
  energy.daily.consumption <- subset(energy.daily.consumption, energy.daily.consumption$energy >=0)
  energy.daily.consumption$day.id.num <- ifelse(energy.daily.consumption$day.id=='Monday',1,ifelse(energy.daily.consumption$day.id=='Tuesday',2,ifelse(energy.daily.consumption$day.id=='Wednesday',3,ifelse(energy.daily.consumption$day.id=='Thursday',4,ifelse(energy.daily.consumption$day.id=='Friday',5,ifelse(energy.daily.consumption$day.id=='Saturday',6,7))))))
  
  #Normalized data for each micro.level id
  ehouse.list <- list()
  for (a in 1:length(unique.microids)){
    norm.henergy.microid <- subset(energy.daily.consumption,energy.daily.consumption$house.id==unique.microids[a])
    norm.henergy.microid$norm.energy <- (norm.henergy.microid$energy - min(norm.henergy.microid$energy))/(max(norm.henergy.microid$energy)-min(norm.henergy.microid$energy))
    ehouse.list[[a]] <- norm.henergy.microid
  }
  
  edaily.normalized <- ehouse.list[[1]]
  
  return(edaily.normalized)
}



###################### 5. OPOWER NEIGHBOUR COMPARISON



neighbor.comparison <- function(data_input){

#Getting rid of zeros and NaNs  
unique.data.input <- date.vars.simple(date.data.frames.simple.v2(data_input))
unique.data.input <-  unique(unique.data.input[c("house.id","date","hour","minute","second","house_Energy")])
unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs

#Making sure that we are keeping non-negative and non-outlier data
unique.microids <- unique(unique.data.input$house.id)
microids.df <- list()

#Getting rid of outliers    
for (i in 1:length(unique.microids)) {
  iterate.df <- subset(unique.data.input,unique.data.input$house.id == unique.microids[i])
  iterate.df$lagged <-  lag(iterate.df$house_Energy,k=1)
  iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
  iterate.df$diff <- iterate.df$house_Energy - iterate.df$lagged
  microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
}

microid.data.input <- rbind(microids.df[[1]],microids.df[[2]],microids.df[[3]],microids.df[[4]],microids.df[[5]],microids.df[[6]],microids.df[[7]],microids.df[[8]],microids.df[[9]],microids.df[[10]],microids.df[[11]],microids.df[[12]],microids.df[[13]],microids.df[[14]],microids.df[[15]],microids.df[[16]],microids.df[[17]],microids.df[[18]],microids.df[[19]],microids.df[[20]],microids.df[[21]],microids.df[[22]],microids.df[[23]],microids.df[[24]],microids.df[[25]])

#Setting unique combinations of microids
unique.microids <- unique(microid.data.input$house.id)
unique.subset <- data.frame(matrix(ncol = 2, nrow = length(unique.microids))) %>% mutate(house.id = X1,energy=X2) %>% select(house.id,energy)


for (i in 1:length(unique.microids)) {

  subset.microid <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
  unique.subset$house.id[i] <- unique.microids[i]
  unique.subset$energy[i] <- tail(subset.microid$house_Energy,1) - head(subset.microid$house_Energy,1)
      }
  

unique.subset$norm.energy <- (unique.subset$energy - min(unique.subset$energy))/(max(unique.subset$energy)-min(unique.subset$energy))

return(unique.subset)
}







































################ 4. 5 MIN ENERGY CONSUMPTION

fivemin.energy.consumption.hour <- function(data_input) {
  
  #Getting rid of zeros and NaNs  
  unique.data.input <-  date.vars.simple(data_input) 
  unique.data.input <-  unique(unique.data.input[c("house.id","date","hour","minute","house_Energy")])
  unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  
  ###### Creating set of unique minute-by-minute ids data points to plot one for each minute
  t.id <- data.frame(matrix(NA, nrow = 1440, ncol = 2))
  t.id$X1 <- rep(seq(0,23),each=60)
  t.id$X2 <- rep(seq(1,60,by=1),times=24)
  t.id$time.id <- seq(1:1440)
  names(t.id)[1] <- 'hour'
  names(t.id)[2] <- 'minute'
  unique.data.input <- merge(unique.data.input,t.id,by=c('hour','minute'))
  
  
  #Making sure that we are keeping non-negative and non-outlier data
  unique.microids <- unique(unique.data.input$house.id)
  microids.df <- list()
  
  for (i in 1:length(unique.microids)) {
    iterate.df <- subset(unique.data.input,unique.data.input$house.id == unique.microids[i])
    iterate.df$lagged <-  lag(iterate.df$house_Energy,k=1)
    iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
    iterate.df$diff <- iterate.df$house_Energy - iterate.df$lagged
    microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
  }
  
  microid.data.input <- rbind(microids.df[[1]],microids.df[[2]],microids.df[[3]],microids.df[[4]],microids.df[[5]],microids.df[[6]],microids.df[[7]],microids.df[[8]],microids.df[[9]],microids.df[[10]],microids.df[[11]],microids.df[[12]],microids.df[[13]],microids.df[[14]],microids.df[[15]],microids.df[[16]],microids.df[[17]],microids.df[[18]],microids.df[[19]],microids.df[[20]],microids.df[[21]],microids.df[[22]],microids.df[[23]],microids.df[[24]],microids.df[[25]])
  
  #Setting unique combinations and values so that we can iterate
  unique.combinations <- unique(microid.data.input[c("house.id","date","hour","minute")])
  unique.microids <- unique(unique.combinations$house.id)
  unique.subset <- c()
  count <- 0
  
  for (i in 1:length(unique.microids)) {
    
    subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
    unique.dateys <- unique(subset.hours.df$date)
    
    for (j in 1:length(unique.dateys)) {
      
      subset.hours.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date == unique.dateys[j])
      unique.hours <- unique(subset.hours.df$hour)
      
      for (k in 1:length(unique.hours)) {
        count <- count + 1
        subset.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i] & microid.data.input$date == unique.dateys[j]  & microid.data.input$hour == unique.hours[k])
        unique.subset[count] <- tail(subset.df$house_Energy,1) - head(subset.df$house_Energy,1)
        
      }
    }
  }
  
  # This is the first data frame that is returned; it includes energy consumption by hour. Afterwards, the data is normalized so that the load shapes can be compared.
  energy.hourly.consumption <- cbind(unique.combinations,unique.subset) %>% mutate(energy = unique.subset) %>% select(house.id,date.y,hour,energy)
  energy.hourly.consumption <- subset(energy.hourly.consumption, energy.hourly.consumption$energy >=0)
  
  #Normalized data by micro.level id
  ehouse.list <- list()
  for (a in 1:length(unique.microids)){
    norm.henergy.microid <- subset(energy.hourly.consumption,energy.hourly.consumption$house.id==unique.microids[a])
    norm.henergy.microid$norm.energy <- (norm.henergy.microid$energy - min(norm.henergy.microid$energy))/(max(norm.henergy.microid$energy)-sd(norm.henergy.microid$energy))
    ehouse.list[[a]] <- norm.henergy.microid
  }
  
  ehouse.normalized <- rbind(ehouse.list[[1]],ehouse.list[[2]],ehouse.list[[3]],ehouse.list[[4]],ehouse.list[[5]],ehouse.list[[6]],ehouse.list[[7]],ehouse.list[[8]],ehouse.list[[9]],ehouse.list[[10]],ehouse.list[[11]],ehouse.list[[12]],ehouse.list[[13]],ehouse.list[[14]],ehouse.list[[15]],ehouse.list[[16]],ehouse.list[[17]],ehouse.list[[18]],ehouse.list[[19]],ehouse.list[[20]],ehouse.list[[21]],ehouse.list[[22]],ehouse.list[[23]],ehouse.list[[24]],ehouse.list[[24]])
  
  return(list(energy.hourly.consumption,ehouse.normalized))
  
}



      
