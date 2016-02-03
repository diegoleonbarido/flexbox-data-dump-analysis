############ Cleaning Data

#Cleaning data by removing only the 99th percentile % Also divides house power by 1000
clean.data.percentile <- function(data){
  
  clean.data.one <- subset(data, data$house_Energy < quantile(data$house_Energy , c(.99),na.rm=TRUE))
  clean.data.one$house_Power <- clean.data.one$house_Power/1000 
  
  return(clean.data.one)
}


#Cleaning data by removing: 1) the 99th percentile, 2) zero values
clean.data.percentile.zero <- function(data){
  
  clean.data.one <- subset(data, data$house_Energy < quantile(data$house_Energy , c(.99),na.rm=TRUE))
  clean.data.one <- subset(clean.data.one, clean.data.one$house_Energy != 0,na.rm=TRUE)
  clean.data.one$house_Power <- clean.data.one$house_Power/1000 
  
  return(clean.data.one)
}


