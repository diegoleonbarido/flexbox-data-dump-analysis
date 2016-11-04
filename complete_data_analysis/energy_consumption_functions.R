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
  
  microid.data.input$date_hour <- paste(microid.data.input$date,microid.data.input$hour)
  microid.data.input$time_stamp <- strptime(microid.data.input$date_hour,"%Y-%m-%d %H")
  
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

fenergy.consumption.hour <- function(data_input,dump) {
  
  #Getting rid of zeros and NaNs  
  
  unique.data.input <-  unique(data_input[c("house.id","date","hour","energy_sum3")])
  unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  
  if (dim(unique.data.input)[1] == 0 | length(unique(unique.data.input$energy_sum3))/length(unique.data.input$energy_sum3) < .30 ) {
    unique.data.input <-  unique(data_input[c("house.id","date","hour","energy_sum2")])
    unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
    unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
    
    if (dim(unique.data.input)[1] == 0 | length(unique(unique.data.input$energy_sum2))/length(unique.data.input$energy_sum2) < 0.30 ) {
      unique.data.input <-  unique(data_input[c("house.id","date","hour","energy_sum1")])
      unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
      unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
    } else {}
    
  } else {}
  
  #if (data_input$house.id == 'A26') {
  #  unique.data.input <-  unique(data_input[c("house.id","date","hour","energy_sum2")])
  #  unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
  #  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  #} else {}
  
  names(unique.data.input)[4] <- "energy_sum3"
  
  #Making sure that we are keeping non-negative and non-outlier data
  unique.microids <- unique(unique.data.input$house.id)
  microids.df <- list()
  
  #Getting rid of outliers
  for (i in 1:length(unique.microids)) {
    iterate.df <- subset(unique.data.input,unique.data.input$house.id == unique.microids[i])
    iterate.df$lagged <-  lag(iterate.df$energy_sum3,k=1)
    iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
    iterate.df$diff <- iterate.df$energy_sum3 - iterate.df$lagged
    microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
  }
  
  #microid.data.input <- rbind(microids.df[[1]],microids.df[[2]],microids.df[[3]],microids.df[[4]],microids.df[[5]],microids.df[[6]],microids.df[[7]],microids.df[[8]],microids.df[[9]],microids.df[[10]],microids.df[[11]],microids.df[[12]],microids.df[[13]],microids.df[[14]],microids.df[[15]],microids.df[[16]],microids.df[[17]],microids.df[[18]],microids.df[[19]],microids.df[[20]])
  microid.data.input <- microids.df[[1]]
  
  ### Making sure that if the MFI was reset that we are keeping good data
  
  for (i in 1:length(unique.microids)) {
    iterate.df <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
    iterate.df$lagged_again <-  lag(iterate.df$energy_sum3,k=1)
    iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
    iterate.df$diff_again <- iterate.df$energy_sum3 - iterate.df$lagged_again
    #microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
  }
  
  mfi_reset_events <- subset(iterate.df,iterate.df$diff_again < quantile(iterate.df$diff_again,c(0.01)))
  diff_values <- mfi_reset_events$diff_again
  
  ### Runnig a for loop to make sure that the fridge energy consumption is always montonically increasing
  
  if (length(diff_values) >= 1) {
  
  for (i in 1:length(diff_values)){
    
    if (i == 1) {
      index1 = which(iterate.df$diff_again == diff_values[1])
      first_subset <- iterate.df[1:(index1-1),]
    
      first_subset2 <- iterate.df[(index1+1):length(iterate.df$house.id),]
      first_subset2$energy_sum3 <- first_subset2$energy_sum3 + tail(first_subset$energy_sum3,1)
      
    } 
      else if(i == 2 & length(diff_values) >= 2) {
        index2 = which(iterate.df$diff_again == diff_values[2])
        second_subset <- iterate.df[(index1+1):index2-1,]
        second_subset$energy_sum3 <- second_subset$energy_sum3 + tail(first_subset$energy_sum3,1)
    } 
      else if(i == 3 & length(diff_values) >= 3) {
        index3 = which(iterate.df$diff_again == diff_values[3])
        third_subset <- iterate.df[(index2+1):index3-1,]
        third_subset$energy_sum3 <- third_subset$energy_sum3 + tail(second_subset$energy_sum3,1)
    } 
      else if(i == 4 & length(diff_values) >= 4) {
        index4 = which(iterate.df$diff_again == diff_values[4])
        fourth_subset <- iterate.df[(index3+1):index4-1,]
        fourth_subset$energy_sum3 <- fourth_subset$energy_sum3 + tail(third_subset$energy_sum3,1)
    } 
      else if(i == 5 & length(diff_values) >= 5) {
        index5 = which(iterate.df$diff_again == diff_values[5])
        fifth_subset <- iterate.df[(index4+1):index5-1,]
        fifth_subset$energy_sum3 <- fifth_subset$energy_sum3 + tail(fourth_subset$energy_sum3,1)
    } 
      else if(i == 6 & length(diff_values) >= 6) {
        index6 = which(iterate.df$diff_again == diff_values[6])
        sixth_subset <- iterate.df[(index5+1):index6-1,]
        sixth_subset$energy_sum3 <- sixth_subset$energy_sum3 + tail(fifth_subset$energy_sum3,1)
      } 
      else if(i == 7 & length(diff_values) >= 7) {
        index7 = which(iterate.df$diff_again == diff_values[7])
        seventh_subset <- iterate.df[(index6+1):index7-1,]
        seventh_subset$energy_sum3 <- seventh_subset$energy_sum3 + tail(sixth_subset$energy_sum3,1)
      } 
      else if(i == 8 & length(diff_values) >= 8) {
        index8 = which(iterate.df$diff_again == diff_values[8])
        eighth_subset <- iterate.df[(index7+1):index8-1,]
        eighth_subset$energy_sum3 <- eighth_subset$energy_sum3 + tail(seventh_subset$energy_sum3,1)
      } 
      else if(i == 9 & length(diff_values) >= 9) {
        index9 = which(iterate.df$diff_again == diff_values[9])
        ninth_subset <- iterate.df[(index8+1):index9-1,]
        ninth_subset$energy_sum3 <- ninth_subset$energy_sum3 + tail(eighth_subset$energy_sum3,1)
      } 
      else if(i == 10 & length(diff_values) >= 10) {
        index10 = which(iterate.df$diff_again == diff_values[10])
        tenth_subset <- iterate.df[(index9+1):index10-1,]
        tenth_subset$energy_sum3 <- tenth_subset$energy_sum3 + tail(ninth_subset$energy_sum3,1)
      } 
       else if(i == 11 & length(diff_values) >= 11) {
      index11 = which(iterate.df$diff_again == diff_values[10])
      eleventh_subset <- iterate.df[(index10+1):index11-1,]
      eleventh_subset$energy_sum3 <- eleventh_subset$energy_sum3 + tail(tenth_subset$energy_sum3,1)
       }
      else if(i == 12 & length(diff_values) >= 12) {
         index12 = which(iterate.df$diff_again == diff_values[10])
         twelfth_subset <- iterate.df[(index11+1):index12-1,]
         twelfth_subset$energy_sum3 <- twelfth_subset$energy_sum3 + tail(eleventh_subset$energy_sum3,1)
      } 
     else if(i == 13 & length(diff_values) >= 13) {
      index13 = which(iterate.df$diff_again == diff_values[10])
      thirteenth_subset <- iterate.df[(index12+1):index13-1,]
      thirteenth_subset$energy_sum3 <- thirteenth_subset$energy_sum3 + tail(twelfth_subset$energy_sum3,1)
     } 
    else if(i == 14 & length(diff_values) >= 14) {
      index14 = which(iterate.df$diff_again == diff_values[10])
      fourteenth_subset <- iterate.df[(index13+1):index14-1,]
      fourteenth_subset$energy_sum3 <- fourteenth_subset$energy_sum3 + tail(thirteenth_subset$energy_sum3,1)
    } 
    else if(i == 15 & length(diff_values) >= 15) {
      index15 = which(iterate.df$diff_again == diff_values[10])
      fifeteenth_subset <- iterate.df[(index14+1):index15-1,]
      fifeteenth_subset$energy_sum3 <- fifeteenth_subset$energy_sum3 + tail(fourteenth_subset$energy_sum3,1)
    } 
    else if(i == 16 & length(diff_values) >= 16) {
      index16 = which(iterate.df$diff_again == diff_values[10])
      sixteenth_subset <- iterate.df[(index14+1):index15-1,]
      sixteenth_subset$energy_sum3 <- sixteenth_subset$energy_sum3 + tail(fifeteenth_subset$energy_sum3,1)
    } 
    else if(i == 17 & length(diff_values) >= 17) {
      index17 = which(iterate.df$diff_again == diff_values[10])
      seventeenth_subset <- iterate.df[(index14+1):index15-1,]
      seventeenth_subset$energy_sum3 <- seventeenth_subset$energy_sum3 + tail(sixteenth_subset$energy_sum3,1)
    }
    else if(i == 18 & length(diff_values) >= 18) {
      index18 = which(iterate.df$diff_again == diff_values[10])
      eighteenth_subset <- iterate.df[(index14+1):index15-1,]
      eighteenth_subset$energy_sum3 <- eighteenth_subset$energy_sum3 + tail(seventeenth_subset$energy_sum3,1)
    } 
    else if(i == 19 & length(diff_values) >= 19) {
      index19 = which(iterate.df$diff_again == diff_values[10])
      nineteenth_subset <- iterate.df[(index14+1):index15-1,]
      nineteenth_subset$energy_sum3 <- nineteenth_subset$energy_sum3 + tail(eighteenth_subset$energy_sum3,1)
    } 
    else if(i == 20 & length(diff_values) >= 20) {
      index20 = which(iterate.df$diff_again == diff_values[10])
      twentieth_subset <- iterate.df[(index14+1):index15-1,]
      twentieth_subset$energy_sum3 <- twentieth_subset$energy_sum3 + tail(nineteenth_subset$energy_sum3,1)
    }
    else if(i == 21 & length(diff_values) >= 21) {
      index21 = which(iterate.df$diff_again == diff_values[10])
      twentyfirst_subset <- iterate.df[(index14+1):index15-1,]
      twentyfirst_subset$energy_sum3 <- twentyfirst_subset$energy_sum3 + tail(twentieth_subset$energy_sum3,1)
    }
    else if(i == 22 & length(diff_values) >= 22) {
      index22 = which(iterate.df$diff_again == diff_values[10])
      twentysecond_subset <- iterate.df[(index14+1):index15-1,]
      twentysecond_subset$energy_sum3 <- twentysecond_subset$energy_sum3 + tail(twentyfirst_subset$energy_sum3,1)
    } else {} 
  } 
    
  #Binding all the slices and or subsets together
    
    if (length(diff_values) == 1){
      super <- rbind(first_subset,first_subset2)
    } else if (length(diff_values) == 2){
      super <- rbind(first_subset,second_subset)
    } else if (length(diff_values) == 3) {
      super <- rbind(first_subset,second_subset,third_subset)
    } else if (length(diff_values) == 4) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset)
    } else if (length(diff_values) == 5) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset)
    } else if (length(diff_values) == 6) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset)
    } else if (length(diff_values) == 7) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset)
    } else if (length(diff_values) == 8) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset)
    } else if (length(diff_values) == 9) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset)
    } else if (length(diff_values) == 10) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset)
    } else if (length(diff_values) == 11) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset)
    } else if (length(diff_values) == 12) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset)
    } else if (length(diff_values) == 13) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset,thirteenth_subset)
    } else if (length(diff_values) == 14) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset,thirteenth_subset,fourteenth_subset)
    } else if (length(diff_values) == 15) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset,thirteenth_subset,fourteenth_subset,fifeteenth_subset)
    } else if (length(diff_values) == 16) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset,thirteenth_subset,fourteenth_subset,fifeteenth_subset,sixteenth_subset)
    } else if (length(diff_values) == 17) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset,thirteenth_subset,fourteenth_subset,fifeteenth_subset,sixteenth_subset, seventeenth_subset)
    } else if (length(diff_values) == 18) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset,thirteenth_subset,fourteenth_subset,fifeteenth_subset,sixteenth_subset, seventeenth_subset,eighteenth_subset)
    } else if (length(diff_values) == 19) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset,thirteenth_subset,fourteenth_subset,fifeteenth_subset,sixteenth_subset, seventeenth_subset,eighteenth_subset,nineteenth_subset)
    } else if (length(diff_values) == 20) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset,thirteenth_subset,fourteenth_subset,fifeteenth_subset,sixteenth_subset, seventeenth_subset,eighteenth_subset,nineteenth_subset,twentieth_subset)
    } else if (length(diff_values) == 21) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset,thirteenth_subset,fourteenth_subset,fifeteenth_subset,sixteenth_subset, seventeenth_subset,eighteenth_subset,nineteenth_subset,twentieth_subset,twentyfirst_subset)
    } else if (length(diff_values) == 22) {
      super <- rbind(first_subset,second_subset,third_subset,fourth_subset,fifth_subset,sixth_subset,seventh_subset,eighth_subset,ninth_subset,tenth_subset,eleventh_subset,twelfth_subset,thirteenth_subset,fourteenth_subset,fifeteenth_subset,sixteenth_subset, seventeenth_subset,eighteenth_subset,nineteenth_subset,twentieth_subset,twentyfirst_subset,twentysecond_subset)
    }
    else {}
    
    microid.data.input <- super
  } else {
    
    microid.data.input <- iterate.df
    
  }
  
  ########################## MAKING SURE THAT THE FRIDGE VALUES ARE MONOTINCALLY INCREASING
  
      energy_plot <- ggplot(microid.data.input,aes(1:length(microid.data.input$energy_sum3),microid.data.input$energy_sum3)) + geom_line()
      plot.name <- unique(microid.data.input$house.id)
      name <- substring(plot.name,1,35)
      mypath <- file.path(paste("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports/",dump,"/Latex/figure/figure_fridge_checks/",plot.name,".jpg",sep = ""))
      jpeg(file=mypath)
      print(energy_plot)
      dev.off()

  
  
  ##########################################################################################
  
  microid.data.input$date_hour <- paste(microid.data.input$date,microid.data.input$hour)
  microid.data.input$time_stamp <- strptime(microid.data.input$date_hour,"%Y-%m-%d %H")
  
  
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
        subset.df$energy_sum3 <- subset.df$energy_sum3/1000
        unique.subset[count] <- tail(subset.df$energy_sum3,1) - head(subset.df$energy_sum3,1)
        
      }
    }
  }
  
  fenergy.hourly.consumption <- cbind(unique.combinations,unique.subset) %>% mutate(fridge.energy = unique.subset) %>% select(house.id,date,hour,fridge.energy)
  fenergy.hourly.consumption <- subset(fenergy.hourly.consumption,fenergy.hourly.consumption$fridge.energy >= 0)
  fenergy.hourly.consumption$norm.energy <- (fenergy.hourly.consumption$fridge.energy - min(fenergy.hourly.consumption$fridge.energy))/(max(fenergy.hourly.consumption$fridge.energy )-min(fenergy.hourly.consumption$fridge.energy ))
  
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


neighbor.comparison <- function(data_input,dump){
  
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
    sub.iterate <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
  
    if (dim(sub.iterate)[1] == 0) {
      
    } else {
      if (tail(sub.iterate$house_Energy,1) < head(sub.iterate$house_Energy,1)) {
        
        sub.iterate$lagged2 <-  lag(sub.iterate$house_Energy,k=1)
        sub.iterate$diff2 <- sub.iterate$house_Energy - sub.iterate$lagged2 
        val.id <- subset(sub.iterate,sub.iterate$diff2 < 0)
        val.row <- which(sub.iterate$diff2 == head(val.id$diff2,1))
        
        sub.iterate.one <- sub.iterate[1:(val.row-1),]
        sub.iterate.two <- sub.iterate[val.row:length(sub.iterate$diff2),]
        sub.iterate.two$house_Energy2 <- tail(sub.iterate.one$house_Energy,1) + sub.iterate.two$house_Energy
        sub.iterate.two$house_Energy <- sub.iterate.two$house_Energy2 
        vars <- c("house.id","date","hour","minute","second","house_Energy","lagged","diff")
        pass.df <- rbind(sub.iterate.one[,vars],sub.iterate.two[,vars])
        
        microids.df[[i]] <- pass.df
      } else {
        
        microids.df[[i]] <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
      }
      
    }
    }
    
  #microid.data.input <- rbind(microids.df[[1]],microids.df[[2]],microids.df[[3]],microids.df[[4]],microids.df[[5]],microids.df[[6]],microids.df[[7]],microids.df[[8]],microids.df[[9]],microids.df[[10]],microids.df[[11]],microids.df[[12]],microids.df[[13]],microids.df[[14]],microids.df[[15]],microids.df[[16]],microids.df[[17]],microids.df[[18]],microids.df[[19]])
  
  #Reincorporate this when we get data for A8
  #Reincorporate this wben we get data for A6 back
  #microid.data.input <- rbind(microids.df[[1]],microids.df[[2]],microids.df[[3]],microids.df[[4]],microids.df[[5]],microids.df[[6]],microids.df[[7]],microids.df[[8]],microids.df[[9]],microids.df[[10]],microids.df[[11]],microids.df[[12]],microids.df[[13]],microids.df[[14]],microids.df[[15]],microids.df[[16]],microids.df[[17]],microids.df[[18]],microids.df[[19]],microids.df[[20]])
  microid.data.input <- rbind(microids.df[[1]],microids.df[[2]],microids.df[[3]],microids.df[[4]],microids.df[[5]],microids.df[[6]],microids.df[[7]],microids.df[[8]],microids.df[[9]],microids.df[[10]],microids.df[[11]],microids.df[[12]],microids.df[[13]],microids.df[[14]],microids.df[[15]],microids.df[[16]],microids.df[[17]],microids.df[[18]],microids.df[[19]])
  
  #Setting unique combinations of microids
  unique.microids <- unique(microid.data.input$house.id)
  unique.subset <- data.frame(matrix(ncol = 2, nrow = length(unique.microids))) %>% mutate(house.id = X1,energy=X2) %>% select(house.id,energy)
  
  ################ ***************** ######################################
  #### Plotting to make sure that the energy values are good for all houses
  
  
  for (i in 1:length(unique.microids)) {
    if (is.null(dim(microids.df[[i]]))) {
      } 
    else {      
      energy_plot <- ggplot(microids.df[[i]],aes(1:length(microids.df[[i]]$house_Energy),microids.df[[i]]$house_Energy)) + geom_line()
    plot.name = unique(microids.df[[i]]$house.id)
    name <- substring(plot.name,1,35)
    mypath <- file.path(paste("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports/",dump,"/Latex/figure/figure_energy_checks/",plot.name,".jpg",sep = ""))
    jpeg(file=mypath)
    print(energy_plot)
    dev.off()
    }
  }
  
  
  ##########################################################################
  
  
  for (i in 1:length(unique.microids)) {
    
    subset.microid <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
    unique.subset$house.id[i] <- unique.microids[i]
    unique.subset$energy[i] <- tail(subset.microid$house_Energy,1) - head(subset.microid$house_Energy,1)
  }
  
  
  unique.subset$norm.energy <- (unique.subset$energy - min(unique.subset$energy))/(max(unique.subset$energy)-min(unique.subset$energy))
  
  return(unique.subset)
}



################## 6. Neighbor Comparison in Parallel


neighbor.comparison.parallel <- function(data_input){
  
  #Getting rid of zeros and NaNs  
  unique.data.input <- date.vars.simple(date.data.frames.simple.v2(data_input))
  unique.data.input <-  unique(unique.data.input[c("house.id","datetime_rgx","house_Energy")])
  unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  unique.data.input$lagged <-  lag(unique.data.input$house_Energy,k=1)
  
  
  #Making sure that we are keeping non-negative and non-outlier data
  unique.microids <- unique(unique.data.input$house.id)
  microids.df <- list()
  
  #Getting rid of outliers
  microids.df <- foreach(i=1:length(unique.microids),.combine=rbind) %dopar% {
    iterate.df <- subset(unique.data.input,unique.data.input$house.id == unique.microids[[i]])
    iterate.df <- iterate.df[complete.cases(iterate.df),] #Getting rid of all NAs
    iterate.df$diff <- iterate.df$house_Energy - iterate.df$lagged
    microids.df.loop <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
    return(iterate.df)
  }
  
  microid.data.input<- microids.df
  
  unique.subset <- data.frame(matrix(ncol = 2, nrow = length(unique.microids))) %>% mutate(house.id = X1,energy=X2) %>% select(house.id,energy)
  
  unique.subset.df <- foreach(i=1:length(unique.microids),.combine=rbind) %dopar% {
    subset.microid <- subset(microid.data.input,microid.data.input$house.id == unique.microids[i])
    unique.subset$house.id[[i]] <- unique.microids[[i]]
    unique.subset$energy[[i]] <- tail(subset.microid$house_Energy,1) - head(subset.microid$house_Energy,1)
    return(unique.subset)
  } 
  
  unique.subset.df <- unique.subset.df[complete.cases(unique.subset.df),] 
  
  unique.subset.df$norm.energy <- (unique.subset.df$energy - min(unique.subset.df$energy))/(max(unique.subset.df$energy)-min(unique.subset.df$energy))
  return(unique.subset.df)
}



#################  7. HOUSEHOLD ENERGY CONSUMPTION PER HOUR: INDIVIDUAL HOUSE   - PARALLEL

henergy.consumption.hour.house.parallel <- function(data_input) {
  
  #Getting rid of zeros and NaNs  
  unique.data.input <- date.vars.simple(date.data.frames.simple.v2(data_input))
  unique.data.input <-  unique(unique.data.input[c("house.id","date","hour","minute","second","house_Energy")])
  unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  unique.data.input$lagged <-  lag(unique.data.input$house_Energy,k=1)
  
  #Getting rid of outliers    
  iterate.df <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  iterate.df$diff <- iterate.df$house_Energy - iterate.df$lagged
  microids.df <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
  microid.data.input<- microids.df
  
  #Setting unique combinations and values so that we can iterate
  unique.combinations <- unique(microids.df[c("house.id","date","hour")])
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
  energy.hourly.consumption$norm.energy <- (energy.hourly.consumption$energy - min(energy.hourly.consumption$energy))/(max(energy.hourly.consumption$energy)-min(energy.hourly.consumption$energy))
  
  return(energy.hourly.consumption)
  
}





####################  8 Parallel house

eday.analysis.house.parallel <- function(data_input) {
  
  data_input <- date.vars.simple(data_input)  
  data_input$day.type <- weekdays(data_input$date)
  
  #Getting rid of zeros and NaNs  
  unique.data.input <-  unique(data_input[c("house.id","date","day.type","hour","minute","second","house_Energy")])
  unique.data.input[unique.data.input==0] <- NA #Getting rid of zeros
  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  unique.data.input <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  unique.data.input$lagged <-  lag(unique.data.input$house_Energy,k=1)
  
  #Making sure that we are keeping non-negative and non-outlier data
  unique.microids <- unique(unique.data.input$house.id)
  microids.df <- list()
  
  #Getting rid of outliers    
  iterate.df <- unique.data.input[complete.cases(unique.data.input),] #Getting rid of all NAs
  iterate.df$diff <- iterate.df$house_Energy - iterate.df$lagged
  microids.df <- subset(iterate.df,iterate.df$diff <= quantile(iterate.df$diff,c(0.99)) & iterate.df$diff > 0)
  
  microid.data.input <-  microids.df
  
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
  energy.daily.consumption$norm.energy <- (energy.daily.consumption$energy - min(energy.daily.consumption$energy))/(max(energy.daily.consumption$energy)-min(energy.daily.consumption$energy))
  
  return(energy.daily.consumption)
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




