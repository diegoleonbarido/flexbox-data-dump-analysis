# Function: Turn into a Montonically Increasing Trend
# MFI: Gets rid of outliers and makes sure that everything is monotonically increasing 

cleaning_mfi_resets <- function(data,variable) {
  
  data_sub <-  data[,c('hostname','datetime',variable),with=FALSE]
  data_sub[data_sub==0] <- NA #Getting rid of zeros
  data_sub <- na.omit(data_sub) #Getting rid of all NAs
  
  data_sub$lagged <-  lag(data_sub[[variable]],k=1)
  data_sub <- na.omit(data_sub) #Getting rid of all NAs
  data_sub$diff <- data_sub[[variable]] - data_sub$lagged
  
  
  events <- data_sub[data_sub$diff < 0]
  events_sub <- events[events$diff < quantile(events$diff,c(0.99)) ]
  diff_values <- events_sub$diff
  
  iterate.df <- data_sub
  iterate.df$diff_again <- iterate.df$diff
  
  if (length(diff_values) >= 1) {
    
    for (i in 1:length(diff_values)){
      
      if (i == 1) {
        index1 = which(iterate.df$diff_again == diff_values[1])
        first_subset <- iterate.df[1:(index1-1),]
        
        first_subset2 <- iterate.df[(index1+1):length(iterate.df$datetime),]
        first_subset2[[variable]] <- first_subset2[[variable]] + tail(first_subset[[variable]],1)
        
      } 
      else if(i == 2 & length(diff_values) >= 2) {
        index2 = which(iterate.df$diff_again == diff_values[2])
        second_subset <- iterate.df[(index1+1):index2-1,]
        second_subset[[variable]]<- second_subset[[variable]] + tail(first_subset[[variable]],1)
      } 
      else if(i == 3 & length(diff_values) >= 3) {
        index3 = which(iterate.df$diff_again == diff_values[3])
        third_subset <- iterate.df[(index2+1):index3-1,]
        third_subset[[variable]]<- third_subset[[variable]]+ tail(second_subset[[variable]],1)
      } 
      else if(i == 4 & length(diff_values) >= 4) {
        index4 = which(iterate.df$diff_again == diff_values[4])
        fourth_subset <- iterate.df[(index3+1):index4-1,]
        fourth_subset[[variable]]<- fourth_subset[[variable]]+ tail(third_subset[[variable]],1)
      } 
      else if(i == 5 & length(diff_values) >= 5) {
        index5 = which(iterate.df$diff_again == diff_values[5])
        fifth_subset <- iterate.df[(index4+1):index5-1,]
        fifth_subset[[variable]]<- fifth_subset[[variable]]+ tail(fourth_subset[[variable]],1)
      } 
      else if(i == 6 & length(diff_values) >= 6) {
        index6 = which(iterate.df$diff_again == diff_values[6])
        sixth_subset <- iterate.df[(index5+1):index6-1,]
        sixth_subset[[variable]]<- sixth_subset[[variable]]+ tail(fifth_subset[[variable]],1)
      } 
      else if(i == 7 & length(diff_values) >= 7) {
        index7 = which(iterate.df$diff_again == diff_values[7])
        seventh_subset <- iterate.df[(index6+1):index7-1,]
        seventh_subset[[variable]]<- seventh_subset[[variable]]+ tail(sixth_subset[[variable]],1)
      } 
      else if(i == 8 & length(diff_values) >= 8) {
        index8 = which(iterate.df$diff_again == diff_values[8])
        eighth_subset <- iterate.df[(index7+1):index8-1,]
        eighth_subset[[variable]]<- eighth_subset[[variable]]+ tail(seventh_subset[[variable]],1)
      } 
      else if(i == 9 & length(diff_values) >= 9) {
        index9 = which(iterate.df$diff_again == diff_values[9])
        ninth_subset <- iterate.df[(index8+1):index9-1,]
        ninth_subset[[variable]]<- ninth_subset[[variable]]+ tail(eighth_subset[[variable]],1)
      } 
      else if(i == 10 & length(diff_values) >= 10) {
        index10 = which(iterate.df$diff_again == diff_values[10])
        tenth_subset <- iterate.df[(index9+1):index10-1,]
        tenth_subset[[variable]]<- tenth_subset[[variable]]+ tail(ninth_subset[[variable]],1)
      } 
      else if(i == 11 & length(diff_values) >= 11) {
        index11 = which(iterate.df$diff_again == diff_values[10])
        eleventh_subset <- iterate.df[(index10+1):index11-1,]
        eleventh_subset[[variable]]<- eleventh_subset[[variable]]+ tail(tenth_subset[[variable]],1)
      }
      else if(i == 12 & length(diff_values) >= 12) {
        index12 = which(iterate.df$diff_again == diff_values[10])
        twelfth_subset <- iterate.df[(index11+1):index12-1,]
        twelfth_subset[[variable]]<- twelfth_subset[[variable]]+ tail(eleventh_subset[[variable]],1)
      } 
      else if(i == 13 & length(diff_values) >= 13) {
        index13 = which(iterate.df$diff_again == diff_values[10])
        thirteenth_subset <- iterate.df[(index12+1):index13-1,]
        thirteenth_subset[[variable]]<- thirteenth_subset[[variable]]+ tail(twelfth_subset[[variable]],1)
      } 
      else if(i == 14 & length(diff_values) >= 14) {
        index14 = which(iterate.df$diff_again == diff_values[10])
        fourteenth_subset <- iterate.df[(index13+1):index14-1,]
        fourteenth_subset[[variable]]<- fourteenth_subset[[variable]]+ tail(thirteenth_subset[[variable]],1)
      } 
      else if(i == 15 & length(diff_values) >= 15) {
        index15 = which(iterate.df$diff_again == diff_values[10])
        fifeteenth_subset <- iterate.df[(index14+1):index15-1,]
        fifeteenth_subset[[variable]]<- fifeteenth_subset[[variable]]+ tail(fourteenth_subset[[variable]],1)
      } else {} 
    } 
    
    #Binding all the slices and or subsets together
    
    if (length(diff_values) == 1){
      super <- rbind(first_subset,first_subset2)
    }   else if (length(diff_values) == 2){
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
    } else {}
    
    microid.data.input <- super
    
  } else {
    
    microid.data.input <- iterate.df
  }
  
  return(microid.data.input)
}


# Functions: Mean Energy by Hour
#            Calculates energy consumption by hour

mean_hour_energy <- function(data,variable){

  microid.data.input <- data
  microid.data.input$date <- as.Date(microid.data.input$datetime)
  microid.data.input$hour <- hour(microid.data.input$datetime)
  microid.data.input <- as.data.frame(microid.data.input)
  
  
  #Setting unique combinations and values so that we can iterate
  unique.combinations <- unique(microid.data.input[c("date","hour")])
  unique.subset <- c()
  count <- 0
  
  subset.hours.df <- microid.data.input
  unique.dateys <- unique(subset.hours.df$date)
    
  for (j in 1:length(unique.dateys)) {
      subset.hours.df <- subset(microid.data.input, microid.data.input$date == unique.dateys[j])
      unique.hours <- unique(subset.hours.df$hour)
      
      for (k in 1:length(unique.hours)) {
        count <- count + 1
        subset.df <- subset(microid.data.input, microid.data.input$date == unique.dateys[j]  & microid.data.input$hour == unique.hours[k])
        subset.df$energy <- subset.df$energy/1000
        unique.subset[count] <- tail(subset.df$energy,1) - head(subset.df$energy,1)
      }
    }

  
  fenergy.hourly.consumption <- cbind(unique.combinations,unique.subset) %>% mutate(fridge.energy = unique.subset) %>% select(date,hour,fridge.energy)
  fenergy.hourly.consumption <- subset(fenergy.hourly.consumption,fenergy.hourly.consumption$fridge.energy >= 0)
  fenergy.hourly.consumption$norm.energy <- (fenergy.hourly.consumption$fridge.energy - min(fenergy.hourly.consumption$fridge.energy))/(max(fenergy.hourly.consumption$fridge.energy )-min(fenergy.hourly.consumption$fridge.energy ))

  return(fenergy.hourly.consumption)
}



  
  
  
  
  
  



