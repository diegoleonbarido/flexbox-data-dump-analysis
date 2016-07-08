library(timeDate)

data <- date.data.frame(data.list.houses[[1]],data.list.houses[[2]],data.list.houses[[3]],data.list.houses[[4]],data.list.houses[[5]])


inside <- date.vars.simple(data[[1]])
ambient <- date.vars.simple(data[[2]])
switch <- date.vars.simple(data[[3]])
house <- date.vars.simple(data[[4]])
fridge <- date.vars.simple(data[[5]])

unique.house.ids <- unique(inside$house.id)


count = 0
count.plot.inside <- list()
count.plot.power <- list()
count.plot.name.inside <- list()
count.plot.name.power <- list()

for (i in 1:length(unique.house.ids))  {

  subset_inside <- subset(inside,inside$house.id == unique.house.ids[i])
  subset_power <-  subset(fridge,fridge$house.id == unique.house.ids[i])
  
  unique.dates <- unique(subset_inside$date)
  
  for (y in 1:length(unique.dates)) {

  count = count +1
  
  subset_inside_day <- subset(subset_inside,subset_inside$house.id == unique.house.ids[i] & subset_inside$date == unique.dates[y])
  subset_power_day <- subset(subset_power,subset_power$house.id == unique.house.ids[i] & subset_power$date == unique.dates[y])
  
  lim1 <- (as.POSIXct(paste(unique.dates[y]," 00:00:00"), format="%Y-%m-%d %H:%M:%S"))
  lim2 <- (as.POSIXct(paste(unique.dates[y]," 23:59:59"), format="%Y-%m-%d %H:%M:%S"))

  plt1 <- ggplot(subset_inside_day,aes(time_stamp,inside_temp1)) + geom_point()+ xlim(lim1,lim2) 
  plt2 <- ggplot(subset_power_day,aes(time_stamp,active_pwr3)) + geom_point()+ xlim(lim1,lim2)

  count.plot.inside[[count]] = plt1
  count.plot.power[[count]] = plt2
  count.plot.name.inside[[count]] = paste(unique.house.ids[i],unique.dates[y],"-","fridge",sep="")
  count.plot.name.power[[count]]  =  paste(unique.house.ids[i],unique.dates[y],"-","power",sep="")
  
  }

}

# Save plots to jpeg making a separate file for each plot.
for (j in 1:count) {
  plot.name = count.plot.name.inside[[j]]
  name <- substring(plot.name,1,35)
  mypath <- file.path("/Users/Diego/Desktop/figures_for_hours_and_temp",paste(name,".jpg",sep = ""))
  
  tryCatch({
  if (grepl("NA", mypath) == FALSE) {
  jpeg(file=mypath)
  print(count.plot.inside[[j]])
  dev.off()
  } else {}
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

for (p in 1:count) {
  plot.name = count.plot.name.power[[p]]
  name <- substring(plot.name,1,35)
  mypath <- file.path("/Users/Diego/Desktop/figures_for_hours_and_temp",paste(name,".jpg",sep = ""))
  
  tryCatch({
    if (grepl("NA", mypath) == FALSE) {
      jpeg(file=mypath)
      print(count.plot.power[[p]])
      dev.off()
    } else {}
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


# Plot temperature 1 
# Plot temperature 2
# A1, A3, A6 MFI is not working 
# A7 switches the oulet from one place to ther other or the MFI might not be working
# A17 temperature is not working
# A18 has a timer, figure out what's going on with this fridge
# A25 problem with the MFI . Make sure that everything is working. 
# A28 has really whack values many times
# Check all temperature sensors make sure they are working






