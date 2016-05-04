# This file reads the files in before beginning the analysis
# 1) sets the path, 2) reads files


#Brings in data from one house
read.data <- function(dump,flexboxid) {
  
  path0 <- '/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/DUMPS/'
  true_path <- paste(path0,dump,sep='')
  
  setwd(true_path)
  
  files <- list.files(path=true_path, pattern="*.csv", full.names=T, recursive=FALSE)
  
  part1 <- "(?=.*"
  part2 <- flexboxid
  part3 <- "\\.)(?=.*inside)"
  part4 <- "\\.)(?=.*ambient)"
  part5 <- "\\.)(?=.*fridge)" #before it was: part5 <- "\\.)(?=.*mfi)"
  part6 <- "\\.)(?=.*switch)"
  part7 <- "\\.)(?=.*house)" #before it was:  part7 <- "\\.)(?=.*zwave)" 
  
  inside_regex <- paste(part1,part2,part3,sep='')
  ambient_regex <- paste(part1,part2,part4,sep='')
  mfi_regex <- paste(part1,part2,part5,sep='')
  switch_regex <- paste(part1,part2,part6,sep='')
  zwave_regex <- paste(part1,part2,part7,sep='')
  
  true_inside <- grepl(inside_regex, files,perl=TRUE)
  true_ambient <- grepl(ambient_regex, files,perl=TRUE)
  true_mfi <- grepl(mfi_regex, files,perl=TRUE)
  true_switch <- grepl(switch_regex, files,perl=TRUE)
  zwave_house <- grepl(zwave_regex, files,perl=TRUE)
  
  true_inside_i <- match('TRUE',true_inside)
  true_ambient_i <- match('TRUE',true_ambient)
  true_mfi_i <- match('TRUE',true_mfi)
  true_switch_i <- match('TRUE',true_switch)
  true_zwave_i <- match('TRUE',zwave_house)
  
  inside <- read.csv(files[true_inside_i])
  ambient <- read.csv(files[true_ambient_i])
  refrigerator <- read.csv(files[true_mfi_i])
  switch <- read.csv(files[true_switch_i]) %>% mutate(open=switch) %>% select(id,datetime,open)
  house <- read.csv(files[true_zwave_i])
  
  inside$house.id <- flexboxid
  inside$inside_temp1 <- inside$inside_temp1/1000
  inside$inside_temp2 <- inside$inside_temp2/1000
  inside$fridge_temp1 <- inside$inside_temp1
  inside$fridge_temp2 <- inside$fridge_temp2
  ambient$house.id <- flexboxid
  refrigerator$house.id <- flexboxid
  switch$house.id <- flexboxid
  switch$open <- gsub(" ", "",switch$open, fixed = TRUE) 
  switch$value <- ifelse(as.character(switch$open)=='t',1,0)
  house$house.id <- flexboxid
  
  return(list(inside,ambient,refrigerator,switch,house))
}



#Brings in data from several houses

read.data.all <- function(dump,flexboxlist) {
  
  for(i in 1:length(flexboxlist)) {
    
    path0 <- '/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/DUMPS/'
    true_path <- paste(path0,dump,sep='')
    
    setwd(true_path)
    
    files <- list.files(path=true_path, pattern="*.csv", full.names=T, recursive=FALSE)
    
    if (dump == 'DUMP1') {
    part1 <- "(?=.*"
    part2 <- flexboxlist[i]
    part3 <- "\\.)(?=.*inside)"
    part4 <- "\\.)(?=.*ambient)"
    part5 <- "\\.)(?=.*mfi)" # DUMP1: mfi, DUMPX: fridge
    part6 <- "\\.)(?=.*switch)"
    part7 <- "\\.)(?=.*zwave)" # DUMP1: zwave, DUMPX: house
    } else {
      part1 <- "(?=.*"
      part2 <- flexboxlist[i]
      part3 <- "\\.)(?=.*inside)"
      part4 <- "\\.)(?=.*ambient)"
      part5 <- "\\.)(?=.*fridge)" # DUMP1: mfi, DUMPX: fridge
      part6 <- "\\.)(?=.*switch)"
      part7 <- "\\.)(?=.*house)" # DUMP1: zwave, DUMPX: house
    }
    
    inside_regex <- paste(part1,part2,part3,sep='')
    ambient_regex <- paste(part1,part2,part4,sep='')
    mfi_regex <- paste(part1,part2,part5,sep='')
    switch_regex <- paste(part1,part2,part6,sep='')
    zwave_regex <- paste(part1,part2,part7,sep='')
    
    true_inside <- grepl(inside_regex, files,perl=TRUE)
    true_ambient <- grepl(ambient_regex, files,perl=TRUE)
    true_mfi <- grepl(mfi_regex, files,perl=TRUE)
    true_switch <- grepl(switch_regex, files,perl=TRUE)
    zwave_house <- grepl(zwave_regex, files,perl=TRUE)
    
    true_inside_i <- match('TRUE',true_inside)
    true_ambient_i <- match('TRUE',true_ambient)
    true_mfi_i <- match('TRUE',true_mfi)
    true_switch_i <- match('TRUE',true_switch)
    true_zwave_i <- match('TRUE',zwave_house)
    
    if (i==1) {
      inside <- read.csv(files[true_inside_i])
      ambient <- read.csv(files[true_ambient_i])
      refrigerator <- read.csv(files[true_mfi_i])
      switch<- read.csv(files[true_switch_i])
      house<- read.csv(files[true_zwave_i])
      
      inside$house.id <- flexboxlist[i]
      inside$inside_temp1 <- inside$fridge_temp1/1000 # DUMP1: fridge_temp1 , DUMPX: inside_temp1
      inside$inside_temp2 <- inside$fridge_temp2/1000 # DUMP2: fridge_temp2 , DUMPX: inside_temp2
      inside$fridge_temp1 <- inside$inside_temp1
      inside$fridge_temp2 <- inside$inside_temp2
      ambient$house.id <- flexboxlist[i]
      refrigerator$house.id <- flexboxlist[i]
      switch$house.id <- flexboxlist[i]
      house$house.id <- flexboxlist[i]
    } else {
      
      inside.else <- read.csv(files[true_inside_i])
      ambient.else <- read.csv(files[true_ambient_i])
      refrigerator.else <- read.csv(files[true_mfi_i])
      switch.else <- read.csv(files[true_switch_i])
      house.else <- read.csv(files[true_zwave_i])
      
      inside.else$house.id <- flexboxlist[i]
      inside.else$inside_temp1 <- inside.else$fridge_temp1/1000 # DUMP1: fridge_temp1 , DUMPX: inside_temp1
      inside.else$inside_temp2 <- inside.else$fridge_temp2/1000 # DUMP2: fridge_temp2 , DUMPX: inside_temp2
      inside.else$fridge_temp1 <- inside.else$inside_temp1/1000
      inside.else$fridge_temp2 <- inside.else$inside_temp2/1000
      
      ambient.else$house.id <- flexboxlist[i]
      refrigerator.else$house.id <- flexboxlist[i]
      switch.else$house.id <- flexboxlist[i]
      house.else$house.id <- flexboxlist[i]
      
      inside <- rbind(inside,inside.else)
      ambient <- rbind(ambient,ambient.else)
      refrigerator <- rbind(refrigerator,refrigerator.else)
      switch <- rbind(switch,switch.else)
      house <- rbind(house,house.else)
    }
  }
  return(list(inside,ambient,refrigerator,switch,house))
}


###### Running it in parallel in the cluster

read.data.all.parallel <- function(dump,flexboxlist) {
  
  out.dfs  <- foreach(i = 1:length(flexboxlist)) %dopar% {
    
    path0 <- '/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/DUMPS/'
    #path0 <- '/home/diego/Data/NicaraguaDumps/'
    true_path <- paste(path0,dump,sep='')
    
    setwd(true_path)
    
    files <- list.files(path=true_path, pattern="*.csv", full.names=T, recursive=FALSE)
    
    part1 <- "(?=.*"
    part2 <- flexboxlist[[i]]
    part3 <- "\\.)(?=.*inside)"
    part4 <- "\\.)(?=.*ambient)"
    part5 <- "\\.)(?=.*fridge)" # (DUMP1 was mfi)(DUMP4 was fridge)
    part6 <- "\\.)(?=.*switch)"
    part7 <- "\\.)(?=.*house)" # (DUMP1 was zwave)(DUMP4 was house)
    
    inside_regex <- paste(part1,part2,part3,sep='')
    ambient_regex <- paste(part1,part2,part4,sep='')
    mfi_regex <- paste(part1,part2,part5,sep='')
    switch_regex <- paste(part1,part2,part6,sep='')
    zwave_regex <- paste(part1,part2,part7,sep='')
    
    true_inside <- grepl(inside_regex, files,perl=TRUE)
    true_ambient <- grepl(ambient_regex, files,perl=TRUE)
    true_mfi <- grepl(mfi_regex, files,perl=TRUE)
    true_switch <- grepl(switch_regex, files,perl=TRUE)
    zwave_house <- grepl(zwave_regex, files,perl=TRUE)
    
    true_inside_i <- match('TRUE',true_inside)
    true_ambient_i <- match('TRUE',true_ambient)
    true_mfi_i <- match('TRUE',true_mfi)
    true_switch_i <- match('TRUE',true_switch)
    true_zwave_i <- match('TRUE',zwave_house)
    
    inside <- read.csv(files[true_inside_i])
    ambient <- read.csv(files[true_ambient_i])
    refrigerator <- read.csv(files[true_mfi_i])
    switch.df<- read.csv(files[true_switch_i])
    house<- read.csv(files[true_zwave_i])
    
    inside$house.id <- flexboxlist[[i]]
    inside$inside_temp1 <- inside$inside_temp1/1000
    inside$inside_temp2 <- inside$inside_temp2/1000
    inside$fridge_temp1 <- inside$inside_temp1
    inside$fridge_temp2 <- inside$inside_temp2
    ambient$house.id <- flexboxlist[[i]]
    refrigerator$house.id <- flexboxlist[[i]]
    switch.df$house.id <- flexboxlist[[i]]
    switch.df$open <- gsub(" ", "",switch.df$switch, fixed = TRUE) 
    switch.df$value <- ifelse(as.character(switch.df$open)=='t',1,0)
    house$house.id <- flexboxlist[[i]]
    
    
    ####################### Date Function goes here
    
    ambient$datetime_rgx <- gsub("\\..*","",ambient$datetime)
    inside$datetime_rgx <- gsub("\\..*","", inside$datetime)
    refrigerator$datetime_rgx <- gsub("\\..*","", refrigerator$datetime)
    switch.df$datetime_rgx <- gsub("\\..*","", switch.df$datetime)
    house$datetime_rgx <- gsub("\\..*","", house$datetime)
    
    #Dates with times 
    ambient$datetime_rgx_new <- paste(ambient$datetime_rgx,"-0000",sep=" ")
    ambient$time_stamp <- strptime(ambient$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
    
    inside$datetime_rgx_new <- paste(inside$datetime_rgx,"-0000",sep=" ")
    inside$time_stamp <- strptime(inside$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
    
    refrigerator$datetime_rgx_new <- paste(refrigerator$datetime_rgx,"-0000",sep=" ")
    refrigerator$time_stamp <- strptime(refrigerator$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
    
    switch.df$datetime_rgx_new <- paste(switch.df$datetime_rgx,"-0000",sep=" ")
    switch.df$time_stamp <- strptime(switch.df$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
    
    house$datetime_rgx_new <- paste(house$datetime_rgx,"-0000",sep=" ")
    house$time_stamp <- strptime(house$datetime_rgx_new,"%Y-%m-%d %H:%M:%S %z")
    
    
    #Just dates (day without time)
    ambient$date <- as.Date(ambient$time_stamp)
    inside$date <- as.Date(inside$time_stamp)
    switch.df$date <- as.Date(switch.df$time_stamp)
    house$date <- as.Date(house$time_stamp)
    refrigerator$date <- as.Date(refrigerator$time_stamp)
    
    #Only complete cases
    ambient.df <- ambient[complete.cases(ambient$date),]
    inside.df <- inside[complete.cases(inside$date),]
    switch.df <- switch.df[complete.cases(switch.df$date),]
    house.df <- house[complete.cases(house$date),]
    refrigerator.df <- refrigerator[complete.cases(refrigerator$date),]
    
    #Staying values
    ambient.export <- subset(ambient.df,ambient.df$date >= (tail(unique(inside$date),1) - 30))
    inside.export <- subset(inside.df,inside.df$date >= (tail(unique(inside.df$date),1) - 30))
    switch.export <- subset(switch.df,switch.df$date >= (tail(unique(switch.df$date),1) - 30))
    house.export <- subset(house.df,house.df$date >= (tail(unique(house.df$date),1) - 30))
    refrigerator.export <- subset(refrigerator.df,refrigerator.df$date >= (tail(unique(refrigerator.df$date),1) - 30))
    #################################
    
    df.lists <- list(inside.export,ambient.export,refrigerator.export,switch.export,house.export)
    return(df.lists)
  }
  return(out.dfs)
}


############ Read data fridge - house


read.house.fridge <- function(dump,flexboxlist) {
  
  for(i in 1:length(flexboxlist)) {
    
    path0 <- '/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/DUMPS/'
    true_path <- paste(path0,dump,sep='')
    
    setwd(true_path)
    
    files <- list.files(path=true_path, pattern="*.csv", full.names=T, recursive=FALSE)
    
    part1 <- "(?=.*"
    part2 <- flexboxlist[i]
    part5 <- "\\.)(?=.*fridge)" # DUMP1: mfi, DUMPX: fridge
    part7 <- "\\.)(?=.*house)" # DUMP1: zwave, DUMPX: house
    
    mfi_regex <- paste(part1,part2,part5,sep='')
    zwave_regex <- paste(part1,part2,part7,sep='')
    true_mfi <- grepl(mfi_regex, files,perl=TRUE)
    zwave_house <- grepl(zwave_regex, files,perl=TRUE)
    
    true_mfi_i <- match('TRUE',true_mfi)
    true_zwave_i <- match('TRUE',zwave_house)
    
    if (i==1) {
      refrigerator <- read.csv(files[true_mfi_i])
      house<- read.csv(files[true_zwave_i])

      refrigerator$house.id <- flexboxlist[i]
      
      if (flexboxlist[i]=='A1' | flexboxlist[i]=='A17' | flexboxlist[i]=='A24' | flexboxlist[i]=='A26' | flexboxlist[i]=='A28'){
      vars <- c('id','datetime','houseAll_Voltage','houseAll_Current','houseAll_Power','houseAll_Energy')
      house <- house[,vars]
      house$house.id <- flexboxlist[i] 
      } else {
      vars <- c('id','datetime','house_Voltage','house_Current','house_Power','house_Energy')
      house <- house[,vars] %>% mutate(houseAll_Voltage=house_Voltage,houseAll_Current=house_Current,houseAll_Power=house_Power,houseAll_Energy=house_Energy) %>% select(id,datetime,houseAll_Voltage,houseAll_Current,houseAll_Power,houseAll_Energy)
      house$house.id <- flexboxlist[i] 
      }
      
    } else {
      
      refrigerator.else <- read.csv(files[true_mfi_i])
      house.else <- read.csv(files[true_zwave_i])
      
      refrigerator.else$house.id <- flexboxlist[i]
      house.else$house.id <- flexboxlist[i]
      
      if (flexboxlist[i]=='A1' | flexboxlist[i]=='A17' | flexboxlist[i]=='A24' | flexboxlist[i]=='A26' | flexboxlist[i]=='A28'){
        vars <- c('id','datetime','houseAll_Voltage','houseAll_Current','houseAll_Power','houseAll_Energy')
        house.else <- house.else[,vars]
        house.else$house.id <- flexboxlist[i] 
        } else {
      vars <- c('id','datetime','house_Voltage','house_Current','house_Power','house_Energy')
      house.else <- house.else[,vars] %>% mutate(houseAll_Voltage=house_Voltage,houseAll_Current=house_Current,houseAll_Power=house_Power,houseAll_Energy=house_Energy) %>% select(id,datetime,houseAll_Voltage,houseAll_Current,houseAll_Power,houseAll_Energy)
      house.else$house.id <- flexboxlist[i] 
    }
      
      refrigerator <- rbind(refrigerator,refrigerator.else)
      house <- rbind(house,house.else)
    }
  }
  return(list(refrigerator,house))
}


########## Read Survey Data





