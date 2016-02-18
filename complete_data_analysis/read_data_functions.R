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
  
  part1 <- "(?=.*"
  part2 <- flexboxlist[i]
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
  
  if (i==1) {
  inside <- read.csv(files[true_inside_i])
  ambient <- read.csv(files[true_ambient_i])
  refrigerator <- read.csv(files[true_mfi_i])
  switch<- read.csv(files[true_switch_i])
  house<- read.csv(files[true_zwave_i])
  
  inside$house.id <- flexboxlist[i]
  inside$inside_temp1 <- inside$inside_temp1/1000
  inside$inside_temp2 <- inside$inside_temp2/1000
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
    inside.else$inside_temp1 <- inside.else$inside_temp1/1000
    inside.else$inside_temp2 <- inside.else$inside_temp2/1000
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


