# This file reads the files in before beginning the analysis
# 1) sets the path, 2) reads files

read.data <- function(dump,flexboxid) {
  
  path0 <- '/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/DUMPS/'
  true_path <- paste(path0,dump,sep='')
  
  setwd(true_path)
  
  files <- list.files(path=true_path, pattern="*.csv", full.names=T, recursive=FALSE)
  
  part1 <- "(?=.*"
  part2 <- flexboxid
  part3 <- "\\.)(?=.*inside)"
  part4 <- "\\.)(?=.*ambient)"
  part5 <- "\\.)(?=.*mfi)"
  part6 <- "\\.)(?=.*switch)"
  part7 <- "\\.)(?=.*zwave)"
  
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
  switch <- read.csv(files[true_switch_i])
  house <- read.csv(files[true_zwave_i])
  
  
  return(list(inside,ambient,refrigerator,switch,house))
}


