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
      inside$inside_temp1 <- inside$inside_temp1/1000 # DUMP1: fridge_temp1 , DUMPX: inside_temp1
      inside$inside_temp2 <- inside$inside_temp2/1000 # DUMP2: fridge_temp2 , DUMPX: inside_temp2
      inside$fridge_temp1 <- inside$inside_temp1
      inside$fridge_temp2 <- inside$inside_temp2
      ambient$house.id <- flexboxlist[i]
      refrigerator$house.id <- flexboxlist[i]
      switch$house.id <- flexboxlist[i]
      house$house.id <- flexboxlist[i]
      
      vars <- c('id','datetime','houseAll_Voltage','houseAll_Current','houseAll_Power','houseAll_Energy')
      house <- house[,vars]
      house$house.id <- flexboxlist[i] 
      
      #if (flexboxlist[i]=='A1' | flexboxlist[i]=='A17' | flexboxlist[i]=='A24' | flexboxlist[i]=='A26' | flexboxlist[i]=='A28'){
       # vars <- c('id','datetime','houseAll_Voltage','houseAll_Current','houseAll_Power','houseAll_Energy')
        #house <- house[,vars]
        #house$house.id <- flexboxlist[i] 
      #} else {
       # vars <- c('id','datetime','house_Voltage','house_Current','house_Power','house_Energy')
      #  house <- house[,vars] %>% mutate(houseAll_Voltage=house_Voltage,houseAll_Current=house_Current,houseAll_Power=house_Power,houseAll_Energy=house_Energy) %>% select(id,datetime,houseAll_Voltage,houseAll_Current,houseAll_Power,houseAll_Energy)
      #  house$house.id <- flexboxlist[i] 
      #}
      
      
    } else {
      
      inside.else <- read.csv(files[true_inside_i])
      ambient.else <- read.csv(files[true_ambient_i])
      refrigerator.else <- read.csv(files[true_mfi_i])
      switch.else <- read.csv(files[true_switch_i])
      house.else <- read.csv(files[true_zwave_i])
      
      inside.else$house.id <- flexboxlist[i]
      inside.else$inside_temp1 <- inside.else$inside_temp1/1000 # DUMP1: fridge_temp1 , DUMPX: inside_temp1
      inside.else$inside_temp2 <- inside.else$inside_temp2/1000 # DUMP2: fridge_temp2 , DUMPX: inside_temp2
      inside.else$fridge_temp1 <- inside.else$inside_temp1
      inside.else$fridge_temp2 <- inside.else$inside_temp2
      
      ambient.else$house.id <- flexboxlist[i]
      refrigerator.else$house.id <- flexboxlist[i]
      switch.else$house.id <- flexboxlist[i]
      house.else$house.id <- flexboxlist[i]
      
      inside <- rbind(inside,inside.else)
      ambient <- rbind(ambient,ambient.else)
      refrigerator <- rbind(refrigerator,refrigerator.else)
      switch <- rbind(switch,switch.else)
      
      vars <- c('id','datetime','houseAll_Voltage','houseAll_Current','houseAll_Power','houseAll_Energy')
      house.else <- house.else[,vars]
      house.else$house.id <- flexboxlist[i] 
      
      #if (flexboxlist[i]=='A1' | flexboxlist[i]=='A17' | flexboxlist[i]=='A24' | flexboxlist[i]=='A26' | flexboxlist[i]=='A28'){
       # vars <- c('id','datetime','houseAll_Voltage','houseAll_Current','houseAll_Power','houseAll_Energy')
        #house.else <- house.else[,vars]
        #house.else$house.id <- flexboxlist[i] 
      #} else {
       # vars <- c('id','datetime','house_Voltage','house_Current','house_Power','house_Energy')
        #house.else <- house.else[,vars] %>% mutate(houseAll_Voltage=house_Voltage,houseAll_Current=house_Current,houseAll_Power=house_Power,houseAll_Energy=house_Energy) %>% select(id,datetime,houseAll_Voltage,houseAll_Current,houseAll_Power,houseAll_Energy)
        #house.else$house.id <- flexboxlist[i] 
      #}
      
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
      vars <- c('id','datetime','houseAll_Voltage','houseAll_Current','houseAll_Power','houseAll_Energy')
      house <- house[,vars]
      house$house.id <- flexboxlist[i] 
      
      #if (flexboxlist[i]=='A1' | flexboxlist[i]=='A17' | flexboxlist[i]=='A24' | flexboxlist[i]=='A26' | flexboxlist[i]=='A28'){
      #vars <- c('id','datetime','houseAll_Voltage','houseAll_Current','houseAll_Power','houseAll_Energy')
      #house <- house[,vars]
      #house$house.id <- flexboxlist[i] 
      #} else {
      #vars <- c('id','datetime','house_Voltage','house_Current','house_Power','house_Energy')
      #house <- house[,vars] %>% mutate(houseAll_Voltage=house_Voltage,houseAll_Current=house_Current,houseAll_Power=house_Power,houseAll_Energy=house_Energy) %>% select(id,datetime,houseAll_Voltage,houseAll_Current,houseAll_Power,houseAll_Energy)
      #house$house.id <- flexboxlist[i] 
      #}
      
    } else {
      
      refrigerator.else <- read.csv(files[true_mfi_i])
      house.else <- read.csv(files[true_zwave_i])
      
      refrigerator.else$house.id <- flexboxlist[i]
      house.else$house.id <- flexboxlist[i]
      vars <- c('id','datetime','houseAll_Voltage','houseAll_Current','houseAll_Power','houseAll_Energy')
      house.else <- house.else[,vars]
      house.else$house.id <- flexboxlist[i] 
      
      #if (flexboxlist[i]=='A1' | flexboxlist[i]=='A17' | flexboxlist[i]=='A24' | flexboxlist[i]=='A26' | flexboxlist[i]=='A28'){
      #  vars <- c('id','datetime','houseAll_Voltage','houseAll_Current','houseAll_Power','houseAll_Energy')
      # house.else <- house.else[,vars]
      #  house.else$house.id <- flexboxlist[i] 
      #  } else {
      #vars <- c('id','datetime','house_Voltage','house_Current','house_Power','house_Energy')
      #house.else <- house.else[,vars] %>% mutate(houseAll_Voltage=house_Voltage,houseAll_Current=house_Current,houseAll_Power=house_Power,houseAll_Energy=house_Energy) %>% select(id,datetime,houseAll_Voltage,houseAll_Current,houseAll_Power,houseAll_Energy)
      #house.else$house.id <- flexboxlist[i] 
     #}
      
      refrigerator <- rbind(refrigerator,refrigerator.else)
      house <- rbind(house,house.else)
    }
  }
  return(list(refrigerator,house))
}


########## Read Survey Data

read.survey.data <- function(flexlist) {
  
  
  setwd('/Users/Diego/Desktop/Nicaragua/Surveys/')
  
  #######   NOTE: Initial Baseline Survey Data   #######
  
  #NOTE1: This has the baseline data for all the pulperias
  initial.baseline.survey <- read.csv('Survey I Winter 2014/Results/All_Surveys_With_Pics_Reviewed_Final.csv')
  initial.baseline.data <-subset(initial.baseline.survey,initial.baseline.survey$error == 0)
  baseline_receipt_data <- subset(initial.baseline.data, initial.baseline.data$foto_gasto_electrico == 'si') %>% mutate(encuesta_id=survey_id,baseline_gasto_electrico=gasto_electrico,baseline_monthly_cordobas=r_monthly_cordobas) %>% select(encuesta_id,baseline_gasto_electrico,baseline_monthly_cordobas,gasto_electrico,r_monthly_cordobas)
  
  baseline_receipt_data_for_merge <- baseline_receipt_data %>% select(encuesta_id,baseline_gasto_electrico,baseline_monthly_cordobas)
  baseline_receipt_data_for_plot <- baseline_receipt_data %>% mutate(data='baseline') %>% select(encuesta_id,gasto_electrico,r_monthly_cordobas,data)
  
  #NOTE2: This has the baseline survey data for the houses and the implementation survey data for the pulperias
  implementation.baseline.survey <- read.csv('Implementation Baseline/Cool Joule Project Management - Verificacion de Ubicacion y Enc.csv')
  
  ####### Reading data from the survey data
  
  survey.data.complete <- read.csv('Survey Monthly Updates/Results/all_monthly_updates_odaly_for analysis.csv', na.strings=c(""," ","n/a"))
  survey.data <- read.csv('Survey Monthly Updates/Results/all_monthly_updates_odaly_for analysis.csv', na.strings=c(""," ","n/a")) %>% select(flexbox_id,encuesta_id,start,end,today,r_total_cordobas,r_monthly_cordobas,r_publiclight,r_comercialization,r_subsidy_under150,r_subsidy_commercialization1,r_subsidylighting150,r_subsidy_elderly,r_rechargedelay,r_ineregulation,r_tax,r_subsidy,r_reconnection_charge,r_arreglo_deuda,r_cuota_deuda)
  
  importe.data <- read.csv('Survey Monthly Updates/Results/importes_odaly.csv') %>% select(Casa,Mes,Ano,tipo,importe,cosa_revisado)
  importe.data.subset <- subset(importe.data,importe.data$tipo == "Recibos de energia electrica")
  importe.data.subset <- subset(importe.data.subset,importe.data.subset$cosa_revisado == "Cobrado") %>% select(Casa,Mes,Ano,tipo,importe)
  energia.data <- read.csv('Survey Monthly Updates/Results/energia_odaly.csv') %>% mutate(Casa=FLEXBOX.ID) %>% select(Casa,Mes,Ano,Energia,Energia_Ajustada)
  
  im_en <- merge(energia.data,importe.data.subset,by=c("Mes","Ano","Casa"),all=T)
  
  im_en$mes <- ifelse(im_en$Mes=="Enero",1,ifelse(im_en$Mes=="Febrero",2,ifelse(im_en$Mes=="Marzo",3,ifelse(im_en$Mes=="Abril",4,ifelse(im_en$Mes=="Mayo",5,ifelse(im_en$Mes=="Junio",6,ifelse(im_en$Mes=="Julio",7,ifelse(im_en$Mes=="Agosto",8,ifelse(im_en$Mes=="Septiembre",9,ifelse(im_en$Mes=="Octubre",10,ifelse(im_en$Mes=="Noviembre",11,12)))))))))))
  im_en$fecha <- as.Date(paste(paste(im_en$Ano,im_en$mes,sep="-"),"-01",sep=""))
  im_en$energia <- as.numeric(gsub("[[:punct:]]", " ", sub(".*=", "", im_en$Energia)))
  im_en$energia_ajustada <- as.numeric(gsub("[[:punct:]]", " ", sub(".*=", "", im_en$Energia_Ajustada)))
  
  im_en_sorted <- im_en[order(im_en$Casa,im_en$fecha),]
  
  
  ###### Control group begins
  
  importe.data.control <- read.csv('Survey Monthly Updates/Results/control_importe.csv') %>% mutate(Casa=Encuesta_Id) %>% select(Casa,Mes,Ano,tipo,importe,cosa_revisado)
  importe.data.subset.control <- subset(importe.data.control,importe.data.control$tipo == "Recibos de energia electrica")
  importe.data.subset.control <- subset(importe.data.subset.control,importe.data.subset.control$cosa_revisado == "Cobrado") %>% select(Casa,Mes,Ano,tipo,importe)
  energia.data.control <- read.csv('Survey Monthly Updates/Results/control_energia.csv') %>% mutate(Casa=Encuesta_Id) %>%select(Casa,Mes,Ano,Energia,Energia_Ajustada)
  
  im_en_control <- merge(energia.data.control,importe.data.subset.control,by=c("Mes","Ano","Casa"),all=T)
  
  im_en_control$mes <- ifelse(im_en_control$Mes=="Enero",1,ifelse(im_en_control$Mes=="Febrero",2,ifelse(im_en_control$Mes=="Marzo",3,ifelse(im_en_control$Mes=="Abril",4,ifelse(im_en_control$Mes=="Mayo",5,ifelse(im_en_control$Mes=="Junio",6,ifelse(im_en_control$Mes=="Julio",7,ifelse(im_en_control$Mes=="Agosto",8,ifelse(im_en_control$Mes=="Septiembre",9,ifelse(im_en_control$Mes=="Octubre",10,ifelse(im_en_control$Mes=="Noviembre",11,12)))))))))))
  im_en_control$fecha <- as.Date(paste(paste(im_en_control$Ano,im_en_control$mes,sep="-"),"-01",sep=""))
  im_en_control$energia <- as.numeric(gsub("[[:punct:]]", " ", sub(".*=", "", im_en_control$Energia)))
  im_en_control$energia_ajustada <- as.numeric(gsub("[[:punct:]]", " ", sub(".*=", "", im_en_control$Energia_Ajustada)))
  
  
  im_en_sorted_control <- im_en_control[order(im_en_control$Casa,im_en_control$fecha),]
  
  # Porcentaje de la factura que esta dedicado a la energia
  recibos_control <- read.csv('Survey Monthly Updates/Results/recibos_detalles_control.csv')
  recibos_control$fraction_bill_energy <- recibos_control$Energia_Cordobas/recibos_control$Importe_Total
  mean_energia_recibos_control <- aggregate(recibos_control$fraction_bill_energy,by=list(recibos_control$Encuesta_Id),FUN=mean,na.rm=TRUE) %>% mutate(house.id=Group.1,mean.val=x) %>% select(house.id,mean.val)
  median_energia_recibos_control <- aggregate(recibos_control$fraction_bill_energy,by=list(recibos_control$Encuesta_Id),FUN=median,na.rm=TRUE) %>% mutate(house.id=Group.1,median.val=x) %>% select(house.id,median.val)
  e_bill_fraction_control <- merge(mean_energia_recibos_control,median_energia_recibos_control,by=c('house.id'))
  e_bill_fraction_control$type.val<- "Gasto Energía (kWh)"
  e_bill_fraction_control$cost <- 1
  
  ###### Control group ends
  
  #Comercializacion is supposed to be a negative
  
  #Doing a new sum to make sure that we are calculating the total correctly
  survey.data$r_total_calculated  <- rowSums(cbind(survey.data$r_monthly_cordobas,survey.data$r_publiclight, survey.data$r_comercialization, survey.data$r_rechargedelay, survey.data$r_ineregulation , survey.data$r_tax, survey.data$r_reconnection_charge, survey.data$r_subsidy_under150 , survey.data$r_subsidy_commercialization1 , survey.data$r_subsidylighting150 , survey.data$r_subsidy_elderly,survey.data$r_arreglo_deuda,survey.data$r_cuota_deuda),na.rm=TRUE)
  survey.data$r_total_costs  <- rowSums(cbind(survey.data$r_monthly_cordobas,survey.data$r_publiclight, survey.data$r_comercialization, survey.data$r_rechargedelay, survey.data$r_ineregulation , survey.data$r_tax, survey.data$r_reconnection_charge,survey.data$r_arreglo_deuda,survey.data$r_cuota_deuda),na.rm=TRUE)
  
  survey.data$not_correct <- ifelse(round(survey.data$r_total_cordobas) == round(survey.data$r_total_calculated),0,1)
  #whywrong <- subset(survey.data,survey.data$not_correct==1)
  #Exported this file to the xls so that Odaly can check why so many variables are missing and why it is so wrong!
  #write.csv(survey.data,'whywrong.csv')
  
  # Keep going with only the data that we have
  survey.data.correct <- subset(survey.data,survey.data$not_correct == 0)
  
  survey.data.correct$pct_energia <- (survey.data.correct$r_monthly_cordobas/survey.data.correct$r_total_cordobas)*100
  survey.data.correct$pct_luz <- (survey.data.correct$r_publiclight/survey.data.correct$r_total_cordobas)*100
  survey.data.correct$pct_recargo <- (survey.data.correct$r_rechargedelay/survey.data.correct$r_total_cordobas)*100
  survey.data.correct$pct_comercializacion <- (survey.data.correct$r_comercialization/survey.data.correct$r_total_cordobas)*100
  survey.data.correct$pct_inereg <- (survey.data.correct$r_ineregulation/survey.data.correct$r_total_cordobas)*100
  survey.data.correct$pct_tax <- (survey.data.correct$r_tax/survey.data.correct$r_total_cordobas)*100
  survey.data.correct$pct_reconnection <- (survey.data.correct$r_reconnection_charge/survey.data.correct$r_total_cordobas)*100
  survey.data.correct$pct_arreglo_deuda <- (survey.data.correct$r_arreglo_deuda/survey.data.correct$r_total_cordobas)*100
  survey.data.correct$pct_cuota_deuda <- (survey.data.correct$r_cuota_deuda/survey.data.correct$r_total_cordobas)*100
 
  survey.data.correct$pct_subsidy_under150 <- (abs(survey.data.correct$r_subsidy_under150)/survey.data.correct$r_total_costs)*100
  survey.data.correct$pct_subsidycommercialization1 <- (abs(survey.data.correct$r_subsidy_commercialization1)/survey.data.correct$r_total_costs)*100
  survey.data.correct$pct_subsidylighting150 <- (abs(survey.data.correct$r_subsidylighting150)/survey.data.correct$r_total_costs)*100
  survey.data.correct$pct_subsidy_elderly <- (abs(survey.data.correct$r_subsidy_elderly)/survey.data.correct$r_total_costs)*100
 
  #Iterating over all values and houses 
  list_true_vars <- c("pct_energia","pct_luz","pct_recargo","pct_comercializacion","pct_inereg","pct_tax","pct_reconnection","pct_arreglo_deuda","pct_cuota_deuda","pct_subsidy_under150","pct_subsidycommercialization1","pct_subsidylighting150","pct_subsidy_elderly")
  list_name_vars <- c("Gasto Energía (kWh)","Alumbrado Público","Cargo por Recargo","Comercialización","Regulación INE","IVA","Reconección","Arreglo Deuda","Cuota Deuda","S.Menos 150","S.Comercialización","S.Alumbrado","S.Jubilados")
  
  # Calculates the percentage for each of the cost pieces 
  for (i in 1:length(flexlist)) {
    
    house.survey <- subset(survey.data.correct,survey.data.correct$flexbox_id == flexlist[i])
    
    empty.energy.df <- data.frame(matrix(vector(), 1, 3, dimnames=list(c(), c("house.id", "mean.val", "type.val"))), stringsAsFactors=F)
    other <- data.frame(matrix(vector(), 1, 3, dimnames=list(c(), c("house.id", "mean.val", "type.val"))), stringsAsFactors=F)
    
    for (j in 1:length(list_true_vars)) {
  
      if (j == 1) {
      empty.energy.df$house.id <- flexlist[i]
      empty.energy.df$mean.val <- mean(house.survey[,list_true_vars[j]],na.rm=TRUE)
      empty.energy.df$median.val <- median(house.survey[,list_true_vars[j]],na.rm=TRUE)
      empty.energy.df$type.val <- list_name_vars[j]
      } 
        else {
      other$house.id <- flexlist[i]
      other$mean.val <- mean(house.survey[,list_true_vars[j]],na.rm=TRUE)
      other$median.val <- mean(house.survey[,list_true_vars[j]],na.rm=TRUE)
      other$type.val <- list_name_vars[j]
      
      empty.energy.df<- rbind(empty.energy.df,other)
        }
    }
    
    if (i==1) {
      houses.energy.df <- empty.energy.df
    } else {
      houses.energy.df <- rbind(houses.energy.df,empty.energy.df)
    }
  }
  
  houses.energy.df$cost <- ifelse(houses.energy.df$type.val == "S.Menos 150" | houses.energy.df$type.val == "S.Comercialización" | houses.energy.df$type.val == "S.Alumbrado" | houses.energy.df$type.val == "S.Jubilados",0,1)
  
  return(list(houses.energy.df,im_en_sorted,im_en_sorted_control,survey.data.complete,baseline_receipt_data_for_merge,implementation.baseline.survey,survey.data.correct,e_bill_fraction_control))
}



##### Reading Server Data for the Energy Efficiency Report

read.server.data <- function(date1,date2,date3,flexboxid) {
  
  # Power Data
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv = PostgreSQL(), user = "flexbox", dbname = "flexbox_db_server", host = "localhost",  password = "flexbox")
  
  fridge.database <- dbSendQuery(con,statement=paste("SELECT * from fridge_power"));
  fridge_data <- fetch(fridge.database,n=-1)
  fridge.data.table<- data.table(fridge_data)
  fridge.data.table$datetime <- fridge.data.table$datetime  - 6*60*60 #Changing it to Nicaragua time
  
  house.subset <- fridge.data.table[fridge.data.table$hostname == flexboxid & fridge.data.table$datetime >= date1 & fridge.data.table$datetime <= date3 ]
  
  house.subset$second <- second(house.subset$datetime)
  house.subset$minute <- minute(house.subset$datetime)
  house.subset$hour<- hour(house.subset$datetime)
  house.subset$day<- day(house.subset$datetime)
  house.subset$month<- month(house.subset$datetime)
  
  energy_week1 <- subset(house.subset,house.subset$datetime >= date1 & house.subset$datetime < date2)
  energy_week2 <- subset(house.subset,house.subset$datetime >= date2) 
  
  clean_week1 <- cleaning_mfi_resets(energy_week1,names(energy_week1)[14]) #%>% mutate(energy=energy_sum2,week=1) %>% select(hostname,datetime,energy,week)
  clean_week2 <- cleaning_mfi_resets(energy_week2,names(energy_week2)[15]) #%>% mutate(energy=energy_sum3,week=2) %>% select(hostname,datetime,energy,week)
  
  
  # Inside Temps Data
  
  inside.database <- dbSendQuery(con,statement=paste("SELECT * from inside_temps"));
  inside_data <- fetch(inside.database,n=-1)
  inside_data.table<- data.table(inside_data)
  inside_data.table$datetime <- inside_data.table$datetime  - 6*60*60 #Changing it to Nicaragua time
  
  inside.subset <- inside_data.table[inside_data.table$hostname == flexboxid & inside_data.table$datetime >= date1 & inside_data.table$datetime <= date3 ]
  inside.subset$second <- second(inside.subset$datetime)
  inside.subset$minute <- minute(inside.subset$datetime)
  inside.subset$hour<- hour(inside.subset$datetime)
  inside.subset$day<- day(inside.subset$datetime)
  inside.subset$month<- month(inside.subset$datetime)
  inside.subset$date <- as.Date(inside.subset$datetime)
  
  temp_week1 <- subset(inside.subset,inside.subset$datetime >= date1 & inside.subset$datetime < date2)
  temp_week2 <- subset(inside.subset,inside.subset$datetime >= date2) 
  
  
  return(list(house.subset,energy_week1,energy_week2,clean_week1,clean_week2,temp_week1,temp_week2))

}



