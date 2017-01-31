# Tariff analysis and rates

###### 1. Bringing in the data 

library(RPostgreSQL)
library(DBI)
library(sqldf)
library(RSQLite)
library(RMySQL)
library(data.table)
library(lubridate)

source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/read_data_functions.R')


####### Sever Connection

#Set up connection to server 
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv = PostgreSQL(), user = "cosmos", dbname = "cosmos_ni", host = "cosmosdata.niuera.co",  password = "nosotrossomoscosmos",port = "5432")
dbClearResult(dbListResults(con)[[1]]) # Close result set . Reset connection first 

#Retrieve Data
tariff_data <- dbSendQuery(con,statement=paste("SELECT * from user_prices_nio where datetime > '01-01-2013'"));
tariff_data.dt <- as.data.table(fetch(tariff_data,n=-1))

tariff_data.dt$month <- month(tariff_data.dt$datetime)
tariff_data.dt$Ano <- year(tariff_data.dt$datetime)
tariff_data.dt$Mes <- ifelse(tariff_data.dt$month==1,"Enero",ifelse(tariff_data.dt$month==2,"Febrero",ifelse(tariff_data.dt$month==3,"Marzo",ifelse(tariff_data.dt$month==4,"Abril",ifelse(tariff_data.dt$month==5,"Mayo",ifelse(tariff_data.dt$month==6,"Junio",ifelse(tariff_data.dt$month==7,"Julio",ifelse(tariff_data.dt$month==8,"Agosto",ifelse(tariff_data.dt$month==9,"Septiembre",ifelse(tariff_data.dt$month==10,"Octubre",ifelse(tariff_data.dt$month==11,"Noviembre","Diciembre")))))))))))


Casa <- c("A1","A3","A6","A7","A9","A11","A12","A14","A16","A17","A18","A19","A20","A21","A22","A24","A25","A26","A29")
Tariff_Code <- c("T-0","T-J","T-1","T-0","T-0","T-1","T-1","T-0","T-0","T-1","T-J","T-1","T-0","T-1","T-0","T-0","T-0","T-1","T-1")
casa_tariff_code <- data.frame(Casa,Tariff_Code)

control_group_list <- c("7","20","26","45","77","124","185","191","203","204","302","304","305","307","310","311")
Tariff_Code_Control <- c("T-1","T-1","T-0","T-0","T-1","T-0","T-1","T-1","T-0","T-1","T-0","T-0","T-0","T-0","T-0","T-0")
control_tariff_code <- data.frame(control_group_list,Tariff_Code_Control)




# Survey Data

energy.receipt.data <- read.survey.data(flexlist)[[1]]
e_i_treatment <- read.survey.data(flexlist)[[2]]
e_i_control <- read.survey.data(flexlist)[[3]]

energy.receipt.control <- read.survey.data(flexlist)[[8]]



####### 2. Analysis

# Merging houses with tariff code 
e_i_treatment_tariff <- merge(e_i_treatment,casa_tariff_code,by='Casa')
analysis_subset <- subset(e_i_treatment_tariff,e_i_treatment_tariff$Ano<2016 & e_i_treatment_tariff$Mes!="Noviembre"| e_i_treatment_tariff$Ano<2016 & e_i_treatment_tariff$Mes!="Diciembre")
analysis_subset <- analysis_subset[is.na(analysis_subset$energia),]

# Merging with the percentage of energy per bill to get the correct amount 
energy.receipt.data.energia <- subset(energy.receipt.data,energy.receipt.data$type.val=="Gasto Energía (kWh)") %>% mutate(Casa=house.id) %>% select(Casa,mean.val,median.val,type.val,cost)
analysis_subset <- merge(energy.receipt.data.energia,analysis_subset,by=c('Casa'))


#####  The text below was used to test if the for loop was working, it seemed to be working well for the most part need to check 
#####  Previoulsy I used energy receipt data to verify that I was calculating it well

#Casa <- c("A1","A3","A6","A7","A9","A11","A12","A14","A16","A17","A18","A19","A20","A21","A22","A24","A25","A26","A28","A29")
#importe_mayo <- c(1821.85,974.84,1727.67,1209.76,1261.11,9077.81,862.48,395.73,2736.27,3049.81,1052.47,2058.85,1080.23,3799.5,372.79,2830.52,1504.02,10850.79,2186.68,6505.32)
#energia_verdad_mayo <- c(276,221,181,184,193,1093,149,129,387,345,232,222,166,438,123,400,200,1142,238,478)
#importe_junio <- data.frame(Casa=c("A1","A3","A6","A7","A9","A11","A12","A14","A16","A17","A18","A19","A20","A21","A22","A24","A25","A26","A28","A29"),importe_junio=c(2299.17,823.61,1590.48,1174.67,1234.86,9576.11,789.75,403.25,2666.23,2940.56,1211.72,1935.45,1073.07,3340.35,338.88,2874.14,1183.1,7792.27,667.29,6528.6))
#energia_verdad_junio <- data.frame(Casa=c("A1","A3","A6","A7","A9","A11","A12","A14","A16","A17","A18","A19","A20","A21","A22","A24","A25","A26","A28","A29"), energia_junio=c(326,199,164,178,189,1157,136,131,378,332,257,207,165,382,113,407,180,734,114,471))
#importe_julio <- data.frame(Casa=c("A1","A3","A6","A7","A9","A11","A12","A14","A16","A17","A18","A19","A20","A21","A22","A24","A25","A26","A28","A29"),importe_junio=c(222.88,1002.72,749.01,462.25,1212.90,9398.15,753.85,384.37,2843.74,3039.99,1135.97,1991.81,438.51,3055.75,323.36,2950.97,1009.35,7617.79,636.48,6239.08))
#energia_verdad_julio <- data.frame(Casa=c("A1","A3","A6","A7","A9","A11","A12","A14","A16","A17","A18","A19","A20","A21","A22","A24","A25","A26","A28","A29"), energia_junio=c(314,225,128,149,185,1129,129,126,400,343,245,213,142,345,108,416,167,740,108,425))

#test <- merge(importe_julio,energia_verdad_julio,by=c('Casa'))
#test <- merge(test,energy.receipt.data.energia,by=c('Casa'))
#test <- merge(test,casa_tariff_code,by=c('Casa'))
#test$energia <- NA
################################################################################################################################################

test <- time.series.receipt.dt[is.na(time.series.receipt.dt$energia),]
test <- test[complete.cases(test$importe),] %>% select(Casa,importe,Mes,Ano)
test <- merge(test,energy.receipt.data.energia,by=c('Casa'))
test <- merge(test,casa_tariff_code,by=c('Casa'))
unique_months <- unique(as.data.frame(test)[c("Mes","Ano")],)

#Subsetting the file that we're going to analyze
pepe <- subset(tariff_data.dt,tariff_data.dt$Mes==unique_months$Mes[i] & tariff_data.dt$Ano==unique_months$Ano[i])



for(i in 1:length(unique_months$Ano)){

subset_test <- subset(test,test$Mes== unique_months$Mes[i] & test$Ano==unique_months$Ano[i])
subset_test$energia <- NA

    for(j in 1:length(subset_test$Casa)){
    
    importe <- subset_test$importe[j] * (subset_test$median.val[j])/100
    tariff_code <- subset_test$Tariff_Code[j]
    
    if(tariff_code == "T-0" | tariff_code == "T-J"){
      
      # T0 price ranges
      first_price <- pepe[start_range==0 & end_range==25 & tariff_code == "T-0",price]
      second_price <- pepe[start_range==25 & end_range==50 & tariff_code == "T-0",price]
      third_price <- pepe[start_range==50 & end_range==100 & tariff_code == "T-0",price]
      fourth_price <- pepe[start_range==100 & end_range==150 & tariff_code == "T-0",price]
      fifth_price <- pepe[start_range==150 & end_range==500 & tariff_code == "T-0",price]
      sixth_price <- pepe[start_range==500 & end_range==1000 & tariff_code == "T-0",price]
      seventh_price <- pepe[start_range==1000 & end_range==1000000 & tariff_code == "T-0",price]
      
      if(importe <=25*first_price){
        energia <-importe/first_price
        energia_tot <- energia
      } else if(importe>25*first_price & importe<=(25*first_price+25*second_price)){
        energia <- (importe - (25*first_price))/second_price
        energia_tot <- energia + 25
      } else if(importe>(25*first_price+25*second_price) & importe<=(25*first_price+25*second_price+50*third_price)){
        energia <- (importe - (25*first_price + 25*second_price))/third_price
        energia_tot <- energia + 25 +25
      } else if(importe>(25*first_price+25*second_price+50*third_price) & importe<=(25*first_price+25*second_price+50*third_price+50*fourth_price)){
        energia <- (importe - (25*first_price + 25*second_price + 50*third_price))/fourth_price
        energia_tot <- energia + 25 + 25 + 50
      } else if(importe>(25*first_price+25*second_price+50*third_price+50*fourth_price) & importe<=(25*first_price+25*second_price+50*third_price+50*fourth_price+350*fifth_price)){
        energia <- (importe - (25*first_price + 25*second_price + 50*third_price +50*fourth_price))/fifth_price
        energia_tot <- energia + 25 + 25 + 50 + 50
      } else if(importe>(25*first_price+25*second_price+50*third_price+50*fourth_price+350*fifth_price) & importe<=(25*first_price+25*second_price+50*third_price+50*fourth_price+350*fifth_price+500*sixth_price)){
        energia <- (importe - (25*first_price + 25*second_price + 50*third_price +50*fourth_price+350*fifth_price))/sixth_price
        energia_tot <- energia + 25 + 25 + 50 + 50 + 350
      } else if(importe>(25*first_price+25*second_price+50*third_price+50*fourth_price+350*fifth_price+500*sixth_price) & importe<=(25*first_price+25*second_price+50*third_price+50*fourth_price+350*fifth_price+500*sixth_price+999000*seventh_price)){
        energia <- (importe - (25*first_price + 25*second_price + 50*third_price +50*fourth_price+350*fifth_price+500*sixth_price))/seventh_price
        energia_tot <- energia + 25 + 25 + 50 + 50 + 350 + 500
      } 
      
    } else if(tariff_code == "T-1"){
      
      # T1 price ranges
      first_price <- pepe[start_range==0 & end_range==150 & tariff_code=="T-1",price]
      second_price <- pepe[start_range==150 & end_range==1000000 & tariff_code=="T-1",price]
      
      if(importe <=150*first_price){
        energia <-importe/first_price
        energia_tot <- energia
      } else if(importe>150*first_price & importe<=(150*first_price+1000000*second_price)){
        energia <- (importe - (150*first_price))/second_price
        energia_tot <- energia + 150
      }
    }
    
    subset_test$energia[j] <- energia_tot
    
    if(subset_test$Casa[j]=="A18" | subset_test$Casa[i]== "A3"){
      subset_test$energia[j] <- energia_tot+energia_tot*0.05
    } else if(subset_test$Casa[j]=="A29" | subset_test$Casa[i]=="A11"){
      subset_test$energia[i] <- energia_tot-energia_tot*0.05
    } else if(subset_test$Casa[j]=="A17"){
      subset_test$energia[i] <- energia_tot-energia_tot*0.15
    } else if(subset_test$Casa[j]=="A19"){
      subset_test$energia[j] <- energia_tot-energia_tot*0.248
    } else if(subset_test$Casa[j]=="A21"){
      subset_test$energia[j] <- energia_tot-energia_tot*0.12
    } else if(subset_test$Casa[j]=="A26"){
      subset_test$energia[j] <- energia_tot-energia_tot*0.02
    } else if(subset_test$Casa[j]=="A6"){
      subset_test$energia[j] <- energia_tot-energia_tot*0.31
    } else if(subset_test$Casa[j]=="A7"){
      subset_test$energia[j] <- energia_tot-energia_tot*0.01
    }
    }

    if(i==1){
      subset_test_append <- subset_test
    } else{
      subset_test_append <- rbind(subset_test_append,subset_test)
    }
}

subset_test_keep <- subset(subset_test, select=c("Casa","Mes","Ano","importe","energia","Tariff_Code"))



#This piece of code was used to calculate the percent error prediction between
#subset_test$dff <- ((subset_test$energia_junio - subset_test$energia)/subset_test$energia_junio)*100






