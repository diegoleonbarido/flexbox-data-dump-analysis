##### Marginal Value of Information

# 1. Timeseries Plots for Households 
  # 1.1 Treatment (plot, adjustments,etc)
  # 1.2 Control
  # 1.3 Comparison between treatment and control groups, analyze how this varies by willingness to pay, and how it varies by people who kept the energy data or not
  # 1.4 Coefficient of variability
  # 1.5 Who is saving energy? Those who like the information more, or the ones who like the report more? Is it those who like the texts the most? Is it those with the willingness to pay?
  # 1.6 Are the people who chose the information those who have the largest energy savings? Are the people with the highest willingness to pay saving energy with information?
  # Things to control for: number of people living inside the house, number of appliances, business modifictions

# 2. Learning
  # 2.1 Accuracy of energy expenditures and energy costs (calculate mean historical values vs. the one time payment for the control group, not only compare the one month to the one remembrance)
  # 2.2 Tariff Values


# 3. Comparison of survey response vs. actual data
  # 3.1 Energy Consumed (kWh)
  # 3.2 Energy Expenditures ($)
  # 3.3 Door Openings
  # 3.4 Comparing people who say they are implementing fridge energy saving strategies and schedules  
  # 3.5 When people consume most energy vs. when they think they consume the most energy (time of day, day of week, month)
  # 3.6 Do households consume more energy in hot days, or is this just their impression? Compare with surveys and the fact that they say that they consume more energy on hot days
  # 3.7 What are the houses that experience the greatest varaiblity in their energy consumption? Wy? Efficiency? Poverty?
  # 3.8 Compare satisfaction with the data products based on income
  # 3.9 Do people who implement fridge energy savings strategies experience savings? 
  # 3.9.1 Do people who say they've experienced greater savings, or increased control actually experience savgins?
  # 3.9.2 Of the people who paid for willingness to pay, and those who actually kept the information, what were their perceptions on information before the study began?

# 4. Sensor data comparison 
  # 3.1 Z-wave vs. time series data from CNDC - how far off are the values? (changes over time)
  # 3.2 Z-wave vs refrigerator data (changes over time)
  # 3.3 Evaluation after first reports, after SMS, after implementation of energy efficiency strategies

# 4. Reports and Text Messages
  # 4.1 Existence and bersistence of behavioral change after paper reports (need dates from Odaly) vs. sms (server)

# 5. Scarcity 


# 6. DR, grid data and carbon
  # 4.2 


############ Libraries

library(RPostgreSQL)
library(DBI)
library(sqldf)
library(RSQLite)
library(RMySQL)
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(xtable)
library(grid)
library(Hmisc)
library(rtf)
library(foreach)
library(doParallel)
library(doMC)
library(tidyr)
library(qdap)


# Turn warnings off
options(warn=-1)

#Intitialize cluster
detectCores()
cl<-makeCluster(4)
registerDoParallel(cl)

#Function libraries
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/date_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/merging_binding_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/read_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/time_series_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/hourly_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_consumption_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/scraping_scripts.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/tariff_analysis_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/marginal_value_information_plotting_functions.R')

############ Define Functions

#Trimming
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Month by month reductions
month_by_month <- function(group_data,variable){
  group_data[, lag_variable:=c(NA, get(variable)[-.N]), by=Casa]
  group_data$diff_variable <- group_data[,get(variable)] - group_data[,lag_variable]
  return(group_data)
}


#Differences for each month one year afterwards (e.g June 2016 - June 2015)
month_diffs <- function(group_data,variable){
  
  years_data <- c(2014,2015,2016)
  Casa_ID <- unique(group_data$Casa)
  
  for(i in 1:length(Casa_ID)){
    
    for(j in 1:length(years_data)) {
      
      t_year1_year2 <-  group_data[Casa==Casa_ID[i] & Ano==years_data[j] |  Casa==Casa_ID[i] & Ano==(years_data[j]+1) ]
      
      n_occur <- data.frame(table(t_year1_year2$Mes))
      n_occur_freq2 <- n_occur[n_occur$Freq > 1,]
      
      keep_months_casa <- t_year1_year2[Mes %in% n_occur_freq2$Var1,]
      
      just_diffs <- keep_months_casa[, month_diff := diff(get(variable)), by = list(Mes)]
      keep_year <- just_diffs[Ano >= max(just_diffs$Ano)]
      
      if(j==1){
        keep_year_large <- keep_year
      } else {
        keep_year_large <- rbind(keep_year_large,keep_year)
      }
    }
    
    if(i==1){
      keep_house_large <- keep_year_large
    } else {
      keep_house_large <- rbind(keep_house_large,keep_year_large)
    }
  }
  
  if(group_data$treatment == 'Treatment'){
    keep_house_large <- keep_house_large[,list(Casa,month_diff,treatment,Mes,Ano,intervention_group,report_intervention_month,information_group,sms_intervention_month,sms_intervention_month_text,wtp_lw_md_h,fraction_lw_md_h)]
    return(keep_house_large)
  } else {
    keep_house_large <- keep_house_large[,list(Casa,month_diff,treatment,Mes,Ano,intervention_group,report_intervention_month,information_group,sms_intervention_month,sms_intervention_month_text)]
    keep_house_large$wtp_lw_md_h <- NA
    keep_house_large$fraction_lw_md_h <- NA
    return(keep_house_large)
  }
}

# Plot with subset

call_plot_subset <- function(df,group_var,subset_val1,subset_val2,analyze_var,xlab_,ylab_,title_plot) {
  
  mean_one <- df[get(group_var)==subset_val1,mean(na.omit(get(analyze_var)))]
  mean_two <- df[get(group_var)==subset_val2,mean(na.omit(get(analyze_var)))]
  Means = c(mean_one,mean_two)
  Names = c(subset_val1,subset_val2)
  lines_df = data.frame(Names,Means)
  
  if(analyze_var=="month_diff"){
  t_result = t.test(na.omit(df[get(group_var)==subset_val1] %>% select(month_diff)),na.omit(df[get(group_var)==subset_val2] %>% select(month_diff)))$p.value
  plot_this <- ggplot(df, aes(get(analyze_var), fill = get(group_var))) + geom_density(alpha = 0.2) + xlab(xlab_) + ylab(ylab_) + ggtitle(title_plot) + geom_vline(data=lines_df,aes(xintercept=Means,yintercept=0,linetype=Names,colour = Names), show_guide = TRUE) + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.position="bottom") +  guides(fill=guide_legend(title=NULL)) + annotate("text",x=0,y=0.01,hjust=-0.8,label = paste("Tr:",toString(round(mean_one,0))," Cl:", toString(round(mean_two,0)),", p:",toString(round(t_result,3))))
  } else {
  t_result = t.test(na.omit(df[get(group_var)==subset_val1] %>% select(diff_variable)),na.omit(df[get(group_var)==subset_val2] %>% select(diff_variable)))$p.value
  plot_this <- ggplot(df, aes(get(analyze_var), fill = get(group_var))) + geom_density(alpha = 0.2) + xlab(xlab_) + ylab(ylab_) + ggtitle(title_plot) + geom_vline(data=lines_df,aes(xintercept=Means,yintercept=0,linetype=Names,colour = Names), show_guide = TRUE) + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.position="bottom") +  guides(fill=guide_legend(title=NULL)) + annotate("text",x=0,y=0.01,hjust=-0.8,label = paste("Tr:",toString(round(mean_one,0))," Cl:", toString(round(mean_two,0)),", p:",toString(round(t_result,3))))
    
  }
  return(plot_this)
}


# Three variables to subset
call_plot_subset_three <- function(df,group_var,subset_val1,subset_val2,subset_val3,analyze_var,xlab_,ylab_,title_plot) {
  mean_one <- df[get(group_var)==subset_val1,mean(na.omit(get(analyze_var)))]
  mean_two <- df[get(group_var)==subset_val2,mean(na.omit(get(analyze_var)))]
  mean_three <- df[get(group_var)==subset_val3,mean(na.omit(get(analyze_var)))]
  
  Means = c(mean_one,mean_two,mean_three)
  Names = c(subset_val1,subset_val2,subset_val3)
  lines_df = data.frame(Names,Means)   
  
  plot_this <- ggplot(df, aes(get(analyze_var), fill = get(group_var))) + geom_density(alpha = 0.2) + xlab(xlab_) + ylab(ylab_) + ggtitle(title_plot) + geom_vline(data=lines_df,aes(xintercept=Means,yintercept=0,linetype=Names,colour = Names), show_guide = TRUE) + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.position="bottom") +  guides(fill=guide_legend(title=NULL)) 
  return(plot_this)
}

# Plot several variables to subset
call_plot_subset_several <- function(df,group_var,subset_val1,subset_val2,subset_val3,subset_val4,analyze_var,xlab_,ylab_,title_plot) {
  mean_one <- df[get(group_var)==subset_val1,mean(na.omit(get(analyze_var)))]
  mean_two <- df[get(group_var)==subset_val2,mean(na.omit(get(analyze_var)))]
  mean_three <- df[get(group_var)==subset_val3,mean(na.omit(get(analyze_var)))]
  mean_four <- df[get(group_var)==subset_val4,mean(na.omit(get(analyze_var)))]
  
  Means = c(mean_one,mean_two,mean_three,mean_four)
  Names = c(subset_val1,subset_val2,subset_val3,subset_val4)
  lines_df = data.frame(Names,Means)   
  
  plot_this <- ggplot(df, aes(get(analyze_var), fill = get(group_var))) + geom_density(alpha = 0.2) + xlab(xlab_) + ylab(ylab_) + ggtitle(title_plot) +geom_vline(data=lines_df,aes(xintercept=Means,yintercept=0,linetype=Names,colour = Names), show_guide = TRUE) + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.position="bottom") +  guides(fill=guide_legend(title=NULL)) 
  return(plot_this)
}


# Five  variables to subset
call_plot_subset_five <- function(df,group_var,subset_val1,subset_val2,subset_val3,subset_val4,subset_val5,analyze_var,xlab_,ylab_,title_plot) {
  mean_one <- df[get(group_var)==subset_val1,mean(na.omit(get(analyze_var)))]
  mean_two <- df[get(group_var)==subset_val2,mean(na.omit(get(analyze_var)))]
  mean_three <- df[get(group_var)==subset_val3,mean(na.omit(get(analyze_var)))]
  mean_four <- df[get(group_var)==subset_val4,mean(na.omit(get(analyze_var)))]
  mean_five <- df[get(group_var)==subset_val5,mean(na.omit(get(analyze_var)))]
  
  Means = c(mean_one,mean_two,mean_three,mean_four,mean_five)
  Names = c(subset_val1,subset_val2,subset_val3,subset_val4,subset_val5)
  lines_df = data.frame(Names,Means)   
  
  plot_this <- ggplot(df, aes(get(analyze_var), fill = get(group_var))) + geom_density(alpha = 0.2) + xlab(xlab_) + ylab(ylab_) + ggtitle(title_plot) + geom_vline(data=lines_df,aes(xintercept=Means,yintercept=0,linetype=Names,colour = Names), show_guide = TRUE) + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.position="bottom") +  guides(fill=guide_legend(title=NULL)) 
  return(plot_this)
}

#Treatment vs Control Group Post Implementation


############ Define Groups and Variables

# Missing A1 and A25 - add them for the next round
flexlist <- c('A3','A6','A7','A11','A12','A14','A16','A17','A18','A19','A20','A21','A24','A25','A26','A28','A29') #Missing 'A1' Removed'A9','A22',
treatment_group_list <- c('A1','A3','A6','A7','A9','A11','A12','A14','A16','A17','A18','A19','A20','A21','A22','A24','A25','A26','A28','A29')
treatment_encuesta_id <- c('40','3','43','33','309','40','37','117','116','219','312','59','317','163','315','316','318','197','14','12')
control_group_list <- c(7,20,26,45,77,124,185,191,203,204,302,304,305,307,310,311)
info_paying_houses <- c('A3','A6','A11','A12','A17','A18','A20','A21','A25','A26') #houses that are paying for information

# Tariff Types
Casa <- c("A1","A3","A6","A7","A9","A11","A12","A14","A16","A17","A18","A19","A20","A21","A22","A24","A25","A26","A28","A29")
Tariff_Code <- c("T-0","T-J","T-1","T-0","T-0","T-1","T-1","T-0","T-0","T-1","T-J","T-1","T-0","T-1","T-0","T-0","T-0","T-1","T-1","T-1")
Tariff_Code_Control <- c("T-1","T-1","T-0","T-0","T-1","T-0","T-1","T-1","T-0","T-1","T-0","T-0","T-0","T-0","T-0","T-0")
casa_tariff_code <- data.frame(Casa,Tariff_Code)
control_tariff_code <- data.frame(control_group_list,Tariff_Code_Control) %>% mutate(Casa=control_group_list,Tariff_Code=Tariff_Code_Control) %>% select(Casa,Tariff_Code)


# House or Micro-Enterprise
control_house_me_list <- c('pulperia','pulperia','pulperia','pulperia','pulperia','pulperia','pulperia','pulperia','pulperia','pulperia','casa','casa','casa','casa','casa','casa')
treatment_house_me_list <- c('casa','pulperia','pulperia','pulperia','casa','pulperia','pulperia','pulperia','pulperia','pulperia','casa','pulperia','casa','pulperia','casa','casa','casa','pulperia','pulperia','pulperia')
control_house_me_df <- data.frame(control_group_list,control_house_me_list) %>% mutate(Casa=control_group_list,Lugar=control_house_me_list) %>% select(Casa,Lugar)
treatment_house_me_df <- data.frame(treatment_group_list,treatment_house_me_list) %>% mutate(Casa=treatment_group_list,Lugar=treatment_house_me_list) %>% select(Casa,Lugar)


# Steps in the implementation
dump <- 'DUMP13'

# Houses that have too much variability: A29


############
############ Reading Data
############

### Sever Connection

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

#Sensor Data
data.list.houses <- read.house.fridge(dump,flexlist) # read.house.fridge or  read.data.all  or   read.data.all.parallel
    cluster.house <- data.list.houses[[2]] %>% mutate(house_Energy=houseAll_Energy) %>% select(id,datetime,houseAll_Voltage,houseAll_Current,houseAll_Power,house_Energy,house.id)
    cluster.fridge <- data.list.houses[[1]]


#Survey Data
survey.data.results <- read.survey.data(Casa)
energy.receipt.data <- read.survey.data(Casa)[[1]]
energy.receipt.control <- read.survey.data(Casa)[[8]]
e_i_treatment <- survey.data.results[[2]]
e_i_control <- read.survey.data(Casa)[[3]]
survey.data.responses <- survey.data.results[[4]]
flexlistid_encuesta_id <- subset(survey.data.results[[6]],survey.data.results[[6]]$control_estudio=='estudio') %>% select(encuesta_id,flexbox_id)
control_encuesta_id <- subset(survey.data.results[[6]],survey.data.results[[6]]$control_estudio=='control') %>% select(encuesta_id,flexbox_id)

#Creating groups with high willingness to pay, and high willingness to pay as a fraction of income

valor_papelito <- survey.data.responses[c('flexbox_id','encuesta_id','bdm_valor_dinero','bdm_valor_informacion','valor_papelito')]

      valor_papelito$bdm_valor_dinero[is.na(valor_papelito$bdm_valor_dinero)] <- 0
      valor_papelito$bdm_valor_informacion[is.na(valor_papelito$bdm_valor_informacion)] <- 0
      valor_papelito$valor_papelito[is.na(valor_papelito$valor_papelito)] <- 0
      valor_papelito$bet <- valor_papelito$bdm_valor_dinero + valor_papelito$bdm_valor_informacion
      wtp <- valor_papelito[c('flexbox_id','encuesta_id','valor_papelito','bet')]
      wtp <- subset(wtp,wtp$bet !=0 & wtp$valor_papelito !=0)
      
      wtp_values <- c(100,70,80,200,50,100,150,200,200,100,100,100,80,100,125,20,200,0,0,0)
      wtp_paper <- c(60,20,60,100,60,80,160,180,40,200,120,40,160,100,200,100,0,0,0,0)
      Casa <- c('A25','A26','A12','A11','A9','A6','A7','A3','A17','A29','A28','A20','A19','A18','A14','A16','A21','A1','A22','A24')
      
      wtp_df <- data.frame(Casa,wtp_paper,wtp_values)
      #Creating low, medium low, medium high, and high values
      wtp_df$wtp_lw_md_h <- ifelse(wtp_df$wtp_values>=0 & wtp_df$wtp_values <= 50,'low',ifelse(wtp_df$wtp_values>50 & wtp_df$wtp_values<=100,'medium-low',ifelse(wtp_df$wtp_values>100 & wtp_df$wtp_values <= 150,'medium-high','high')))
      #Calculating the mean value for each household 
      e_i_treatment_sub <- subset(e_i_treatment,e_i_treatment$Ano==2016)
      e_i_treatment_means <- aggregate(e_i_treatment_sub$importe,by=list(e_i_treatment_sub$Casa),FUN=mean,na.rm=TRUE) %>% mutate(Casa=Group.1,mean_cost=x) %>% select(Casa,mean_cost)

      wtp_df <- merge(wtp_df,e_i_treatment_means,by=c('Casa'))
      wtp_df$fraction_wtp <- wtp_df$wtp_values/wtp_df$mean_cost
      wtp_df$fraction_lw_md_h <- ifelse(wtp_df$fraction_wtp>=0 & wtp_df$fraction_wtp <= 0.05,'low',ifelse(wtp_df$fraction_wtp>0.05 & wtp_df$fraction_wtp<0.1,'medium-low',ifelse(wtp_df$fraction_wtp>=0.1 & wtp_df$fraction_wtp <= 0.2,'medium-high','high')))
      
      
        


############# Analysis

####### NOTE1: Need to add a date (and a distribution) for when some of the households lost their energy reports! ##############
####### NOTE2: Need to add the time series with adjusted energy values for treatment and control. Once you've added the adjusted time series 
######  NOTE3: I suggest adding a 1 or a 0 to code whether or not the house was in treatment or in control
      
time.series.receipt <- survey.data.results[[2]] %>% mutate(treatment="Treatment")
    time.series.receipt$Casa <- as.character(time.series.receipt$Casa)
    
    #Dates of Report intervention (all of them receving reports)
    time.series.receipt$report_intervention_month <- ifelse(time.series.receipt$Mes == "Febrero" | time.series.receipt$Mes == "Marzo" | time.series.receipt$Mes == "Abril" | time.series.receipt$Mes == "Mayo" | time.series.receipt$Mes == "Junio" | time.series.receipt$Mes == "Julio" | time.series.receipt$Mes == "Agosto" | time.series.receipt$Mes == "Septiembre" | time.series.receipt$Mes == "Octubre",1,0 )
    #Dates of SMS intervention (all of them receiving texts)
    time.series.receipt$intervention_group <- ifelse(time.series.receipt$fecha>="2016-06-01","Treatment Post-Intervention","Treatment Pre-Intervention")
    time.series.receipt$sms_intervention_month <- ifelse(time.series.receipt$Mes == "Junio" | time.series.receipt$Mes == "Julio" | time.series.receipt$Mes == "Agosto" | time.series.receipt$Mes == "Septiembre" | time.series.receipt$Mes == "Octubre", 1,0 )
    time.series.receipt$sms_intervention_month_text <- ifelse(time.series.receipt$Mes == "Junio" | time.series.receipt$Mes == "Julio" | time.series.receipt$Mes == "Agosto" | time.series.receipt$Mes == "Septiembre" | time.series.receipt$Mes == "Octubre", "Actively Receiving SMS","No SMS")
    #Adding an ID for whether or not the group is keeping or isn't keeping the information
    time.series.receipt$information_group <- ifelse(time.series.receipt$Casa == 'A3' | time.series.receipt$Casa == 'A6' | time.series.receipt$Casa == 'A11' | time.series.receipt$Casa == 'A12' | time.series.receipt$Casa == 'A17' | time.series.receipt$Casa == 'A18' | time.series.receipt$Casa == 'A20' | time.series.receipt$Casa == 'A21' | time.series.receipt$Casa == 'A25' | time.series.receipt$Casa == 'A26',"Won Willingness to Pay Information Bid","Lost Willingness to Pay Information Bid")
    #Merge with WTP data from above
    time.series.receipt <- merge(time.series.receipt,wtp_df,by=c('Casa'))
    #Turn the importe to dollars
    time.series.receipt$importe_dl <- time.series.receipt$importe/29
    #Merge with house info
    time.series.receipt <- merge(time.series.receipt,treatment_house_me_df,by=c('Casa'))
    
    # Calculating 'energia' values from historical 'importe' values
    calculated_i_e_treatment <- get.tariff.data('treatment',e_i_treatment,casa_tariff_code,time.series.receipt,energy.receipt.data,tariff_data.dt) 
    time.series.receipt <- merge(calculated_i_e_treatment,time.series.receipt,by=c('Casa','Mes','Ano'),all=T)
    time.series.receipt$energia <- 'NA'
    
    for(i in 1:length(time.series.receipt$energia.y)) {
      if(is.na(time.series.receipt$energia.y[i])==T){
        time.series.receipt$energia[i] = time.series.receipt$energia.x[i]
        time.series.receipt$energia_ajustada[i] = time.series.receipt$energia.x[i]
      } else {time.series.receipt$energia[i] = time.series.receipt$energia.y[i]
      time.series.receipt$energia_ajustada[i] = gsub("[[:punct:]]", " ", sub(".*=", "", time.series.receipt$Energia_Ajustada[i]))
      time.series.receipt$energia_ajustada[i] = gsub(" ",".",time.series.receipt$energia_ajustada[i])
      time.series.receipt$energia_ajustada[i] = gsub("..","",time.series.receipt$energia_ajustada[i],fixed =TRUE)
      }
    }
    
    time.series.receipt <- time.series.receipt[order(time.series.receipt$Casa,time.series.receipt$fecha),]
    time.series.receipt$energia <- as.integer(time.series.receipt$energia)
    time.series.receipt$energia_ajustada <- as.integer(time.series.receipt$energia_ajustada)
    time.series.receipt$importe_dl <- as.integer(time.series.receipt$importe_dl)
    
    #Turn into data table
    time.series.receipt.dt <- as.data.table(time.series.receipt)
    
time.series.receipt.control <-  survey.data.results[[3]] %>% mutate(treatment="Control")
time.series.receipt.control$Casa <- as.character(time.series.receipt.control$Casa)
    
    # Dates of Report intervention
    time.series.receipt.control$report_intervention_month <- ifelse(time.series.receipt.control$Mes == "Marzo" | time.series.receipt.control$Mes == "Abril" | time.series.receipt.control$Mes == "Mayo" | time.series.receipt.control$Mes == "Junio" | time.series.receipt.control$Mes == "Julio" | time.series.receipt.control$Mes == "Agosto" | time.series.receipt.control$Mes == "Septiembre" | time.series.receipt.control$Mes == "Octubre" | time.series.receipt.control$Mes == "Noviembre",1,0 )
    # Dates of SMS intervention
    time.series.receipt.control$intervention_group <- ifelse(time.series.receipt.control$fecha>="2016-06-01","Control Post-Intervention","Control Pre-Intervention")
    time.series.receipt.control$sms_intervention_month <- ifelse(time.series.receipt.control$Mes == "Junio" | time.series.receipt.control$Mes == "Julio" | time.series.receipt.control$Mes == "Agosto" | time.series.receipt.control$Mes == "Septiembre" | time.series.receipt.control$Mes == "Octubre" | time.series.receipt.control$Mes == "Noviembre",1,0 )
    time.series.receipt.control$sms_intervention_month_text <- ifelse(time.series.receipt.control$Mes == "Junio" | time.series.receipt.control$Mes == "Julio" | time.series.receipt.control$Mes == "Agosto" | time.series.receipt.control$Mes == "Septiembre" | time.series.receipt.control$Mes == "Octubre" | time.series.receipt.control$Mes == "Noviembre","Actively Receiving SMS","No SMS")
    # Adding an ID for whether or not the group is keeping or isn't keeping the information
    time.series.receipt.control$information_group <- 'Control'
    #Turn the importe to dollars
    time.series.receipt.control$importe_dl <- time.series.receipt.control$importe/29
    # Adding NA variables so that we can do the merge
    time.series.receipt.control$wtp_paper <- NA
    time.series.receipt.control$wtp_values <- NA
    time.series.receipt.control$wtp_lw_md_h <- 'Control'
    time.series.receipt.control$mean_cost <- NA
    time.series.receipt.control$fraction_wtp <- NA
    time.series.receipt.control$fraction_lw_md_h <- 'Control'
    # Merging with type of establishment
    time.series.receipt.control <- merge(time.series.receipt.control,control_house_me_df,by=c('Casa'))
    
    # Calculating 'energia' values from historical 'importe' values
    calculated_i_e_control <- get.tariff.data('control',e_i_control,control_tariff_code,time.series.receipt.control,energy.receipt.control,tariff_data.dt)
    time.series.receipt.control <- merge(calculated_i_e_control,time.series.receipt.control,by=c('Casa','Mes','Ano'),all=T)
    time.series.receipt.control$energia <- 'NA'
    
    for(i in 1:length(time.series.receipt.control$energia.y)) {
      if(is.na(time.series.receipt.control$energia.y[i])==T){
        time.series.receipt.control$energia[i] = time.series.receipt.control$energia.x[i]
        time.series.receipt.control$energia_ajustada[i] = time.series.receipt.control$energia.x[i]
      } else {time.series.receipt.control$energia[i] = time.series.receipt.control$energia.y[i]
      time.series.receipt.control$energia_ajustada[i] = gsub("[[:punct:]]", " ", sub(".*=", "", time.series.receipt.control$Energia_Ajustada[i]))
      time.series.receipt.control$energia_ajustada[i] = gsub(" ",".",time.series.receipt.control$energia_ajustada[i])
      time.series.receipt.control$energia_ajustada[i] = gsub("..","",time.series.receipt.control$energia_ajustada[i],fixed =TRUE)
      }
    }
 
    time.series.receipt.control <- time.series.receipt.control[order(time.series.receipt.control$Casa,time.series.receipt.control$fecha),]
    time.series.receipt.control$energia <- as.integer(time.series.receipt.control$energia)
    time.series.receipt.control$energia_ajustada <- as.integer(time.series.receipt.control$energia_ajustada)
    time.series.receipt.control$importe_dl <- as.integer(time.series.receipt.control$importe_dl)
    
    
    # Turn into data table
    time.series.receipt.control.dt <- as.data.table(time.series.receipt.control)
    time.series.receipt.control.dt <- time.series.receipt.control.dt[,list(Casa,Mes,Ano,energia.x,Energia,Energia_Ajustada,tipo,importe,mes,fecha,energia.y,energia_ajustada,treatment,report_intervention_month,intervention_group,sms_intervention_month,sms_intervention_month_text,information_group,wtp_paper,wtp_values,wtp_lw_md_h,mean_cost,fraction_wtp,fraction_lw_md_h,importe_dl,Lugar,energia)]

    
# Binding
data_time_series <- rbind(time.series.receipt,time.series.receipt.control)
data_time_series.dt <- rbind(time.series.receipt.dt,time.series.receipt.control.dt)


##########################
######### Changes to make before proceeding to analysis  

# 1. Before moving forward you can decide whether or not you want to remove outliers and what 
# that does to the analysis.

ggplot(time.series.receipt,aes(Casa,energia_ajustada)) + geom_point()
ggplot(time.series.receipt.control,aes(Casa,energia_ajustada)) + geom_point()

# 2. Remove outliers 
      # 26 & 11 are outliers
      # 29 had too many problems and no assertions could be made about the trendline
      # For the control group, 45 and 191 we could not get data for 2015 so they were dropped from the post implementation analysis

#Treatment      
time.series.receipt.nooutliers <- subset(time.series.receipt, time.series.receipt$Casa!="A26" & time.series.receipt$Casa!="A11" & time.series.receipt$Casa!="A29" & time.series.receipt$Casa!="A24")
time.series.receipt.nooutliers.dt <- as.data.table(time.series.receipt.nooutliers)

#Control
time.series.receipt.control.nooutliers <- subset(time.series.receipt.control, time.series.receipt.control$Casa!="45" & time.series.receipt.control$Casa!="191" )
time.series.receipt.control.nooutliers.dt <- as.data.table(time.series.receipt.control.nooutliers)

data_time_series_nooutliers <- subset(data_time_series, data_time_series$Casa!="A26" & data_time_series$Casa!="A11" & data_time_series$Casa!="A29" & time.series.receipt$Casa!="A24" & data_time_series$Casa!="191" & data_time_series$Casa!="45" )
data_time_series_nooutliers.dt <- as.data.table(data_time_series_nooutliers)
data_time_series_nooutliers.dt$energia <- as.numeric(data_time_series_nooutliers.dt$energia)
data_time_series_nooutliers.dt$energia_ajustada <- as.numeric(data_time_series_nooutliers.dt$energia_ajustada)



# Binding
#data_time_series_nooutliers <- rbind(time.series.receipt.nooutliers,time.series.receipt.control)
#data_time_series_nooutliers.dt <- rbind(time.series.receipt.nooutliers.dt,time.series.receipt.control.dt)



#####
#####
##### 3. Making sure that the groups are balanced: see if the outcome variable is balanced before the intervention

# All data from houses including the one previous calculated before the experiment began
ggplot(subset(time.series.receipt,time.series.receipt$fecha<="2016-06-01" & time.series.receipt$fecha>="2015-01-01"), aes(x=Casa, y=importe_dl)) + geom_point() + geom_boxplot(outlier.colour=NA, fill=NA, colour="grey20") + geom_hline(yintercept=mean(subset(time.series.receipt,time.series.receipt$fecha<"2016-06-01")$importe_dl,na.rm=TRUE)) + xlab('House ID') + ylab('Energy Costs ($US)') + ggtitle('Treatment: Energy Costs pre Implementation') +theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) 
ggplot(subset(time.series.receipt,time.series.receipt$fecha<="2016-06-01" & time.series.receipt$fecha>="2015-01-01"), aes(x=Casa, y=energia)) + geom_point() + geom_boxplot(outlier.colour=NA, fill=NA, colour="grey20") + geom_hline(yintercept=mean(subset(time.series.receipt,time.series.receipt$fecha<"2016-06-01")$energia,na.rm=TRUE))  + xlab('House ID') + ylab('Energy Consumption (kWh)') + ggtitle('Treatment: Energy Consumption pre Implementation') +theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) 

ggplot(subset(time.series.receipt.control,time.series.receipt.control$fecha<="2016-06-01" & time.series.receipt$fecha>="2015-01-01"), aes(x=Casa, y=importe_dl)) + geom_point() + geom_boxplot(outlier.colour=NA, fill=NA, colour="grey20") + geom_hline(yintercept=mean(subset(time.series.receipt.control,time.series.receipt.control$fecha<"2016-06-01")$importe_dl,na.rm=TRUE))  + xlab('House ID') + ylab('Energy Costs ($US)') + ggtitle('Control: Energy Costs pre Implementation') +theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) 
ggplot(subset(time.series.receipt.control,time.series.receipt.control$fecha<="2016-06-01" & time.series.receipt$fecha>="2015-01-01"), aes(x=Casa, y=energia)) + geom_point() + geom_boxplot(outlier.colour=NA, fill=NA, colour="grey20") + geom_hline(yintercept=mean(subset(time.series.receipt.control,time.series.receipt.control$fecha<"2016-06-01")$energia,na.rm=TRUE))  + xlab('House ID') + ylab('Energy Costs ($US)') + ggtitle('Control: Energy Costs pre Implementation') +theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) 

# Same data but after removing outliers
ggplot(subset(time.series.receipt.nooutliers,time.series.receipt.nooutliers$fecha<="2016-06-01" & time.series.receipt$fecha>="2015-01-01"), aes(x=Casa, y=importe_dl)) + geom_point() + geom_boxplot(outlier.colour=NA, fill=NA, colour="grey20") + geom_hline(yintercept=mean(subset(time.series.receipt.nooutliers,time.series.receipt.nooutliers$fecha<"2016-06-01")$importe_dl,na.rm=TRUE)) + xlab('House ID') + ylab('Energy Costs ($US)') + ggtitle('Treatment: Energy Costs pre Implementation') +theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) 
ggplot(subset(time.series.receipt.nooutliers,time.series.receipt.nooutliers$fecha<="2016-06-01" & time.series.receipt$fecha>="2015-01-01"), aes(x=Casa, y=energia)) + geom_point() + geom_boxplot(outlier.colour=NA, fill=NA, colour="grey20") + geom_hline(yintercept=mean(subset(time.series.receipt.nooutliers,time.series.receipt.nooutliers$fecha<"2016-06-01")$energia,na.rm=TRUE)) + xlab('House ID') + ylab('Energy Costs ($US)') + ggtitle('Treatment: Energy Costs pre Implementation') +theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) 


# T- test to make sure that energia and importe are balanced before the intervention
#All Data
t.test(time.series.receipt$importe_dl, time.series.receipt.control$importe_dl) #importe
t.test(time.series.receipt$energia, time.series.receipt.control$energia) #energia

#No outliers
t.test(time.series.receipt.nooutliers$importe_dl, time.series.receipt.control.nooutliers$importe_dl) #importe
t.test(time.series.receipt.nooutliers$energia, time.series.receipt.control.nooutliers$energia) #energia

houses_before_implementation <- as.data.table(rbind(time.series.receipt,time.series.receipt.control))
houses_before_implementation_no_outliers <- as.data.table(rbind(time.series.receipt.nooutliers,time.series.receipt.control.nooutliers))

call_plot_subset(houses_before_implementation,"treatment","Treatment","Control",'energia',"Monthly Energy Consumption (kWh)","Density","Before Implementation (kWH/Month): Treatment vs. Control")
call_plot_subset(houses_before_implementation_no_outliers,"treatment","Treatment","Control",'energia',"Monthly Energy Consumption (kWh)","Density","Before Implementation (kWH/Month): Treatment vs. Control")

call_plot_subset(houses_before_implementation_no_outliers,"treatment","Treatment","Control",'importe',"Monthly Energy Expenditures ($US)","Density","Before Implementation (kWH/Month): Treatment vs. Control")
call_plot_subset(houses_before_implementation_no_outliers,"treatment","Treatment","Control",'importe',"Monthly Energy Expenditures ($US)","Density","Before Implementation (kWH/Month): Treatment vs. Control")


#########
#########
#########  4. Month to Month Annual Differences

plot_energy_cost_ft(time.series.receipt.dt,time.series.receipt.control.dt,data_time_series.dt,"energia","full_data","energia")
plot_energy_cost_ft(time.series.receipt.dt,time.series.receipt.control.dt,data_time_series.dt,"energia_ajustada","full_data","energia_ajustada")

plot_energy_cost_ft(time.series.receipt.nooutliers.dt,time.series.receipt.control.dt,data_time_series_nooutliers.dt,"energia","no_outliers","energia")
plot_energy_cost_ft(time.series.receipt.nooutliers.dt,time.series.receipt.control.dt,data_time_series_nooutliers.dt,"energia_ajustada","no_outliers","energia_ajustada")


#########
#########
#########  5. Treatment vs. Control


plot_tr_ctl(data_time_series.dt,'energia','full_data')
plot_tr_ctl(data_time_series.dt,'energia_ajustada','full_data')

plot_tr_ctl(data_time_series_nooutliers.dt,'energia','no_outliers')
plot_tr_ctl(data_time_series_nooutliers.dt,'energia_ajustada','no_outliers')


########################################
##########
##########   
########## 2 Learning


baseline_receipt_data_for_merge <- survey.data.results[[5]] # Baseline data

house_baseline <- survey.data.results[[10]] # House Baseline Data

survey.data.complete <- survey.data.results[[4]] # Monthly Updates
    survey.data.complete$gasto_electrico <- as.numeric(as.character(survey.data.complete$gasto_electrico))
    survey.data.complete$gasto_electrico_cordobas <- as.numeric(as.character(survey.data.complete$gasto_electrico_cordobas))
    survey.data.complete$gasto_tarifa_electrica <- as.numeric(as.character(survey.data.complete$gasto_tarifa_electrica))
    survey.data.complete$agua_gasto <- as.numeric(survey.data.complete$agua_gasto)
    survey.data.complete$agua_gasto_verdad <- as.numeric(survey.data.complete$agua_gasto_verdad)
    
implementation.baseline <- survey.data.results[[6]] # Implementation Baseline
      implementation.baseline$r_total <- as.numeric(as.character(implementation.baseline$r_total))
      implementation.baseline$gasto_electrico <- as.numeric(as.character(implementation.baseline$gasto_electrico))
      
aggregate_tretment_data <- survey.data.results[[12]]
      
final.control.survey <- survey.data.results[[9]] # Final Control Survey
    final.control.survey$agua_gasto <- as.numeric(as.character(final.control.survey$agua_gasto))
    final.control.survey$agua_gasto_verdad <- as.numeric(as.character(final.control.survey$agua_gasto_verdad))
    
control.survey.data <- survey.data.results[[11]] # Aggregate Survey Results

aggregate_control_data <- survey.data.results[[13]]
          
learning.data <- merge(survey.data.complete,baseline_receipt_data_for_merge,by="encuesta_id",all=T)
      learning.data$gasto_electrico <- as.numeric(as.character(learning.data$gasto_electrico))
      learning.data$gasto_electrico_cordobas <- as.numeric(as.character(learning.data$gasto_electrico_cordobas))
      learning.data$gasto_tarifa_electrica <- as.numeric(as.character(learning.data$gasto_tarifa_electrica))
      learning.data$today <- as.character(learning.data$today)
      learning.data$year <- as.numeric(substrRight(learning.data$today, 2))
      learning.data$month <- as.numeric(gsub( "/.*$", "", learning.data$today))
      day_list <- apply(learning.data[4],1,function(x) rm_between(x, "/", "/", extract=TRUE))
      learning.data$day <- data.frame(matrix(unlist(day_list), nrow=length(learning.data$today), byrow=T),stringsAsFactors=FALSE)$matrix.unlist.day_list...nrow...length.learning.data.today...
      learning.data$date <- paste(learning.data$month,learning.data$day,learning.data$year,sep = "/")
      learning.data$date <- strptime(learning.data$date,"%m/%d/%y")

      #Energy
      learning.data$kwh_estimate_diff <- learning.data$r_monthly_kwh - learning.data$gasto_electrico
      learning.data$kwh_estimate_diff_pct <- ((abs(learning.data$r_monthly_kwh - learning.data$gasto_electrico))/learning.data$r_monthly_kwh)*100
      
      #Cost
      learning.data$cordobas_estimate_diff <- learning.data$r_monthly_cordobas - learning.data$gasto_electrico_cordobas
      learning.data$cordobas_estimate_diff_pct <- ((abs(learning.data$r_monthly_cordobas - learning.data$gasto_electrico_cordobas))/learning.data$r_monthly_cordobas)*100
      learning.data$total_cordobas_estimate_diff <- learning.data$r_total_cordobas - learning.data$gasto_electrico_cordobas
      learning.data$total_cordobas_estimate_diff_pct <- ((abs(learning.data$r_total_cordobas - learning.data$gasto_electrico_cordobas))/learning.data$r_total_cordobas)*100
      
      #Tariff NOTE: need to add the kwh in step changes, not only the average value
      learning.data$tariff_diff <-learning.data$r_monthly_cordobas/learning.data$r_monthly_kwh - learning.data$gasto_tarifa_electrica
      learning.data$tariff_diff_pct <-(abs(learning.data$r_monthly_cordobas/learning.data$r_monthly_kwh - learning.data$gasto_tarifa_electrica)/(learning.data$r_monthly_cordobas/learning.data$r_monthly_kwh))*100
          
    
    # 2.1 Accuracy of energy expenditures and energy costs
          
          # Expected Energy vs. Actual Energy Consumption
          ggplot(learning.data, aes(gasto_electrico, r_monthly_kwh)) + geom_point(aes(size = kwh_estimate_diff)) + geom_abline(slope=1, intercept=0)
          # Expected Energy vs. Actual Energy Consumption
          ggplot(learning.data, aes(gasto_electrico_cordobas, r_monthly_cordobas)) + geom_point(aes(size = cordobas_estimate_diff_pct))+ geom_abline(slope=1, intercept=0)
          ggplot(learning.data, aes(gasto_electrico_cordobas, r_total_cordobas)) + geom_point(aes(size = total_cordobas_estimate_diff_pct)) + scale_colour_gradient(low = "blue") + geom_abline(slope=1, intercept=0)
          
          #Change "diff" or "pct" depeding on whether you want to see the diff or the pct
          # kwh_estimate_diff total_cordobas_estimate_diff tariff_diff
          for(i in 1:length(unique(learning.data$flexbox_id))){
            subset_house_data <- subset(learning.data,learning.data$flexbox_id == unique(learning.data$flexbox_id)[i])
            
            plot_diff <- ggplot(subset_house_data,aes(date,tariff_diff_pct)) + geom_point()
            selected.house <- unique(learning.data$flexbox_id)[i] 
            
            plot.name = paste(selected.house,sep="")
            mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/learning/tariff/pct",paste(plot.name,".jpg",sep=""))
            
            jpeg(file=mypath)
            print(plot_diff)
            dev.off()
          }
    
          
     #2.1.1 Plotting the baseline, control, and treatment groups against each other

      survey.data.plot <- survey.data.complete %>% mutate(data='treatment') %>% select(encuesta_id,r_total_cordobas,gasto_electrico_cordobas,data)
      baseline_receipt_data_for_plot <- baseline_receipt_data_for_merge %>% mutate(data='Baseline',r_total_cordobas=baseline_monthly_cordobas,gasto_electrico_cordobas=baseline_gasto_electrico) %>% select(encuesta_id,r_total_cordobas,gasto_electrico_cordobas,data)
      implementation_baseline <- implementation.baseline %>% mutate(data='Mid-Baseline',r_total_cordobas=r_total,gasto_electrico_cordobas=gasto_electrico) %>% select(encuesta_id,r_total_cordobas,gasto_electrico_cordobas,data)
      baseline_study_households <- subset(implementation.baseline,implementation.baseline$control_estudio=='estudio' & implementation.baseline$tipo_establecimiento=='casa') %>% mutate(data='baseline',r_total_cordobas=r_total,gasto_electrico_cordobas=gasto_electrico) %>% select(encuesta_id,r_total_cordobas,gasto_electrico_cordobas,data)
      baseline_all_households <- house_baseline %>% mutate(data='household baseline') %>% select(encuesta_id,r_total_cordobas,gasto_electrico_cordobas,data)
      
      baseline_current_plot <- rbind(baseline_receipt_data_for_plot,survey.data.plot,baseline_households) %>% mutate(total_cordobas_estimate_diff_pct=r_total_cordobas-gasto_electrico_cordobas)

      all_baseline_data <- rbind(baseline_receipt_data_for_plot,baseline_study_households,baseline_all_households)
      all_baseline_data$gasto_electrico_cordobas <- as.numeric(all_baseline_data$gasto_electrico_cordobas)
      
      # All Baseline
      ggplot(subset(all_baseline_data,all_baseline_data$gasto_electrico_cordobas<20000), aes(gasto_electrico_cordobas, r_total_cordobas)) + geom_point(alpha=0.5) + geom_abline(slope=1, intercept=0) + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.position="bottom") +  guides(fill=guide_legend(title=NULL)) + xlab("Perceived Electricity Cost") + ylab("Actual Electricity Cost") + ggtitle("Baseline Actual vs Perceived Costs")
      
      # Treatment

      ggplot(aggregate_tretment_data, aes(gasto_electrico_cordobas, r_total_cordobas,group=data,colour=data)) + geom_point(alpha=0.3) + geom_abline(slope=1, intercept=0) + 
        theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
        theme(legend.position="bottom") +  guides(fill=guide_legend(title="Timeline:"))  + xlab("Perceived Electricity Cost") + ylab("Actual Electricity Cost") 

      #Control
      control_baseline <- subset(baseline_receipt_data_for_plot, as.character(baseline_receipt_data_for_plot$encuesta_id) %in% control_group_list) 
      control_baseline_households <- subset(implementation_baseline, as.character(implementation_baseline$encuesta_id) %in% control_group_list)
      final_control_survey <- final.control.survey %>% mutate(data='Control End')%>% select(encuesta_id,r_total_cordobas,gasto_electrico_cordobas,data)
            final_control_survey$gasto_electrico_cordobas <- as.numeric(as.character(final_control_survey$gasto_electrico_cordobas))
            final_control_survey <- na.omit(final_control_survey)
            
              #Evaluating Missing Data
              merge_control_1 <- merge(control_baseline,control_baseline_households,by=c('encuesta_id'),all=TRUE)
              merge_control_2 <- merge(merge_control_1,na.omit(final_control_survey),by=c('encuesta_id'),all=TRUE)
              
      control_group_all_data <- rbind(control_baseline,control_baseline_households,final_control_survey)
      control_group_all_data$gasto_electrico_cordobas <- as.numeric(control_group_all_data$gasto_electrico_cordobas)
      
      # Merged Data
      ggplot(aggregate_control_data, aes(gasto_electrico_cordobas, r_total_cordobas,group=data,colour=data)) + geom_point(alpha=0.3) + geom_abline(slope=1, intercept=0) + 
        theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
        theme(legend.position="bottom") +  guides(fill=guide_legend(title="Timeline:"))  + xlab("Perceived Electricity Cost") + ylab("Actual Electricity Cost") 
      
      
#####################################################
#####################################################
# Getting estimates for understanding before and after
#   Need to plot how far off people are in terms of kwh
      aggregate_tretment_data$diff <- aggregate_tretment_data$gasto_electrico_cordobas - aggregate_tretment_data$r_total_cordobas
      aggregate_control_data$diff <- aggregate_control_data$gasto_electrico_cordobas - aggregate_control_data$r_total_cordobas
      
      call_plot_subset_three(as.data.table(aggregate_tretment_data)[diff<5000 & diff>-5000],"data","Baseline","Mid-Baseline","Treatment Ongoing","diff","Diff","Coso","Peso")
      call_plot_subset_three(as.data.table(aggregate_control_data)[diff>-2000],"data","Baseline","Mid-Baseline","Control End","diff","Diff","Coso","Peso")
      
      # Descriptive stats of learning
      # Keeping variables that are between the 1st and 99th percentile
      
      #Treatment
      baseline_treatment_accuracy <- subset(aggregate_tretment_data,aggregate_tretment_data$data == "Baseline" & aggregate_tretment_data$diff > quantile(aggregate_tretment_data$diff,c(0.01),na.rm=TRUE) & aggregate_tretment_data$diff < quantile(aggregate_tretment_data$diff,c(0.99),na.rm=TRUE))
      mid_treatment_accuracy <- subset(aggregate_tretment_data,aggregate_tretment_data$data == "Mid-Baseline" & aggregate_tretment_data$diff > quantile(aggregate_tretment_data$diff,c(0.01),na.rm=TRUE) & aggregate_tretment_data$diff < quantile(aggregate_tretment_data$diff,c(0.99),na.rm=TRUE))
      ongoing_treatment_accuracy <- subset(aggregate_tretment_data,aggregate_tretment_data$data == "Treatment Ongoing" & aggregate_tretment_data$diff > quantile(aggregate_tretment_data$diff,c(0.01),na.rm=TRUE) & aggregate_tretment_data$diff < quantile(aggregate_tretment_data$diff,c(0.99),na.rm=TRUE))
      end_treatment_accuracy <- subset(aggregate_tretment_data,aggregate_tretment_data$data == "Treatment End" & aggregate_tretment_data$diff > quantile(aggregate_tretment_data$diff,c(0.01),na.rm=TRUE) & aggregate_tretment_data$diff < quantile(aggregate_tretment_data$diff,c(0.99),na.rm=TRUE))
      
      mean_list_treatment <- c(mean(subset(baseline_treatment_accuracy,baseline_treatment_accuracy$diff > quantile(baseline_treatment_accuracy$diff,c(0.01),na.rm=TRUE) & baseline_treatment_accuracy$diff < quantile(baseline_treatment_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE),
      mean(subset(mid_treatment_accuracy,baseline_treatment_accuracy$diff > quantile(mid_treatment_accuracy$diff,c(0.01),na.rm=TRUE) & mid_treatment_accuracy$diff < quantile(mid_treatment_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE),
      mean(subset(ongoing_treatment_accuracy,ongoing_treatment_accuracy$diff > quantile(ongoing_treatment_accuracy$diff,c(0.01),na.rm=TRUE) & ongoing_treatment_accuracy$diff < quantile(ongoing_treatment_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE),
      mean(subset(end_treatment_accuracy,end_treatment_accuracy$diff > quantile(end_treatment_accuracy$diff,c(0.01),na.rm=TRUE) & end_treatment_accuracy$diff < quantile(end_treatment_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE))
      
      mean_list_types <- c("Baseline","Mid-Baseline","Ongoing","Endline")
      treatment_means <- as.data.frame(mean_list_treatment,mean_list_types)
      treatment_means$Group <- "Treatment"
      
      
      hist(subset(baseline_treatment_accuracy,baseline_treatment_accuracy$diff > quantile(baseline_treatment_accuracy$diff,c(0.01),na.rm=TRUE) & baseline_treatment_accuracy$diff < quantile(baseline_treatment_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE)
      hist(subset(mid_treatment_accuracy,baseline_treatment_accuracy$diff > quantile(mid_treatment_accuracy$diff,c(0.01),na.rm=TRUE) & mid_treatment_accuracy$diff < quantile(mid_treatment_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE)
      hist(subset(ongoing_treatment_accuracy,ongoing_treatment_accuracy$diff > quantile(ongoing_treatment_accuracy$diff,c(0.01),na.rm=TRUE) & ongoing_treatment_accuracy$diff < quantile(ongoing_treatment_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE)
      hist(subset(end_treatment_accuracy,end_treatment_accuracy$diff > quantile(end_treatment_accuracy$diff,c(0.01),na.rm=TRUE) & end_treatment_accuracy$diff < quantile(end_treatment_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE)
      
      # Control 
      baseline_control_accuracy <- subset(aggregate_control_data,aggregate_control_data$data == "Baseline" & aggregate_control_data$diff > quantile(aggregate_control_data$diff,c(0.01),na.rm=TRUE) & aggregate_control_data$diff < quantile(aggregate_control_data$diff,c(0.99),na.rm=TRUE))
      mid_control_accuracy <- subset(aggregate_control_data,aggregate_control_data$data == "Mid-Baseline" & aggregate_control_data$diff > quantile(aggregate_control_data$diff,c(0.01),na.rm=TRUE) & aggregate_control_data$diff < quantile(aggregate_control_data$diff,c(0.99),na.rm=TRUE))
      end_control_accuracy <- subset(aggregate_control_data,aggregate_control_data$data == "Control End" & aggregate_control_data$diff > quantile(aggregate_control_data$diff,c(0.01),na.rm=TRUE) & aggregate_control_data$diff < quantile(aggregate_control_data$diff,c(0.99),na.rm=TRUE))
      
      mean(subset(baseline_control_accuracy,baseline_control_accuracy$diff > quantile(baseline_control_accuracy$diff,c(0.01),na.rm=TRUE) & baseline_control_accuracy$diff < quantile(baseline_control_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE)
      mean(subset(mid_control_accuracy,mid_control_accuracy$diff > quantile(mid_control_accuracy$diff,c(0.01),na.rm=TRUE) & mid_control_accuracy$diff < quantile(mid_control_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE)
      mean(subset(end_control_accuracy,end_control_accuracy$diff > quantile(end_control_accuracy$diff,c(0.01),na.rm=TRUE) & end_control_accuracy$diff < quantile(end_control_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE)
      
      hist(subset(baseline_control_accuracy,baseline_control_accuracy$diff > quantile(baseline_control_accuracy$diff,c(0.01),na.rm=TRUE) & baseline_control_accuracy$diff < quantile(baseline_control_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE)
      hist(subset(mid_control_accuracy,mid_control_accuracy$diff > quantile(mid_control_accuracy$diff,c(0.01),na.rm=TRUE) & mid_control_accuracy$diff < quantile(mid_control_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE)
      hist(subset(end_control_accuracy,end_control_accuracy$diff > quantile(end_control_accuracy$diff,c(0.01),na.rm=TRUE) & end_control_accuracy$diff < quantile(end_control_accuracy$diff,c(0.99),na.rm=TRUE))$diff,na.rm=TRUE)
      
      
      # Treatment vs. Control Learning Plot
      
    
###################
###################
      # Get tariff accuracy
      
      
      
      # Get water accuracy
      
        #Treatment
        treatment_water <- survey.data.complete[,c("agua_gasto",("agua_gasto_verdad"))] %>% mutate(Group="Treatment")
        control_water <- final.control.survey[,c("agua_gasto",("agua_gasto_verdad"))] %>% mutate(Group="Control")
        water_accuracy <- rbind(treatment_water,control_water)
        water_accuracy$diff <- water_accuracy$agua_gasto_verdad - water_accuracy$agua_gasto
        
        mean(subset(water_accuracy,water_accuracy$Group=="Treatment")$diff,na.rm=TRUE)
        mean(subset(water_accuracy,water_accuracy$Group=="Control")$diff,na.rm=TRUE)
        
        ggplot(water_accuracy,aes(agua_gasto,agua_gasto_verdad,group=Group,colour=Group)) + geom_point(alpha=0.3) + geom_abline(slope=1, intercept=0) +
          theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
          theme(legend.position="bottom") + guides(fill=guide_legend(title="Timeline:")) + xlab("Perceived Water Expenditure") + ylab("Actual Water Expenditure") 
        


      # 2.2 Treatment group at baseline, after 1st report, after beggining of text messages
      
      
      # 2.4 How have perceptions of the usefulness of information from the utility been changing in time?
      #Need to divide this by interview periods, need to divide this by date
      #Plot 'survey.data$utility_info_score' by date and 'survey.data$project_info_score' by date
      #How have the scores of these two variables been changing month by month?

            
###############
###############    
############### 3. Comparison of survey response vs. actual data

#When is it that you consume the most energy?      
ggplot(data.frame(survey.data.responses), aes(x=mas_gasta_tiempo)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      

# Peak consumption
hora_pico_energia
hora_pico_energia_code
dia_pico_energia
dia_pico_energia_code1
dia_pico_energia_code2
ano_pico_energia
ano_pico_energia_code1

#Appliance consumption
consumo_abanico
consumo_luces
consumo_television
consumo_celular
consumo_radio
consumo_refrigerador
      

#What percentage do different appliances consume?
      #Understand what percentage the refrigerator consumes most energy?
      
      
#Does temperature affect your refrigerator?
      #Check with houses that experience reduced efficiency 
      
      
#How hard is it for you to pay your bills?
      #Check in time if the costs of paying have been coming down and how they pair with people's responses

    
#Has this project helped you better pay your bills?
      #Check if people who say yes are actually seeing reductions in month by month consumption
      #Check if they have actually reduced their consumption from a year ago
      
#How has the information in the project helped you?
      # For the people who said that they had 'greater_energy_understanding', 'increased_control','energy_reports'
      # did they increase the amount of knowledge about their electricity bill?
      # How did energy changes happen between the people who said the informatino is very useful and those who don't say that the informaion is useful?
      # Compare specifically the people who find it useful to keep control
      
#Do you use the information we give you to manage your consumption?
      # Compare energy and yes-no between groups
      # Compare between groups that actively manage their conusumption? Are there particular behaviors that were related to households and MEs that reduce consumption?
      # Are there differences in energy consumption with what type of information people found to be most useful?
      
#For people who find the energy useful, are they doing better than people who don't find it useful?
      # How useful are you finding the text messages?
      #Why do you find the messages very useful?
      #Why do you find the messages useful?
      

# Is there a difference in savings with the different kinds of information that people need?
      # What is the thing that has allowed you to increse the understanding of your consumption?
      
  
      
# What has been more important to you throughout this project, information or money?
      # Compare the answers for people who have said information, to how effective the information has been.
      
      
# Compare the choice of information vs. money for how people have been affected by information and the project in general?
      # If you had to choose between the information and the money, what woud you choose?


# Compare groups of people who want to implement energy efficiency measures with the effects on energy consumption?
      #Are you going to pursue the implementation of energy efficiency projects?
      

# Compare savings in energy vs. the willingness to pay
      
      
###############
###############    
############### 5. Scarcity
      
      
#How hard is it for you to pay your bills?
      

