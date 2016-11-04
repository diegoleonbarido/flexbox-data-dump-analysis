##### Marginal Value of Information

# 1. Timeseries Plots for Households 
  # 1.1 Treatment (plot, adjustments,etc)
  # 1.2 Control
  # 1.3 Comparison between treatment and control groups for cost and energy time series data
  # 1.4 Coefficient of variability

  # Propensity score matching, pairing groups, looking at trends month after month rather than month comparisons
  # Make sure that for the houses that received the text message, you include the months when the texting began 
  # Things to control for: number of people living inside the house, number of appliances, business modifictions

# 2. Learning
  # 2.1 Accuracy of energy expenditures and energy costs
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

# 4. Sensor data comparison 
  # 3.1 Z-wave vs. time series data from CNDC - how far off are the values? (changes over time)
  # 3.2 Z-wave vs refrigerator data (changes over time)
  # 3.3 Evaluation after first reports, after SMS, after implementation of energy efficiency strategies

# 4. Reports and Text Messages
  # 4.1 Existence and bersistence of behavioral change after paper reports (need dates from Odaly) vs. sms (server)

# 5. DR, grid data and carbon
  # 4.2 


############ Libraries

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


############ Define Functions

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

############ Define Groups

flexlist <- c('A1','A3','A6','A7','A9','A11','A12','A14','A16','A17','A18','A19','A20','A21','A22','A24','A25','A26','A28','A29')
control_group_list <- c("7","20","26","45","77","124","185","191","203","204","302","304","305","307","310","311")

############ Reading Data


survey.data.results <- read.survey.data(flexlist) # Survey Data 


flexlistid_encuesta_id <- subset(survey.data.results[[6]],survey.data.results[[6]]$control_estudio=='estudio') %>% select(encuesta_id,flexbox_id)
control_encuesta_id <- subset(survey.data.results[[6]],survey.data.results[[6]]$control_estudio=='control') %>% select(encuesta_id,flexbox_id)

 



############# Analysis

####### NOTE1: Need to add a date (and a distribution) for when some of the households lost their energy reports! ##############
####### NOTE2: Need to add the time series with adjusted energy values for treatment and control. Once you've added the adjusted time series 
######  NOTE3: I suggest adding a 1 or a 0 to code whether or not the house was in treatment or in control



energy.receipt.data <- survey.data.results[[1]]

time.series.receipt <- survey.data.results[[2]] %>% mutate(treatment="Treatment")
    time.series.receipt$Casa <- as.character(time.series.receipt$Casa)
    
    #Dates of Report intervention
    time.series.receipt$report_intervention_month <- ifelse(time.series.receipt$Mes == "Marzo" | time.series.receipt$Mes == "Abril" | time.series.receipt$Mes == "Mayo" | time.series.receipt$Mes == "Junio" | time.series.receipt$Mes == "Julio" | time.series.receipt$Mes == "Agosto" | time.series.receipt$Mes == "Septiembre" | time.series.receipt$Mes == "Octubre",1,0 )
    #Dates of SMS intervention
    time.series.receipt$intervention_group <- ifelse(time.series.receipt$fecha>="2016-06-01","Treatment Post-Intervention","Treatment Pre-Intervention")
    time.series.receipt$sms_intervention_month <- ifelse(time.series.receipt$Mes == "Junio" | time.series.receipt$Mes == "Julio" | time.series.receipt$Mes == "Agosto" | time.series.receipt$Mes == "Septiembre" | time.series.receipt$Mes == "Octubre",1,0 )
    #Bind
    time.series.receipt.dt <- as.data.table(time.series.receipt)
    
time.series.receipt.control <-  survey.data.results[[3]] %>% mutate(treatment="Control")
    time.series.receipt.control$Casa <- as.character(time.series.receipt.control$Casa)
    
    #Dates of Report intervention
    time.series.receipt.control$report_intervention_month <- ifelse(time.series.receipt.control$Mes == "Marzo" | time.series.receipt.control$Mes == "Abril" | time.series.receipt.control$Mes == "Mayo" | time.series.receipt.control$Mes == "Junio" | time.series.receipt.control$Mes == "Julio" | time.series.receipt.control$Mes == "Agosto" | time.series.receipt.control$Mes == "Septiembre" | time.series.receipt.control$Mes == "Octubre",1,0 )
    #Dates of SMS intervention
    time.series.receipt.control$intervention_group <- ifelse(time.series.receipt.control$fecha>="2016-06-01","Control Post-Intervention","Control Pre-Intervention")
    time.series.receipt.control$sms_intervention_month <- ifelse(time.series.receipt.control$Mes == "Junio" | time.series.receipt.control$Mes == "Julio" | time.series.receipt.control$Mes == "Agosto" | time.series.receipt.control$Mes == "Septiembre" | time.series.receipt.control$Mes == "Octubre",1,0 )
    time.series.receipt.control.dt <- as.data.table(time.series.receipt.control)

data_time_series <- rbind(time.series.receipt,time.series.receipt.control)




#######  1. Timeseries Plots for Treatment and Control 

# Timeseries Energy and Cost

    ggplot(time.series.receipt,aes(fecha,energia,group=Casa,colour=Casa)) + geom_path(alpha=0.5) 
    ggplot(time.series.receipt.control,aes(fecha,energia,group=Casa,colour=Casa)) + geom_path(alpha=0.5) 
    
    ggplot(time.series.receipt,aes(fecha,importe,group=Casa,colour=Casa)) + geom_path(alpha=0.5) 
    ggplot(time.series.receipt.control,aes(fecha,importe,group=Casa,colour=Casa)) + geom_path(alpha=0.5) 

# Densities Energy and Cost
  
      # All Data
      ggplot(data_time_series, aes(energia, fill = treatment)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density")
      ggplot(data_time_series, aes(importe, fill = treatment)) + geom_density(alpha = 0.2) + xlab("Cordobas") + ylab("Density")
    
      # Pre & Post Implementation *** Energy Data is missing for Control Group ***
      #*** There is something wrong with the plot immediately below - for now I have removed A24 because it's causing trouble add it in afterwards ***
      
      #All data for energy treatment and control
      ggplot(time.series.receipt, aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density")
      ggplot(time.series.receipt.control, aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density")
          #Months with and without PAPER ENERGY REPORTS
          ggplot(subset(time.series.receipt,time.series.receipt$report_intervention_month==1), aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density")
          ggplot(subset(time.series.receipt.control,time.series.receipt.control$report_intervention_month==1), aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density")
          #Months with and without SMS
          ggplot(subset(time.series.receipt,time.series.receipt$sms_intervention_month==1), aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density")
          ggplot(subset(time.series.receipt.control,time.series.receipt.control$sms_intervention_month==1), aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density")
          
  
      ggplot(time.series.receipt, aes(importe, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density")
      ggplot(time.series.receipt.control, aes(importe, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density")
          #Months with and without PAPER ENERGY REPORTS
          ggplot(subset(time.series.receipt,time.series.receipt$report_intervention_month==1), aes(importe, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Cordobas") + ylab("Density")
          ggplot(subset(time.series.receipt.control,time.series.receipt.control$report_intervention_month==1), aes(importe, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Cordobas") + ylab("Density")
          #Months with and without SMS
          ggplot(subset(time.series.receipt,time.series.receipt$sms_intervention_month==1), aes(importe, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Cordobas") + ylab("Density")
          ggplot(subset(time.series.receipt.control,time.series.receipt.control$sms_intervention_month==1), aes(importe, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Cordobas") + ylab("Density")
      
      
        # All densities for each unique house (you can plot energia or importe here)
            for(i in 1:length(unique(data_time_series$Casa))){
              subset_house_data <- subset(data_time_series,data_time_series$Casa == unique(data_time_series$Casa)[i])
                mean_pre_intervention = mean(subset(subset_house_data,subset_house_data$intervention_group == unique(subset_house_data$intervention_group)[1])$energia,na.rm=TRUE)
                mean_post_intervention = mean(subset(subset_house_data,subset_house_data$intervention_group == unique(subset_house_data$intervention_group)[2])$energia,na.rm=TRUE)
              
              density_plot <- ggplot(subset_house_data, aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density") + geom_vline(xintercept=mean_pre_intervention,colour="blue") + geom_vline(xintercept=mean_post_intervention,colour="red")  
        
              selected.house <- unique(data_time_series$Casa)[i] 
              group_type <- unique(subset_house_data$treatment)
              
              plot.name = paste(selected.house,"_",group_type,sep="")
              mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/densities",paste(plot.name,".jpg",sep=""))
              
              jpeg(file=mypath)
              print(density_plot)
              dev.off()
            }
    
        # Comparing the energy report intervention and the SMS intervention
        # NOTE CHANGE: report_intervention_month & sms_intervention_month
          
          for(i in 1:length(unique(data_time_series$Casa))){
            subset_house_data <- subset(data_time_series,data_time_series$Casa == unique(data_time_series$Casa)[i])
            subset_reports <- subset(subset_house_data,subset_house_data$sms_intervention_month ==1)
              mean_pre_intervention = mean(subset(subset_reports,subset_reports$intervention_group == unique(subset_reports$intervention_group)[1])$energia,na.rm=TRUE)
              mean_post_intervention = mean(subset(subset_reports,subset_reports$intervention_group == unique(subset_reports$intervention_group)[2])$energia,na.rm=TRUE)
            
            density_plot <- ggplot(subset_reports, aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density") + geom_vline(xintercept=mean_pre_intervention,colour="blue") + geom_vline(xintercept=mean_post_intervention,colour="red")  
            
            selected.house <- unique(data_time_series$Casa)[i] 
            group_type <- unique(subset_reports$treatment)
            
            plot.name = paste(selected.house,"_",group_type,sep="")
            mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/densities/sms_reports",paste(plot.name,".jpg",sep=""))
            
            jpeg(file=mypath)
            print(density_plot)
            dev.off()
          }
      
      
#  1.2. Month differences: Differences for each month one year afterwards (e.g June 2016 - June 2015)
      
    # NOTE: Plot this for the months in which they had information (June, July, August, September, October)
    month_diffs <- function(group_data,variable) {
      just_diffs <- group_data[, month_diff := diff(get(variable)), by = list(Casa, Mes)]
      complete_diffs <- just_diffs[complete.cases(just_diffs),]
      month_difference <- complete_diffs[Ano==2016] %>% select(Casa,month_diff,treatment)
      return(month_difference)
    }

  # Energy - **** need to calculate energy data for the treatment group ****
     month_difference <- month_diffs(time.series.receipt.dt,'energia') # Treatment
     month_difference_control <- month_diffs(time.series.receipt.control.dt,'energia') # Control
     distribution_differences <- rbind(month_difference,month_difference_control)
     
    ggplot(distribution_differences, aes(month_diff, fill = treatment)) + geom_density(alpha = 0.2) + xlab("Energy (kWh)") + ylab("Density")

  # Cost
    month_difference <- month_diffs(time.series.receipt.dt,'importe') # Treatment
    month_difference_control <- month_diffs(time.series.receipt.control.dt,'importe') # Control
    distribution_differences_money <- rbind(month_difference,month_difference_control)
    
    ggplot(distribution_differences_money, aes(month_diff, fill = treatment)) + geom_density(alpha = 0.2) + xlab("Cordobas") + ylab("Density")
    
    
    
    
#  1.3. Month by month reductions: (e.g Feb2016 - Jan2016 )
    
    month_by_month <- function(group_data,variable){
      group_data[, lag_variable:=c(NA, get(variable)[-.N]), by=Casa]
      group_data$diff_variable <- group_data[,get(variable)] - group_data[,lag_variable]
      return(group_data)
    }
    

  # Energy
    mbm <- month_by_month(time.series.receipt.dt,'energia')
    mbm_control <- month_by_month(time.series.receipt.control.dt,'energia')
    mbm_bind <- rbind(mbm,mbm_control)
    ggplot(mbm_bind, aes(diff_variable, fill = treatment)) + geom_density(alpha = 0.2) + xlab("Energy (kWh)") + ylab("Density")

  # Cordobas
    mbm <- month_by_month(time.series.receipt.dt,'importe')
    mbm_control <- month_by_month(time.series.receipt.control.dt,'importe')
    mbm_bind <- rbind(mbm,mbm_control)
    ggplot(mbm_bind, aes(diff_variable, fill = treatment)) + geom_density(alpha = 0.2) + xlab("Cordobas") + ylab("Density")
    
          # All Houses Month by Month
          for(i in 1:length(unique(mbm_bind$Casa))){
            subset_house_data <- subset(mbm_bind,mbm_bind$Casa == unique(mbm_bind$Casa)[i])
              mean_pre_intervention = mean(subset(subset_house_data,subset_house_data$intervention_group == unique(subset_house_data$intervention_group)[1])$energia,na.rm=TRUE)
              mean_post_intervention = mean(subset(subset_house_data,subset_house_data$intervention_group == unique(subset_house_data$intervention_group)[2])$energia,na.rm=TRUE)
            
            density_plot <- ggplot(subset_house_data, aes(diff_variable, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density") + geom_vline(xintercept=mean_pre_intervention,colour="blue") + geom_vline(xintercept=mean_post_intervention,colour="red")
            
            selected.house <- unique(mbm_bind$Casa)[i] 
            group_type <- unique(subset_house_data$treatment)
            
            plot.name = paste(selected.house,"_",group_type,sep="")
            mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/month_by_month",paste(plot.name,".jpg",sep=""))
            
            jpeg(file=mypath)
            print(density_plot)
            dev.off()
          }
    
          # Houses Month by Month only comparing the months with energy reports
          for(i in 1:length(unique(data_time_series$Casa))){
            subset_house_data <- subset(mbm_bind,mbm_bind$Casa == unique(mbm_bind$Casa)[i])
              subset_reports <- subset(subset_house_data,subset_house_data$sms_intervention_month ==1)
                mean_pre_intervention = mean(subset(subset_reports,subset_reports$intervention_group == unique(subset_reports$intervention_group)[1])$energia,na.rm=TRUE)
                mean_post_intervention = mean(subset(subset_reports,subset_reports$intervention_group == unique(subset_reports$intervention_group)[2])$energia,na.rm=TRUE)
            
              density_plot <- ggplot(subset_reports, aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density") + geom_vline(xintercept=mean_pre_intervention,colour="blue") + geom_vline(xintercept=mean_post_intervention,colour="red")  
            
              selected.house <- unique(data_time_series$Casa)[i] 
              group_type <- unique(subset_reports$treatment)
              
              plot.name = paste(selected.house,"_",group_type,sep="")
              mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/month_by_month/month_by_month_sms_intervention",paste(plot.name,".jpg",sep=""))
              
              jpeg(file=mypath)
              print(density_plot)
              dev.off()
          }
    
    

##########
##########   
########## 2 Learning
    
    survey.data.complete <- survey.data.results[[4]] #All answers in the survey data 
        survey.data.complete$gasto_electrico <- as.numeric(as.character(survey.data.complete$gasto_electrico))
        survey.data.complete$gasto_electrico_cordobas <- as.numeric(as.character(survey.data.complete$gasto_electrico_cordobas))
        survey.data.complete$gasto_tarifa_electrica <- as.numeric(as.character(survey.data.complete$gasto_tarifa_electrica))
    
    baseline_receipt_data_for_merge <- survey.data.results[[5]] #Baseline data
    implementation.baseline <- survey.data.results[[6]] #Baseline and implementation survey data
    
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
    
     #1. Plotting the baseline, control, and treatment groups against each other
     #   Need to get kwh estimates for the other groups from the mid baseline and 
     #   Need to plot how far off people are in terms of kwh
      survey.data.plot <- survey.data.complete %>% mutate(data='treatment') %>% select(encuesta_id,r_total_cordobas,gasto_electrico_cordobas,data)
      baseline_receipt_data_for_plot <- baseline_receipt_data_for_merge %>% mutate(data='baseline_pulperias',r_total_cordobas=baseline_monthly_cordobas,gasto_electrico_cordobas=baseline_gasto_electrico)  %>% select(encuesta_id,r_total_cordobas,gasto_electrico_cordobas,data)
      
      # Plot the baseline for the treatment group once we have the data back from Odaly
      baseline_households <- subset(implementation.baseline,implementation.baseline$control_estudio=='estudio' & baseline$tipo_establecimiento=='casa') %>% mutate(data='baseline_casas',r_total_cordobas=,gasto_electrico_cordobas=gasto_electrico)
      
      # Need to bind gh data from the treatment group here
      baseline_current_plot <- rbind(baseline_receipt_data_for_plot,survey.data.plot)
      
      ggplot(baseline_current_plot, aes(gasto_electrico_cordobas, r_total_cordobas,group=data,colour=data)) + geom_point(alpha=0.5) + geom_abline(slope=1, intercept=0)
      ggplot(subset(baseline_current_plot,baseline_current_plot$gasto_electrico_cordobas<30000), aes(gasto_electrico_cordobas, r_total_cordobas,group=data,colour=data)) + geom_point(alpha=0.5) + geom_abline(slope=1, intercept=0)
      ggplot(baseline_current_plot, aes(gasto_electrico, r_monthly_kwh,group=data,colour=data)) + geom_point(alpha=0.5) + geom_abline(slope=1, intercept=0)
      

            
      #2. Treatment and control group
      
      
      control_group_data <- baseline_current_plot[match(as.character(baseline_current_plot$encuesta_id),control_group_list),]
      
          
      #3. Treatment group at baseline, after 1st report, after beggining of text messages
      
      
    





