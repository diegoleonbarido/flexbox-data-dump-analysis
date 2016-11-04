

setwd('/Users/Diego/Desktop/Nicaragua/Surveys/Survey Monthly Updates/Results')

survey.data <- read.csv('all_monthly_updates_odaly_for analysis.csv')

#######   Initial Baseline Survey Data   #######
initial.baseline.survey <- read.csv('/Users/Diego/Desktop/Nicaragua/Surveys/Survey I Winter 2014/Results/All_Surveys_With_Pics_Reviewed_Final.csv')
initial.baseline.data <-subset(initial.baseline.survey,initial.baseline.survey$error == 0)
baseline_receipt_data <- subset(initial.baseline.data, initial.baseline.data$foto_gasto_electrico == 'si') %>% mutate(encuesta_id=survey_id,baseline_gasto_electrico=gasto_electrico,baseline_monthly_cordobas=r_monthly_cordobas) %>% select(encuesta_id,baseline_gasto_electrico,baseline_monthly_cordobas,gasto_electrico,r_monthly_cordobas)

baseline_receipt_data_for_merge <- baseline_receipt_data %>% select(encuesta_id,baseline_gasto_electrico,baseline_monthly_cordobas)
baseline_receipt_data_for_plot <- baseline_receipt_data %>% mutate(data='baseline') %>% select(encuesta_id,gasto_electrico,r_monthly_cordobas,data)

###############################################


survey.data <- merge(survey.data,baseline_receipt_data_for_merge,by="encuesta_id",all=T)

survey.data$gasto_electrico <- as.numeric(as.character(survey.data$gasto_electrico))
survey.data$gastop_electrico_cordobas <- as.numeric(as.character(survey.data$gastop_electrico_cordobas))

survey.data$today <- as.character(survey.data$today)
survey.data$date <- strptime(survey.data$today,"%m/%d/%y")

#Energy
survey.data$kwh_estimate_diff <- (abs(survey.data$r_monthly_kwh - survey.data$gasto_electrico))/survey.data$r_monthly_kwh

#Cost
survey.data$cordobas_estimate_diff <- ((abs(survey.data$r_monthly_cordobas - survey.data$gastop_electrico_cordobas))/survey.data$r_monthly_cordobas)*100
survey.data$total_cordobas_estimate_diff <- ((abs(survey.data$r_total_cordobas - survey.data$gastop_electrico_cordobas))/survey.data$r_total_cordobas)*100


#"gasto_electrico"                                
#"gastop_electrico_cordobas"                     
# "gasto_tarifa_electrica"

plot(survey.data$r_monthly_kwh,survey.data$gasto_electrico)

plot(survey.data$r_monthly_cordobas,survey.data$gastop_electrico_cordobas)
plot(survey.data$r_total_cordobas,survey.data$gastop_electrico_cordobas)

survey.data$date_number <- as.numeric(as.POSIXct(survey.data$date))


# Expected Energy vs. Actual Energy Consumption
ggplot(survey.data, aes(gasto_electrico, r_monthly_kwh)) + geom_point(aes(size = kwh_estimate_diff,colour=date_number)) + scale_colour_gradient(low = "blue") + geom_abline(slope=1, intercept=0)
# Expected Energy vs. Actual Energy Consumption
ggplot(survey.data, aes(gastop_electrico_cordobas, r_monthly_cordobas)) + geom_point(aes(size = cordobas_estimate_diff,colour=date_number)) + scale_colour_gradient(low = "blue") + geom_abline(slope=1, intercept=0)
ggplot(survey.data, aes(gastop_electrico_cordobas, r_total_cordobas)) + geom_point(aes(size = total_cordobas_estimate_diff,colour=date_number)) + scale_colour_gradient(low = "blue") + geom_abline(slope=1, intercept=0)


plot(survey.data$date,survey.data$kwh_estimate_diff)
plot(survey.data$date,survey.data$cordobas_estimate_diff)
plot(survey.data$date,survey.data$total_cordobas_estimate_diffs)

for(i in 1:length(unique(survey.data$flexbox_id))){
  subset_house_data <- subset(survey.data,survey.data$flexbox_id == unique(survey.data$flexbox_id)[i])

  plot_diff <- ggplot(subset_house_data,aes(date,total_cordobas_estimate_diff)) + geom_point()
  selected.house <- unique(survey.data$flexbox_id)[i] 

  plot.name = paste(selected.house,sep="")
  mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/learning/total_cost",paste(plot.name,".jpg",sep=""))
  
  jpeg(file=mypath)
  print(plot_diff)
  dev.off()
}


############ Plotting all samples vs. treatment group

#Renamed variables 
survey.data.plot <- survey.data %>% mutate(data='treatment',r_monthly_cordobas=r_total_cordobas,gasto_electrico=gastop_electrico_cordobas) %>% select(encuesta_id,gasto_electrico,r_monthly_cordobas,data)

plot(survey.data.plot$gasto_electrico,survey.data.plot$r_monthly_cordobas)

baseline_current_plot <- rbind(baseline_receipt_data_for_plot,survey.data.plot)


ggplot(baseline_current_plot, aes(gasto_electrico, r_monthly_cordobas,group=data,colour=data)) + geom_point(alpha=0.5) + geom_abline(slope=1, intercept=0)







