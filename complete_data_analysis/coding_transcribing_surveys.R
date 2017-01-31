

setwd('/Users/Diego/Desktop/Nicaragua/Surveys/Survey Monthly Updates/Results')
survey.data <- read.csv('all_monthly_updates_odaly_for analysis.csv')

#### 1. Cleaning Variables
#### 2. Initial Transcribing Survyes
#### 3. Coding values into Surveys


####### 1. Cleaning Variables

survey.data$participacion_contento <- as.character(survey.data$participacion_contento)
survey.data$molesta_instrumentacion <- as.character(survey.data$molesta_instrumentacion)


####### 2. Initial Transcribing Survyes

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



#### 3. Coding values into Surveys


#Are you happy or unsastisfied with the project?
survey.data$project_satisfaction <- ifelse(survey.data$participacion_contento == 'contento',2,ifelse(survey.data$participacion_contento == 'no_bien_no_mal',1,0))

#What has been positive about the project?
ggplot(data.frame(survey.data), aes(x=aspecto_positivo_razon1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data.frame(survey.data), aes(x=aspecto_positivo_razon2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data.frame(survey.data), aes(x=aspecto_positivo_razon3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#What has been negative about the project?
ggplot(data.frame(survey.data), aes(x=aspecto_negativo_razon)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#What would you change about the project?
ggplot(data.frame(survey.data), aes(x=cambio_estudio_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#What would you improve about the project?
ggplot(data.frame(survey.data), aes(x=mejorar_estudio_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Does the hardware and flexbox bother you?
survey.data$flexbox_bother <- ifelse(survey.data$molesta_instrumentacion == 'si',1,0)
hist(survey.data$flexbox_bother)

#Why does the flexbox bother you?
ggplot(data.frame(survey.data), aes(x=porque_molesta_instrumentacion_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#When is it that you consume the most energy?

      # REASON1 - Why do you consume more energy during the:
      morning <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="manana"), aes(x=porque_mas_gasta_tiempo_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      half_day <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="medio_dia"), aes(x=porque_mas_gasta_tiempo_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      evening <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="tarde"), aes(x=porque_mas_gasta_tiempo_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      night <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="noche"), aes(x=porque_mas_gasta_tiempo_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      # REASON2 - Why do you consume more energy during the:
      morning2 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="manana"), aes(x=porque_mas_gasta_tiempo_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      half_day2 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="medio_dia"), aes(x=porque_mas_gasta_tiempo_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      evening2 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="tarde"), aes(x=porque_mas_gasta_tiempo_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      night2 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="noche"), aes(x=porque_mas_gasta_tiempo_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      # REASON3 - Why do you consume more energy during the:
      morning3 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="manana"), aes(x=porque_mas_gasta_tiempo_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      half_day3 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="medio_dia"), aes(x=porque_mas_gasta_tiempo_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      evening3 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="tarde"), aes(x=porque_mas_gasta_tiempo_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      night3 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="noche"), aes(x=porque_mas_gasta_tiempo_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      # REASON4 - Why do you consume more energy during the:
      morning4 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="manana"), aes(x=porque_mas_gasta_tiempo_code4)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      half_day4 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="medio_dia"), aes(x=porque_mas_gasta_tiempo_code4)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      evening4 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="tarde"), aes(x=porque_mas_gasta_tiempo_code4)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      night4 <- ggplot(subset(survey.data,survey.data$mas_gasta_tiempo=="noche"), aes(x=porque_mas_gasta_tiempo_code4)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      

# For how long have you had this frige (months)?
ggplot(data.frame(survey.data), aes(x=cuantos_anos_meses_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
#Was your fridge new or old?
unique(survey.data$nuevo_usado)
ggplot(data.frame(survey.data), aes(x=nuevo_usado)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Where did you buy it from?
ggplot(data.frame(survey.data), aes(x=donde_compro_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

      #Of the people who said new, how many bought it in used places?
      ggplot(subset(survey.data, survey.data$nuevo_usado=='nuevo'), aes(x=donde_compro_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      ggplot(subset(survey.data, survey.data$nuevo_usado=='usado'), aes(x=donde_compro_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#When would you like to buy a new fridge?
ggplot(data.frame(survey.data), aes(x=cuantos_anos_cambiar_months_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
#Would you want to buy a new fridge?          
ggplot(data.frame(survey.data), aes(x=refrigerador_nuevo)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#What's limiting you from buying a new fridge?
ggplot(data.frame(survey.data), aes(x=limitante_nuevo_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Do you turn off your refigerator at different times?
ggplot(data.frame(survey.data), aes(x=apaga_refrigerador)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

    #Why do you turn the refrigerator off?
    ggplot(data.frame(survey.data), aes(x=decision_apaga_refrigerador_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=decision_apaga_refrigerador_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #Do you experience any savings by turning your refrigerator off?
    ggplot(data.frame(survey.data), aes(x=observar_ahorros)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #How do you know you've experienced savings?
    ggplot(data.frame(survey.data), aes(x=medicion_ahorros_si_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #Why haven't you experience savings??
    ggplot(data.frame(survey.data), aes(x=medicion_ahorros_no_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
#Do you know what your fridge's condenser is?
ggplot(data.frame(survey.data), aes(x=sabe_condensador)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    survey.data$condenser_never<- ifelse(survey.data$frecuencia_condensador.nunca == 'TRUE',1,0)
    survey.data$condenser_once_year <- ifelse(survey.data$frecuencia_condensador.una_vez_ano == 'TRUE',1,0)
    survey.data$condenser_once_month <- ifelse(survey.data$frecuencia_condensador.una_vez_mes == 'TRUE',1,0)
    survey.data$condenser_once_day <- ifelse(survey.data$frecuencia_condensador.una_vez_dia == 'TRUE',1,0)
    
    
#Do you know what your fridge's thermostat is?
ggplot(data.frame(survey.data), aes(x=sabe_termostato)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #What type of control does your fridge have?
    ggplot(data.frame(survey.data), aes(x=tipo_de_control)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #Have you ever changed your refrigerator's controls so that they operate differently?
    ggplot(data.frame(survey.data), aes(x=cambiado_control)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #Why have you changed the thermostat controls?
    ggplot(data.frame(survey.data), aes(x=porque_cambio_termostato_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    

#Do you know what your fridge's compressor is?
    ggplot(data.frame(survey.data), aes(x=sabe_compresor)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #How often do you check your fridge's compressor?
    survey.data$compressor_never<- ifelse(survey.data$frecuencia_mantenimiento.nunca == 'TRUE',1,0)
    survey.data$compressor_once_year <- ifelse(survey.data$frecuencia_mantenimiento.una_vez_ano == 'TRUE',1,0)
    survey.data$compressor_once_month <- ifelse(survey.data$frecuencia_mantenimiento.una_vez_mes == 'TRUE',1,0)
    survey.data$compressor_once_day <- ifelse(survey.data$frecuencia_mantenimiento.una_vez_dia == 'TRUE',1,0)
    
    ggplot(data.frame(survey.data), aes(x=compressor_once_day)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
#What percentage do different appliances consume?
    ggplot(data.frame(survey.data), aes(x=consumo_abanico)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=consumo_luces)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=consumo_television)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=consumo_celular)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=consumo_radio)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=consumo_refrigerador)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #Importance of different appliances
    ggplot(data.frame(survey.data), aes(x=importancia_abanico)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=importancia_luces)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=importancia_television)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=importancia_celular)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=importancia_radio)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=importancia_refrigerador)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

    
    #Why are different appliances essential?
    ggplot(data.frame(survey.data), aes(x=abanico_esencial_por1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=abanico_esencial_por2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=abanico_muyutil_por_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=luces_esencial_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=luces_muyutil_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=television_esencial_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=television_muyutil_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=television_muyutil_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=celular_esencial_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=celular_muyutil_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=radio_esencial_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=radio_muyutil_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=refrigerador_esencial_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=refrigerador_muyutil_por)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #What is the most important appliance?
    survey.data$abanico_score <- ifelse(survey.data$importancia_abanico=="poco",1,ifelse(survey.data$importancia_abanico=='no_muy',2,ifelse(survey.data$importancia_abanico=='mas_o_menos',3,ifelse(survey.data$importancia_abanico=='muy_util',4,ifelse(survey.data$importancia_abanico=='esencial',5,0)))))
    survey.data$luces_score <- ifelse(survey.data$importancia_luces=="poco",1,ifelse(survey.data$importancia_luces=='no_muy',2,ifelse(survey.data$importancia_luces=='mas_o_menos',3,ifelse(survey.data$importancia_luces=='muy_util',4,ifelse(survey.data$importancia_luces=='esencial',5,0)))))
    survey.data$television_score <- ifelse(survey.data$importancia_television=="poco",1,ifelse(survey.data$importancia_television=='no_muy',2,ifelse(survey.data$importancia_television=='mas_o_menos',3,ifelse(survey.data$importancia_television=='muy_util',4,ifelse(survey.data$importancia_television=='esencial',5,0)))))
    survey.data$celular_score <- ifelse(survey.data$importancia_celular=="poco",1,ifelse(survey.data$importancia_celular=='no_muy',2,ifelse(survey.data$importancia_celular=='mas_o_menos',3,ifelse(survey.data$importancia_celular=='muy_util',4,ifelse(survey.data$importancia_celular=='esencial',5,0)))))
    survey.data$radio_score <- ifelse(survey.data$importancia_radio=="poco",1,ifelse(survey.data$importancia_radio=='no_muy',2,ifelse(survey.data$importancia_radio=='mas_o_menos',3,ifelse(survey.data$importancia_radio=='muy_util',4,ifelse(survey.data$importancia_radio=='esencial',5,0)))))
    survey.data$refrigerador_score <- ifelse(survey.data$importancia_refrigerador=="poco",1,ifelse(survey.data$importancia_refrigerador=='no_muy',2,ifelse(survey.data$importancia_refrigerador=='mas_o_menos',3,ifelse(survey.data$importancia_refrigerador=='muy_util',4,ifelse(survey.data$importancia_refrigerador=='esencial',5,0)))))
    

#Does temperature affect your refrigerator?
ggplot(data.frame(survey.data), aes(x=temperatura_afecta)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #How does temperature affect your fridge?
    ggplot(data.frame(survey.data), aes(x=como_temperatura_afecta_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    
#How hard is it for you to pay your bills?
ggplot(data.frame(survey.data), aes(x=dificultad_pago)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data.frame(survey.data), aes(x=muy_dificil_porque_pago)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data.frame(survey.data), aes(x=especifique_porque_dificil_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data.frame(survey.data), aes(x=especifique_porque_dificil_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data.frame(survey.data), aes(x=especifique_porque_dificil_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data.frame(survey.data), aes(x=especifique_porque_mdificil_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data.frame(survey.data), aes(x=especifique_porque_mdificil_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data.frame(survey.data), aes(x=especifique_porque_mdificil_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Has this project helped you better pay your bills?
ggplot(data.frame(survey.data), aes(x=poryecto_ayuda_pagos)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

    #How has it helped you pay your bills?
    ggplot(data.frame(survey.data), aes(x=porque_poryecto_ayuda_pagossi_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=porque_poryecto_ayuda_pagossi_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
	  #Why hasn't it helped you pay your bills?
    ggplot(data.frame(survey.data), aes(x=porque_poryecto_ayuda_pagosno_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=porque_poryecto_ayuda_pagosno)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    #What do you do with the money that is given to you from the project?
    ggplot(data.frame(survey.data), aes(x=para_que_usa_pagos_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=para_que_usa_pagos_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    	
#Has the information in the project helped you in any way?
ggplot(data.frame(survey.data), aes(x=poryecto_ayuda_informacion)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #How has the information in the project helped you?
    ggplot(data.frame(survey.data), aes(x=porque_poryecto_informacion_pagossi_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=porque_poryecto_informacion_pagossi_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=porque_poryecto_informacion_pagossi_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #Why hasn't the information helped you?
    ggplot(data.frame(survey.data), aes(x=porque_poryecto_ayuda_informacionno)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    
#Do you keep track of your energy consumption within the household?
ggplot(data.frame(survey.data), aes(x=control_de_consumo)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    

    #What do you do to control your energy?
    ggplot(data.frame(survey.data), aes(x=como_contro_de_consumo_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=como_contro_de_consumo_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #How many days a week do you keep the control?
    ggplot(data.frame(survey.data), aes(x=cuantos_dias_control)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #How long does it take you every day to keep track of your consumption?
    ggplot(data.frame(survey.data), aes(x=cuanto_tiempo_dia_control)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #Why don't you keep a control?
    ggplot(data.frame(survey.data), aes(x=control_de_consumo_no_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
# In general, how useful is the information that comes from the utility?
ggplot(data.frame(survey.data), aes(x=informacion_util)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #What is the score that you would give to information
    survey.data$utility_info_score <- ifelse(survey.data$informacion_util=='no_me_sirve',1,ifelse(survey.data$informacion_util=='mas_o_menos',2,ifelse(survey.data$informacion_util=='util',3,ifelse(survey.data$informacion_util=='muy_util',4,0))))
    
    #Whys is information not useful?
    ggplot(data.frame(survey.data), aes(x=para_que_util_nada_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=para_que_util_nada_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #Why is information more or less useful?
    ggplot(data.frame(survey.data), aes(x=para_que_util_masmenos_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=para_que_util_masmenos_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #Why is the information useful?
    ggplot(data.frame(survey.data), aes(x=para_que_util_uil1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=para_que_util_uil2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #Why is the information very useful?
    ggplot(data.frame(survey.data), aes(x=para_que_util_muyutil_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=para_que_util_muyutil_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
 
# In general, how useful do you find the project's information to be?
ggplot(data.frame(survey.data), aes(x=informacion_util_proyecto)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #What is the score that you would give to the project's information
    survey.data$project_info_score <- ifelse(survey.data$informacion_util_proyecto=='no_me_sirve',1,ifelse(survey.data$informacion_util_proyecto=='mas_o_menos',2,ifelse(survey.data$informacion_util_proyecto=='util',3,ifelse(survey.data$informacion_util_proyecto=='muy_util',4,0))))

    #Why has the information not been useful?
    ggplot(data.frame(survey.data), aes(x=para_que_util_nada_proyecto_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #Why has the information been more or less useful?
    ggplot(data.frame(survey.data), aes(x=para_que_util_masmenos_proyecto_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=para_que_util_masmenos_proyecto_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #Why is the information useful:    
    ggplot(data.frame(survey.data), aes(x=para_que_util_uil_proyecto_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=para_que_util_uil_proyecto_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=para_que_util_uil_proyecto_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #Why is the information very useful:    
    ggplot(data.frame(survey.data), aes(x=para_que_util_muyutil_proyecto_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=para_que_util_muyutil_proyecto_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=para_que_util_muyutil_proyecto_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
# Do you use the information we give you to manage your energy consumption?  	
ggplot(data.frame(survey.data), aes(x=administra_consumo_info)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #What do you do to manage your consumption?
    ggplot(data.frame(survey.data), aes(x=administra_consumo_info_si_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=administra_consumo_info_si_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=administra_consumo_info_si_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

    #Why don't you use the information to actively manage your consumption?
    ggplot(data.frame(survey.data), aes(x=administra_consumo_info_no_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #What other kinds of information would you like to receieve?
    ggplot(data.frame(survey.data), aes(x=tipo_informacion_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=tipo_informacion_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=tipo_informacion_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplot(data.frame(survey.data), aes(x=tipo_informacion_code4)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

    #What information is better?
    ggplot(data.frame(survey.data), aes(x=cual_info_mejor)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
		    #Why is disnorte informatino better?
        ggplot(data.frame(survey.data), aes(x=mejor_disnorte_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        #Why is flexbox information better?
        ggplot(data.frame(survey.data), aes(x=mejor_flexbox_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mejor_flexbox_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mejor_flexbox_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        #Why is the information provided the same?
        ggplot(data.frame(survey.data), aes(x=informacion_iguales_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=informacion_iguales_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=informacion_iguales_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        #Why are they bad?
        ggplot(data.frame(survey.data), aes(x=informacion_malos_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        #What could be done to improve the quality of the information presented?
        ggplot(data.frame(survey.data), aes(x=informacion_malos_sugerencia_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
  
#What is more useful to you, the energy reports or the text messages?
ggplot(data.frame(survey.data), aes(x=mensajes_paperreport)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        #Why are the texts more useful?
        ggplot(data.frame(survey.data), aes(x=mensajes_mas_text_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mensajes_mas_text_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mensajes_mas_text_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        #Why are paper reports more useful?
        ggplot(data.frame(survey.data), aes(x=papel_mas_text_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=papel_mas_text_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=papel_mas_text_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
# How useful are you finding the text messages?
ggplot(data.frame(survey.data), aes(x=mensaje_util)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        #Why are the texts not useful?
        ggplot(data.frame(survey.data), aes(x=mensaje_util_nosirve_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        #Why are the texts more or less useful?
        ggplot(data.frame(survey.data), aes(x=mensaje_util_masmenos_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        #Why do you find the messages useful?
        ggplot(data.frame(survey.data), aes(x=mensaje_util_util_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mensaje_util_util_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        #Why do you find the messages very useful?
        ggplot(data.frame(survey.data), aes(x=mensaje_util_muyutil_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mensaje_util_muyutil_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mensaje_util_muyutil_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        #How could we improve the usefulness of the texts?
        ggplot(data.frame(survey.data), aes(x=mejorar_mensaje_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
# What is the thing that has allowed you to increse the understanding of your consumption?
ggplot(data.frame(survey.data), aes(x=utilidad_modo_informacion_general)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        # Why only receipts?
        ggplot(data.frame(survey.data), aes(x=mejor_solo_recibos_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mejor_solo_recibos_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mejor_solo_recibos_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why receipts and texts?
        ggplot(data.frame(survey.data), aes(x=mejor_recibos_textos_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mejor_recibos_textos_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why only texts?
        ggplot(data.frame(survey.data), aes(x=mejor_solo_textos_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
        
# What information allows you to better manage your day-to-day consumption? What information is in your day-to-day?
ggplot(data.frame(survey.data), aes(x=utilidad_modo_informacion_diadia)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
   
        # Why only receipts on the day to day management?
        ggplot(data.frame(survey.data), aes(x=mejor_solo_recibos_diadia_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mejor_solo_recibos_diadia_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why receipts and texts?
        ggplot(data.frame(survey.data), aes(x=mejor_recibos_textos_diadia_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mejor_recibos_textos_diadia_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mejor_recibos_textos_diadia_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why only texts?
        ggplot(data.frame(survey.data), aes(x=mejor_solo_textos_diadia_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mejor_solo_textos_diadia_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=mejor_solo_textos_diadia_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
# What source of information would you choose if you only could keep one?
ggplot(data.frame(survey.data), aes(x=elija_un_reporte)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        

        # Why would you chose the texts?
        ggplot(data.frame(survey.data), aes(x=porque_textos_si_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=porque_textos_si_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why would you chose our reports?
        ggplot(data.frame(survey.data), aes(x=porque_nuestro_si_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=porque_nuestro_si_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why would you chose the utility's reports?
        ggplot(data.frame(survey.data), aes(x=porque_DISNORTE_si_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=porque_DISNORTE_si_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        	
# What has been more important to you throughout this project, information or money?
ggplot(data.frame(survey.data), aes(x=importancia_tipo_ayuda)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        # Why is information the most important thing?
        ggplot(data.frame(survey.data), aes(x=importante_informacion_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=importante_informacion_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why are both equally important?
        ggplot(data.frame(survey.data), aes(x=importante_ambos_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=importante_ambos_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=importante_ambos_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
# Would you participate in the study if we were only providing information?  		
ggplot(data.frame(survey.data), aes(x=solo_informacion_ayuda)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        #Why would you be willing to continue only with information?
        ggplot(data.frame(survey.data), aes(x=solo_informacion_ayuda_porque_si_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=solo_informacion_ayuda_porque_si_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=solo_informacion_ayuda_porque_si_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        #Why would you not like to continue on the project only with information?
        ggplot(data.frame(survey.data), aes(x=solo_informacion_ayuda_porque_no_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=solo_informacion_ayuda_porque_no_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
# Would you participate in the study, if it provided money but no information?
ggplot(data.frame(survey.data), aes(x=solo_dinero_ayuda)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        # Why would you continue even without the information?
        ggplot(data.frame(survey.data), aes(x=solo_dinero_ayuda_porque_si_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=solo_dinero_ayuda_porque_si_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why would you not like to participate?
        ggplot(data.frame(survey.data), aes(x=solo_dinero_ayuda_porque_no_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=solo_dinero_ayuda_porque_no_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=solo_dinero_ayuda_porque_no_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
# If you had to choose between the information and the money, what woud you choose?
ggplot(data.frame(survey.data), aes(x=dinero_o_informacion)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
       
# Did you read the energy efficiency recommendations?
ggplot(data.frame(survey.data), aes(x=leyo_recomendaciones)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

          # Why did you read them?
          ggplot(data.frame(survey.data), aes(x=porque_si_leyo_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          ggplot(data.frame(survey.data), aes(x=porque_si_leyo_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          # Why did you not read them?
          ggplot(data.frame(survey.data), aes(x=porque_no_leyo_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          ggplot(data.frame(survey.data), aes(x=porque_no_leyo_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          #What are the things that interested you most about energy efficiency?
          ggplot(data.frame(survey.data), aes(x=atencion_eficiencia_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          ggplot(data.frame(survey.data), aes(x=atencion_eficiencia_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          #Are you going to pursue the implementation of energy efficiency projects?
          ggplot(data.frame(survey.data), aes(x=llevaracabo_atencion_eficiencia)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          #What efficiency projects are you going to pursue? What else would you need to make efficiency happen?
          ggplot(data.frame(survey.data), aes(x=llevaracabo_atencion_eficiencia_textsi_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          ggplot(data.frame(survey.data), aes(x=llevaracabo_atencion_eficiencia_textsi_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          ggplot(data.frame(survey.data), aes(x=llevaracabo_atencion_eficiencia_textsi_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          #why are you not going to pursue them?
          ggplot(data.frame(survey.data), aes(x=llevaracabo_atencion_eficiencia_textno_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          #Have you implemented any of the energy efficiency suggestions yet?
          ggplot(data.frame(survey.data), aes(x=recomendaciones_eficiencia_si)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          #What have you implemented?
          ggplot(data.frame(survey.data), aes(x=implementacion_eficiencia_energetica_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          ggplot(data.frame(survey.data), aes(x=implementacion_eficiencia_energetica_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          ggplot(data.frame(survey.data), aes(x=implementacion_eficiencia_energetica_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          #Are you looking forward to implementing more solutions?
          ggplot(data.frame(survey.data), aes(x=piensa_implementar)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          #What do you think of implementing in the future?
          ggplot(data.frame(survey.data), aes(x=piensa_implementar_si_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          ggplot(data.frame(survey.data), aes(x=piensa_implementar_si_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

          #Why can't you implement the solution?
          ggplot(data.frame(survey.data), aes(x=piensa_implementar_no_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          #Would you use the money we give you to implement energy efficiency measures?
          ggplot(data.frame(survey.data), aes(x=dinero_implementar)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          #What would you immplement with the money that has been given to you?
          ggplot(data.frame(survey.data), aes(x=dinero_implementar_si_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          ggplot(data.frame(survey.data), aes(x=dinero_implementar_si_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          #What do you use the money for instead of investing in energy efficiency?
          ggplot(data.frame(survey.data), aes(x=dinero_implementar_no_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          
# Would you change your payment for an energy efficiency implementation?
ggplot(data.frame(survey.data), aes(x=cambio_pago_eficiencia)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
         #How many months of payments would you be willing to exchange of an energy efficiency implementation?
         ggplot(data.frame(survey.data), aes(x=cambio_pago_eficiencia_meses)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
         #What would you implement if we could help you fix?
         ggplot(data.frame(survey.data), aes(x=cambio_pago_eficiencia_si_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
         ggplot(data.frame(survey.data), aes(x=cambio_pago_eficiencia_si_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
         
         #Why would you not like to make this exchange?
         ggplot(data.frame(survey.data), aes(x=cambio_pago_eficiencia_no_code)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

         
# How do you like to make your payments
ggplot(data.frame(survey.data), aes(x=type_pago)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
         
        # How long does it take for you to pay for electricity?
        ggplot(data.frame(survey.data), aes(x=tiempo_pago)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # How easy is it to make this payment?
        ggplot(data.frame(survey.data), aes(x=type_pago_quality)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        # Why is it easy?
        ggplot(data.frame(survey.data), aes(x=porque_facil_pago_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=porque_facil_pago_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=porque_facil_pago_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why is it sort of easy to make this payment?
        ggplot(data.frame(survey.data), aes(x=porque_relafacil_pago_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why is it hard to pay like this?
        ggplot(data.frame(survey.data), aes(x=porque_complicado_pago_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=porque_complicado_pago_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=porque_complicado_pago_code3)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # Why is it very hard to pay like this?
        ggplot(data.frame(survey.data), aes(x=porque_muycomplicado_pago_code1)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplot(data.frame(survey.data), aes(x=porque_muycomplicado_pago_code2)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
        
        	
        		
        
        
        
        		
    
          

         
         
         
         
          		
          
          	
          	

	


	
        
        
        

		

	
        
        
        
        	
	
        
        
        
        		
  
  

        
        
        
        		
        
        
        
        
        
		
        
        
        
        
        		
        
        		
    
    
    
    	
    	
    
    
		

    
    
    
