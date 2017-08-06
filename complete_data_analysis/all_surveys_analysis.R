library(tidyr)
library(plyr)

setwd('/Users/diego/Desktop/Data/nicaragua_surveys/Final_Analysis')

###########################
#Functions

#Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

baseline_survey_pulperias <- read.csv('All_Surveys_with_Pics_Reviewed_Final.csv')
baseline_survey_pulperias$group <- 'pulperia'
baseline_survey_pulperias$tiempo_pago <- ''
baseline_survey_pulperias_merge <-baseline_survey_pulperias  %>% mutate(encuesta_id=survey_id,cambio_tarifa=precio_electricidad) %>%select(today,encuesta_id,ano,sexo,nivel_educacion,dificultad_pago,type_pago,tiempo_pago,cambio_tarifa,nivel_satisfaccion_confiabilidad,nivel_satisfaccion_calidad,nivel_satisfaccion_precios,luz_confiabilidad,group)

baseline_survey_houses <- read.csv('houses_2017.csv') 
baseline_survey_houses$ano <- ''
baseline_survey_houses$group <- 'house'
baseline_survey_houses$nivel_satisfaccion_precios <- ''
baseline_survey_houses_merge <- baseline_survey_houses %>% select(today,encuesta_id,ano,sexo,nivel_educacion,dificultad_pago,type_pago,tiempo_pago,cambio_tarifa,nivel_satisfaccion_confiabilidad,nivel_satisfaccion_calidad,nivel_satisfaccion_precios,luz_confiabilidad,group)

baseline_survey_houses <- baseline_survey_houses[,c('today','encuesta_id','ano','sexo','nivel_educacion',
                                                   'problema_actual','problema_actual_razon1','problema_actual_razon2',
                                                   'problema_actual_razon3', 'futuros_problemas_primero','futuros_problemas_segundo',
                                                   'futuros_problemas_tercero','cambio_climatico_sabe', 'cambio_climatico_causa1',
                                                   'cambio_climatico_causa2','cambio_climatico_causa3', 'preocupacion_cambio_climatico',	
                                                   'preocupacion_eventos_climaticos','dificultad_pago','gasto_electrico_cordobas','gastos_mensuales')]

baseline_survey_houses$gasto_electrico_cordobas <- as.numeric(as.character(baseline_survey_houses$gasto_electrico_cordobas))
baseline_survey_houses$gastos_mensuales <- as.numeric(as.character(baseline_survey_houses$gastos_mensuales))


#####################
# Baseline Parameters 
baseline_df <- rbind(baseline_survey_pulperias_merge,baseline_survey_houses_merge)

baseline_df$tiempo_pago <- as.integer(as.character(baseline_df$tiempo_pago))

baseline_df$ano <- as.numeric(baseline_df$ano)
mean(baseline_df$ano,na.rm=TRUE)
sd(baseline_df$ano,na.rm=TRUE)

baseline_df$edu_id <- ifelse(baseline_df$nivel_educacion == 'sin_educacion',1,ifelse(baseline_df$nivel_educacion=='primaria',2,ifelse(baseline_df$nivel_educacion=='ciclo_basico',3,ifelse(baseline_df$nivel_educacion=='ciclo_diversificado',4,ifelse(baseline_df$nivel_educacion=='universidad',5,6)))))
getmode(baseline_df$edu_id)
sd(baseline_df$edu_id,na.rm=TRUE)

summary(baseline_df$dificultad_pago)
hard_bill <- c(81/435,191/435,80/435)
#mobile_money # person_bank #person_utilty #phone smart_phone mail internet person_comes phone_light phone_mpeso
bill_pay <- c(5/435,149/435,192/435,22/435,4/435,7/435,12/435,27/435,10/435,6,435,2/435)
bill_pay_time <- aggregate(baseline_df$tiempo_pago,by=list(baseline_df$type_pago),FUN=mean)

summary(baseline_df$cambio_tarifa)
# increased same decreased
prices_ip <- c(314/435,93/435,20/435)

#Perception Quality of Service
summary(baseline_df$nivel_satisfaccion_calidad)

#####################
# Perception on the quality of service
nivel_satisfaccion_confiabilidad	nivel_satisfaccion_calidad	nivel_satisfaccion_precios	

luz_confiabilidad	frequencia_confiabilidad
nivel_satisfaccion_confiabilidad	nivel_satisfaccion_calidad




###################
# Current and Future Issues

# Current Problmes
summary(baseline_survey_houses$problema_actual_razon1)
# Energy, Food, Basic Servies, Unemployment
top_prob <- c(51/216, 42/216,27/216,22/216)

# Future Issues
summary(baseline_survey_houses$futuros_problemas_segundo)
#Climate change, oil dependency, electricity prices
top_future_issue <- c(79/216, 52/216,45/216)
sec_future_issue <- c(46/216, 67/216,61/216)
th_future_issue <- c(37/216, 62/216,77/216)

# What is the main cause of climate change?
summary(baseline_survey_houses$cambio_climatico_causa3)
#Deforestation, Pollution, human being
cli_future_issue <- c(45/216, 28/216,16/216)

#How concerned are you with climate change
summary(baseline_survey_houses$preocupacion_cambio_climatico)
# Very worried, worried, indiferent
cli_concern <- c(53/216, 112/216,33/216)
summary(baseline_survey_houses$preocupacion_eventos_climaticos)
cli_ev_concern <- c(143/216, 57/216,12/216)






####################
# Total Costs in Business

summary(baseline_survey_pulperias$principal_gastos_ME)
# energy, loans, rent
costs <- c(193/219,11/219,8/219)
summary(baseline_survey_pulperias$segundo_gastos_ME)
#loans, employees, energy
costs_2 <- c(54/219,26/219,11/219)
summary(baseline_survey_pulperias$tercero_gastos_ME)
#no_more, other, employees
costs_3 <- c(74/219,26/219,11/219)

baseline_survey_pulperias$energy_pct_business <-baseline_survey_pulperias$gasto_electrico/baseline_survey_pulperias$gastos_totales_negocio
summary(baseline_survey_pulperias$energy_pct_business)

summary(baseline_survey_pulperias$dificultad_pago)

#Houses
baseline_survey_houses$energy_pct_business <- baseline_survey_houses$gasto_electrico_cordobas/baseline_survey_houses$gastos_mensuales
summary(baseline_survey_houses$energy_pct_business,na.rm=TRUE)




####################
# 1. Perception

especifique_gasto_electrico
r_monthly_kwh	r_monthly_cordobas	r_total_cordobas
precio_electricidad

#Houses
gasto_electrico	gasto_electrico_cordobas gasto_tarifa_electrica
especifique_gasto_electrico	especifique_gasto_electrico_cordobas
cambio_tarifa
r_monthly_kwh	r_monthly_cordobas	r_total_cordobas




######################
# Electrical Appliances and Seasonal Energy Consumption
diferentes_epocas

diferentes_epocas	equipo_electronico/bombillas	equipo_electronico/celular	equipo_electronico/internet	equipo_electronico/radio	equipo_electronico/television	equipo_electronico/computadora	equipo_electronico/refrigerador	equipo_electronico/abanico	equipo_electronico/aire_acondicionado	equipo_electronico/microondas	equipo_electronico/licuadora	equipo_electronico/equipo	equipo_electronico/lavadora	equipo_electronico/plancha
re_jan_kwh	re_feb	re_march	re_april	re_may	re_june	re_july	re_august	re_sept	re_oct	re_nov	re_dec

#houses
equipo_electronico/bombillas	equipo_electronico/celular	equipo_electronico/internet	equipo_electronico/radio	equipo_electronico/television	equipo_electronico/computadora	equipo_electronico/refrigerador	equipo_electronico/abanico	equipo_electronico/aire_acondicionado	equipo_electronico/microondas	equipo_electronico/licuadora	equipo_electronico/equipo	equipo_electronico/lavadora	equipo_electronico/secadora	equipo_electronico/plancha
mas_gasta_tiempo
hora_pico_energia_code
dia_pico_energia_code
ano_pico_energia_code



#limitations for energy efficiency


##########################
# Turning off refrigerator
apaga_refrigerador



###########################
# tracking energy consumption


#houses
como_contro_de_consumo_code
medicion_ahorros_si_code



############################
# usefulness of information

#houses
informacion_util
para_que_util_nada_code
para_que_util_masmenos_code1	para_que_util_masmenos_code2
para_que_util_uil_code1	para_que_util_uil_code2
para_que_util_muyutil_code1	para_que_util_muyutil_code2



