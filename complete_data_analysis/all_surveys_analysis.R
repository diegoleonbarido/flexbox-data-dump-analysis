library(tidyr)
library(plyr)
library(reshape)
library(ggplot2)


###########################
# Functions

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


###########################
# Read Data In

baseline_df <- all.survey.analysis.data()[1]
baseline_survey_pulperias_merge <- all.survey.analysis.data()[2]
baseline_survey_houses_merge <- all.survey.analysis.data()[3]



###########################
# Begin Analysis

baseline_df$tiempo_pago <- as.integer(as.character(baseline_df$tiempo_pago))
baseline_df$como_contro_de_consumo_code <- as.factor(baseline_df$como_contro_de_consumo_code)
baseline_df$como_contro_de_consumo_code <- as.factor(baseline_df$como_contro_de_consumo_code)

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
summary(baseline_df$luz_confiabilidad)
#yes, no
yes_no <- c(232/433,201/433)

summary(as.factor(baseline_df$frequencia_confiabilidad))
# todos dias, una vez mes, una vez semana, una vez dia
fre_conf <- c(6/84,50/84,25/84)

summary(baseline_df$nivel_satisfaccion_confiabilidad)
# satisfecho, nada satisfecho, muy satisfecho
conf <- c(224/435,76/435,70/435)

summary(baseline_df$nivel_satisfaccion_calidad)
# very satisfied, satisfied, not satisfied
conf2 <- c(216/435,11/435,62/435)

## Turning refrigerator off
summary(baseline_df$apaga_refrigerador)
apaga <- c(313/435,122/435)

##houses
summary(baseline_df$como_contro_de_consumo_code)

## seasonal energy consumption
summary(baseline_df$diferentes_epocas)
281/(281+16)




		



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


#Importance of Data
#limitations for energy efficiency
############################
# usefulness of information

summary(baseline_survey_houses$informacion_util)
#useful, useless, more or less useful
use <- c(104/216,53/216,39/216)

summary(baseline_survey_houses$para_que_util_masmenos_code1)
summary(baseline_survey_houses$para_que_util_uil_code1)
summary(baseline_survey_houses$para_que_util_muyutil_code1)





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



######################
# Electrical Appliances and Seasonal Energy Consumption

#Pulperias
summary(baseline_survey_pulperias$equipo_electronico.bombillas) 
summary(baseline_survey_pulperias$equipo_electronico.celular) 
summary(baseline_survey_pulperias$equipo_electronico.internet) 
summary(baseline_survey_pulperias$equipo_electronico.radio) 
summary(baseline_survey_pulperias$equipo_electronico.television) 
summary(baseline_survey_pulperias$equipo_electronico.computadora) 
summary(baseline_survey_pulperias$equipo_electronico.refrigerador) 
summary(baseline_survey_pulperias$equipo_electronico.abanico) 
summary(baseline_survey_pulperias$equipo_electronico.microondas) 
summary(baseline_survey_pulperias$equipo_electronico.aire_acondicionado) 
summary(baseline_survey_pulperias$equipo_electronico.licuadora) 
summary(baseline_survey_pulperias$equipo_electronico.lavadora) 
summary(baseline_survey_pulperias$equipo_electronico.plancha) 

#Houses
summary(baseline_survey_houses$equipo_electronico.bombillas) 
summary(baseline_survey_houses$equipo_electronico.celular) 
summary(baseline_survey_houses$equipo_electronico.internet) 
summary(baseline_survey_houses$equipo_electronico.radio) 
summary(baseline_survey_houses$equipo_electronico.television) 
summary(baseline_survey_houses$equipo_electronico.computadora) 
summary(baseline_survey_houses$equipo_electronico.refrigerador) 
summary(baseline_survey_houses$equipo_electronico.abanico) 
summary(baseline_survey_houses$equipo_electronico.microondas) 
summary(baseline_survey_houses$equipo_electronico.aire_acondicionado) 
summary(baseline_survey_houses$equipo_electronico.licuadora) 
summary(baseline_survey_houses$equipo_electronico.lavadora) 
summary(baseline_survey_houses$equipo_electronico.plancha) 

             
###################                    
#### Plotting Time Series of Pulperias and Houses
baseline_survey_pulperias$appliance_group <- ifelse(baseline_survey_pulperias$equipo_electronico.aire_acondicionado==TRUE,'AC',ifelse(baseline_survey_pulperias$equipo_electronico.lavadora==TRUE,'Washing Machine',ifelse(baseline_survey_pulperias$equipo_electronico.television==TRUE,'Television','Other Appliances')))


plot_pulperias <-baseline_survey_pulperias[,c('re_jan_kwh','re_feb','re_march','re_april',
                                              're_may','re_june','re_july','re_august','re_sept','re_oct','re_dec','appliance_group')]

melted_pulperias <- melt(plot_pulperias, id=c("appliance_group")) 
melted_pulperias$value <- as.numeric(as.character(melted_pulperias$value))
melted_pulperias$Month <- sapply(melted_pulperias$variable,function(x) gsub("re_",'',x,perl=TRUE))
melted_pulperias$Month <- sapply(melted_pulperias$Month,function(x) gsub("_kwh",'',x,perl=TRUE))

aggregate_month <- aggregate(melted_pulperias$value,by=list(melted_pulperias$Month),FUN=mean,na.rm=TRUE) %>% mutate(Group.2=Group.1) %>% select(Group.2,x)
aggregate_month$Group.1 <- 'Median'
aggregate_month <- aggregate_month[,c('Group.1','Group.2','x')]

aggregated_pulperias <- aggregate(melted_pulperias$value,by=list(melted_pulperias$appliance_group,melted_pulperias$Month),FUN=mean,na.rm=TRUE)

aggregated_pulperias <- rbind(aggregated_pulperias,aggregate_month)
aggregated_pulperias$month_num <- ifelse(aggregated_pulperias$Group.2=='jan',1,ifelse(aggregated_pulperias$Group.2=='feb',2,ifelse(aggregated_pulperias$Group.2=='march',3,
                                         ifelse(aggregated_pulperias$Group.2=='april',4,ifelse(aggregated_pulperias$Group.2=='may',5,ifelse(aggregated_pulperias$Group.2=='june',6,
                                         ifelse(aggregated_pulperias$Group.2=='july',7,ifelse(aggregated_pulperias$Group.2=='august',8,ifelse(aggregated_pulperias$Group.2=='sept',9,
                                         ifelse(aggregated_pulperias$Group.2=='oct',10, ifelse(aggregated_pulperias$Group.2=='dec',12,11)))))))))))

ii <- 1:12
ggplot(aggregated_pulperias,aes(month_num,x,group=Group.1,colour=Group.1)) + geom_line() +scale_x_continuous(breaks=ii) + theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom",legend.title=element_blank()) + xlab('Month') + ylab('Monthly Energy Consumption (kWh)')




#############
### Costs

pulp_cons_median <- median(baseline_survey_pulperias$r_monthly_kwh,na.rm=TRUE)
pulperia_median <- median((baseline_survey_pulperias$r_monthly_cordobas/29),na.rm=TRUE)
pulperia_median <- median((baseline_survey_pulperias$r_monthly_cordobas/29)/baseline_survey_pulperias$r_monthly_kwh,na.rm=TRUE)
pulperia_tot_costs <-  median((baseline_survey_pulperias$r_total_cordobas/29),na.rm=TRUE)

pulp_cons_house <- median(baseline_survey_houses$r_monthly_kwh,na.rm=TRUE)
pulperia_house <- median((baseline_survey_houses$r_monthly_cordobas/29),na.rm=TRUE)
house_median <- median((baseline_survey_houses$r_monthly_cordobas/29)/baseline_survey_houses$r_monthly_kwh,na.rm=TRUE)
house_tot_costs <-  median((baseline_survey_houses$r_total_cordobas/29),na.rm=TRUE)



#############
### Histogram Plots


baseline_df$label <- ifelse(baseline_df$group=='house','Houses','Micro-Enterprises')
ggplot(subset(baseline_df,baseline_df$r_total_cordobas<25000),aes(r_total_cordobas/29,fill=label)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',bins =50,alpha=0.5) +
  theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + 
  labs(colour= 'ID') + theme(legend.key = element_rect(fill = "white")) +  theme(legend.position="bottom") +
  xlab('Monthly Energy Costs ($US)') + ylab('Density') + theme(legend.position="bottom",legend.title=element_blank())

ggplot(baseline_df,aes(r_monthly_kwh,fill=label)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',bins =50,alpha=0.5) +
  theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + 
  labs(colour= 'ID') + theme(legend.key = element_rect(fill = "white")) +  theme(legend.position="bottom") +
  xlab('Monthly Energy Consumption (kWh)') + ylab('Density') + theme(legend.position="bottom",legend.title=element_blank())











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















