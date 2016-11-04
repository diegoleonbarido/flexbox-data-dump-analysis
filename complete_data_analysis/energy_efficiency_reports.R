############ Script to Analyze data from A1
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
library(data.table)

source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/date_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/merging_binding_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/read_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/time_series_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/hourly_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_consumption_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/scraping_scripts.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/resampling_functions.R')



#flexboxid = "flxbxD20"
#date1 = as.POSIXct("2016-07-17 12:00:00")
#date2 = as.POSIXct("2016-07-20 12:00:00")
#date3 = as.POSIXct("2016-07-23 12:00:00")

#flexboxid = "flxbxD6" 
#date1 = as.POSIXct("2016-07-17 15:00:00")
#date2 = as.POSIXct("2016-07-25 15:00:00")
#date3 = as.POSIXct("2016-08-02 15:00:00")

#flexboxid = "flxbxD29"
#date1 = as.POSIXct("2016-07-17 12:00:00")
#date2 = as.POSIXct("2016-07-25 12:00:00")
#date3 = as.POSIXct("2016-08-02 12:00:00")

flexboxid = "flxbxD18"
date1 = as.POSIXct("2016-07-17 13:00:00")
date2 = as.POSIXct("2016-07-25 13:00:00")
date3 = as.POSIXct("2016-08-02 13:00:00")


count.plot <-  0
text.count <- 0
text.list <- c()
text.list.name <- c()
count.plot.list <- c()
count.plot.name <- c()

data_report <- read.server.data(date1,date2,date3,flexboxid) # house.subset,energy_week1,energy_week2,clean_week1,clean_week2,temp_week1,temp_week2

house.subset <- data_report[[1]]
energy_week1 <- data_report[[2]]
energy_week2 <- data_report[[3]]
clean_week1 <- data_report[[4]]
clean_week2 <- data_report[[5]]
temp_week1 <- data_report[[6]] %>% select(datetime,hostname,inside_temp1,inside_temp2,minute,hour,day,month,date)
temp_week2 <- data_report[[7]] %>% select(datetime,hostname,inside_temp1,inside_temp2,minute,hour,day,month,date)


# Dates

text1 <- paste("Estos graficos demuestran un experimento de eficiencia energetica. Durante una semana se dejo prendido el refrigerador, y durante la otra semana se dejo apagado. Aqui comparamos varios graficos que demuestran los hallazgos. ","En esta casa la duracion del experimento fue de ",round(unclass(date2 - date1)[1])," dias.")
text.count <- text.count + 1
text.list[[text.count]] <- text1
text.list.name[[text.count]] <- 'text1'


# House Power Plot
house_power_plot <- ggplot(house.subset, aes(datetime)) + geom_line(aes(y = active_pwr2),col='black') + geom_line(aes(y = active_pwr3),col='red') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + xlab("Fecha") + ylab("Potencia (Watt)")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = house_power_plot
count.plot.name[[count.plot]] = 'house_power_plot'



text2 = "Esta grafica demuestra la potencia del refrigerador. El color negro demuestra la potencia durante la semana cuando se dejo prendido el refrigeraodr. El colo rojo demuestra la potencia durante la semana cuando el refrigerador se estuvo prendiendo y apagando. Las lineas largas demuestran cuando el compresor se prende."
text.count <- text.count + 1
text.list[[text.count]] <- text2
text.list.name[[text.count]] <- 'text2'



######### Hourly Energy Plots

energy_week1 <- tail(clean_week1$energy_sum2,1) - head(clean_week1$energy_sum2,1)
energy_week2 <- tail(clean_week2$energy_sum3,1) - head(clean_week2$energy_sum3,1)

if (energy_week1 > energy_week2 ){
  text3 = paste("En esta casa, cuando se DEJO ENCHUFADO el refrigerador el consumo se INCREMENTO por un", round(((energy_week1 - energy_week2)/energy_week1)*100),"porciento. ") 
  text.count <- text.count + 1
  text.list[[text.count]] <- text3
  text.list.name[[text.count]] <- 'text3'
  } else {
    text3 = paste("En esta casa, cuando se DESENCHUFO el refrigerador el consumo se INCREMENTO por un", round(((energy_week2 - energy_week1)/energy_week2)*100),"porciento. ") 
  text.count <- text.count + 1
  text.list[[text.count]] <- text3
  text.list.name[[text.count]] <- 'text3'
  }




# Hourly Mean Plots

energy_refrigerator_week1 <- mean_hour_energy(clean_week1,energy)
energy_refrigerator_week2 <- mean_hour_energy(clean_week2,energy)

norm_mean_week1 <- aggregate(energy_refrigerator_week1$fridge.energy,by=list(energy_refrigerator_week1$hour),FUN=mean,na.rm=TRUE) %>% mutate(hour=Group.1,energy=x) %>% select(hour,energy)
norm_mean_week2 <- aggregate(energy_refrigerator_week2$fridge.energy,by=list(energy_refrigerator_week2$hour),FUN=mean,na.rm=TRUE) %>% mutate(hour=Group.1,energy=x) %>% select(hour,energy)

norm_median_week1 <- aggregate(energy_refrigerator_week1$fridge.energy,by=list(energy_refrigerator_week1$hour),FUN=median,na.rm=TRUE) %>% mutate(hour=Group.1,energy=x) %>% select(hour,energy)
norm_median_week2 <- aggregate(energy_refrigerator_week2$fridge.energy,by=list(energy_refrigerator_week2$hour),FUN=median,na.rm=TRUE) %>% mutate(hour=Group.1,energy=x) %>% select(hour,energy)


f.plot.norm.median_1 <- ggplot(norm_median_week1,aes(x=hour,y=energy)) + geom_line()+ geom_point(size=2)+ xlab('Hora del Dia') + ylab('Energia por Hora (Normalizada de 0 a 1)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = f.plot.norm.median_1
count.plot.name[[count.plot]] = 'f_plot_norm_median_1'

text4 = "Esta grafica demuestra el consumo de energia promedio por hora durante la semana que NO SE DESENCHUFO el refrigerador. Se puede observar que el refrigerador esta respetando sus ciclos, apangose y prendiendose solito."
text.count <- text.count + 1
text.list[[text.count]] <- text4
text.list.name[[text.count]] <- 'text4'


f.plot.norm.median_2 <- ggplot(norm_median_week2,aes(x=hour,y=energy)) + geom_line(color="red")+ geom_point(size=2,color="red")+ xlab('Hora del Dia') + ylab('Energia por Hora (Normalizada de 0 a 1)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = f.plot.norm.median_2
count.plot.name[[count.plot]] = 'f_plot_norm_median_2'

text5 = "Esta grafica demuestra el consumo de energia promedio por hora durante la semana que SE DESENCHUFO el refrigerador deacuerdo a un horario establecido. Se puede observar que el refrigerador esta consumiendo energia igual casi todo el tiempo, excepto cuando se le indica que este apagado."
text.count <- text.count + 1
text.list[[text.count]] <- text5
text.list.name[[text.count]] <- 'text5'


e_increasing_week1 <- ggplot(clean_week1,aes(x=datetime,y=energy_sum2)) + geom_line() + xlab('Fecha') + ylab('Incremento de Energia') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = e_increasing_week1
count.plot.name[[count.plot]] = 'e_increasing_week1'

text6 = "Esta grafica demuestra COMO CRECIO el consumo de energia durante la semana que NO SE DESENCHUFO  el refrigerador. Se puede observar que el consumo de energia incrementa de una manera continua."
text.count <- text.count + 1
text.list[[text.count]] <- text6
text.list.name[[text.count]] <- 'text6'


e_increasing_week2 <- ggplot(clean_week2,aes(x=datetime,y=energy_sum3)) + geom_line(color="red") + xlab('Fecha') + ylab('Incremento de Energia') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = e_increasing_week2
count.plot.name[[count.plot]] = 'e_increasing_week2'

text7 = "Esta grafica demuestra COMO CRECIO el consumo de energia durante la semana que SE DESENCHUFO el refrigerador. Se puede observar que el consumo de energia incrementa solo cuando el refrigerador esta prendido."
text.count <- text.count + 1
text.list[[text.count]] <- text7
text.list.name[[text.count]] <- 'text7'



##### Hourly Temp Plots


temp_week1$temp_mean <- ((temp_week1$inside_temp1 + temp_week1$inside_temp2)/2)/1000
temp_week2$temp_mean <- ((temp_week2$inside_temp1 + temp_week2$inside_temp2)/2)/1000

temp_mean_week_1 <- aggregate(temp_week1$temp_mean,by=list(temp_week1$hour),FUN=mean,na.rm=TRUE) %>% mutate(hour=Group.1,temp=x) %>% select(hour,temp)
temp_mean_week_2 <- aggregate(temp_week2$temp_mean,by=list(temp_week2$hour),FUN=mean,na.rm=TRUE) %>% mutate(hour=Group.1,temp=x) %>% select(hour,temp)


if (mean(temp_mean_week_1$temp) > mean(temp_mean_week_2$temp)) {
  text8= paste("La temperatura cuando el refrigerador estuvo ENCHUFADO fue", round((mean(temp_mean_week_1$temp) - mean(temp_mean_week_2$temp)/(mean(temp_mean_week_1$temp))*100)), "por ciento mas ALTA" )
  text.count <- text.count + 1
  text.list[[text.count]] <- text8
  text.list.name[[text.count]] <- 'text8'
  } else {
  text8= paste("La temperatura cuando el refrigerador estuvo ENCHUFADO fue", round((mean(temp_mean_week_1$temp) - mean(temp_mean_week_2$temp)/(mean(temp_mean_week_1$temp))*100)), "por ciento mas BAJA" )
  text.count <- text.count + 1
  text.list[[text.count]] <- text8
  text.list.name[[text.count]] <- 'text8'
  }


temp_plot_week1 <- ggplot(temp_mean_week_1,aes(x=hour,y=temp)) + geom_line()+ geom_point(size=2)+ xlab('Hora del Dia') + ylab('Temperatura Interna por Hora del Dia') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = temp_plot_week1
count.plot.name[[count.plot]] = 'temp_plot_week1'

text9 = "Esta grafica demuestra como vario la temperatura durante la semana cuando el refrigerador estuvo ENCHUFADO. Se puede observar que la temperatura es mas baja, y que varia con el consumo de energia."
text.count <- text.count + 1
text.list[[text.count]] <- text9
text.list.name[[text.count]] <- 'text9'


temp_plot_week2 <- ggplot(temp_mean_week_2,aes(x=hour,y=temp)) + geom_line(color="red")+ geom_point(size=2,color="red")+ xlab('Hora del Dia') + ylab('Temperatura Interna por Hora del Dia') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = temp_plot_week2
count.plot.name[[count.plot]] = 'temp_plot_week2'

text10 = "Esta grafica demuestra como vario la temperatura durante la semana cuando el refrigerador estuvo DESENCHUFADO. Se puede observar que la temperatura es mucho mas alta, y que sube significativamente cuando se apaga el refrigerador."
text.count <- text.count + 1
text.list[[text.count]] <- text10
text.list.name[[text.count]] <- 'text10'



for (j in 1:count.plot) {
  plot.name = paste(flexboxid,"_",count.plot.name[[j]],sep="")
  name <- substring(plot.name,1,35)
  mypath <- file.path("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports/energy_efficiency_report/Latex/figure",paste(plot.name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(count.plot.list[[j]])
  dev.off()
}


# Saving text files to file
for (l in 1:text.count) {
  filepath <- file(paste("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports/energy_efficiency_report/Latex/",flexboxid,text.list.name[[l]],'.txt',sep=''))
  writeLines(text.list[[l]], filepath)
  close(filepath)
}




