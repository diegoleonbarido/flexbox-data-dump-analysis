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


###### NOTE
# Section 1: Reading data
# Section X: Survey Rate Plots
# Section X: Neighbor Comparison
# Section X: Hour of the Day
# Section X: Day of the week analysis
# Section X: Fridge Hour of the Day

###### Set list of houses and variables


flexlist <- c('A1','A3','A6','A7','A9','A11','A12','A14','A16','A17','A18','A19','A20','A21','A22','A24','A25','A26','A28','A29')
dump <- 'DUMP11'


############# Section 1: Grid Analysis

grid.data <- get.data(as.Date(timeSequence(from = (Sys.Date() - 32), to = (Sys.Date() - 2))))

grid.plot1 <- ggplot(grid.data[[1]], aes(Hora, generation)) + geom_line(aes(color = resource),size=3) + xlab("Hora del Dia") + ylab("Generacion y Consumo de Energia en Nicaragua (MW)") + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.title=element_blank())

mypath <- file.path(paste("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports",dump,"Latex","figure","gridplot1.jpg",sep = "/"))
jpeg(file=mypath)
grid.draw(grid.plot1)
dev.off()

grid.plot2 <- ggplot(grid.data[[2]], aes(x = resources, y = pct, fill = resource)) +  geom_bar(stat = "identity",width=0.6) + xlab('') + ylim(0,sum(grid.data[[2]]$pct)) + ylab('Contribucion de Cada Recurso a la Generacion de Energia en Nicaragua (%)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.title=element_blank()) + scale_fill_brewer(palette="YlGnBu")

mypath <- file.path(paste("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports",dump,"Latex","figure","gridplot2.jpg",sep = "/"))
jpeg(file=mypath)
grid.draw(grid.plot2)
dev.off()

grid_text1 <- paste("Esta es una grafica que demuestra la demanda promedio por hora para todo el pais de Nicaragua de los ultimos treinta dias. La linea roja demuestra el consumo promedio de energia por hora del pais, la azul demuestra la reduccion de la demanda cuando se toma en cuenta el viento, y la linea verde demuestra la generacion de energia por parte de el viento. Mientras mas energia renovable hay en el pais, menos petroleo se consume. Su participacion en este proyecto esta ayudando a mejor integrar la energia renovable en la red electrica Nicaraguense. Cuando el viento se va, entonces nosotros estaremos adaptando el consumo de su refrigerador para que Nicaragua consuma menos petroleo y pueda integrar mejor a las energias renovables.")
grid_text2 <- paste("La segunda grafica demuestra la diversidad de la energia que fue producida en Nicaragua en los ultimos 30 dias. En este mes el petroleo produjo el ",round(grid.data[[2]]$pct[which(grid.data[[2]]$resource == 'Petroleo')]),"por ciento de la energia, mientras que el Viento produjo el ",round(grid.data[[2]]$pct[which(grid.data[[2]]$resource == 'Viento')]),"por ciento de la energia. La Biomasa y la Geotermia producieron",round(grid.data[[2]]$pct[which(grid.data[[2]]$resource == 'Biomasa')]),"por ciento y",round(grid.data[[2]]$pct[which(grid.data[[2]]$resource == 'Geotermia')]),"por ciento respectivamente. La energia hidroelectrica produjo aproximadamente el ",round(grid.data[[2]]$pct[which(grid.data[[2]]$resource == 'Hidroelectrica')]),"por ciento de la energia de los ultimos 30 dias. Finalmente, la interconexion regional contribuyo con un ", round(grid.data[[2]]$pct[which(grid.data[[2]]$resource == 'Interconexion')]),"por ciento a la produccion de energa en Nicaragua.")

filepath <- file(paste("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports",dump,"Latex","grid_text1.txt",sep="/"))
writeLines(grid_text1, filepath)
close(filepath)

filepath <- file(paste("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports",dump,"Latex","grid_text2.txt",sep="/"))
writeLines(grid_text2, filepath)
close(filepath)




###############  Section 2: Reading data (specify houses, specify dump)


#### Survey Data 

survey.data.results <- read.survey.data(flexlist)

#### Cluster house 

data.list.houses <- read.house.fridge(dump,flexlist) # read.house.fridge or  read.data.all  or   read.data.all.parallel
cluster.house <- data.list.houses[[2]] %>% mutate(house_Energy=houseAll_Energy) %>% select(id,datetime,houseAll_Voltage,houseAll_Current,houseAll_Power,house_Energy,house.id)
cluster.fridge <- data.list.houses[[1]]
dates_data_frame <- date.fridge.house(cluster.fridge,cluster.house) 

e.neighbor <- neighbor.comparison(cluster.house,dump)

######
###### Iterate
#####

for (i in 1:length(flexlist)) {
  
  #Loop Settings
  count.plot.list <- list()
  count.plot.name <- list()
  text.list <- list()
  count.plot <- 0
  options(warn=-1)
  
  #Subsetting
  selected.house <- flexlist[i]  
  refrigerator <- subset(dates_data_frame[[1]],house.id == selected.house)
  house <- subset(dates_data_frame[[2]],house.id == selected.house)
  
  
############# Section X: Survey Data
  
#Cost plot

  energy.receipt.data <- survey.data.results[[1]]
  time.series.receipt <- survey.data.results[[2]]
  
  unique.house.sdata <- subset(energy.receipt.data,energy.receipt.data$house.id == flexlist[i])
  
  #Receipt Data Plots
  
  costvars <- subset(unique.house.sdata,unique.house.sdata$cost==1)
  subvars <- subset(unique.house.sdata,unique.house.sdata$cost==0)
  
  costvars_plot <- ggplot(costvars, aes(x = house.id, y = mean.val, fill = type.val)) +  geom_bar(stat = "identity",width=0.6) + xlab('') + ylim(0,max(max(costvars$mean.val,na.rm=TRUE),max(subvars$mean.val,na.rm=TRUE),na.rm=TRUE)+50) + ylab('Contribucion al Gasto Total Energetico (%)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.title=element_blank())
  subvars_plot <- ggplot(subvars, aes(x = house.id, y = mean.val, fill = type.val)) +  geom_bar(stat = "identity",width=0.6) + xlab('') + ylim(0,max(max(costvars$mean.val,na.rm=TRUE),max(subvars$mean.val,na.rm=TRUE),na.rm=TRUE)+50) + ylab('Contribucion al Gasto Total Energetico (%)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.title=element_blank())
  
  if (is.nan(mean(unique.house.sdata$mean.val,na.rm=TRUE)) == FALSE ) {
  
  #Storing in file
  count.plot <- count.plot + 1
  count.plot.list[[count.plot]] = costvars_plot
  count.plot.name[[count.plot]] = 'costvars_plot'
  
  text.survey <- paste("En general el costo de energia es el que causa el mayor gasto en su recibo. Usted tambien tiene otros varios gastos relacionados al IVA, la comercializacion, retrasos en el gasto, y alumbrado publico entre otros. Los colores en el grafico demuestran los diferentes gastos y cada uno tiene un porcentaje. Como el gasto energetico es el gasto mas alto, esto quiere decir que usted tiene todavia una gran oportunidad para reducir su gasto energetico.")
  text.list[[count.plot]] <- text.survey
  
      if (is.nan(mean(subvars$mean.val,na.rm=TRUE)) == FALSE) {
      count.plot <- count.plot + 1
      count.plot.list[[count.plot]] = subvars_plot
      count.plot.name[[count.plot]] = 'subvars_plot'
      
      text.survey.subs <- paste("A usted, los subsidios lo ayudan a pagar su gasto electrico. La grafica de la derecha demuestra el porcentaje de su tarifa que el subsidio le ayuda a pagar.")
      text.list[[count.plot]] <- text.survey.subs
      
      } else {}
  
  } else {}
  
  
  #Energy and Cost Time Series Plots
  
  your.data <- subset(time.series.receipt,time.series.receipt$Casa == flexlist[i])
  your.data$color <- ifelse(your.data$fecha== tail(your.data$fecha,1),"this_day","other_days")
  
  historico.energia <- ggplot(your.data, aes(x=fecha,y=energia)) + geom_line(stat='identity') + geom_point() + xlab('Fecha (Mes)') + ylab('Consumo de Energia Historico (kWh)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + guides(fill=FALSE)
  
  #Storing in file
  count.plot <- count.plot + 1
  count.plot.list[[count.plot]] = historico.energia
  count.plot.name[[count.plot]] = 'historico_energia'
  
  max.energia.date <- your.data$fecha[which(your.data$energia == max(your.data$energia))]
  min.energia.date <- your.data$fecha[which(your.data$energia == min(your.data$energia))]
  text.list[[count.plot]] <-  paste("Su consumo de energia historico mas alto ocurrio el mes de ",max.energia.date," y el mes de consumo mas bajo ocurrio el ",min.energia.date)
  
  
  historico.cordobas <- ggplot(your.data, aes(x=fecha,y=importe)) + geom_line(stat='identity') + geom_point() + xlab('Fecha (Mes)') + ylab('Gasto en Energia Historico (Cordobas)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + guides(fill=FALSE)
  
  #Storing in file
  count.plot <- count.plot + 1
  count.plot.list[[count.plot]] = historico.cordobas
  count.plot.name[[count.plot]] = 'historico_cordobas'
  
  max.cordobas.date <- your.data$fecha[which(your.data$importe == max(your.data$importe))]
  min.cordobas.date <- your.data$fecha[which(your.data$importe == min(your.data$importe))]
  text.list[[count.plot]] <- paste("Su consumo de Cordobas mas alto ocurrio el mes de ",max.cordobas.date," y el mes de gasto mas bajo ocurrio el ",min.cordobas.date)
   

  ############### Section X: Neighbor Comparison
  
  e.neighbor$level <- ifelse(e.neighbor$norm.energy <= quantile(e.neighbor$norm.energy, c(.25)),'Bajo',ifelse(e.neighbor$norm.energy > quantile(e.neighbor$norm.energy, c(.25)) & e.neighbor$norm.energy <= quantile(e.neighbor$norm.energy, c(.50)),'Medio',ifelse(e.neighbor$norm.energy > quantile(e.neighbor$norm.energy, c(.50)) & e.neighbor$norm.energy <= quantile(e.neighbor$norm.energy, c(.75)),'Alto','Muy Alto' )))
  e.neighbor.average <- aggregate(e.neighbor$norm.energy,by=list(e.neighbor$level),FUN=mean) %>% mutate(group=Group.1,norm.energy=x) %>% select(group,norm.energy)
  
  selected.house.df <- subset(e.neighbor,e.neighbor$house.id==selected.house) %>% mutate(group=house.id) %>% select(group,norm.energy)
  e.neighbor.average <- rbind(e.neighbor.average,selected.house.df)
  e.neighbor.average$group[e.neighbor.average$group == selected.house] <- "Usted"
  
  #Neighbor Barplot
  group.colors <- c('Usted' = "orangered1", 'Muy Alto' = "lightskyblue1",'Medio' = "lightskyblue1",'Bajo' = "lightskyblue1",'Alto' = "lightskyblue1")
  neighbor.plot <- ggplot(e.neighbor.average, aes(x=reorder(group,norm.energy),y=norm.energy,fill=group)) + geom_bar(stat='identity') +scale_fill_manual(values=group.colors)+ xlab('Grupo de Consumo') + ylab('Energia (Normalizada de 0 a 1)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + guides(fill=FALSE)
  
  #Neighbor Text
  ordered <- e.neighbor.average[order(e.neighbor.average$norm.energy),]
  index.ord <- match('Usted',ordered$group)
  word1 <- as.character(ordered$group[index.ord-1])
  word2 <- as.character(ordered$group[index.ord+1])
  
  vecino1 <- ifelse(index.ord == 1, 'Bajo',ifelse(index.ord == 2,'Medio',ifelse(index.ord == 3,'Alto',ifelse(index.ord == 4,'Muy Alto','Muy Alto'))))
  
  #Storing in file
  count.plot <- count.plot + 1
  count.plot.list[[count.plot]] = neighbor.plot
  count.plot.name[[count.plot]] = 'neighbor_plot'
  
  texta <- paste("Dentro de todas las casas y micro-empresas del estudio usted es parte del grupo de consumo energetico",vecino1,". De todas las casas y micro-empresas que estan en el estudio usted gasta mas que el grupo de consumo",word1," y un poco menos que otras casas y micro-empresas dentro de el grupo",word2,". Usted podria utilizar su energia mas eficientemente.")
  textb <- paste("Dentro de todas las casas y micro-empresas del estudio usted es parte del grupo de consumo energetico",vecino1,". Usted es parte del grupo que consumo mas energia dentro del programa. Tiene que ser mas consciente de las horas y dias cuando consume mas energia y prestar atencion a su consumo. En el futuro, utilizaremos un modem para que usted pueda verificar su consumo mas frecuentemente, y asi administrar mejor su energia.")
  textc <- paste("Dentro de todas las casas y micro-empresas del estudio usted es parte del grupo de consumo energetico",vecino1,". Usted es parte del grupo que consumo menos energia dentro del programa. Aunque usted consume menos que otras personas, todavia tiene oportunidad para consumir menos y continuar prestando atencion a su consumo. En el futuro, utilizaremos un modem para que usted pueda verificar su consumo mas frecuentemente, y asi administrar mejor su energia.")
  
  real.text <- ifelse(vecino1=='Medio' |vecino1=='Alto',texta,ifelse(vecino1=='Bajo',textc,textb))
  
  text.list[[count.plot]] <- real.text


################ Section 3: Hour of the day

  if (length(unique(house$house_Energy)) > 30) {
    energy.house <- henergy.consumption.hour.house(house)
    
    norm.mean <- aggregate(energy.house$norm.energy,list(energy.house$house.id,energy.house$hour),FUN=mean) %>% mutate(house.id=Group.1,hour=Group.2,norm.energy=x) %>% select(house.id,hour,norm.energy)
    norm.median <- aggregate(energy.house$norm.energy,list(energy.house$house.id,energy.house$hour),FUN=median) %>% mutate(house.id=Group.1,hour=Group.2,norm.energy=x) %>% select(house.id,hour,norm.energy)
    
    plot.norm.mean<- ggplot(norm.mean,aes(x=hour,y=norm.energy)) + geom_line()
    plot.norm.median<- ggplot(norm.median,aes(x=hour,y=norm.energy)) + geom_line() + geom_point(size=3)+ xlab('Hora del Dia') + ylab('Energia por Hora (Normalizada de 0 a 1)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))
    
    madrugada <- subset(norm.median,norm.median$hour>=2 & norm.median$hour <=5)
    manana <- subset(norm.median,norm.median$hour>=6 & norm.median$hour <=11)
    tarde <- subset(norm.median,norm.median$hour>=12 & norm.median$hour <= 18)
    noche <- subset(norm.median,norm.median$hour>18 & norm.median$hour <= 23) 
    noche <- rbind(noche,subset(norm.median,norm.median$hour<=1))
    
    hora0 <- as.character(madrugada$hour[match(max(madrugada$norm.energy),madrugada$norm.energy)])
    hora1 <- as.character(manana$hour[match(max(manana$norm.energy),manana$norm.energy)])
    hora2 <- as.character(tarde$hour[match(max(tarde$norm.energy),tarde$norm.energy)])
    hora3 <- as.character(noche$hour[match(max(noche$norm.energy),noche$norm.energy)])
    hora4 <- as.character(norm.median$hour[match(max(norm.median$norm.energy),norm.median$norm.energy)])
    hora5 <- ifelse(as.numeric(hora4)<=5 & as.numeric(hora4)>=2,'am de la madrugada',ifelse(as.numeric(hora4)>5 & as.numeric(hora4) <=11,'am  de la manana',ifelse(as.numeric(hora4)>=12 & as.numeric(hora4) <=18,"tarde","noche")))
    
    
    #Storing in file
    count.plot <- count.plot + 1
    count.plot.list[[count.plot]] = plot.norm.median
    count.plot.name[[count.plot]] = 'plot_norm_median'
    
    text.list[[count.plot]] <- paste("Durante la madrugada (2am - 5am) su consumo mas alto ocurre a las",hora0,"am",". Durante la manana (5am-11am) su consumo mas alto es las",hora1,"de la manana. Durante la tarde (12pm-18pm) su consumo mas alto es a las",hora2,"pm del dia. Durante la noche (de las 19 horas a la 1 am), su consumo mas alto ocurre a las",hora3,"de la noche. En promedio, su consumo mas alto ocurre durante las",hora4,"de la",hora5)

  } else {}

################# Section 4: Day of the week Analysis
# NOTE: Doing it only for houses
  if (length(unique(house$house_Energy)) > 30) {
    day.week.analysis <- eday.analysis.house(house)
    unique.days.analysis <- unique(day.week.analysis[c('day.id.num','day.id')])
    
    day.week.stats <- aggregate(day.week.analysis$norm.energy,by=list(day.week.analysis$day.id),FUN=mean) %>% mutate(day.id=Group.1,Energy=x)
    day.week.stats <- merge(day.week.stats,unique.days.analysis,by=c('day.id'))
    day.week.stats <- day.week.stats[order(day.week.stats$day.id.num),]
    day.week.stats$day.id.esp <- ifelse(day.week.stats$day.id=='Monday','Lunes',ifelse(day.week.stats$day.id=='Tuesday','Martes',ifelse(day.week.stats$day.id=='Wednesday','Miercoles',ifelse(day.week.stats$day.id=='Thursday','Jueves',ifelse(day.week.stats$day.id=='Friday','Viernes',ifelse(day.week.stats$day.id=='Saturday','Sabado','Domingo'))))))
    
    #Barplot
    day.of.week.plot <- ggplot(day.week.stats, aes(x=reorder(day.id.esp,day.id.num),y=Energy)) + geom_bar(stat='identity',fill="skyblue1", colour="black") + xlab('Dia de la Semana') + ylab('Energia (Normalizada de 0 a 1)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))
    
    #Arranging text
    dia1 <- as.character(day.week.stats$day.id.esp[match(max(day.week.stats$Energy),day.week.stats$Energy)])
    sub.dia <- subset(day.week.stats,day.week.stats$day.id.esp != dia1)
    dia2 <- as.character(sub.dia$day.id.esp[match(max(sub.dia$Energy),sub.dia$Energy)])
    
    #Storing in file
    count.plot <- count.plot + 1
    count.plot.list[[count.plot]] = day.of.week.plot
    count.plot.name[[count.plot]] = 'day_of_week_plot'
    
    text.list[[count.plot]] <- paste("Los dias cuando usted consume mas energia son los ",dia1," y los ",dia2,". Usted deberia prestas mas atencion en estos dias para poder moderar su consumo!",sep='')
    
  } else {}


################# # Section 5: Fridge Analysis 

# 5.1 Normalized Fridge Energy Consumption

energy.refrigerator <- fenergy.consumption.hour(refrigerator,dump)

norm.mean <- aggregate(energy.refrigerator$norm.energy,list(energy.refrigerator$house.id,energy.refrigerator$hour),FUN=mean,na.rm=TRUE) %>% mutate(house.id=Group.1,hour=Group.2,norm.energy=x) %>% select(house.id,hour,norm.energy)
norm.median <- aggregate(energy.refrigerator$norm.energy,list(energy.refrigerator$house.id,energy.refrigerator$hour),FUN=median,na.rm=TRUE) %>% mutate(house.id=Group.1,hour=Group.2,norm.energy=x) %>% select(house.id,hour,norm.energy)

f.plot.norm.mean<- ggplot(norm.mean,aes(x=hour,y=norm.energy)) + geom_line()
f.plot.norm.median<- ggplot(norm.median,aes(x=hour,y=norm.energy)) + geom_line() + geom_point(size=3)+ xlab('Hora del Dia') + ylab('Energia por Hora (Normalizada de 0 a 1)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))

madrugada <- subset(norm.median,norm.median$hour>=2 & norm.median$hour <=5)
manana <- subset(norm.median,norm.median$hour>=6 & norm.median$hour <=11)
tarde <- subset(norm.median,norm.median$hour>=12 & norm.median$hour <= 18)
noche <- subset(norm.median,norm.median$hour>18 & norm.median$hour <= 23) 
noche <- rbind(noche,subset(norm.median,norm.median$hour<=1))

hora0 <- as.character(madrugada$hour[match(max(madrugada$norm.energy),madrugada$norm.energy)])
hora1 <- as.character(manana$hour[match(max(manana$norm.energy),manana$norm.energy)])
hora2 <- as.character(tarde$hour[match(max(tarde$norm.energy),tarde$norm.energy)])
hora3 <- as.character(noche$hour[match(max(noche$norm.energy),noche$norm.energy)])
hora4 <- as.character(norm.median$hour[match(max(norm.median$norm.energy),norm.median$norm.energy)])
hora5 <- ifelse(as.numeric(hora4)<=5 & as.numeric(hora4)>=2,'am de la madrugada',ifelse(as.numeric(hora4)>5 & as.numeric(hora4) <=11,'am  de la manana',ifelse(as.numeric(hora4)>=12 & as.numeric(hora4) <=18,"tarde","noche")))

#Storing in file
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = f.plot.norm.median
count.plot.name[[count.plot]] = 'fplot_norm_median'

text.list[[count.plot]] <- paste("Con respecto a su refrigerador, durante la madrugada (2am - 5am) su consumo mas alto ocurre a las",hora0,"am",". Durante la manana (5am-11am) su consumo mas alto es las",hora1,"de la manana. Durante la tarde (12pm-18pm) su consumo mas alto es a las",hora2,"pm del dia. Durante la noche (de las 19 horas a la 1 am), su consumo mas alto ocurre a las",hora3,"de la noche. En promedio, el consumo mas alto ocurre durante las",hora4,"de la",hora5)


# 5.2 Percentage Energy Consumption from Fridge

if (length(unique(house$house_Energy)) > 30) {
  fridge_pct <- join(energy.refrigerator,energy.house,by=c("house.id","date","hour"))
  
  if (is.na(unique(fridge_pct$energy)) == FALSE) {
    fridge_pct$fridge_percent <- (fridge_pct$fridge.energy/fridge_pct$energy)*100
    accurate_fridge_pct <- subset(fridge_pct,fridge_pct$fridge_percent<=100)
    pct_aggregation <- aggregate(accurate_fridge_pct$fridge_percent,by=list(accurate_fridge_pct$hour),FUN=mean,na.rm=TRUE) %>% mutate(hour=Group.1,pct_energy=x) %>% select(hour,pct_energy)
    
    pct_aggregation$pct_energy[pct_aggregation$pct_energy == "Inf"] <- 0
    
    fridge_energy_pct <- ggplot(pct_aggregation, aes(x=hour,y=pct_energy)) + geom_bar(stat='identity',fill="lightskyblue1",color="red",width = 0.8) + xlab('Hora del Dia') + ylab('Porcentaje de Consumo del Refrigerador por Hora  (%)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))
    
    #Storing in file
    count.plot <- count.plot + 1
    count.plot.list[[count.plot]] = fridge_energy_pct
    count.plot.name[[count.plot]] = 'fridge_energy_pct'
    
    text.list[[count.plot]] <- paste("En promedio su refrigerador consume el",round(mean(pct_aggregation$pct_energy,na.rm=TRUE))," porciento de la energia total de su hogar o micro-empresa. Usted puede observar que a las",which(pct_aggregation$pct_energy == max(pct_aggregation$pct_energy)),"horas del dia es cuando su refrigerador consume el porcentaje mas alto de la energia del hogar.")
    
  } else {}

} else {}

##########################



# Save plots to jpeg making a separate file for each plot.
for (j in 1:count.plot) {
  plot.name = paste(selected.house,"_",count.plot.name[[j]],sep="")
  name <- substring(plot.name,1,35)
  mypath <- file.path("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports",dump,"Latex/figure",paste(plot.name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(count.plot.list[[j]])
  dev.off()
}

#plot.name = paste(selected.house,"_",count.plot.name[[4]],sep="")
#name <- substring(plot.name,1,35)
#mypath <- file.path("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports",dump,"Latex/figure",paste(plot.name,".jpg",sep = ""))
#jpeg(file=mypath)
#grid.draw(count.plot.list[[4]])
#dev.off()

# Saving text files to file
for (l in 1:count.plot) {
  if(is.na(text.list[[l]]) == FALSE){
    filepath <- file(paste("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports/",dump,"/Latex/",selected.house,"_",count.plot.name[[l]],'.txt',sep=''))
    writeLines(text.list[[l]], filepath)
    close(filepath)
  } else{}
}

} #Finishes for loop at the top








