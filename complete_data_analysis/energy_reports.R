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
library(regex)

#Function libraries
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/date_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/merging_binding_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/read_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/time_series_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/cleaning_data_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/hourly_plots_functions.R')
source('/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_consumption_functions.R')


###### NOTE
# Section 1: Reading data
# Section 2: Neighbor Comparison
# Section 3: Hour of the Day
# Section 4: Day of the week analysis
# Section 5: Temperature correlations



###############  Section 1: Reading data (specify houses, specify dump)

flexlist <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15','A16','A17','A18','A19','A20','A21','A22','A23','A24','A25','A26','A27','A28','A29')
dump <- 'DUMP4'

data.list.houses <- read.data.all(dump,flexlist) 
cluster.house <- data.list.houses[[5]]


# Iterate

for (i in 1:length(flexlist)) {

selected.house <- flexlist[i]  #We will iterate through households in the for loop
data_list <- read.data(dump,flexlist[i]) 

inside <- data_list[[1]]
ambient <- data_list[[2]]
refrigerator <- data_list[[3]]
switch <- data_list[[4]]
house <- data_list[[5]]

#Dates 
dates_data_frame <- date.data.frame(ambient,inside,refrigerator,switch,house) 
ambient <- dates_data_frame[[1]]
inside <- dates_data_frame[[2]]
switch <- dates_data_frame[[3]]
house <- dates_data_frame[[4]]
refrigerator <- dates_data_frame[[5]]

# Iterating lists and plots
count.plot.list <- list()
count.plot.name <- list()
text.list <- list()
count.plot <- 0

# Turning warnings off
options(warn=-1)


############### Section 2: Neighbor Comparison

e.neighbor <- neighbor.comparison(cluster.house)

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

texta <- paste("Dentro de todas las casas y micro-empresas del estudio usted es parte del grupo de consumo energetico",vecino1,". De todas las casas y micro-empresas que están en el estudio usted gasta mas que el grupo de consumo",word1," y un poco menos que otras casas y micro-empresas dentro de el grupo",word2,". Usted podria utilizar su energia mas eficientemente.")
textb <- paste("Dentro de todas las casas y micro-empresas del estudio usted es parte del grupo de consumo energetico",vecino1,". Usted es parte del grupo que consumo mas energia dentro del programa. Tiene que ser mas consciente de las horas y dias cuando consume mas energia y prestar atencion a su consumo. En el futuro, utilizaremos un modem para que usted pueda verificar su consumo mas frecuentemente, y asi administrar mejor su energia.")
textc <- paste("Dentro de todas las casas y micro-empresas del estudio usted es parte del grupo de consumo energetico",vecino1,". Usted es parte del grupo que consumo menos energia dentro del programa. Aunque usted consume menos que otras personas, todavia tiene oportunidad para consumir menos y continuar prestando atencion a su consumo. En el futuro, utilizaremos un modem para que usted pueda verificar su consumo mas frecuentemente, y asi administrar mejor su energia.")

real.text <- ifelse(vecino1=='Medio' |vecino1=='Alto',texta,ifelse(vecino1=='Bajo',textc,textb))

text.list[[count.plot]] <- real.text


################ Section 3: Hour of the day

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

text.list[[count.plot]] <- paste("Durante la madrugada (2am - 5am) su consumo mas alto ocurre a las",hora0,"am",". Durante la manana (5am-11am) su consumo mas alto es las",hora1,"de la manana. Durante la tarde (12pm-18pm) su consumo mas alto es a las",hora2,"pm del dia. Durante la noche (de las 19 horas a la 1 am), su consumo mas alto ocurre a las",hora3,"de la noche. En promedio, su cosumo mas alto ocurre durante las",hora4,"de la",hora5)


################# Section 4: Day of the week Analysis

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





# Section 5: Temperature correlations
merged.data.list.clean <- merge.bind.clean(ambient,refrigerator,inside,house,switch) 

##### Correlation Plots 

plot2 <- ggplot(merged.data.list.clean[[2]],aes(x=average_hr_ambient, y=average_hr_power,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Consumo Energético Refrigerador") + ylab(expression(paste("Energía (Wh)"))) + xlab(expression(paste("Temperatura Ambiente",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1)) + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.position="none")
plot3 <- ggplot(merged.data.list.clean[[3]],aes(x=average_hr_ambient, y=average_hr_hpower,colour=hour)) + geom_point(size=2) + scale_colour_gradientn(colours=rainbow(3)) + labs(title="Consumo Energético Casa") + ylab(expression(paste("Energía (Wh)"))) + xlab(expression(paste("Temperatura Ambiente",degree,"C"))) + stat_smooth(method = "lm", se=TRUE, color="blue", aes(group=1)) + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + guides(fill=FALSE)
correlaciones <- grid.arrange(plot2,plot3,ncol=2,nrow=1)


corr.var1 <- cor.test(merged.data.list.clean[[2]]$average_hr_ambient,merged.data.list.clean[[2]]$average_hr_power,method="spearman")$estimate

strength.corr1 <- ifelse(abs(corr.var1) > 0 & abs(corr.var1) <= 0.19,'muy debil',ifelse(abs(corr.var1) >.19 & abs(corr.var1) <= 0.39,'debil',ifelse(abs(corr.var1) >0.39 & abs(corr.var1) <= 0.59,'moderada',ifelse(abs(corr.var1) >0.59 & abs(corr.var1) <= 0.79,'fuerte','muy fuerte'))))
sign.corr1 <- ifelse(corr.var<0,'negativa','positiva')

int1 <- paste("La relacion entre la temperatura del dia y el consumo energetico de su refrigerador es ",strength.corr1," y ",sign.corr1,".")

corr.var2 <- cor.test(merged.data.list.clean[[3]]$average_hr_ambient,merged.data.list.clean[[3]]$average_hr_hpower,method="spearman")$estimate

strength.corr2 <- ifelse(abs(corr.var2) > 0 & abs(corr.var2) <= 0.19,'muy debil',ifelse(abs(corr.var2) >.19 & abs(corr.var2) <= 0.39,'debil',ifelse(abs(corr.var2) >0.39 & abs(corr.var2) <= 0.59,'moderada',ifelse(abs(corr.var2) >0.59 & abs(corr.var2) <= 0.79,'fuerte','muy fuerte'))))
sign.corr2 <- ifelse(corr.var2<0,'negativa','positiva')

int2 <- paste("La relacion entre la temperatura del dia y el consumo energetico de su casa es ",strength.corr," y ",sign.corr1,".",sep="")
int3 <- "Una relacion fuerte y positiva significa que el calor afecta el consumo energetico de gran manera. Una relacion moderada y positiva, tambien significa que el calor afecta el consumo energetico. Una relacion negativa significa que el calor tiene un efecto de reducir su consumo energetico (solamente porque usted apaga los equipos cuando hace mas calor). Una relacion debil o muy debil señala que no hay mucho impacto entre la temperatura y el consumo energetico.) "

text.to.add <- paste(int1,int2,int3)

#Storing in file
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = correlaciones
count.plot.name[[count.plot]] = 'correlaciones'

text.list[[count.plot]] <- text.to.add


# Save plots to jpeg making a separate file for each plot.
for (j in 1:3) {
  plot.name = paste(selected.house,"_",count.plot.name[[j]],sep="")
  name <- substring(plot.name,1,35)
  mypath <- file.path("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports/",dump,"/",paste(plot.name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(count.plot.list[[j]])
  dev.off()
}

plot.name = paste(selected.house,"_",count.plot.name[[4]],sep="")
name <- substring(plot.name,1,35)
mypath <- file.path("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports/",dump,"/",paste(plot.name,".jpg",sep = ""))
jpeg(file=mypath)
grid.draw(count.plot.list[[4]])
dev.off()


# Saving text files to file
for (l in 1:4) {
  filepath <- file(paste("/Users/Diego/Desktop/Projects_Code/flexbox-data-dump-analysis/complete_data_analysis/energy_reports/",dump,"/",selected.house,"_",count.plot.name[[l]],'.txt',sep=''))
  writeLines(text.list[[l]], filepath)
  close(filepath)
}


} #Finishes for loop at the top








