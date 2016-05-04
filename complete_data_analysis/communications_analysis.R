###### Communications analysis taking the outfile as an input

library(RPostgreSQL)
library(DBI)
library(sqldf)
library(RSQLite)
library(RMySQL)
library(data.table)
library(lubridate)


#Set up connection to server 
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv = PostgreSQL(), user = "flexbox", dbname = "flexbox_db_server", host = "162.243.146.213",  password = "flexbox",port = "5432")
dbClearResult(dbListResults(con)[[1]]) # Close result set . Reset connection first 

#Retrieve Data
since_february <- dbSendQuery(con,statement=paste("SELECT * from network_tests where datetime > '02-01-2016'"));
since.feb.dt <- as.data.table(fetch(since_february,n=-1))

ping.dt <- since.feb.dt[parm=='ping']
  #Adding date parameters
  ping.dt$second <- second(ping.dt$datetime)
  ping.dt$minute <- minute(ping.dt$datetime)
  ping.dt$hour<- hour(ping.dt$datetime)
  ping.dt$day<- day(ping.dt$datetime)
  ping.dt$month<- month(ping.dt$datetime)
  ping.dt$date<- as.Date(ping.dt$datetime)

d1 <- ping.dt[hostname=='flxbxD1']
d17 <- ping.dt[hostname=='flxbxD17']
d24 <- ping.dt[hostname=='flxbxD24']
d26 <- ping.dt[hostname=='flxbxD26']
d28 <- ping.dt[hostname=='flxbxD28']


#### Begin Analysis

# 1. Distribution of Max of ping per household and across all devices
# 2. Distribution of Avg of ping per household and across all devices
# 3. Plot all the cdfs together for ping and avg and max

# 1. Distribution of max of ping per household and across all devices

avg.latency.density <- ggplot(data=as.data.frame(ping.dt), aes(x=avg, fill=hostname)) + geom_density(alpha=.3) + xlim(150, 1500) + xlab("Average Latency (Milliseconds)") + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) #+ theme(legend.position="none")
max.latency.density <- ggplot(data=as.data.frame(ping.dt), aes(x=max, fill=hostname)) + geom_density(alpha=.3) + xlim(150, 4000) + xlab("Maximum Latency (Milliseconds)") + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))

# CDF for Average Latency Values

par(mfrow=c(1,1))
plot(ecdf(d1$avg),xlim=c(0,3000),col='red',main='',xlab='')
par(new=TRUE)
plot(ecdf(d17$avg),xlim=c(0,3000),col='khaki2',main='',xlab='')
par(new=TRUE)
plot(ecdf(d24$avg),xlim=c(0,3000),col='mediumspringgreen',main='',xlab='')
par(new=TRUE)
plot(ecdf(d26$avg),xlim=c(0,3000),col='lightskyblue',main='',xlab='')
par(new=TRUE)
plot(ecdf(d28$avg),xlim=c(0,3000),col='mediumorchid1',main='Latency CDF Five Units',xlab='Latency (Milliseconds)')
legend(2000,0.6,c("A1","A17","A24","A26","A28") ,lty=c(1,1), lwd=c(2,2),col=c("red","khaki2","mediumspringgreen","lightskyblue","mediumorchid1"),pt.cex = 0.3)

# CDF for Maximum Latency Values

par(mfrow=c(1,1))
plot(ecdf(d1$max),xlim=c(0,3000),col='red',main='',xlab='')
par(new=TRUE)
plot(ecdf(d17$max),xlim=c(0,3000),col='khaki2',main='',xlab='')
par(new=TRUE)
plot(ecdf(d24$max),xlim=c(0,3000),col='mediumspringgreen',main='',xlab='')
par(new=TRUE)
plot(ecdf(d26$max),xlim=c(0,3000),col='lightskyblue',main='',xlab='')
par(new=TRUE)
plot(ecdf(d28$max),xlim=c(0,3000),col='mediumorchid1',main='Latency CDF Five Units',xlab='Latency (Milliseconds)')
legend(2000,0.6,c("A1","A17","A24","A26","A28") ,lty=c(1,1), lwd=c(2,2),col=c("red","khaki2","mediumspringgreen","lightskyblue","mediumorchid1"),pt.cex = 0.3)



# Testing how different the distributions are from each other

# Kolmigorov 
ks.test(d17$avg, d24$avg, alternative = c("two.sided"))
ks.test(d17$avg, d26$avg, alternative = c("two.sided"))
ks.test(d17$avg, d28$avg, alternative = c("two.sided"))
ks.test(d17$avg, d1$avg, alternative = c("two.sided"))

ks.test(d24$avg, d26$avg, alternative = c("two.sided"))
ks.test(d24$avg, d28$avg, alternative = c("two.sided"))
ks.test(d24$avg, d1$avg, alternative = c("two.sided"))

ks.test(d26$avg, d28$avg, alternative = c("two.sided"))
ks.test(d26$avg, d1$avg, alternative = c("two.sided"))

ks.test(d28$avg, d1$avg, alternative = c("two.sided"))

#Wilcox test
wilcox.test(d17$avg, d24$avg,  exact=FALSE,correct=FALSE)
wilcox.test(d17$avg, d26$avg,  exact=FALSE,correct=FALSE)
wilcox.test(d17$avg, d28$avg,  exact=FALSE,correct=FALSE)
wilcox.test(d17$avg, d1$avg,  exact=FALSE,correct=FALSE)

wilcox.test(d24$avg, d26$avg,  exact=FALSE,correct=FALSE)
wilcox.test(d24$avg, d28$avg,  exact=FALSE,correct=FALSE)
wilcox.test(d24$avg, d1$avg,  exact=FALSE,correct=FALSE)

wilcox.test(d26$avg, d28$avg,  exact=FALSE,correct=FALSE)
wilcox.test(d26$avg, d1$avg,  exact=FALSE,correct=FALSE)

wilcox.test(d28$avg, d1$avg,  exact=FALSE,correct=FALSE)









# 4. Time of day: avg of ping$avg by minute of day and hour of day


#Average Latency by Hour

ping.dt.hour <- aggregate(as.data.frame(ping.dt$avg),by=list(ping.dt$hostname,ping.dt$hour),FUN=mean,na.rm=TRUE)
names(ping.dt.hour)[1] <- 'id'
names(ping.dt.hour)[2] <- 'hour'
names(ping.dt.hour)[3] <- 'avg'

ping.hour.all <- aggregate(as.data.frame(ping.dt$avg),by=list(ping.dt$hour),FUN=mean,na.rm=TRUE)
names(ping.hour.all)[1] <- 'hour'
names(ping.hour.all)[2] <- 'avg'
ping.hour.all$id <- 'mean'
ping.hour.all <- ping.hour.all[,c('id','hour','avg')]

pings <- rbind(ping.dt.hour,ping.hour.all)

ggplot(pings) + geom_line(aes(hour,avg,color=id)) + xlab('Hour') + ylab('Average Latency by Hour (Milliseconds)') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.position="none")


#Max Latency by Hour

ping.dt.hour <- aggregate(as.data.frame(ping.dt$max),by=list(ping.dt$hostname,ping.dt$hour),FUN=mean,na.rm=TRUE)
names(ping.dt.hour)[1] <- 'id'
names(ping.dt.hour)[2] <- 'hour'
names(ping.dt.hour)[3] <- 'max'

ping.hour.all <- aggregate(as.data.frame(ping.dt$max),by=list(ping.dt$hour),FUN=mean,na.rm=TRUE)
names(ping.hour.all)[1] <- 'hour'
names(ping.hour.all)[2] <- 'max'
ping.hour.all$id <- 'mean'
ping.hour.all <- ping.hour.all[,c('id','hour','max')]

pings.max <- rbind(ping.dt.hour,ping.hour.all)

legend(2000,0.6,c("A1","A17","A24","A26","A28") ,lty=c(1,1), lwd=c(2,2),col=c("red","khaki2","mediumspringgreen","lightskyblue","mediumorchid1"),pt.cex = 0.3)


ggplot(pings.max,aes(x=factor(hour),y=max,fill=factor(id),alpha=0.75), color=factor(id)) + stat_summary(fun.y=mean,position=position_dodge(),geom="bar") + xlab('Hour') + ylab('Average Latency by Hour') + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + theme(legend.position="none")



# 5. Dropped packets: Discontinuity in seqn, per day per house, how many times was the seqn broken. 
    # When the sequence breaks calculate the difference in the sequence numbers is the number of lost connections
    # Estimate this on average per hour per house


id.list <- c('flxbxD1','flxbxD17','flxbxD24','flxbxD26','flxbxD28')
hour.event <- data.frame(matrix(NA, nrow = 5, ncol = 2)) %>% mutate(id=X1,mean=X2) %>% select(id,mean)

for (i in 1:length(id.list)) {
  
dt.host <- ping.dt[hostname==id.list[i]]

dt.host <- dt.host[order(-datetime),]

dt.host$lag.seqn <- lag(dt.host$seqn,1) 
dt.host$lag.diff <- dt.host$lag.seqn - dt.host$seqn
date.vec <- unique(dt.host$date)


for (j in 1:length(unique(dt.host$date))) {
  
  date.sub <- subset(dt.host,dt.host$date == date.vec[j])
  date.sub.diff  <- subset(date.sub,date.sub$lag.diff != 1 & date.sub$lag.diff >0 ,na.rm=TRUE)
  date.sub.diff$e.duration <- date.sub.diff$lag.diff * 30
  date.sub.diff$event <- 1
  date.sub.diff$hostname.id <- id.list[i]
  date.sub.diff$lag.date <- lag(date.sub.diff$datetime,1) #lag date
  
  if (dim(date.sub.diff)[1] != 0) {
  sum.events.hour <- aggregate(date.sub.diff$event,by=list(date.sub.diff$hour),FUN=sum,na.rm=TRUE) %>% mutate(hour=Group.1,events=x) 
  sum.events.hour <- sum.events.hour[,c('hour','events')]
  } else {}
  
  if (j ==1 ){
    sum.events.hour.df <- sum.events.hour
    
    if (dim(date.sub.diff)[1] != 0) {
     e.duration.df <- as.data.frame(date.sub.diff)[,c('e.duration','hostname.id')]
     differences <- as.data.frame(date.sub.diff$lag.date - date.sub.diff$datetime)
     names(differences)[1] <- 'time.diff'
     
    } else{}
    
  } else {
    sum.events.hour.df <- rbind(sum.events.hour.df,sum.events.hour)
    
    if (dim(date.sub.diff)[1] != 0) {
      to.bind <- as.data.frame(date.sub.diff)[,c('e.duration','hostname.id')]
      e.duration.df <- rbind(e.duration.df,to.bind)
      differences.new <- as.data.frame(date.sub.diff$lag.date - date.sub.diff$datetime)
      names(differences.new)[1] <- 'time.diff'
      differences <- rbind(differences,differences.new)
    } else{}
    
  }
}

sum.events.hour.df$id <- id.list[[i]]
hour.event$id[i] <- id.list[[i]]
hour.event$mean[i] <- mean(sum.events.hour.df$events)

if (i ==1 ){
  all.hosts <- sum.events.hour.df
  e.duration.dt <- e.duration.df
  differences.dt <- differences
} else {
  all.hosts <- rbind(all.hosts,sum.events.hour.df)
  e.duration.dt <- rbind(e.duration.dt,e.duration.df)
  differences.dt <- rbind(differences.dt,differences)
}
}




# Plotting the probability mass function

par(mfrow=c(1,1))
plot(subset(all.hosts,all.hosts$id == 'flxbxD1')$events,dpois(x=subset(all.hosts,all.hosts$id == 'flxbxD1')$events,lambda=1.369501),col='red',xlim=c(0,13),ylim=c(0,0.5),xlab='',ylab='')
par(new=TRUE)
plot(subset(all.hosts,all.hosts$id == 'flxbxD17')$events,dpois(x=subset(all.hosts,all.hosts$id == 'flxbxD17')$events,lambda=1.407407),col='khaki2',xlim=c(0,13),ylim=c(0,0.5),xlab='',ylab='')
par(new=TRUE)
plot(subset(all.hosts,all.hosts$id == 'flxbxD24')$events,dpois(x=subset(all.hosts,all.hosts$id == 'flxbxD24')$events,lambda=1.705600),col='mediumspringgreen',xlim=c(0,13),ylim=c(0,0.5),xlab='',ylab='')
par(new=TRUE)
plot(subset(all.hosts,all.hosts$id == 'flxbxD26')$events,dpois(x=subset(all.hosts,all.hosts$id == 'flxbxD26')$events,lambda=1.556765),col='lightskyblue',xlim=c(0,13),ylim=c(0,0.5),xlab='',ylab='')
par(new=TRUE)
plot(subset(all.hosts,all.hosts$id == 'flxbxD28')$events,dpois(x=subset(all.hosts,all.hosts$id == 'flxbxD28')$events,lambda=1.556765),col='mediumorchid1',xlim=c(0,13),ylim=c(0,0.5),xlab='k = Events (Dropped Packets/Hour)',ylab='P(x=k)')

legend(8,0.5,c(paste("A1 lambda:",1.37),paste("A17 lambda:",1.41),paste("A24 lambda:",1.71),paste("A26 lambda:",1.56),paste("A28 labmda:",1.56)) ,lty=c(1,1), lwd=c(2,2),col=c("red","khaki2","mediumspringgreen","lightskyblue","mediumorchid1"),cex=0.8,pt.cex = 0.8)


# Statistics for the Duration of Events

hist(e.duration.dt$e.duration,n=2000,prob=TRUE,xlab='Event Duration (Seconds)',main='') # Without removing outliers the distribution is skewed to the right

  #removing outliers

  e.duration.dt.sub <- subset(e.duration.dt,e.duration.dt$e.duration < 2000)
  
  hist(e.duration.dt.sub$e.duration,n=2000,prob=TRUE,xlab='No Outliers: Event Duration (Seconds)',main='') # Without removing outliers the distribution is skewed to the right
  x.est <- fitdistr(e.duration.dt.sub$e.duration, "exponential")$estimate
  curve(dexp(x, rate = x.est), add = TRUE, col = "red", lwd = 2)


  # Calculating statistics for the time between events

  differences.dt$time.diff <- as.numeric(differences.dt$time.diff)
  hist(differences.dt$time.diff,n=2000,xlab='Time Interval Between Events (Seconds)',main='')

  differences.sub <- subset(differences.dt,differences.dt$time.diff<600)
  
  hist(differences.sub$time.diff,prob=FALSE,xlab='No Outliers: Time Interval Between Events (Seconds)',main='') # Without removing outliers the distribution is skewed to the right
  x.est <- fitdistr(differences.sub$time.diff, "exponential")$estimate
  curve(dexp(x, rate = x.est), add = TRUE, col = "red", lwd = 2)
  
  








# PLotting the duration of events

x.gen <- rexp(4191, rate = 0.003735994)


hist(e.duration.dt$e.duration,n=2000,prob=TRUE,xlim=c(0,2000))
library(MASS)
library(Matching)
x.est <- fitdistr(e.duration.dt$e.duration, "exponential")
x.est <- fitdistr(e.duration.dt$e.duration, "exponential")$estimate

ks.test(e.duration.dt$e.duration, "pexp", x.est$estimate)

curve(dexp(x, rate = x.est), add = TRUE, col = "red", lwd = 2)

pepe <- unique(e.duration.dt$e.duration)

ks.boot(e.duration.dt$e.duration, x.gen, nboots=1000, alternative="two.sided")


plot(e.duration.dt$e.duration, x.gen)




















plot(1:length(subset(all.hosts,all.hosts$id == 'flxbxD1')$events),dexp(subset(all.hosts,all.hosts$id == 'flxbxD1')$events, 1.369501))

plot(subset(all.hosts,all.hosts$id == 'flxbxD1')$events,dpois(x=subset(all.hosts,all.hosts$id == 'flxbxD1')$events,lambda=1.369501),col='red',xlim=c(0,13),ylim=c(0,0.5),xlab='',ylab='')



plot(subset(all.hosts,all.hosts$id == 'flxbxD1')$events,dpois(x=subset(all.hosts,all.hosts$id == 'flxbxD1')$events,lambda=1.369501))

ggplot(all.hosts, aes(all.hosts$events)) + geom_point(aes(y=dpois(subset(all.hosts,all.hosts$id == 'flxbxD1')$events, 1.369501)), colour="red") + geom_line(aes(y=dpois(subset(all.hosts,all.hosts$id == 'flxbxD1')$events, 1.369501)), colour="red") 

ggplot(subset(all.hosts,all.hosts$id == 'flxbxD1'), aes(subset(all.hosts,all.hosts$id == 'flxbxD1')$events)) + geom_point(aes(y=dpois(subset(all.hosts,all.hosts$id == 'flxbxD1')$events, 1.369501)), colour="red") + geom_line(aes(y=dpois(subset(all.hosts,all.hosts$id == 'flxbxD1')$events, 1.369501)), colour="red") 

ggplot(data.frame(x=c(0:10)), aes(x)) + stat_function(geom="point", n=11, fun=dpois, args=list(1))

#Bands min and avg are in seconds 



x.gen <- rexp(1000, rate = 3)
hist(x.gen, prob = TRUE)

library(MASS)
x.est <- fitdistr(x.gen, "exponential")$estimate

curve(dexp(x, rate = x.est), add = TRUE, col = "red", lwd = 2)


