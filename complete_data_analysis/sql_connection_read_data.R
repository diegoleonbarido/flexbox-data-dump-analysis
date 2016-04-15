library(RPostgreSQL)
library(DBI)
library(sqldf)

library(RSQLite)
library(RMySQL)

drv <- dbDriver("PostgreSQL")


con <- dbConnect(drv = PostgreSQL(), user = "flexbox", dbname = "flexbox_db_server", host = "162.243.146.213",  password = "flexbox",port = "5432")


rs <- dbSendQuery(con,statement=paste("SELECT * from ambient order by datetime desc limit 5"));

abpril <- dbSendQuery(con,statement=paste("SELECT * from ambient where hostname = 'flxbxD28' and datetime > '03-09-2016' and datetime < '03-11-2016'  order by datetime desc limit 5"));

df <- fetch(abpril,n=-1)




