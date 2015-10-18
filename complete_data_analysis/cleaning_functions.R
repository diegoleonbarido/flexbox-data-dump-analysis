#Cleaning functions

#Check Unique dates
date.check <- function(refrigerator,inside,house,ambient) {

v1_refrigerator <- tail(refrigerator$date,n=1) - head(refrigerator$date,n=1)
v2_refrigerator <- as.character(head(refrigerator$date,n=1))
v3_refrigerator <- as.character(tail(refrigerator$date,n=1))

v1_inside <- tail(inside$date,n=1) - head(inside$date,n=1)
v2_inside <- as.character(head(inside$date,n=1))
v3_inside <- as.character(tail(inside$date,n=1))

v1_house <- tail(house$date,n=1) - head(house$date,n=1)
v2_house <- as.character(head(house$date,n=1))
v3_house <- as.character(tail(house$date,n=1))

v1_ambient <- tail(ambient$date,n=1) - head(ambient$date,n=1)
v2_ambient <- as.character(head(ambient$date,n=1))
v3_ambient <- as.character(tail(ambient$date,n=1))

table <- matrix(c(v1_refrigerator,v2_refrigerator,v3_refrigerator,v1_inside,v2_inside,v3_inside,v1_house,v2_house,v3_house,v1_ambient,v2_ambient,v3_ambient),ncol=3,byrow=TRUE)
colnames(table) <- c("N.Days","First Date","Last Date")
rownames(table) <- c("E.Fridge","Inside Temp","E.House","Ambient")

table <- as.table(table)
return(table)
}

table