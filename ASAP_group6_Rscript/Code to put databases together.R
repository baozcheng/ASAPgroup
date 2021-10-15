library(tidyverse)
rst_data<-read.csv("Restrictions data.csv")
covid_data<-read.csv("covid data.csv")
flights_data<-read.csv("flights data\\flights data.csv")

#formatting the flights data
names(flights_data) <- substring(names(flights_data),2,8)
colnames(flights_data)[1]<-"country"

colnames(flights_data)[2:19]<-format(paste(substr(colnames(flights_data)[2:19],1,4),substr(colnames(flights_data)[2:19],6,7),sep="-"))

flights2<-data.frame(t(flights_data))
colnames(flights2)<-paste("Total flights landed",flights2[1,],sep=".")
flights2<-flights2[-c(1),]
flights2$Date<-rownames(flights2)
rownames(flights2)<-c(1:length(flights2[,1]))

#formatting the restrictions data
rst2<-rst_data
rst2$Date<-paste(substr(rst_data$Month.and.year,4,7),substr(rst_data$Month.and.year,1,2),sep="-")
rst2$Month.and.year<-NULL
colnames(rst2)[1:30]<-paste("Restrictions status",colnames(rst2)[1:30],sep='.')

#formatting the COVID data
covid2<-covid_data
y<-data.frame(strsplit(covid_data$Country..Month...Year,"-"))

covid2$Date<-paste(c(y[3,]),c(y[2,]),sep="-")
covid2$Country<-c(y[1,])

covid2$Country..Month...Year<-NULL
colnames(covid2)[1]<-"COVID 19 cases monthly mean"

covid2<-reshape(covid2,idvar="Date",timevar="Country",direction="wide")


#merging dataframes:
covid_and_flights<-merge(covid2,flights2)
final_db<-merge(covid_and_flights,rst2)

write.csv(final_db,"Final database.csv")
