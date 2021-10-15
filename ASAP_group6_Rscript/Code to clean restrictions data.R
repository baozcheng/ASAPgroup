rm(list = ls())
library(tidyverse)

rst_raw<-read.csv("C:/Users/leona/OneDrive/Documents/RSM/Block 1/Advanced statistics and programming/Groupwork/raw_internationaltravel_restrictions.csv")

EU_EEA_iso2<-read.csv("C:\\Users\\leona\\OneDrive\\Documents\\RSM\\Block 1\\Advanced statistics and programming\\Groupwork\\EEA iso2 codes.csv",header = FALSE)
EU_EEA_iso2[1,1]<-"Belgium"
colnames(EU_EEA_iso2)<-c("Country","Country_code")

rst0<-rst_raw%>%
  filter(country_name %in% EU_EEA_iso2$Country)


#rst0<-read.csv("C:\\Users\\leona\\OneDrive\\Documents\\RSM\\Block 1\\Advanced statistics and programming\\Groupwork\\Rst_Europe.csv")
rst0$X.1<-NULL
rst0$X<-NULL


names(rst0) <- substring(names(rst0),2,10)
colnames(rst0)[1]<-"country_code"
colnames(rst0)[2]<-"country_name"
rst0<-rst0[1:which(colnames(rst0) == "30Jun2021")]

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
colnames(rst0)[3:549]<-format(as.Date(paste(substr(colnames(rst0)[3:549], 1, 2),substr(colnames(rst0)[3:549], 3, 5),substr(colnames(rst0)[3:549], 6, 9),sep="/"),format="%d/%b/%Y"))
rst0$country_code<-NULL

x<-as.POSIXct(colnames(rst0)[2:548])
month <- strftime(x, "%m")
year <- strftime(x, "%Y")


month.year<-c(paste(year,month,sep="-"))
# rst2<-rbind(rst0,month.year)
# rst_data<-as.data.frame(t(rst2))
# colnames(rst_data)<-rst2$country_name
# rst_data<-rst_data[-c(1),]

rst3<-as.data.frame(t(rst0))
colnames(rst3)<-rst0$country_name
rst3<-rst3[-c(1),]
rst3<-as.data.frame(lapply(rst3,as.numeric))
rst3$month.year<-as.data.frame(month.year)
colnames(rst3)[31] <- "Month and year"

typeof(rst3[5,12])

 # getmode <- function(v) {
 #   uniqv <- unique(v)
 #   uniqv[which.max(tabulate(match(v, uniqv)))]
 # }

rst_data<- aggregate(rst3[1:30],by = list(month.year),FUN = "mean",na.rm=TRUE)


colnames(rst_data)[1]<-"Date"

long_rst<-gather(rst_data,key="Country",value=RestirctLvl,Austria:Sweden)


long_rst<-merge(long_rst, EU_EEA_iso2, by="Country")

write.csv(rst_data,"C:\\Users\\leona\\OneDrive\\Documents\\RSM\\Block 1\\Advanced statistics and programming\\Groupwork\\restrictions data.csv",row.names =FALSE)
