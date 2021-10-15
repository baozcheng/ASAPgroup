library(tidyverse)

df0<-read.csv("C:\\Users\\leona\\OneDrive\\Documents\\RSM\\Block 1\\Advanced statistics and programming\\Groupwork\\owid-covid-raw-data.csv")

EU_EEA<-c("Belgium","Spain","Hungary","Slovakia",
          "Bulgaria","France","Malta","Finland",
          "Czechia","Croatia","Netherlands","Sweden",
          "Denmark","Italy","Austria","Germany",
          "Cyprus","Poland","Iceland","Estonia",
          "Latvia","Portugal","Liechtenstein","Ireland",
          "Lithuania","Romania","Norway","Greece",
          "Luxembourg","Slovenia")

df1<-df0 %>%
  filter(location %in% EU_EEA)

df2<-df1 %>%
  select(location,date,total_cases)


x <- as.POSIXct(df2$date)
month <- strftime(x, "%m")
year <- strftime(x, "%Y")
df2$country.month.year<-paste(df2$location,month,year, sep="-")

covid_data<-df2 %>%
  select(country.month.year,total_cases)

covid_data<- aggregate(covid_data,by = list(df3$country.month.year),FUN = mean)

colnames(covid_data)[1]<-"Country, Month & Year"
covid_data$country.month.year<-NULL
colnames(covid_data)[2]<-"COVID-19 cases monthly mean"

write.csv(covid_data,"C:\\Users\\leona\\OneDrive\\Documents\\RSM\\Block 1\\Advanced statistics and programming\\Groupwork\\covid data.csv",row.names =FALSE)
