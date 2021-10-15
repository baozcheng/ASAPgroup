library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)

df0<-read.csv("owid-covid-raw-data.csv")

EU_EEA<-c("Belgium","Spain","Hungary","Slovakia",
          "Bulgaria","France","Malta","Finland",
          "Czechia","Croatia","Netherlands","Sweden",
          "Denmark","Italy","Austria","Germany",
          "Cyprus","Poland","Iceland","Estonia",
          "Latvia","Portugal","Liechtenstein","Ireland",
          "Lithuania","Romania","Norway","Greece",
          "Luxembourg","Slovenia", "Switzerland")

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

covid_data<- aggregate(covid_data, 
                       by = list(covid_data$country.month.year),
                       FUN = mean)

colnames(covid_data)[1]<-"Country, Month & Year"
covid_data$country.month.year<-NULL
colnames(covid_data)[2]<-"COVID-19 cases monthly mean"



names(covid_data)[1]<-paste("CMY")

EU_EEA_iso2 <- read.csv2("EEA iso2 codes.csv", sep = ",", header = FALSE)
colnames(EU_EEA_iso2) <- c("country", "c_code")


# Seperate information 
covid_cases2 <- covid_data %>% 
  separate(CMY, into = c("country", "month", "year"), sep = "-")

# Create date column
cols <- c('year', 'month')
covid_cases2$date <- apply( covid_cases2[ , cols ] , 1 , paste , collapse = "-" )
colnames(covid_cases2) <- c("country", "month", "year", "mean_cases", "date")

cov_cases$mean_cases <- as.numeric(as.character(cov_cases$mean_cases))

# Select order of columns 
cov_cases <- select(covid_cases2, "date", "country", "mean_cases")

# Match country code 
cov_cases<-merge(cov_cases, EU_EEA_iso2, by="country")

write.csv(covid_data,"C:\\Users\\leona\\OneDrive\\Documents\\RSM\\Block 1\\Advanced statistics and programming\\Groupwork\\covid data.csv",row.names =FALSE)