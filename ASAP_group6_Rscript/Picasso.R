
library(tidyverse)

df_R_dep<-merge(dfroutesNL, long_rst, by.x = c("country_dep","TIME_PERIOD"),by.y=c("Country_code","Date"))
df_R_dep<- df_R_dep%>% select(country_dep, TIME_PERIOD, airp_pr, OBS_VALUE,country_arr, RestirctLvl)

names(df_R_dep)[names(df_R_dep) == 'RestirctLvl'] <- 'Rest_dep'


df_R_arr<-merge(df_R_dep, long_rst, by.x = c("country_arr","TIME_PERIOD"),by.y=c("Country_code","Date"))
df_R_arr<-df_R_arr%>% select(country_dep, TIME_PERIOD, airp_pr, OBS_VALUE,country_arr,Rest_dep, RestirctLvl)

names(df_R_arr)[names(df_R_arr) == 'RestirctLvl'] <- 'Rest_arr'


mean_covid<-read.csv("C:/Users/leona/OneDrive/Documents/RSM/Block 1/Advanced statistics and programming/Groupwork/Mean_Covid.csv")

df_C_dep<-merge(df_R_arr, mean_covid, by.x = c("country_dep","TIME_PERIOD"),by.y=c("c_code","date"))
df_C_dep<-df_C_dep%>% select(country_dep, TIME_PERIOD, airp_pr, OBS_VALUE,country_arr, Rest_dep, Rest_arr, mean_cases)

names(df_C_dep)[names(df_C_dep) == 'mean_cases'] <- 'Cases_dep'


df_C_arr<-merge(df_C_dep, mean_covid, by.x = c("country_arr","TIME_PERIOD"),by.y=c("c_code","date"))
df_C_arr<-df_C_arr%>% select(country_dep, TIME_PERIOD, airp_pr, OBS_VALUE,country_arr, Rest_dep, Rest_arr, Cases_dep, mean_cases)

names(df_C_arr)[names(df_C_arr) == 'mean_cases'] <- 'Cases_arr'


Final_Table_NL <- df_C_arr
write.csv(Final_Table_NL,"C:/Users/leona/OneDrive/Documents/RSM/Block 1/Advanced statistics and programming/Groupwork/Data NL routes.csv")
