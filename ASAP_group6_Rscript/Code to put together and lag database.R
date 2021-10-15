dfDK<-read.csv("country data output/Data DK Routes.csv")
dfBE<-read.csv("country data output/Data BE Routes.csv")
dfAT<-read.csv("country data output/Data AT Routes.csv")
dfBG<-read.csv("country data output/Data BG Routes.csv")
dfCH<-read.csv("country data output/Data CH Routes.csv")
dfCY<-read.csv("country data output/Data CY Routes.csv")
dfCZ<-read.csv("country data output/Data CZ Routes.csv")
dfDE<-read.csv("country data output/Data DE Routes.csv")
dfEE<-read.csv("country data output/Data EE Routes.csv")
dfEL<-read.csv("country data output/Data EL Routes.csv")
dfES<-read.csv("country data output/Data ES Routes.csv")
dfFI<-read.csv("country data output/Data FI Routes.csv")
dfFR<-read.csv("country data output/Data FR Routes.csv")
dfHR<-read.csv("country data output/Data HR Routes.csv")
dfHU<-read.csv("country data output/Data HU Routes.csv")
dfIE<-read.csv("country data output/Data IE Routes.csv")
dfIS<-read.csv("country data output/Data IS Routes.csv")
dfIT<-read.csv("country data output/Data IT Routes.csv")
dfLT<-read.csv("country data output/Data LT Routes.csv")
dfLU<-read.csv("country data output/Data LU Routes.csv")
dfLV<-read.csv("country data output/Data LV Routes.csv")
dfMT<-read.csv("country data output/Data MT Routes.csv")
dfNL<-read.csv("country data output/Data NL Routes.csv")
dfNO<-read.csv("country data output/Data NO Routes.csv")
dfPL<-read.csv("country data output/Data PL Routes.csv")
dfRO<-read.csv("country data output/Data RO Routes.csv")
dfSE<-read.csv("country data output/Data SE Routes.csv")
dfSI<-read.csv("country data output/Data SI Routes.csv")
dfSK<-read.csv("country data output/Data SK Routes.csv")






#compiling data from different countries on top pf each other
dfAll<-rbind(dfDK,dfBE,dfAT,dfBG,dfCH,dfCY,dfCZ,dfDE,dfEE,dfEL,dfIS,dfFI,dfFR,dfHU,dfHR,dfIE,dfIS,dfIT,dfLT,dfLU,dfLV,dfMT,dfNL,dfNO,dfPL,dfRO,dfSI,dfSE,dfSK)

#aggregating the values per month, country of departure and country of arrival
library(dplyr)
dfAll2<-dfAll %>%
  select(TIME_PERIOD, country_dep,country_arr, OBS_VALUE,Rest_dep,Rest_arr,Cases_dep,Cases_arr)%>%
  group_by(TIME_PERIOD, country_dep,country_arr) %>% 
  summarise_each(funs(sum,mean),OBS_VALUE,Rest_dep,Rest_arr,Cases_dep,Cases_arr)%>%
  select(TIME_PERIOD, country_dep,country_arr, OBS_VALUE_sum,Rest_dep_mean,Rest_arr_mean,Cases_dep_mean,Cases_arr_mean)%>%
  rename(Rest_dep=Rest_dep_mean,Rest_arr=Rest_arr_mean,Cases_dep=Cases_dep_mean,Cases_arr=Cases_arr_mean)

colnames(dfAll2)<- c("TIME_PERIOD","country_dep","country_arr","OBS_VALUE_sum","Rest_dep", "Rest_arr","Cases_dep","Cases_arr")

#laging restrictions and covid cases


dflag<-dfAll2 %>% 
  arrange(country_dep,country_arr)

dflag <- 
  dflag %>%
  group_by(TIME_PERIOD,country_dep,country_arr) %>%
  mutate(OBS_VALUE_sum.lag = dplyr::lag(OBS_VALUE_sum, n = 1, default = NA),
         Rest_dep.lag= dplyr::lag(Rest_dep, n = 1, default = NA),
         Rest_arr.lag=dplyr::lag(Rest_arr, n = 1, default = NA),
        Cases_dep.lag= dplyr::lag(Cases_dep, n = 1, default = NA),
        Cases_arr.lag = dplyr::lag(Cases_arr, n = 1, default = NA)                   
                              )
dflag$OBS_VALUE_sum.lag<-ifelse(dflag$TIME_PERIOD=="2019-01",NA,dflag$OBS_VALUE_sum.lag)
dflag$Rest_dep.lag<-ifelse(dflag$TIME_PERIOD=="2019-01",NA,dflag$Rest_dep.lag)
dflag$Rest_arr.lag<-ifelse(dflag$TIME_PERIOD=="2019-01",NA,dflag$Rest_arr.lag)
dflag$Cases_dep.lag<-ifelse(dflag$TIME_PERIOD=="2019-01",NA,dflag$Cases_dep.lag)
dflag$Cases_arr.lag<-ifelse(dflag$TIME_PERIOD=="2019-01",NA,dflag$Cases_arr.lag)

write.csv(dflag,"Final database.csv")



