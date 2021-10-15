dfroutesNL<-read.csv('C:/Users/leona/OneDrive/Documents/RSM/Block 1/Advanced statistics and programming/Groupwork/estat_avia_par_nl_filtered_en.csv')

dfroutesNL$country_dep<-substr(dfroutesNL$airp_pr,1,2)
dfroutesNL$country_arr<-substr(dfroutesNL$airp_pr,9,10)

EU_EEA_iso2<-read.csv("C:\\Users\\leona\\OneDrive\\Documents\\RSM\\Block 1\\Advanced statistics and programming\\Groupwork\\EEA iso2 codes.csv",header = FALSE)
EU_EEA_iso2[1,1]<-"Belgium"
colnames(EU_EEA_iso2)<-c("Country","Country_code")

df2<-dfroutesNL%>%
  filter(country_arr %in% EU_EEA_iso2$Country_code)

df2<-df2%>%
  filter(country_arr!=country_dep)

df2 <- select(df2, "airp_pr", "TIME_PERIOD", "OBS_VALUE", "country_dep", 
                  "country_arr")
