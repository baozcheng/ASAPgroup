library(plyr)
library(stargazer)
library(plm)
library(ggplot2)





##model 1: no lag
#----------------

mdl1<- OBS_VALUE_sum~Rest_dep + Rest_arr + Cases_dep + Cases_arr + spring + 
  summer+ winter

#Adding a unique id column to provide plm function with valid indexes.

dfseason <- transform(dfseason, 
               route=apply(dfseason[c("country_dep", "country_arr")], 1, 
                        paste, collapse=" to "))


#1. pooled regression
rsltPool1 <- lm (mdl1, data=dfseason)

#2. within regression
rsltWithin1 <-plm(mdl1,data = dfseason, model="within", index = c("route","TIME_PERIOD"))

#3. between regression
rsltBetween1<-plm(mdl1,data = dfseason, model="between",index = c("route","TIME_PERIOD"))

stargazer(rsltPool1,rsltWithin1,rsltBetween1,type="text",intercept.bottom = FALSE)

#4. routes fixed effects
rsltFE.Routes <- 
  plm(mdl1, data = dfseason, 
      index=c("route", "TIME_PERIOD"), model = "within")

#5. routes and month fixed effect: only covid cases data are taken into account
# because the fixed effects interaction capture all differences across countries
# and time periods

mdl2<- OBS_VALUE_sum~Cases_dep + Cases_arr

rsltFE.interaction<-plm(mdl1, data = dfseason, 
                        index=c("route", "TIME_PERIOD"), model = "within",
                        effect="twoways")

#6. random effects regression
rsltRE <- 
  plm(mdl1, data = dfseason, 
      index=c("route", "TIME_PERIOD"), model = "random")


stargazer(rsltFE.Routes,rsltFE.interaction,rsltRE,type="text",intercept.bottom = FALSE)


##model 1: no lag and season dummies
#-----------------------------------

mdl2<- OBS_VALUE_sum~Rest_dep + Rest_arr + Cases_dep + Cases_arr + spring + 
  summer+ winter

#Adding a unique id column to provide plm function with valid indexes.

dfseason <- transform(dfseason, 
                      route=apply(dfseason[c("country_dep", "country_arr")], 1, 
                                  paste, collapse=" to "))


#1. pooled regression
rsltPool1 <- lm (mdl1, data=dfseason)
rsltPool1<-plm(mdl1,)

#2. within regression
rsltWithin1 <-plm(mdl1,data = dfseason, model="within", index = c("route","TIME_PERIOD"))

#3. between regression
rsltBetween1<-plm(mdl1,data = dfseason, model="between",index = c("route","TIME_PERIOD"))

stargazer(rsltPool1,rsltWithin1,rsltBetween1,type="text",intercept.bottom = FALSE)

#4. routes fixed effects
rsltFE.Routes <- 
  plm(mdl1, data = dfseason, 
      index=c("route", "TIME_PERIOD"), model = "within")

#5. routes and month fixed effect: only covid cases data are taken into account
# because the fixed effects interaction capture all differences across countries
# and time periods

mdl2<- OBS_VALUE_sum~Cases_dep + Cases_arr

rsltFE.interaction<-plm(mdl2, data = dfseason, 
                        index=c("route", "TIME_PERIOD"), model = "within",
                        effect="twoways")

#6. random effects regression
rsltRE <- 
  plm(mdl2, data = dfseason, 
      index=c("route", "TIME_PERIOD"), model = "random")


stargazer(rsltFE.Routes,rsltFE.interaction,rsltRE,type="text",intercept.bottom = FALSE)



##model 2: with lag
#------------------

mdl1<- OBS_VALUE_sum~Rest_dep + Rest_arr + Cases_dep + Cases_arr + spring + 
  summer+ winter

#Adding a unique id column to provide plm function with valid indexes.

dflag_season <- transform(dflag_season, 
                      route=apply(dflag_season[c("country_dep", "country_arr")], 1, 
                                  paste, collapse=" to "))


#1. pooled regression

rsltPool2<-plm(mdl1, data = dflag_season, model = "pooling",index = c("route","TIME_PERIOD"))

#2. within regression
rsltWithin2 <-plm(mdl1,data = dflag_season, model="within", index = c("route","TIME_PERIOD"))

#3. between regression
rsltBetween2<-plm(mdl1,data = dflag_season, model="between",index = c("route","TIME_PERIOD"))

stargazer(rsltPool2,rsltWithin2,rsltBetween2,type="text",intercept.bottom = FALSE)

#4. routes fixed effects
rsltFE2.Routes <- 
  plm(mdl1, data = dflag_season, 
      index=c("route", "TIME_PERIOD"), model = "within")

#5. routes and month fixed effect: only covid cases data are taken into account
# because the fixed effects interaction capture all differences across countries
# and time periods

mdl2<- OBS_VALUE_sum~Cases_dep + Cases_arr

rsltFE2.interaction<-plm(mdl2, data = dflag_season, 
                        index=c("route", "TIME_PERIOD"), model = "within",
                        effect="twoways")

#6. random effects regression for routes
rsltRE2.route <- 
  plm(mdl1, data = dflag_season, 
      index=c("route", "TIME_PERIOD"), model = "random")


#7. random effects regression with interaction between routes and time period
rsltRE2.interaction <- 
  plm(mdl2, data = dflag_season, 
      index=c("route", "TIME_PERIOD"), model = "random",
      effect="twoways")


stargazer(rsltFE2.Routes,rsltFE2.interaction,rsltRE2.route,rsltRE2.interaction,type="text",intercept.bottom = FALSE)



##Testing the Normal Distribution Assumption

covid_dep_hist<-ggplot(dfseason,aes(Cases_dep))
covid_dep_hist + geom_histogram() + labs(x="covid cases",y="frequency")

covid_arr_hist<-ggplot(dfseason,aes(Cases_arr))
covid_arr_hist + geom_histogram() + labs(x="covid cases",y="frequency")

qqplot.covid_dep_hist<-qplot(sample=dfseason$Cases_dep,stat="qq")
qqplot.covid_dep_hist

qqplot.covid_arr_hist<-qplot(sample=dfseason$Cases_arr,stat="qq")
qqplot.covid_arr_hist

shapiro.test(dfseason$Cases_dep)
shapiro.test(dfseason$Cases_arr)


##Testing for Heteroskedasticity + Accounting for it in the SE


par(mfrow=c(2,3))
plot(rsltPool2)
plot(rsltFE2.Routes)
plot(rsltFE2.interaction)
plot(rsltRE2.route)
plot(rsltRE2.interaction)


lmtest::bptest(rsltPool2)
lmtest::bptest(rsltFE2.Routes)
lmtest::bptest(rsltFE2.interaction)
lmtest::bptest(rsltRE2.route)
lmtest::bptest(rsltRE2.interaction)

stargazer(full_model,type="text")
#calculating different robust std errors:
seWhitePool<-sqrt(diag(vcovHC(rsltPool2,type="HC0")))
seWhiteFE2.Routes<-sqrt(diag(vcovHC(rsltFE2.Routes,type="HC0")))
seWhiteFE2.interaction<-sqrt(diag(vcovHC(rsltFE2.interaction,type="HC0")))
seWhiteRE2.route<-sqrt(diag(vcovHC(rsltRE2.route,type="HC0")))
seWhiteRE2.interaction<-sqrt(diag(vcovHC(rsltRE2.interaction,type="HC0")))


seClustPool<-sqrt(diag(vcovHC(rsltPool2,cluster="time")))
seClustFE2.Routes<-sqrt(diag(vcovHC(rsltFE2.Routes,cluster="time")))
seClustFE2.interaction<-sqrt(diag(vcovHC(rsltFE2.interaction,cluster="time")))
seClustRE2.route<-sqrt(diag(vcovHC(rsltRE2.route,cluster="time")))
seClustRE2.interaction<-sqrt(diag(vcovHC(rsltRE2.interaction,cluster="time")))


#to view all different std errors with stargazer:
str(dflag_season)
#a.White std errors
stargazer(rsltPool2,rsltFE2.Routes,rsltFE2.interaction,rsltRE2.route,
          rsltRE2.interaction, se=list(seWhitePool,seWhiteFE2.Routes,
                                       seWhiteFE2.interaction,
                                       seWhiteRE2.route,
                                       seWhiteRE2.interaction),
          type="text")

#b. Cluster std errors on time periods
stargazer(rsltPool2,rsltFE2.Routes,rsltFE2.interaction,rsltRE2.route,
          rsltRE2.interaction, se=list(seClustPool,seClustFE2.Routes,
                                       seClustFE2.interaction,
                                       seClustRE2.route,
                                       seWhiteRE2.route,seClustRE2.interaction),
          type="text")



#Testing for errors normal distribution and linear relationship

par(mfrow = c(2, 2))
plot(rsltPool2)

par(mfrow = c(2, 2))
plot(rsltFE2.Routes)

#Tests FE VS pooled

pFtest(rsltFE2.Routes, rsltPool2)
pFtest(rsltFE2.interaction, rsltPool2)

#Tests FE VS RE

phtest(rsltFE2.Routes,rsltRE2.route)
phtest(rsltFE2.interaction,rsltRE2.interaction)

summary(fixef(rsltFE2.interaction,type = "dmean"))


#Plot n of passengers on covid cases

ggplot(dflag_season,aes(x=Cases_dep,y=OBS_VALUE_sum))+geom_point(aes(),size=2) +
xlab("Covid19 infections in the arrival counry")+ylab("Number of passangers")+
geom_smooth(method='lm',se=FALSE,colour="red")

stargazer(dflag_season,type="text")
