setwd('C:/Qualifying Match/Final match')

data=read.csv("Temp+Load.csv")
data$Year=NULL

library(lubridate)

data$Date= mdy(data$Date)
data$year=year(data$Date)
data$Month=month(data$Date)
data$week=wday(data$Date)
data$HM=data$Hour*data$Month

data_train=subset(data, data$year %in% c(2005,2006,2007))
data_test=subset(data, data$year==2008)

lm.fit=lm(data_train$temp7 ~Trend+Hour+Month+week+HM, data=data_train)
summary(lm.fit)

data_test$Temp=predict(lm.fit, newdata=data_test)

#write.csv(data_test,"Peak.CSV")
#library(pROC)
#roc_obj <- roc(data_test$Peak, data_test$Peak1)
#auc(roc_obj)

data_test$error=abs(data_test$temp7-data_test$Temp)/data_test$temp7
mean(data_test$error)

data_train1=subset(data, data$year %in% c(2005,2006,2007,2008))
data_test1=subset(data, data$year==2009)

lm.fit1=lm(data_train1$temp7 ~Trend+Hour+Month+week+HM, data=data_train1)
summary(lm.fit1)

data_test1$temp7=predict(lm.fit, newdata=data_test1)

write.csv(data_test1,"2009temp.CSV")


### Predicting Load for 2009

data_load=read.csv("Temp+Load_2009.csv")
data_load$Year=NULL


library(lubridate)

data_load$Date= mdy(data_load$Date)
data_load$year=year(data_load$Date)
data_load$Month=month(data_load$Date)
data_load$week=wday(data_load$Date)
data_load$HM=data_load$Hour*data_load$Month
data_load$T2=data_load$temp7**2
data_load$T3=data_load$temp7**3
data_load$TH=data_load$temp7*data_load$Hour
data_load$T2H=data_load$T2*data_load$Hour
data_load$T3H=data_load$T3*data_load$Hour
data_load$TM=data_load$temp7*data_load$Month
data_load$T2M=data_load$T2*data_load$Month
data_load$T3M=data_load$T3*data_load$Month


#library(zoo)

#data_load$TL1=lag(data_load$temp7, -2)

data_train3=subset(data_load, data_load$year %in% c(2005,2006,2007))
data_test3=subset(data_load, data_load$year==2008)


lm.fit3=lm(data_train3$Z1~Trend+Hour+Month+week+temp7+HM+T2+T3+TH+T2H+T3H+TM+T2M+T3H, data=data_train3)
summary(lm.fit3)


data_test3$Z2=predict(lm.fit3, newdata=data_test3)

data_test3$error=abs(data_test3$Z1-data_test3$Z2)/data_test$Z1
mean(data_test3$error)


data_train4=subset(data_load, data_load$year %in% c(2005,2006,2007,2008))
data_test4=subset(data_load, data_load$year==2009)


lm.fit4=lm(data_train4$Z1~Trend+Hour+Month+week+temp7+HM+T2+T3+TH+T2H+T3H+TM+T2M+T3H, data=data_train4)
summary(lm.fit4)

data_test4$Z1=predict(lm.fit4, newdata=data_test4)

write.csv(data_test4,"2009Load.CSV")


## Predicting peak for 2009

data_Peak=read.csv("Temp+Load+Peak_2009.csv")
data_Peak$Year=NULL

data_Peak$Date= mdy(data_Peak$Date)
data_Peak$year=year(data_Peak$Date)
data_Peak$Month=month(data_Peak$Date)
data_Peak$week=wday(data_Peak$Date)
data_Peak$HM=data_Peak$Hour*data_Peak$Month
data_Peak$T2=data_Peak$temp7**2
data_Peak$T3=data_Peak$temp7**3
data_Peak$TH=data_Peak$temp7*data_Peak$Hour
data_Peak$T2H=data_Peak$T2*data_Peak$Hour
data_Peak$T3H=data_Peak$T3*data_Peak$Hour
data_Peak$TM=data_Peak$temp7*data_Peak$Month
data_Peak$T2M=data_Peak$T2*data_Peak$Month
data_Peak$T3M=data_Peak$T3*data_Peak$Month


data_train5=subset(data_Peak, data_Peak$year %in% c(2005,2006,2007))
data_test5=subset(data_Peak, data_Peak$year==2008)


glm.fit5=glm(data_train5$Peak ~Z1+Trend+Hour+Month+week+temp7+HM+T2+T3+TH+T2H+T3H+TM+T2M+T3H, 
             family = binomial,data=data_train5)
summary(glm.fit5)

data_test5$Peak1=predict(glm.fit5, newdata=data_test5,type = 'response')
data_test5$Peak1=ifelse(data_test5$Peak1>0.1,1,0)

library(pROC)
roc_obj <- roc(data_test5$Peak, data_test5$Peak1)
auc(roc_obj)


data_train6=subset(data_Peak, data_Peak$year %in% c(2005,2006,2007,2008))
data_test6=subset(data_Peak, data_Peak$year==2009)


glm.fit6=glm(data_train6$Peak ~Z1+Trend+Hour+Month+week+temp7+HM+T2+T3+TH+T2H+T3H+TM+T2M+T3H, 
             family = binomial,data=data_train6)
summary(glm.fit6)

data_test6$Peak=predict(glm.fit6, newdata=data_test6,type = 'response')

write.csv(data_test6,"2009Peak.CSV")


data.peak=read.csv('fm_load_hist.csv')

library(lubridate)

data.peak$Date= mdy(data.peak$Date)
data.peak$year=year(data.peak$Date)
data.peak$Month=month(data.peak$Date)
data.peak$week=wday(data.peak$Date)
data.peak$HM=data.peak$Hour*data.peak$Month

data_train7=subset(data.peak, data.peak$year %in% c(2005,2006,2007))
data_test7=subset(data.peak, data.peak$year==2008)

glm.fit7=glm(data_train7$Peak~Trend+Hour+Month+week+HM, family = binomial,data=data_train7)
summary(glm.fit7)

data_test7$Peak1=predict(glm.fit7, newdata=data_test7,type = 'response')

data_test7$Peak1=ifelse(data_test7$Peak1>0.1,1,0)


library(pROC)
roc_obj <- roc(data_test7$Peak, data_test7$Peak1)
auc(roc_obj)

data_train8=subset(data.peak, data.peak$year %in% c(2005,2006,2007,2008))
data_test8=subset(data.peak, data.peak$year==2009)

glm.fit8=glm(data_train8$Peak~Trend+Hour+Month+week+HM, family = binomial,data=data_train8)
summary(glm.fit8)


data_test8$Peak=predict(glm.fit8, newdata=data_test8,type = 'response')

write.csv(data_test8,"Peak1.CSV")

