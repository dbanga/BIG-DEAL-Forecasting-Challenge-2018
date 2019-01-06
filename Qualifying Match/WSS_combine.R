library(lubridate)

data=read.csv('temp_hist_1.csv')

data= subset(data, data$Station.ID==11)

data$Date=NULL
data$Station.ID=NULL

data_train=subset(data, data$Year %in% c(2005, 2006))
data_test=subset(data, data$Year==2007)
data_Fcst=subset(data, data$Year==2008)

summary(data_train)

data_train$Year=NULL
data_test$Year=NULL
data_Fcst$Year=NULL

data_train_Y=data_train['Load']
data_train_X=data_train[,1:14]

lm.fit=lm(data_train_Y$Load~ ,data=data_train_X)
summary(lm.fit)


data_train_Y$Load_1=predict(lm.fit, newdata = data_train_X)

data_train_Y$error=abs(data_train_Y$Load-data_train_Y$Load_1)
data_train_Y$error=data_train_Y$error/data_train_Y$Load

mean(data_train_Y$error)

