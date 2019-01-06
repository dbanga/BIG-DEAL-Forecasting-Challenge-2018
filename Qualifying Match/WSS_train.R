library(lubridate)

temp_data=read.csv('temp_average.csv')
load_data=read.csv('load_hist.csv')

temp_data$Date <- mdy(temp_data$Date)
temp_data$year=year(temp_data$Date)
temp_data$Month=month(temp_data$Date)


load_data$Date <- mdy(load_data$Date)
load_data$year=year(load_data$Date)
load_data$Month=month(load_data$Date)



temp_data_train=subset(temp_data, temp_data$year %in% c(2005, 2006))
temp_data_test=subset(temp_data, temp_data$year==2007)
temp_data_Fcst=subset(temp_data, temp_data$year==2008)

load_data_train=subset(load_data, load_data$year %in% c(2005, 2006))
load_data_test=subset(load_data, load_data$year==2007)

temp_data_train= subset(temp_data_train, temp_data_train$Station.ID==7)
data_train=merge(temp_data_train,load_data_train , by= c('Month', 'year','Hour','Date'), all= F,sort = F)


data_train$Temp2=data_train$Temperature**2
data_train$Temp3=data_train$Temperature**3
data_train$week=wday(data_train$Date)

data_train$TempHour=data_train$Temperature*data_train$Hour
data_train$Temp2Hour=data_train$Temp2*data_train$Hour
data_train$Temp3Hour=data_train$Temp3*data_train$Hour
data_train$TempMonth=data_train$Temperature*data_train$Month
data_train$Temp2Month=data_train$Temp2*data_train$Month
data_train$Temp3Month=data_train$Temp3*data_train$Month
data_train$HourWeek=data_train$Hour*data_train$week
data_train$year=NULL
data_train$Date=NULL
data_train$Station.ID=NULL

temp_data_test= subset(temp_data_test, temp_data_test$Station.ID==7)
data_test=merge(temp_data_test,load_data_test , by= c('Month', 'year','Hour','Date'), all= F,sort = F)

data_test$Temp2=data_test$Temperature**2
data_test$Temp3=data_test$Temperature**3
data_test$week=wday(data_test$Date)

data_test$TempHour=data_test$Temperature*data_test$Hour
data_test$Temp2Hour=data_test$Temp2*data_test$Hour
data_test$Temp3Hour=data_test$Temp3*data_test$Hour
data_test$TempMonth=data_test$Temperature*data_test$Month
data_test$Temp2Month=data_test$Temp2*data_test$Month
data_test$Temp3Month=data_test$Temp3*data_test$Month
data_test$HourWeek=data_test$Hour*data_test$week
data_test$year=NULL
data_test$Date=NULL
data_test$Station.ID=NULL


data_train=data_train[,c(4,1:3,6:15,5)]


lm.fit=lm(Load~., 
          data=data_train)
summary(lm.fit)


data_test$Load1= predict(lm.fit, newdata = data_test)

data_test$Error=abs(data_test$Load-data_test$Load1)/data_test$Load
mean(data_test$Error)*100