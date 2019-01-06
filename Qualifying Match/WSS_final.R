#library(lubridate)

temp_data=read.csv('Temp_final_7_7.csv')

temp_data$Date= mdy(temp_data$Date)
temp_data$year=year(temp_data$Date)
temp_data$Month=month(temp_data$Date)
temp_data$week=wday(temp_data$Date)
temp_data$Date=NULL

temp_data$Temp2=temp_data$temp7**2
temp_data$Temp3=temp_data$temp7**3


temp_data=temp_data[,c(1:4,6:15,5)]

temp_data_train=subset(temp_data, temp_data$year %in% c(2005, 2006))
temp_data_test=subset(temp_data, temp_data$year==2007)
temp_data_Fcst=subset(temp_data, temp_data$year==2008)

temp_data_train$Year=NULL
temp_data_test$Year=NULL



lm.fit=lm(temp_data_train$Z1~Trend+Hour+temp7+week+Month+Temp2+Temp3+temp7*Hour+temp7*Month+Temp2*Hour+Temp2*Month+Temp3*Hour+Temp3*week+Hour*week+temp_L1+Temp_l2+Temp_L3+temp_L24+TempL48,
          data=temp_data_train)
summary(lm.fit)


temp_data_test$Load1= predict(lm.fit, newdata = temp_data_test)

temp_data_test$Error=abs(temp_data_test$Z1-temp_data_test$Load1)
temp_data_test$Error=temp_data_test$Error/temp_data_test$Z1

mean(temp_data_test$Error)*100

