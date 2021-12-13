setwd("I:/5th sem/DA-Labs")
apr<-read.csv("uber-raw-data-apr14.csv")
jul<-read.csv("uber-raw-data-jul14.csv")
jun<-read.csv("uber-raw-data-jun14.csv")
may<-read.csv("uber-raw-data-may14.csv")

data_14<-rbind(apr,may,jun,jul)

library("ggplot2")
library("ggplot2")
#install.packages("ggthemes")
library("ggthemes")
library("ggthemes")
#install.packages("dplyr")
library("dplyr")
library("dplyr")
#install.packages("lubridate")
library("lubridate")
library("lubridate")
#install.packages("scales")
library("scales")
library("scales")


head(data_14)
str(data_14)
summary(data_14)


data_14$Date.Time<-as.POSIXct(data_14$Date.Time,format="%m/%d/%Y %H:%M:%S")
data_14$Time<-format(as.POSIXct(data_14$Date.Time,format="%m/%d/%Y %H:%M:%S"),format = "%H:%M:%S")
data_14$day<-format(day(data_14$Date.Time))
data_14$month<-format(month(data_14$Date.Time),label=TRUE)
data_14$dayofweek<-format(wday(data_14$Date.Time),label=TRUE)
data_14$hour<-format(hour(data_14$Date.Time),label=TRUE)
data_14$minute<-format(minute(data_14$Date.Time),label=TRUE)
data_14$second<-format(second(data_14$Date.Time),label=TRUE)


View(data_14)

hour_data<-data_14 %>%
  group_by(hour) %>%
  summarise(Total=n())

library("DT")
datatable(hour_data)


ggplot(hour_data,aes(hour,Total))+
  geom_bar(stat="identity",fill="black",color="blue")+
  ggtitle("Trips by Hour")+
  theme(legend.position="none")+
  scale_y_continuous(labels=comma)


month_hour_data<-data_14 %>%
  group_by(month,hour) %>%
  summarise(Total=n())

datatable(month_hour_data)

ggplot(month_hour_data,aes(hour,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by month and Hour")+
  scale_y_continuous(labels=comma)


jul_hour<-data_14 %>%
  group_by(hour,month) %>%
  filter(month==7) %>%
  summarise(Total=n())

ggplot(jul_hour,aes(hour,Total,fill=hour))+
  geom_bar(stat="identity")+
  ggtitle("Trips by hour and month of july")+
  scale_y_continuous(labels=comma)
#around 60k rides on 5 pm


apr_hour<-data_14 %>%
  group_by(hour,month) %>%
  filter(month==4) %>%
  summarise(Total=n())

ggplot(apr_hour,aes(hour,Total,fill=hour))+
  geom_bar(stat="identity")+
  ggtitle("Trips by hour and month of april")+
  scale_y_continuous(labels=comma)
#around 45k rides on 5 pm


may_hour<-data_14 %>%
  group_by(hour,month) %>%
  filter(month==5) %>%
  summarise(Total=n())

ggplot(may_hour,aes(hour,Total,fill=hour))+
  geom_bar(stat="identity")+
  ggtitle("Trips by hour and month of may")+
  scale_y_continuous(labels=comma)
#around 50k rides on 5pm on may


jun_hour<-data_14 %>%
  group_by(hour,month) %>%
  filter(month==6) %>%
  summarise(Total=n())

ggplot(jun_hour,aes(hour,Total,fill=hour))+
  geom_bar(stat="identity")+
  ggtitle("Trips by hour and month of june")+
  scale_y_continuous(labels=comma)

#around 50k rides on 5pm june


day_data<-data_14 %>%
  group_by(day) %>%
  summarise(Total=n())

datatable(day_data)


ggplot(day_data,aes(day,Total))+
  geom_bar(stat = "identity",fill="blue",color="black")+
  ggtitle("Trips by day")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

month_day_data<-data_14 %>%
  group_by(day,month) %>%
  summarise(Total=n())

datatable(month_day_data)

ggplot(month_day_data,aes(day,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by day and month")+
  scale_y_continuous(labels=comma)

jul_day<-data_14 %>%
  group_by(day,month) %>%
  filter(month==7 ) %>%
  summarise(Total=n())

ggplot(jul_day,aes(day,Total,fill=day))+
         geom_bar(stat="identity")+
         ggtitle("Trips by day of month july")+
         scale_y_continuous(labels=comma)


month_data<-data_14 %>%
  group_by(month) %>%
  summarise(Total=n())

datatable(month_data)

#jul month has highest kind of rides and april has lowest rides find why this happens 
#ask the company whether it is due marketing or internaleffect

ggplot(month_data,aes(month,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips of month")+
  scale_y_continuous(labels = comma)


month_weekday_data<-data_14 %>%
  group_by(month,dayofweek) %>%
  summarise(Total=n())


datatable(month_weekday_data)
 
ggplot(month_weekday_data,aes(month,Total,fill=dayofweek))+
  geom_bar(stat="identity")+
  ggtitle("Trips of month based on weekdays")+
  scale_y_continuous(labels=comma)

weekday_data<-data_14 %>%
  group_by(dayofweek) %>%
  summarise(Total=n())
datatable(weekday_data)

ggplot(weekday_data,aes(dayofweek,Total,fill=dayofweek))+
  geom_bar(stat="identity")+
  ggtitle("Trips by day of week")+
  scale_y_continuous()



#analysis of bases

ggplot(data_14,aes(Base))+
  geom_bar(fill="darkred")+
  scale_y_continuous()+
  ggtitle("Trips by Bases")


ggplot(data_14,aes(Base,fill=month))+
  geom_bar(position = "dodge")+
  scale_y_continuous()+
  ggtitle("Trips by Bases and months")


ggplot(data_14,aes(Base,fill=dayofweek))+
  geom_bar(position = "dodge2")+
  scale_y_continuous()+
  ggtitle("Trips by Bases and dayofweek")

min_lat<-40.5774
max_lat<-40.9176
min_long<--74.15
max_long<- -73.7004


ggplot(data_14,aes(x=Lon,y=Lat))+
  geom_point(size=1)+
  scale_x_continuous(limits = c(min_long,max_long))+
  scale_y_continuous(limits = c(min_lat,max_lat))+
  theme_map()+
  ggtitle("NYC (lat-lon chart)MAP BASED ON RIDES BETWEEN APRIL-JULY by BASE")
