#Reproducible Research - Assignment 1

#Load the data

data_row<-read.csv("repdata_data_activity/activity.csv")

#Load dplyr library

library(dplyr)

##Process the data, remove NA

data<-filter(data_row,!is.na(steps))

#What is mean total number of steps taken per day?
##Calculate the total number of steps taken per day

sum_steps_day<-
        data %>%
        group_by(date) %>%
        summarize (sum(steps))

colnames(sum_steps_day)<-c('date','sum_steps')

##Make a histogram of the total number of steps taken each day

hist(sum_steps_day$sum_steps, breaks=20,col="lightslateblue", border='white',
     xlab='Total number of steps per day',
     main="Histogram of the total number of steps per day")
box()

##Calculate and report the mean and median of the total number of steps
##  taken per day 

print('Mean of the total number of steps taken per day')
print(mean(sum_steps_day$sum_steps))
print('Median of the total number of steps taken per day')
print(median(sum_steps_day$sum_steps))

#What is the average daily activity pattern?
##Make a time series plot of interval and the average number of steps taken,
##averaged across all days

avg_steps_per_interval<-
        data %>%
        group_by(interval) %>%
        summarize (mean(steps))

colnames(avg_steps_per_interval)<-c('interval','mean_steps')

plot(mean_steps ~ interval, data = avg_steps_per_interval, type = "l", col="deeppink4", lty=6 ,xlab="5-min interval", ylab="average steps", main="Average daily activity pattern")
box()

## Answer the question:
##Which 5-minute interval, on average across all the days in the dataset,
##contains the maximum number of steps?

a<-avg_steps_per_interval$interval[avg_steps_per_interval$mean_steps==max(avg_steps_per_interval$mean_steps)]
print(a)

#Imputing missing values
##Calculate and report the total number of missing values in the dataset

missing<-nrow(data_row)-nrow(data)
print(missing)

##Filling in all of the missing values in the dataset, create a new dataset

mean1<-round(mean(data$steps))
data_new<-data_row
data_new$steps[is.na(data_new$steps)]<-mean1

##Make a histogram of the total number of steps taken each day

sum_steps_day_new<-
        data_new %>%
        group_by(date) %>%
        summarize (sum(steps))

colnames(sum_steps_day_new)<-c('date','sum_steps')

hist(sum_steps_day_new$sum_steps, breaks=20,col="mediumseagreen", border='white',
     xlab='Total number of steps per day',
     main="Histogram of the total number of steps per day")
box()

##Calculate and report the mean and median total number of steps taken per day

print('Mean of the total number of steps taken per day')
print(mean(sum_steps_day_new$sum_steps))
print('Median of the total number of steps taken per day')
print(median(sum_steps_day_new$sum_steps))

##Mean and median differ from the estimates from the first part of the assignment,
##However the impact of imputing missing is low.

##Are there differences in activity patterns between weekdays and weekends?

data_new$date<-as.Date(data_new$date)
library(lubridate)
data_new_w<-mutate(data_new,week=wday(data_new$date))
data_new_w$week[data_new_w$week==2 | data_new_w$week==3 | data_new_w$week==4 | data_new_w$week==5 | data_new_w$week==6]<-"weekday"
data_new_w$week[data_new_w$week==7 | data_new_w$week==1 ]<-"weekend"


## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval
##and the average number of steps taken, averaged across all weekday days or weekend days

avg_steps_per_week <- aggregate(data_new_w$steps, by = list(data_new_w$interval, data_new_w$week), mean)
colnames(avg_steps_per_week)<-c("interval","week","steps") 

data_new_w$week<-as.factor(data_new_w$week) 


par(mfrow=c(1,1))  
with(avg_steps_per_week, plot(steps ~ interval, type="n", main="Activity Patterns between weekends and weekdays"))  
with(avg_steps_per_week[avg_steps_per_week$week == "weekday",],type="l", lines(steps ~ interval, col="chartreuse4"))  
with(avg_steps_per_week[avg_steps_per_week$week == "weekend",],type="l", lines(steps ~ interval, col="indianred1" ))  
legend("topright", lty=c(1,1), col = c("chartreuse4", "indianred1"), legend = c("Weekday", "Weekend"), seg.len=5)
