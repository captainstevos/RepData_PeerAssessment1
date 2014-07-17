# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

setwd("C:/Users/558966/Documents/Coursera")
activity <- read.csv("activity.csv")
## What is mean total number of steps taken per day?
daily.activity <- tapply(activity$steps, list(activity$date),sum, na.rm=TRUE)
hist(daily.activity, xlab = "Total Steps per Day",ylab = "Frequency", main = "Steps per Day", col="red")
dev.copy(png,'plot1.png', width = 480, height = 480)
dev.off()
mean(daily.activity, na.rm=TRUE)
median(daily.activity, na.rm=TRUE)
## What is the average daily activity pattern?
interval.activity <- sapply(with(activity, split(steps, interval)), mean, na.rm=TRUE)

plot(as.table(interval.activity), type="l",xlab="Interval", ylab="Average Steps", main="Average Steps by Interval", col="blue")
dev.copy(png,'plot2.png', width = 480, height = 480)
dev.off()
interval.activity[which.max(interval.activity)]
##Imputing missing values
sum(is.na(activity))

new.activity <- activity
for(i in 0:max(new.activity$interval)){
  new.activity$steps[new.activity$interval == i & is.na(new.activity$steps)]  <- mean(activity$steps[new.activity$interval == i],na.rm=T)
}

new.daily.activity <- tapply(new.activity$steps, list(new.activity$date),sum)
hist(new.daily.activity, xlab = "Total Steps per Day",ylab = "Frequency", main = "Steps per Day", col="green")
dev.copy(png,'plot3.png', width = 480, height = 480)
dev.off()
mean(new.daily.activity)
median(new.daily.activity, na.rm=TRUE)

## Are there differences in activity patterns between weekdays and weekends?
newer.activity <- activity
newer.activity$date<- as.Date(newer.activity$date, "%Y-%m-%d")
newer.activity$Day <- weekdays(newer.activity$date)

newer.activity$Day.Type <- ifelse(newer.activity$Day == "Monday" | newer.activity$Day == "Tuesday" | newer.activity$Day == "Wednesday" | newer.activity$Day == "Thursday" | newer.activity$Day == "Friday","Weekday","Weekend")
weekend <- aggregate(steps ~ interval, data = newer.activity[newer.activity$Day.Type == "Weekend",], mean, na.rm = TRUE)
weekday <- aggregate(steps ~ interval, data = newer.activity[newer.activity$Day.Type == "Weekday",], mean, na.rm=TRUE)

par(mfrow = c(2, 1))
plot(weekend$steps, type = "l", main = "Average weekend Steps", xlab="Interval", ylab = "Average Steps", ylim = c(0, 250))
plot(weekday$steps, type = "l", main = "Average weekday Steps", xlab="Interval", ylab= "Average Steps",ylim = c(0, 250))
dev.copy(png,'plot4.png', width = 480, height = 480)
dev.off()
