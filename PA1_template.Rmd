---
title: "Courseera"
author: "Gaurav Bansal"
date: "Saturday, July 12, 2014"

---

---
After removing the missing values
---
```{r, echo=TRUE}
activity <- read.csv("activity.csv")
activity_na <- na.omit(activity)
#calculating steps taken per day and ploting the histogram
steps_each_day <- aggregate(steps~date,activity_na,sum)
barplot(steps_each_day$steps,names.arg=steps_each_day$date,ylab="Number of steps",xlab="Date")
```

```{r, echo=TRUE}
#calculating mean of steps taken per day
mean_steps_each_day <- aggregate(steps~date,activity_na,mean)
mean_steps_each_day
```

```{r, echo=TRUE}
#calculating median of steps taken per day
median_steps_each_day <- aggregate(steps~date,activity_na,median)
median_steps_each_day
```

```{r, echo=TRUE}
#calculating steps taken per interval
steps_each_interval <- aggregate(steps~interval,activity_na,mean)
plot(steps_each_interval,type="l",main="Average nummber of steps per interval")

#calculating the 5 minnute interval with maximum number of steps
steps_each_interval[which.max(steps_each_interval$steps),]
```

---
Replacing NA values with the mean of the particular interval
---
```{r, echo=TRUE}
#merging the data sets with mean of each step per interval to activity file with NA values
activity_merge <- merge(activity, steps_each_interval,by="interval", all=TRUE)
colnames(activity_merge)[2] <- c("steps")

#replacing NA values
activity_merge$steps <- ifelse(is.na(activity_merge$steps), activity_merge$steps.y, activity_merge$steps)

activity_new <- activity_merge[,1:3]
```

```{r, echo=TRUE}
#calculating steps taken per day and ploting the histogram after imputing the missing values
steps_each_day_new <- aggregate(steps~date,activity_new,sum)
barplot(steps_each_day_new$steps,names.arg=steps_each_day_new$date,ylab="Number of steps",xlab="Date")
```

```{r, echo=TRUE}
#calculating mean of steps taken per day after imputing the missing values
mean_steps_each_day_new <- aggregate(steps~date,activity_new,mean)
mean_steps_each_day_new
```

```{r, echo=TRUE}
#calculating median of steps taken per day after imputing the missing values
median_steps_each_day_new <- aggregate(steps~date,activity_new,median)
median_steps_each_day_new
```

```{r, echo=TRUE}
#including days of the week to the dataset
activity_new$day <- weekdays(as.Date(activity_new$date))
activity_new$day <- ifelse(activity_new$day==c("Friday","Saturday","Sunday"),'Weekend','Weekday')
weekday <- subset(activity_new,activity_new$day=="Weekday")
weekend <- subset(activity_new,activity_new$day=="Weekend")
weekday_agg <- aggregate(steps~interval,weekday,mean)
weekday_agg$day <- "weekday"
weekend_agg <- aggregate(steps~interval,weekend,mean)
weekend_agg$day <- "weekend"
activity_agg <- rbind(weekday_agg,weekend_agg)
#making the plot for activity patterns of weekdays and weekends
library(lattice)
xyplot(steps~interval|day,activity_agg,type='l',layout=c(1,2),ylab="Number of Steps")
```