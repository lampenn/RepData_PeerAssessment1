---
title: "PA1_template"
author: "Penny Lam"
date: "Monday, August 18, 2014"
output: html_document
---
# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
```{r, echo=TRUE}
## Load data, no formatting required
mydata <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
```{r, echo=TRUE}
## Draw histogram
myresult <- tapply(mydata$steps,mydata$date,sum)
steps <- as.vector(myresult)
hist(steps, main="Histogram of total steps taken per day",
     xlab="Total steps")

## Show mean and median
print("Mean: ")
mean(steps, na.rm=TRUE)
print("Median: ")
median(steps, na.rm=TRUE)
```

## What is the average daily activity pattern?
```{r, echo=TRUE}
## Plot time series of 5-min interval and average steps taken
myresult2 <- tapply(mydata$steps, mydata$interval, mean, na.rm=TRUE)
plot(as.numeric(names(myresult2)), as.vector(myresult2), 
     type="l", main="Average steps per interval", 
     xlab="interval (in min)", ylab="Average steps")

## Show which 5-min interval contains max. number of steps
print("Maximum number of steps:")
names(which.max(myresult2))
```

## Imputing missing values
```{r, echo=TRUE}
## Report total number of missing values in the dataset
print("Total number of missing values in the dataset:")
sum(!complete.cases(mydata))

## Fill in missing values using mean for that time interval
## Store as new dataset
newdata <- mydata

for (n in 1:nrow(newdata)) {
     if (is.na(newdata$steps[n])) {
          newdata$steps[n] <- myresult2[as.character(newdata$interval[n])]
     }    
}


## Histogram of new data set for total number of steps take neach day
newresult <- tapply(newdata$steps,newdata$date,sum)
newsteps <- as.vector(newresult)
hist(as.vector(newresult), 
     main="Histogram of total steps taken per day (no NA's)",
     xlab="Total steps")

## Show mean and median total number of steps taken per day
print("Mean (no NA's): ")
mean(newsteps, na.rm=TRUE)
print("Median (no NA's): ")
median(newsteps, na.rm=TRUE)
```

## Are there differences in activity patterns between weekdays and weekends?
```{r, echo=TRUE}
## Create factor in new data set with two levels "weekday" & "weekend"
dayofweek <- as.POSIXlt(newdata$date)$wday %% 6
dayofweek[dayofweek > 0] <- "weekday"
dayofweek[dayofweek == 0] <- "weekend"
newdata <- cbind(newdata, as.factor(dayofweek))
head(newdata)

## Generate panel plot containing time series plot of avg # of steps
## averaged across weekday days or weekend days
## Using data from the new data set (no NA's)
library(lattice)

s <- aggregate(steps ~ interval*dayofweek, 
               data=newdata, FUN=mean, simplify=FALSE)

xyplot(steps ~ interval|dayofweek, data=s, panel = panel.lines, layout=c(1,2))

```

