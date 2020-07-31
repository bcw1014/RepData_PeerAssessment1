---
title: "Reproducible Research: Peer Assessment 1"
author: "Bryan Williams"
date: "7/30/2020"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
unzip("./activity.zip")
activityData <- read.csv("./activity.csv")
summary(activityData)

names(activityData)

head(activityData)

pairs(activityData)

## What is mean total number of steps taken per day?

stepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)

hist(stepsPerDay$steps)

meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay

medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay

## What is the average daily activity pattern?

stepsPerInterval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")

intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps


## Imputing missing values

totalValuesMissings <- sum(is.na(activityData$steps))
totalValuesMissings

getMeanStepsPerInterval<-function(interval){
    stepsPerInterval[stepsPerInterval$interval==interval,]$steps
}

activityDataNoNA<-activityData
for(i in 1:nrow(activityDataNoNA)){
    if(is.na(activityDataNoNA[i,]$steps)){
        activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
    }
}

totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)

meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)


## Are there differences in activity patterns between weekdays and weekends?
# helper function to decide if a day is a week day or not

activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
    if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
        activityDataNoNA[i,]$day<-"weekend"
    }
    else{
        activityDataNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)

names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
