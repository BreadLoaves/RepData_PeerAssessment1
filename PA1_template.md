---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data


```r
library(lattice)
library(knitr)
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
Steps <- aggregate(steps ~ date, data, sum)
Daily_interval <- aggregate(steps ~ interval, data, FUN = mean)
Average <- aggregate(steps ~ interval, data = data, FUN = mean)

hist(Steps$steps, xlab = paste("Steps taken per day"), col = 'red', main = "Total steps per day")
```

![](C:/Users/alexa/Documents/GitHub/ExData_Plotting1/RepData_PeerAssessment1/PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean <- mean(Steps$steps)
mean
```

```
## [1] 10766.19
```

```r
median <- median(Steps$steps)
median
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
plot(Daily_interval$interval, Daily_interval$steps, type = 'l',
     xlab = "Interval", ylab = "Steps", main = "Average daily activity patten")
```

![](C:/Users/alexa/Documents/GitHub/ExData_Plotting1/RepData_PeerAssessment1/PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
Max_interval <- Daily_interval[which.max(Daily_interval$steps),1]
Max_interval
```

```
## [1] 835
```

## Imputing missing values


```r
fillNA <- numeric()

for(i in 1:nrow(data)) {
  obs <- data[i, ]
  if (is.na(obs$steps)) {
        steps <- subset(Average, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}

new_activity <- data
new_activity$steps <- fillNA

Steps <- aggregate(steps ~ date, new_activity, sum)
hist(Steps$steps, xlab = paste("Steps taken per day"), col = 'red', main = "Total steps per day")
```

![](C:/Users/alexa/Documents/GitHub/ExData_Plotting1/RepData_PeerAssessment1/PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean <- mean(Steps$steps)
mean
```

```
## [1] 10766.19
```

```r
median <- median(Steps$steps)
median
```

```
## [1] 10766.19
```

```r
StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)

hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps")
hist(Steps$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps", add=T)
```

![](C:/Users/alexa/Documents/GitHub/ExData_Plotting1/RepData_PeerAssessment1/PA1_template_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?


```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))

StepsTotalUnion <- aggregate(steps ~ interval + dow, new_activity, sum)

xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l", col = 'black')
```

![](C:/Users/alexa/Documents/GitHub/ExData_Plotting1/RepData_PeerAssessment1/PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
