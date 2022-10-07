---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data

First we will unzip the ilfe, load the data and make a quick data exploration

```r
unzip("./activity.zip")
activityData <- read.csv("./activity.csv")
summary(activityData)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
names(activityData)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(activityData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
plot(activityData[,])
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day.


```r
stepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)
```

2. Histogram of the total number of steps taken each day


```r
hist(stepsPerDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_Steps_Per_Day <- mean(stepsPerDay$steps) ##The mean total number of steps taken each day
mean_Steps_Per_Day
```

```
## [1] 10766.19
```

```r
median_Steps_Per_Day <- median(stepsPerDay$steps) ## The median total number of steps taken each day
median_Steps_Per_Day
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_Per_Interval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
plot(steps~interval, data=steps_Per_Interval, type="l",col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_With_Max_Steps <- steps_Per_Interval[which.max(steps_Per_Interval$steps),]$interval
interval_With_Max_Steps
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA\color{red}{\verb|NA|}NAs)


```r
total_Values_Missings <- sum(is.na(activityData$steps)) # The total number of missing values
total_Values_Missings
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
getMean_Steps_Per_Interval<-function(interval){
    steps_Per_Interval[steps_Per_Interval$interval==interval,]$steps
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityDataNoNA<-activityData
for(i in 1:nrow(activityDataNoNA)){
    if(is.na(activityDataNoNA[i,]$steps)){
        activityDataNoNA[i,]$steps <- getMean_Steps_Per_Interval(activityDataNoNA[i,]$interval)
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_Steps_Per_DayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(total_Steps_Per_DayNoNA$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean_Steps_Per_DayNoNA <- mean(total_Steps_Per_DayNoNA$steps) # The mean total number of steps taken each day with no missing values
mean_Steps_Per_DayNoNA
```

```
## [1] 10766.19
```

```r
median_Steps_Per_DayNoNA <- median(total_Steps_Per_DayNoNA$steps) # The median total number of steps taken each day with no missing values
median_Steps_Per_DayNoNA
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
    if (activityDataNoNA[i,]$day %in% c("星期六","星期日")) { # Since here I am using the Chinese Virsion, so the weekdays are automaticlly changed into Chinese. So if you are going to test it, please change the conditions back to your origin language.
        activityDataNoNA[i,]$day<-"weekend"
    }
    else{
        activityDataNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
```

2. Make a panel plot containing a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1,2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

