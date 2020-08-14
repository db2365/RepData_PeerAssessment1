---
title: "PA1_template"
date: "14/08/2020"
output: 
  html_document: 
    keep_md: yes
---



## Reproducible Research Coursera Course Project 1

# Introduction:

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

# Assignment:

The data for this assignment can be downloaded from here:

Dataset: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
The variables included in this dataset are:
1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
2. date: The date on which the measurement was taken in YYYY-MM-DD format
3. interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

# First Steps: Loading in the necessary libraries:


```r
#Load librarys
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

# Next, read in the necessary data:


```r
activity <- read.csv(file = "activity.csv")
#view data
head(activity)
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

# What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.


```r
#Create steps per day data
StepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(StepsPerDay) <- c("Date", "Steps")
```

1. Make a histogram of the total number of steps taken each day


```r
#Make Histogram using ggplot2
g <- ggplot(StepsPerDay, aes(Steps))
g+geom_histogram(col="darkblue", fill="lightblue", boundary=0, binwidth=2500)+ggtitle("Histogram of Steps Per Day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Calculate and report the mean and median total number of steps taken per day


```r
#Calculate and report the median and mean for total number of steps taken per day
median(StepsPerDay$Steps, na.rm = TRUE)
```

```
## [1] 10765
```

```r
mean(StepsPerDay$Steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

# What is the average daily activity pattern?


```r
#create table with steps per interval
avg_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(avg_daily_activity) <- c("Interval", "Mean")
```

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
#Make time series plot of the 5 min. interval(x-axis) and the average number of steps taken, averaged across all days (y-axis)
h <- ggplot(avg_daily_activity, aes(x=Interval, y=Mean))
h+geom_line(col="blue")+ggtitle("Average Steps Per Time Interval")+xlab("Interval")+ylab("Average Number of Steps")+theme(plot.title = element_text(face="bold", size=12))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#Find interval with max average steps
avg_daily_activity[which.max(avg_daily_activity$Mean), ]$Interval
```

```
## [1] 835
```

# Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
#Find total number of missing values
sum(is.na(activity$steps))
```

```
## [1] 2304
```

1. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
#Strategy for filling in missing NA values
#create mean values to fill in using the data from the day/interval, match them
values <- avg_daily_activity$Mean[match(activity$interval, avg_daily_activity$Interval)]
```

2. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#Create dataset with filled in values
filled_data <- transform(activity, steps = ifelse(is.na(activity$steps), yes = values, no = activity$steps))
total_steps_filled <- aggregate(steps ~ date, filled_data, sum)
names(total_steps_filled) <- c("Date", "Steps")
head(total_steps_filled)
```

```
##         Date    Steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#Create histogram
j <- ggplot(total_steps_filled, aes(Steps))
j+geom_histogram(col="purple", fill="pink", boundary=0, binwidth=2500)+ggtitle("Histogram of Steps Per Day with NA Values filled by Daily Mean")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
#Calculate and report the median and mean for total number of steps taken per day
median(total_steps_filled$Steps)
```

```
## [1] 10766.19
```

```r
mean(total_steps_filled$Steps)
```

```
## [1] 10766.19
```

# Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
#date in correct format
filled_data$RealDate <- as.Date(filled_data$date, format = "%Y-%m-%d")
# create a variable with weekdays name
filled_data$weekday <- weekdays(filled_data$RealDate)
# create a new variable indicating weekday or weekend
filled_data$DayType <- ifelse(filled_data$weekday=='Saturday' | filled_data$weekday=='Sunday', 'Weekend','Weekday')
head(filled_data)
```

```
##       steps       date interval   RealDate weekday DayType
## 1 1.7169811 2012-10-01        0 2012-10-01  Monday Weekday
## 2 0.3396226 2012-10-01        5 2012-10-01  Monday Weekday
## 3 0.1320755 2012-10-01       10 2012-10-01  Monday Weekday
## 4 0.1509434 2012-10-01       15 2012-10-01  Monday Weekday
## 5 0.0754717 2012-10-01       20 2012-10-01  Monday Weekday
## 6 2.0943396 2012-10-01       25 2012-10-01  Monday Weekday
```


```r
#Create plot data:
weekday_end_data <- aggregate(steps~interval+DayType, data = filled_data, FU=mean, na.action = na.omit)
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
#Create time plots:
k <- ggplot(weekday_end_data, aes(x=interval, y=steps, color = DayType))
k+geom_line()+ggtitle("Average Steps Per Time Interval")+xlab("Interval")+ylab("Average Number of Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

The end!
