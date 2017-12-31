---
title: "PA1_template"
output: 
  html_document:
    keep_md: true
---

Load libraries and data file


```r
echo = TRUE
library(ggplot2)

activity <- read.csv("activity.csv", header = TRUE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Modify date variable

```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

############ What is mean total number of steps taken per day? #################

Calculate the total number of steps taken per day

```r
total_step <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

Make a histogram of the total number of steps taken each day

```r
hist (total_step$steps, main = "Total Number of Steps per Day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day

```r
mean (total_step$steps)
```

```
## [1] 10766.19
```

```r
median (total_step$steps)
```

```
## [1] 10765
```

############### What is the average daily activity pattern? ######################

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
average_steps <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot (average_steps$interval, average_steps$steps, type='l', main="Average Number of Steps Taken", xlab="Intervals", ylab="Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average_steps$interval[which.max(average_steps$steps)]
```

```
## [1] 835
```

###################### Imputing missing values #############################

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum (is.na(activity)) 
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
mean (activity$steps, na.rm = T)
```

```
## [1] 37.3826
```

```r
imputation <- activity
for (i in average_steps$interval) {imputation[imputation$interval == i & is.na(imputation$steps), ]$steps <- average_steps$steps[average_steps$interval == i]}
head(imputation)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 

```r
total_imputation <- aggregate(steps ~ date, data = imputation, sum, na.rm = TRUE)
hist (total_imputation$steps, main = "Total Steps per Day - Imputation", xlab = "Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean (total_imputation$steps)
```

```
## [1] 10766.19
```

```r
median (total_imputation$steps)
```

```
## [1] 10766.19
```

######### Are there differences in activity patterns between weekdays and weekends? #########
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
imputation$weekdays <- weekdays(imputation$date)
imputation$weeks[(imputation$weekdays == "Saturday" | imputation$weekdays == "Sunday")] <- "weekend"
imputation$weeks[!(imputation$weekdays == "Saturday" | imputation$weekdays == "Sunday")] <- "weekdays"
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
library(plyr)
new <- ddply(imputation, c("interval", "weeks"), function(x) apply(x[1],2, mean))
head(new)
```

```
##   interval    weeks      steps
## 1        0 weekdays 2.25115304
## 2        0  weekend 0.21462264
## 3        5 weekdays 0.44528302
## 4        5  weekend 0.04245283
## 5       10 weekdays 0.17316562
## 6       10  weekend 0.01650943
```

```r
library(lattice)
xyplot(steps ~ interval | weeks, data = new, type = "l", xlab = "Interval", ylab = "Number of steps", layout = c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->





