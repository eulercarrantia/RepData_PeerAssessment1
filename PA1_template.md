

```r
---
title: "Reproducible Research - Course Project 1"
output: html_document
---

## Loading and preprocessing the data

1. Load the data (i.e. read.csv())
```

```
## Error: <text>:8:4: unexpected symbol
## 7: 
## 8: 1. Load
##       ^
```

```r
data <- read.csv("~/Data Science - Coursera/5. Reproducible Research/Course Project 1/repdata%2Fdata%2Factivity/activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
data$date <- as.Date(data$date)
```


```r
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
total_steps_day <- aggregate(steps ~ date, data, sum)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between
them. Make a histogram of the total number of steps taken each day


```r
hist(total_steps_day$steps, main="Number of steps taken each day", col="red")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(total_steps_day$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```


```r
median(total_steps_day$steps, na.rm=TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps
taken, averaged across all days (y-axis)


```r
average_steps_interval <- aggregate(steps ~ interval, data, mean)
with(average_steps_interval , plot(interval, steps, main="Average number of steps taken", 
                                   xlab="Intervals", ylab="Number of steps", type="l", col="red"))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

     
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
average_steps_interval[which.max(average_steps_interval$steps), "interval"]
```

```
## [1] 835
```
                             
## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(data[!complete.cases(data),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be
sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
average_steps_interval <- aggregate(steps ~ interval, data, mean)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data_fill_missing <- data
data_fill_missing [!complete.cases(data_fill_missing ),"steps"] <- average_steps_interval$steps 
total_steps_day_missing  <- aggregate(steps ~ date, data_fill_missing, sum)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median
total number of steps taken per day. Do these values differ from the estimates from the first part of the
assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(total_steps_day_missing$steps, main="Number of steps taken each day", col="blue")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

```r
mean(total_steps_day_missing$steps)
```

```
## [1] 10766.19
```


```r
median(total_steps_day_missing$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether
a given date is a weekday or weekend day.


```r
for(i in 1:nrow(data_fill_missing)){
if(weekdays(data_fill_missing[i,"date"])=="sábado" | weekdays(data_fill_missing[i,"date"])=="domingo" )
{ data_fill_missing[i,"type_date"] <- "weekend" }
else data_fill_missing[i,"type_date"] <- "weekday" 
}
data_fill_missing$type_date <- as.factor(data_fill_missing$type_date)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the
average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in
the GitHub repository to see an example of what this plot should look like using simulated data.


```r
average_steps_interval_missing <- aggregate(data_fill_missing$steps, by=list(data_fill_missing$interval, 
                                                                             data_fill_missing$type_date), FUN=mean)

names(average_steps_interval_missing) <- c("interval", "type_date", "steps")

library(lattice)
xyplot(steps ~ interval | type_date , data=average_steps_interval_missing, xlab="Interval", ylab="NUmber of steps",
       type="l",
       layout = c(1,2))
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)
```

