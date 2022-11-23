---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Activity monitoring Project 1

### Question 1

1.What is mean total number of steps taken per day?

2.What is the average daily activity pattern?

3.Imputing missing values

4.Are there differences in activity patterns between weekdays and weekends?

Loading the Data


```r
activity <- read.csv('activity.csv')
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

\*\* 1 \*\* importing the necessaries libraries


```r
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
# calculating the total number of steps taken per day
daily_steps <- activity %>% group_by(date) %>% summarise(steps=sum(steps, na.rm=TRUE))
head(daily_steps)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <chr>      <int>
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

\*\* 2 \*\*


```r
with(daily_steps, hist(steps, xlab="Total steps", main="Histogram of Total steps per day"))
```

![](Project1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

\*\* 3 \*\*


```r
mean(daily_steps$steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(daily_steps$steps, na.rm=TRUE)
```

```
## [1] 10395
```

# Question 2

## What is the daily average daily activity pattern?

### The Average Daily Activity Pattern


```r
#importing the necessaries libraries
library(ggplot2)
```


```r
actv1 <- activity %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm=T))
stepmaxindex <- which.max(actv1$steps)
maxstep <- actv1$steps[stepmaxindex]
maxinterval <- actv1$interval[stepmaxindex]
```


```r
#using ggplot for visualization
g <- ggplot(actv1, aes(x=interval, y=steps)) + geom_line()
g <- g + geom_hline(yintercept=maxstep, col = 'magenta', lwd=0.6, alpha=0.5)
g <- g + geom_vline(xintercept = maxinterval, col = 'darkgreen', lwd=0.6, alpha=0.6)
g
```

![](Project1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
#which 5-minute interval, on average across all the days in the #dataset, contains the maximum number of steps?
actv1[actv1$steps == max(actv1$steps),1]
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```

# Question 3

## Imputing Missing Values


```r
nill <- activity[activity$steps %in% NA,]
dim(nill)
```

```
## [1] 2304    3
```

use mean of steps taken every 5-minute interval to fill in all of the missing values in the dataset. there is 8 days missing value, so replicating the mean for 8 time is needed.


```r
meansteps <- actv1$steps
nill$steps <- rep(meansteps, 8)
```

create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
not_nill <- activity[complete.cases(activity),]
newdf <- rbind(not_nill, nill)
table(is.na(newdf$steps))
```

```
## 
## FALSE 
## 17568
```

make a histogram of the total number of steps taken each day and calculate report the **mean** and **median** total number of steps taken per day.Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newdaysteps <- newdf %>% group_by(date) %>% summarise(steps=sum(steps, na.rm=TRUE))
with(newdaysteps, hist(steps, xlab="New total steps without NA", main="New Histogram of Total Steps per day"))
```

![](Project1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
# the mean 
mean(newdaysteps$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
# the median
median(newdaysteps$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels ?C ??weekday?? and ??weekend?? indicating whether a given date is a weekday or weekend day.


```r
newdf$date <- as.Date(newdf$date,"%Y-%m-%d")
newdf$week <- ifelse(weekdays(newdf$date) %in% c("Saturday","Sunday"),"weekend","weekday")
newdf$week <- as.factor(newdf$week)
str(newdf)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-02" "2012-10-02" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ week    : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Make a panel plot containing a time series plot to see the different between weekdays and weekends.


```r
newsteps <- newdf %>% group_by(week,interval) %>% summarise(steps=mean(steps))
```

```
## `summarise()` has grouped output by 'week'. You can override using the
## `.groups` argument.
```

```r
g <- ggplot(newsteps,aes(interval,steps,group=week))
g + geom_line() + facet_grid(week~.)
```

![](Project1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
g
```

![](Project1_files/figure-html/unnamed-chunk-16-2.png)<!-- -->
