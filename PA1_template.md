---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



### Loading and preprocessing the data
- steps: Number of steps taking in a 5-minute interval
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken


```r
dfile <- read.csv("H:/R/Coursera/activity.csv", header = TRUE)
dfile$date <- as.Date(dfile$date)
summary(dfile)
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

### What is mean total number of steps taken per day?


```r
library(plyr)
dfile <- ddply(dfile, .(date),
    mutate,
    per_day = sum(steps))
mean_steps <- mean(dfile$per_day, na.rm = TRUE)
mean_steps <- format(round(mean_steps, 0), scientific=FALSE)
median_steps <- median(dfile$per_day, na.rm = TRUE)
median_steps <- format(round(median_steps, 0), scientific=FALSE)

hist(dfile$per_day, col="red" , breaks=25, xlab = "Steps per day", 
    main = "Total number of steps taken per day")
```

![](PA1_template_files/figure-html/mean-1.png)<!-- -->

```r
mean_steps
```

```
## [1] "10766"
```

```r
median_steps
```

```
## [1] "10765"
```

The mean total number of steps taken per day is 10766, the median is 10765.

### What is the average daily activity pattern?


```r
dfile <- ddply(dfile, .(interval),
 mutate,
  meanSteps = mean(steps, na.rm = TRUE))
library(ggplot2)
ggplot(dfile, aes(interval, meanSteps)) + geom_line() +
    scale_x_continuous("Interval") + scale_y_continuous("Average number of steps") +
    ggtitle("Time series plot")
```

![](PA1_template_files/figure-html/TSplot-1.png)<!-- -->

```r
maxSteps = max(dfile$meanSteps, na.rm = TRUE)
maxInt <- unique(dfile[which(dfile$meanSteps == maxSteps), "interval"])
```

The 5-minute interval number 835 contains the maximum number of steps.


### Imputing missing values


```r
nNA = length(dfile$steps) - length(dfile$steps[!is.na(dfile$steps)])
nNA
```

```
## [1] 2304
```

The dataset contains 2304 missing values.
I'll impute the missing data by using the mean number of steps per 5-minute interval.


```r
# create the new variable
dfile$stepsImp <- dfile$steps
# impute the missed values for steps
dfile$stepsImp[is.na(dfile$stepsImp)] <- dfile$meanSteps[is.na(dfile$stepsImp)]
# create the new dataset (with imputed values)
dfile2 <- dfile[order(dfile$date), c("stepsImp", "date", "interval")]
colnames(dfile2) <- c("steps", "date", "interval")
head(dfile2)
```

```
##         steps       date interval
## 1   1.7169811 2012-10-01        0
## 62  0.3396226 2012-10-01        5
## 123 0.1320755 2012-10-01       10
## 184 0.1509434 2012-10-01       15
## 245 0.0754717 2012-10-01       20
## 306 2.0943396 2012-10-01       25
```


```r
dfile2 <- ddply(dfile2, .(date),
 mutate,
  per_day2 = sum(steps))
mean_steps2 <- mean(dfile2$per_day2, na.rm = TRUE)
mean_steps2 <- format(round(mean_steps2, 0), scientific=FALSE)
median_steps2 <- median(dfile2$per_day2, na.rm = TRUE)
median_steps2 <- format(round(median_steps2, 0), scientific=FALSE)

hist(dfile2$per_day2, col="red" , breaks=25, xlab = "Steps per day", 
    main = "Total number of steps taken per day (Imputed)")
```

![](PA1_template_files/figure-html/mean2-1.png)<!-- -->

```r
mean_steps2
```

```
## [1] "10766"
```

```r
median_steps2
```

```
## [1] "10766"
```

The mean total number of steps taken per day is 10766, the median is 10766. We can see that after imputation the mean and median total number of steps taken per day are equal. On histogram we can also see high peak around 10k-11k steps (with more that 2 times higher frequecy).


### Are there differences in activity patterns between weekdays and weekends?


```r
dfile2 <- ddply(dfile2, .(interval),
    mutate,
    day = weekdays(date),
    weekend = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"),
    meanSteps2 = mean(steps, na.rm = TRUE))
# create a new factor variable
head(dfile2)
```

```
##       steps       date interval per_day2       day weekend meanSteps2
## 1  1.716981 2012-10-01        0 10766.19    Monday Weekday   1.716981
## 2  0.000000 2012-10-02        0   126.00   Tuesday Weekday   1.716981
## 3  0.000000 2012-10-03        0 11352.00 Wednesday Weekday   1.716981
## 4 47.000000 2012-10-04        0 12116.00  Thursday Weekday   1.716981
## 5  0.000000 2012-10-05        0 13294.00    Friday Weekday   1.716981
## 6  0.000000 2012-10-06        0 15420.00  Saturday Weekend   1.716981
```


```r
dfile2 <- ddply(dfile2, .(interval, weekend),
    mutate,
    meanSteps2 = mean(steps, na.rm = TRUE))
#plot
ggplot(dfile2, mapping = aes(interval, meanSteps2)) + geom_line() + 
    facet_grid(weekend ~ ., scales="fixed") +
    scale_x_continuous("Interval") + 
    scale_y_continuous("Average number of steps") +
    ggtitle("Time series plot") 
```

![](PA1_template_files/figure-html/TSplot2-1.png)<!-- -->

From the plot above we can see that the average number of steps taken on weekdays and weekends differs. On weekdays the main activity is before 10 AM (the peak is around 8 AM), while on 
weekends the activity is spread through the day.
