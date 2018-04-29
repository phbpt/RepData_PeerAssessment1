---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---







# Assignment
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). 
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during
the months of October and November, 2012 and include the number of steps taken
in 5 minute intervals each day.

Loading and preprocessing the data
The data for this assignment can be downloaded from the course web site:
. Dataset: Activity monitoring data [52K]
The variables included in this dataset are:
. steps: Number of steps taking in a 5-minute interval (missing values are coded as )
. date: The date on which the measurement was taken in YYYY-MM-DD format
. interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this
dataset.

## 1. Code for reading in the dataset and/or processing the data


```r
### download and unzip data from source
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              destfile = "activity.zip", mode= "wb")
unzip("activity.zip")

### read data and get some infomations about data
activeData <- read.csv("activity.csv", header= TRUE)

head(activeData)
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
tail(activeData)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
str(activeData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activeData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
class(activeData)
```

```
## [1] "data.frame"
```

```r
### Process/transform the data (if necessary) into a format suitable for your analysis
### omit NA for part1
active2 <- tapply(activeData$steps, activeData$date, sum)
```



## 2. Histogram of the total number of steps taken each day

```r
hist(active2, main= "total number of steps taken each day", xlab= "Total daily steps" 
     , col= "lightblue")
```

![](PA1_files/figure-html/hist-1.png)<!-- -->


## 3. Mean and median number of steps taken each day

```r
mean(active2, na.rm= TRUE)
```

```
## [1] 10766.19
```

```r
median(active2, na.rm= TRUE)
```

```
## [1] 10765
```



## 4. Time series plot of the average number of steps taken
Make a time series plot (i.e. ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
active5minSteps <- aggregate(steps ~ interval, data= activeData, FUN= mean, na.rm= TRUE)
plot(active5minSteps$interval, active5minSteps$steps, main= "Time series plot of the average number of steps taken", xlab= "time interval (5 minutes)", ylab= "Average steps",  type= "l")
```

![](PA1_files/figure-html/timeplot-1.png)<!-- -->

## 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
active5minSteps[which(active5minSteps$steps == max(active5minSteps$steps)), ]
```

```
##     interval    steps
## 104      835 206.1698
```


## 6. Code to describe and show a strategy for imputing missing data
1. Calculate and report the total number of missing values in the dataset (i.e. the total number    of rows with s)

```r
naValues <- sum(is.na(activeData))
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does    not need to be sophisticated. For example, you could use the mean/median for that day, or the    mean for that 5-minute interval, etc.
The NA is replaced by the mean in the corresponding interval.

```r
names(active5minSteps) [2] <- "meansteps"
activeDataImpute <- merge(activeData, active5minSteps)
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activeDataImpute$steps[is.na(activeDataImpute$steps)] <- 
    activeDataImpute$meansteps[is.na(activeDataImpute$steps)]
```

## 7. Histogram of the total number of steps taken each day after missing values are imputed and       calculate and report the mean and median total number of steps taken per day.

```r
activeImputed2 <- tapply(activeDataImpute$steps, activeDataImpute$date, sum)
hist(activeImputed2, main= "total number of steps taken each day (after imputation)", xlab= "Total daily steps" , col= "green")
```

![](PA1_files/figure-html/hist_incl_na-1.png)<!-- -->


```r
mean(active2, na.rm= TRUE)
```

```
## [1] 10766.19
```

```r
median(active2, na.rm= TRUE)
```

```
## [1] 10765
```

```r
mean(activeImputed2, na.rm= TRUE)
```

```
## [1] 10766.19
```

```r
median(activeImputed2, na.rm= TRUE)
```

```
## [1] 10766.19
```
The mean and median are almost unchanged after the imputation with the mean in the corresponding interval. The mean is unchanged (10766.19). The median slightly increased (10765 vs. 10766.19).

##  8.  Panel plot comparing the average number of steps taken per 5-minute interval across  weekdays and weekends

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```


```r
activeDataImpute$date  <- as.Date(activeDataImpute$date)
activeDataImpute$weekday <- weekdays(activeDataImpute$date) 
activeDataImpute$weekcategory <- ifelse(activeDataImpute$weekday == "Saturday" | activeDataImpute$weekday == "Sunday",  "weekend", "weekday") 

activeIntervalStepsImp <- aggregate(steps ~ weekcategory + interval, data= activeDataImpute, FUN= mean, na.rm= TRUE)

names(activeIntervalStepsImp) <- c("weekcategory", "interval", "steps")

library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.3
```

```r
ggplot(activeIntervalStepsImp, aes(x=interval, y= steps)) + geom_line() + 
        facet_grid(weekcategory ~.) + xlab("interval") + ylab("steps")  + 
            ggtitle ("Comparison of average number of steps in 5 minutes intervals")
```

![](PA1_files/figure-html/ggplot-1.png)<!-- -->

There are differences when comparing weekdays and weekend average numbers of steps.
During the week the peak is around interval 850 and another peak later the days around interval 1800. During weekend the average  number of steps is higher during the day. The reasons might be that people are getting up later and that activities happens during times when people are at work during weekdays.


