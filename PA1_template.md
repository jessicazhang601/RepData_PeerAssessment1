# Reproducible Research: Peer Assessment 1
## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.



## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
library(ggplot2)
library(plyr)
## clean NAs in "steps" column
activitynoNA <- activity[!is.na(activity$steps),]
```


## What is mean total number of steps taken per day?


```r
# calculate total steps
totalSteps <- aggregate(activity$steps ~ activity$date, FUN=sum, na.rm = TRUE)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
totalSteps
```

```
##    activity$date activity$steps
## 1     2012-10-02            126
## 2     2012-10-03          11352
## 3     2012-10-04          12116
## 4     2012-10-05          13294
## 5     2012-10-06          15420
## 6     2012-10-07          11015
## 7     2012-10-09          12811
## 8     2012-10-10           9900
## 9     2012-10-11          10304
## 10    2012-10-12          17382
## 11    2012-10-13          12426
## 12    2012-10-14          15098
## 13    2012-10-15          10139
## 14    2012-10-16          15084
## 15    2012-10-17          13452
## 16    2012-10-18          10056
## 17    2012-10-19          11829
## 18    2012-10-20          10395
## 19    2012-10-21           8821
## 20    2012-10-22          13460
## 21    2012-10-23           8918
## 22    2012-10-24           8355
## 23    2012-10-25           2492
## 24    2012-10-26           6778
## 25    2012-10-27          10119
## 26    2012-10-28          11458
## 27    2012-10-29           5018
## 28    2012-10-30           9819
## 29    2012-10-31          15414
## 30    2012-11-02          10600
## 31    2012-11-03          10571
## 32    2012-11-05          10439
## 33    2012-11-06           8334
## 34    2012-11-07          12883
## 35    2012-11-08           3219
## 36    2012-11-11          12608
## 37    2012-11-12          10765
## 38    2012-11-13           7336
## 39    2012-11-15             41
## 40    2012-11-16           5441
## 41    2012-11-17          14339
## 42    2012-11-18          15110
## 43    2012-11-19           8841
## 44    2012-11-20           4472
## 45    2012-11-21          12787
## 46    2012-11-22          20427
## 47    2012-11-23          21194
## 48    2012-11-24          14478
## 49    2012-11-25          11834
## 50    2012-11-26          11162
## 51    2012-11-27          13646
## 52    2012-11-28          10183
## 53    2012-11-29           7047
```

```r
summary(totalSteps)
```

```
##     activity$date activity$steps 
##  2012-10-02: 1    Min.   :   41  
##  2012-10-03: 1    1st Qu.: 8841  
##  2012-10-04: 1    Median :10765  
##  2012-10-05: 1    Mean   :10766  
##  2012-10-06: 1    3rd Qu.:13294  
##  2012-10-07: 1    Max.   :21194  
##  (Other)   :47
```

```r
# mean number of steps for each day
meanSteps <- mean(totalSteps$`activity$steps`)
meanSteps
```

```
## [1] 10766.19
```

```r
#dedian number if steps for each day
medianSteps <- median(totalSteps$`activity$steps`)
medianSteps
```

```
## [1] 10765
```

```r
# let's create a histogram to view this
hist(totalSteps$`activity$steps`, 
    main="Total Steps per Day", 
    xlab="Number of Steps per Day", 
    ylab = "Interval",
    col="blue",
    breaks=50)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## What is the average daily activity pattern?


```r
## five minute average using steps to interval - FUN = mean instead of sum
fiveminInterval <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
## line chart
plot(x = fiveminInterval$interval, 
    y = fiveminInterval$steps, 
    type = "l", 
    col = "blue",
    xlab = "5-minute Intervals",
    ylab = "Average Steps Taken ~ Days",
    main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# what is the macimum number of steps within a 5-minute interval?
maxsteps <- fiveminInterval$interval[which.max(fiveminInterval$steps)]
maxsteps
```

```
## [1] 835
```

## Imputing missing values


```r
# calculating total number of missing values in dataset
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
# create new dataset that contains missing values
activity2 <- activity
nas<- is.na(activity2$steps)
avg_interval<- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify = TRUE)
activity2$steps[nas] <- avg_interval[as.character(activity2$interval[nas])]
names(activity2)
```

```
## [1] "steps"    "date"     "interval"
```

```r
# create a histogram to view daily activity with missing data filled in 
library(ggplot2)
library(plyr)
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
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
totalSteps2<- activity2%>%
        group_by(date)%>%
        summarise(total_steps = sum(steps, na.rm=TRUE))
ggplot(totalSteps2, aes(x = total_steps)) +
        geom_histogram(fill = "blue", binwidth = 250)+
        labs(title = "Daily Steps including Missing values", x = "5-minute Intervals", y = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean2<- mean(totalSteps2$total_steps, na.rm=TRUE)
mean2
```

```
## [1] 10766.19
```

```r
median2<- median(totalSteps2$total_steps, na.rm=TRUE)
median2
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
## Add the new weekend/weekday field
activity2<- activity2%>%
        mutate(typeofday= ifelse(weekdays(activity2$date)=="Saturday" | weekdays(activity2$date)=="Sunday", "Weekend", "Weekday"))
head(activity2)
```

```
##       steps       date interval typeofday
## 1 1.7169811 2012-10-01        0   Weekday
## 2 0.3396226 2012-10-01        5   Weekday
## 3 0.1320755 2012-10-01       10   Weekday
## 4 0.1509434 2012-10-01       15   Weekday
## 5 0.0754717 2012-10-01       20   Weekday
## 6 2.0943396 2012-10-01       25   Weekday
```


```r
fiveminInterval2<- aggregate(steps ~ interval, data = activity2, FUN = mean, na.rm = TRUE)
head(fiveminInterval2)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
# plot the activity pattern of weekdays and weenkends
library(ggplot2)
ggplot(activity2, aes(x =interval , y=steps)) +
       geom_line() +
       labs(title = "Ave Daily Steps (type of day)", x = "Interval", y = "Total Number of Steps") +
       facet_wrap(~ typeofday, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
