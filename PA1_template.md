---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---





Set working directory and load packages.

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
## library(lubridate)
library(ggplot2)
```
## Loading and preprocessing the data
Load the data t.


```r
read_data <- tbl_df(read.csv("C:/Users/jonat/coursera/DS/R_wd/repdata/RepData_PeerAssessment1/activity/activity.csv", stringsAsFactors = FALSE))
dat1 <-read_data
```

## What is mean total number of steps taken per day?
Group data by day,and calculate total steps.

```r
data_by_day <- group_by(dat1,date)
total_steps <- summarise(data_by_day,day_total=sum(steps))
```

Plot histogram of total steps per day.

```r
png("plot1.png")
ggplot(data=total_steps, aes(total_steps$day_total))+
  geom_histogram(fill="blue",binwidth=2500)+
  xlab("Total Steps per Day")+
  ylab("Count")+
  scale_y_continuous(breaks=c(0,2,4,6,8,10))
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

```r
dev.off()
```

```
## png 
##   2
```

Calculate mean and median of total steps per day.

```r
mn1<- mean(total_steps$day_total,na.rm=TRUE)
print ("Mean Total Steps taken each day"); mn1
```

```
## [1] "Mean Total Steps taken each day"
```

```
## [1] 10766.19
```

```r
md1<- median(total_steps$day_total,na.rm=TRUE)
print ("Median Total Steps taken each day");  md1
```

```
## [1] "Median Total Steps taken each day"
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Group and average by time interval across all days. 

```r
data_by_interval <- group_by(dat1,interval)
int_avg <- summarise(data_by_interval, interval_avg = mean(steps,na.rm = TRUE))
```

Plot time series of average number of steps by 5 minute interval.

```r
png("plot2.png")
ggplot(int_avg,aes(interval,interval_avg))+
  geom_line()
  scale_x_continuous(breaks=seq(0,2400,300))
```

```
## <ScaleContinuousPosition>
##  Range:  
##  Limits:    0 --    1
```

```r
dev.off()
```

```
## png 
##   2
```


Identify interval with maximum average steps.

```r
maxval<-int_avg$interval_avg[which.max(int_avg$interval_avg)]
intmax<-int_avg$interval[which.max(int_avg$interval_avg)]
```
The maximum value of average steps is "r maxval" in interval "r intmax"}. 

Count the number of records with NAs 

```r
sum(is.na(dat1$steps))
```

```
## [1] 2304
```


## Imputing missing values
Replace NAs with imputed values, using average by interval.
First merge original data set with interval averages, to produce new data set "dat2". 
Then repeat total steps by day grouping, plot histogram of day totals, output mean and median daily step count.

```r
dat2<-tbl_df(merge(dat1,int_avg,by = "interval"))
dat2<-mutate(dat2,steps=ifelse(is.na(steps),interval_avg,steps))

data_by_day <- group_by(dat2,date)
total_steps <- summarise(data_by_day,day_total=sum(steps))
png("plot3.png")
ggplot(data=total_steps, aes(total_steps$day_total))+
  geom_histogram(fill="blue",binwidth=2500)+
  xlab("Total Steps per Day")+
  ylab("Count")+
  scale_y_continuous(breaks=c(0,2,4,6,8,10))
dev.off()
```

```
## png 
##   2
```

```r
mn2<- mean(total_steps$day_total,na.rm=TRUE)
print ("Mean Total Steps taken each day"); mn2
```

```
## [1] "Mean Total Steps taken each day"
```

```
## [1] 10766.19
```

```r
md2<- median(total_steps$day_total,na.rm=TRUE)
print ("Median Total Steps taken each day");  md2
```

```
## [1] "Median Total Steps taken each day"
```

```
## [1] 10766.19
```


Compare to values from first part assignment, prior to adding imputed values.
Original mean and median

```r
print(c("Mean",mn1))
```

```
## [1] "Mean"             "10766.1886792453"
```

```r
print(c("Median",md1))
```

```
## [1] "Median" "10765"
```

```r
print(c("Change in mean", mn2-mn1))
```

```
## [1] "Change in mean" "0"
```

```r
print(c("Change in median",  md2-md1))
```

```
## [1] "Change in median" "1.1886792452824"
```


Add identifier factor with 2 levels, weekday and weekend.
First add date field in POSIXlyt format,then day of the week, then weekday/weekend indicator

```r
dat3<-dat2
dat3<- mutate(dat3,date_form = as.Date(date,"%Y-%m-%d"))
dat3<- mutate(dat3,weekday = weekdays(date_form))
dat3<-mutate(dat3,dayfactor = as.factor(ifelse(weekday %in% c("Saturday","Sunday"), "weekend","weekday")))
```


## Are there differences in activity patterns between weekdays and weekends?
Group and average by time interval across all days. 

```r
data_by_interval_dayfactor <- group_by(dat3,interval,dayfactor)
int_avg <- summarise(data_by_interval_dayfactor, interval_avg = mean(steps,na.rm = TRUE))
```

Plot time series of average number of steps by 5 minute interval.

```r
png("plot4.png")
ggplot(int_avg,aes(interval,interval_avg))+
  geom_line()+
  scale_x_continuous(breaks=seq(0,2400,300))+
  facet_grid(dayfactor~.)
dev.off()
```

```
## png 
##   2
```
