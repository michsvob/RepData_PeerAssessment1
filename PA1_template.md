
```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```


---
title: "PA1_template"
author: "Michal Svoboda"
date: "26 června 2016"
output: html_document
---



## Getting and processing the data 

activityData are downloaded from <href>https://d396qusza40orc.cloudfront.net/repactivityData%2FactivityData%2Factivity.zip</href>, unzipped
and loaded. The intervals are converted from integer representation to POSIXct representation.


```r
#download.file("https://d396qusza40orc.cloudfront.net/repactivityData%2FactivityData%2Factivity.zip","activityactivityData")
#unzip("activityactivityData")
#commented out as the link stopped working

activityData<-read.csv("activity.csv")
activityData$time<-as.POSIXct(strptime(substr(as.character(activityData$interval+10000),2,5),format="%H%M"))
```

## Steps per day

Split the activityData into days and make a histogram. Also calculate mean and median. Note that missing
activityData are removed from the statistics.


```r
total.steps.per.day<-tapply(activityData$steps,activityData$date,sum,na.rm=FALSE)
hist(total.steps.per.day)
```

![](PA1_template_files/figure-html/steps per day-1.png)



```r
mean<-mean(total.steps.per.day,na.rm=TRUE)
median<-median(total.steps.per.day,na.rm=TRUE)
```
Mean is 1.0766189\times 10^{4} and median is 10765.

##steps.patternerage daily steps pattern

Count steps.patternerage steps in 5-minute intervals and make a plot of steps.patternerage daily pattern.
Also get the interval with highest number of steps.


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
steps.pattern<-summarize(group_by(activityData,time),steps=mean(steps,na.rm=TRUE))
plot(steps.pattern,type="l",main="Number of steps in 5-minute intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
#find at which time there were most steps recorded
steps.pattern$time[steps.pattern$steps==max(steps.pattern$steps)]
```

```
## [1] "2016-06-26 08:35:00 CEST"
```

##Imputing missing values

Count the total number of missing values, choose imputing strategy (intervals with missing values 
will be replaced by a mean of this 5minute interval of all the valid days), show histogram of newly 
made dataset, calculate mean and median.

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

```r
#replace NAs by a typical 5minute inverval
impute<-function(interv){
    mean(activityData$steps[activityData$interval==interv],na.rm=TRUE)
}

activityData2<-activityData
activityData2$steps[is.na(activityData2$steps)]<-impute(activityData2$interval[is.na(activityData2$steps)])
```

```
## Warning in activityData$interval == interv: longer object length is not a
## multiple of shorter object length
```

```r
total.steps.per.day2<-tapply(activityData2$steps,activityData2$date,sum,na.rm=FALSE)
hist(total.steps.per.day2)
```

![](PA1_template_files/figure-html/imputing-1.png)

```r
mean2<-mean(total.steps.per.day2)
median2<-median(total.steps.per.day2)
```
Mean is 1.0766189\times 10^{4} and median is 1.0766189\times 10^{4}. Not a big change from previous mean 1.0766189\times 10^{4} and median 10765.

##Weekend vs weekday


```r
#identify weekends or weekdays
activityData$weekday<-sapply(activityData$date,function(x){if(weekdays(as.Date(x))%in%c("Saturday","Sunday")){"Weekend"} else "Weekday"})

par(mfrow=c(2,1))
steps.pattern.weekend<-summarize(group_by(activityData[activityData$weekday=="Weekend",],time),steps=mean(steps,na.rm=TRUE))
steps.pattern.weekday<-summarize(group_by(activityData[activityData$weekday=="Weekday",],time),steps=mean(steps,na.rm=TRUE))

plot(steps.pattern.weekday,type="l",main="Weekday",ylim=c(0,250))
plot(steps.pattern.weekend,type="l",main="Weekend",ylim=c(0,250))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

