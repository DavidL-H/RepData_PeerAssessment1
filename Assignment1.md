---
title: "Reproducible Research: Project 1"
author: "David Lennox-Hvenekilde"
date: "7/29/2021"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
setwd("C:/Users/David/Biosyntia/Administration - David Lennox-Hvenekilde/Bioinformatics and data science/Courses/Data Science in R/Reproducible Research/Week 2/Assignment 1")
activity <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
We aggregate the data by date, summing the steps taken, and save this in a new two column dataframe with dates and total number of steps taken per day and make a histogram of this:

```r
Steps_day <- aggregate(activity$steps, list(activity$date), FUN=sum, na.rm=TRUE)
colnames(Steps_day)<-c("Date", "Total.Steps")

p<-ggplot(Steps_day, aes(Total.Steps))+
      geom_histogram(bins = 12,color="black")+
      labs(title = "Histogram - Total steps taken per day")+
      scale_color_grey()+ 
      theme_classic()
p
```

![](Assignment1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## What is the average daily activity pattern?
To show this, we aggregate the data by 5 minute intervals, using the mean of the steps taken, and save this in a new two column dataframe with intervals and means of steps taken per day:

```r
Steps_Interval <- aggregate(activity$steps, list(activity$interval), FUN=mean, na.rm=TRUE)
colnames(Steps_Interval)<-c("Interval", "Mean.steps")
```

#### We can now plot this as a time-series:

```r
p<-ggplot(Steps_Interval, aes(Interval,Mean.steps))+
      geom_line(color="lightblue",size=2)+
      labs(x="Time (5 minute intervals)", title = "Timeseries - Average steps taken per 5 minute intervals in 24 hours")+
      scale_color_grey()+ 
      theme_classic()+
      scale_x_continuous(breaks = seq(0,2300,100))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p
```

![](Assignment1_files/figure-html/timeseries-1.png)<!-- -->

#### The 5 minute interval with the highest average number of steps is the 5 minute interval starting from 8:35

```r
Steps_Interval[which.max(Steps_Interval$Mean.steps),]
```

```
##     Interval Mean.steps
## 104      835   206.1698
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#### The number of missing values in the dataset:

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
#### We now replace all NAs with rnorm() generated values
First we make a function that calculates the mean and sd of steps for a given time series  
For the same time series it generates a number of normally distributed values based on this mean and sd, equal to the number  of NAs for this time series. If the values is negative, it is replaced by 0

```r
replace_NA_step <- function(time_series, number_of_na){
      replace<-rnorm(number_of_na,mean= mean(activity[activity$interval==time_series,]$steps, na.rm = TRUE), 
                     sd=sd(activity[activity$interval==time_series,]$steps, na.rm = TRUE))
      replace[replace<0]<-0
      as.integer(replace)
}
replace_NA_step(600, 10)
```

```
##  [1] 183   0   0 126 104 103   0   0   0 109
```
We now loop through a copy of the dataframe "activity_nona" and replace all NAs with the function above

```r
activity_nona<-activity
for (time in Steps_Interval$Interval){
      number_of_nas<-sum(is.na(activity_nona[activity_nona$interval==time,]$steps))
      replace_NA_step(time,number_of_nas)
      activity_nona[(activity_nona$interval==time) & is.na(activity_nona$steps),]$steps<-replace_NA_step(time,number_of_nas)
}
```
There are no more NAs

```r
sum(is.na(activity_nona$steps))
```

```
## [1] 0
```
#### Histogram of total steps in the two data sets.
First prepare a clean long type dataframe for easy plotting in ggplot. Then plot the two histograms side by side

```r
Steps_day_nona <- aggregate(activity_nona$steps, list(activity_nona$date), FUN=sum, na.rm=TRUE)
colnames(Steps_day_nona)<-c("Date", "Total.Steps")
Steps_day_nona$NA_type <- "no_na"
Steps_day$NA_type <- "with_na"
Steps_day_full<-rbind(Steps_day,Steps_day_nona)

p<-ggplot(Steps_day_full, aes(Total.Steps, fill = NA_type))+
      geom_histogram(bins = 12,color="black")+
      labs(title = "Histogram - Total steps taken per day \nnew data without NAs and original data with NAs")+
      theme_classic()+
      facet_wrap(~NA_type)
p 
```

![](Assignment1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
As escpected there are fewer days with low total steps now that NAs have been replaced  

## Are there differences in activity patterns between weekdays and weekends?
We first use the weekdays() based R function on the dates in out new data set (with no NAs)  
These first have to be converted to POSIX type dates. We then gather and calculate means at 5 minute intervals as earlier.  We then specify which days are weekends and weekdays and can then plot time series for both.

```r
activity_nona$Day<-weekdays(as.POSIXct(activity_nona$date))
Steps_Interval_nona <- aggregate(activity_nona$steps, list(activity_nona$interval, activity_nona$Day), FUN=mean)
colnames(Steps_Interval_nona)<-c("Interval", "Day", "Mean.steps")
Steps_Interval_nona$Day_type<-"Weekday"
Steps_Interval_nona[(Steps_Interval_nona$Day == "Saturday") |(Steps_Interval_nona$Day == "Sunday"),]$Day_type<-"Weekend"

p<-ggplot(Steps_Interval_nona, aes(Interval,Mean.steps, colour=Day_type))+
      geom_line(size=1)+
      labs(x="Time (5 minute intervals)", title = "Timeseries - Average steps taken per 5 minute intervals in 24 hours\nnew data without NAs. Weekdays vs Weekend")+
      theme_classic()+
      scale_x_continuous(breaks = seq(0,2300,100))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      facet_wrap(~Day_type)
p
```

![](Assignment1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
There seems to be more activity on weekdays in general, especially in the morning hours from 05:00 to 09:00  
Activity ramps up later in the weekend, but there is slightly more activity later in the day from around 20:00.
