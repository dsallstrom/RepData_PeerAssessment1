# Reproducible Research: Peer Assessment 1
Duncan Sallstrom  

This document presents a few analyses performed on individual movement data. In short, personal
movement trackers were given to test subjects, and the data used here are sourced from those personal
movement trackers. This code will answer several questions using the data.

## Loading and preprocessing the data

First, load prerequisite packages ggplot2 and dplyr. You must have these installed to run the program.

```r
library(dplyr)
library(ggplot2)
```

This code loads the dataset and adds performs some operations to make later analyses easier. I reformat the
date variable, create a weekday/weekend indicator, and I drop unneeded variables.

```r
setwd("C:/Users/duncans/Dropbox/Programming/R_WDir")
dta <- read.csv("activity.csv")
dta <- dta %>%
        mutate(Date = as.Date(as.character(date)),
                Weekday = weekdays(Date), 
                Weekend = ifelse(Weekday == "Saturday" | Weekday == "Sunday", 1, 0)) %>%
        select(steps, interval, Date, Weekday, Weekend)
dta$interval <- as.factor(dta$interval)
```

## What is mean total number of steps taken per day?

For brevity, I report mean number of steps per the first five days. Dataset dta_sums has this information
for all days.

```r
dta_sums <- dta %>% 
        na.omit() %>%
        group_by(Date) %>%
        summarize(steps = sum(steps))
head(dta_sums)
```

```
## Source: local data frame [6 x 2]
## 
##         Date steps
##       (date) (int)
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

And here is a histogram showing the mean number of steps per day:

```r
hist(dta_sums$steps, main = "Daily Steps", xlab = "Steps")
```

![unnamed-chunk-4-1](https://github.com/dsallstrom/RepData_PeerAssessment1/blob/master/unnamed-chunk-4-1.png)

These are the mean and median steps per day (across all days):

```r
mean(dta_sums$steps)
```

```
## [1] 10766.19
```

```r
median(dta_sums$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

A line plot shows this information below. Each point represents the mean number of steps taken during a time interval.

```r
dta_patt <- dta %>% 
        na.omit() %>%
        group_by(interval) %>%
        summarize(MeanSteps = mean(steps))
plot(as.numeric(as.character(dta_patt$interval)), dta_patt$MeanSteps, 
        type = 'l', 
        main = "Average Steps by Time of Day",
        xlab = "Millitary Time", ylab = "Average Steps")
```

![unnamed-chunk-6-1](https://github.com/dsallstrom/RepData_PeerAssessment1/blob/master/unnamed-chunk-6-1.png)

This is the five minute interval with the most steps:

```r
f <- filter(dta_patt, MeanSteps == max(dta_patt$MeanSteps))
f[1,1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
##     (fctr)
## 1      835
```


## Imputing missing values

I'll begin by counting the number of rows with missing values:

```r
dta_missing <- mutate(dta, MissingVar = complete.cases(dta))
nrow(filter(dta_missing, MissingVar == FALSE))
```

```
## [1] 2304
```

I choose to attach the mean step count (for a given interval) to instances with missing step information.
The code below accomplishes this task.

```r
dta_editmiss <- merge(x = dta_missing, y = dta_patt,
        by.x = "interval", by.y = "interval", all=TRUE)
dta_editmiss2 <- dta_editmiss %>% 
        mutate(StepsEdit = ifelse(MissingVar == FALSE, MeanSteps, steps)) %>%
        arrange(Date, interval) %>%
        select(StepsEdit, interval, Date, Weekday, Weekend)
```


## Are there differences in activity patterns between weekdays and weekends?

I will create a colored line plot to answer this question. Note that I have already created weekend and weekday
indicator variables. The code below performs a few operations that are necessary prior to making the plot.

```r
dta_finalplot <- dta_editmiss2 %>%
        group_by(interval, Weekend) %>%
        summarize(StepsEdit = mean(StepsEdit))
dta_finalplotEnd <- filter(dta_finalplot, Weekend == 1)
dta_finalplotDay <- filter(dta_finalplot, Weekend == 0)
dta_finalplot2 <- merge(dta_finalplotEnd, dta_finalplotDay,
        by.x = "interval", by.y = "interval")
dta_finalplot3 <- dta_finalplot2 %>%
        rename(WeekEndSteps = StepsEdit.x, WeekDaySteps = StepsEdit.y) %>%
        mutate(Interval = as.numeric(as.character(interval))) %>%
        select(Interval, WeekEndSteps, WeekDaySteps) %>%
        arrange(Interval)
```

Create a colored line plot using the ggplot2 graphics package.

```r
g <- ggplot(data=dta_finalplot3,
        mapping = aes(Interval, WeekDaySteps)) +
        ggtitle("Steps: Comparing Weekdays to Weekends") +
        xlab("Millitary Time") +
        ylab("Steps") +
        geom_line(color="Blue") +
        geom_line(mapping=aes(Interval, WeekEndSteps), color="Orange") +
        annotate("text", x=1500, y=225, color="Blue", label = "Weekday Steps") +
        annotate("text", x=1750, y=210, color="Orange", label = "Weekend Steps")
g
```

![unnamed-chunk-11-1](https://github.com/dsallstrom/RepData_PeerAssessment1/blob/master/unnamed-chunk-11-1.png)
