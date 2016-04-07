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
                Weekend = ifelse(Weekday == "Saturday" | Weekday == "Sunday", "Weekend", "Weekday")) %>%
        select(steps, interval, Date, Weekday, Weekend)
dta$interval <- as.factor(dta$interval)
dta$Weekend <- as.factor(dta$Weekend)
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

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

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

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)

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

I will create a panel plot to answer this question. The code below finds the mean number of steps for every interval.

```r
dta_finalplot <- dta_editmiss2 %>%
        group_by(interval, Weekend) %>%
        summarize(StepsEdit = mean(StepsEdit)) %>%
        mutate(Interval = as.numeric(as.character(interval)))
```


The plot below shows the average number of steps within each time interval. The plot on top shows data from weekdays whereas the plot on bottom displays data from weekends.

The graphs show a noticeable - though not drastic - difference in activity patters between weekend days and weekdays.

```r
g <- ggplot(data=dta_finalplot,
        mapping = aes(Interval, StepsEdit)) +
        xlab("Interval") +
        ylab("Steps") +
        geom_line(aes(Interval, StepsEdit), color="Blue") +
        facet_grid(Weekend ~ .)
g
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)
