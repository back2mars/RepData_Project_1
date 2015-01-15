# Reproducible Research: Peer Assessment 1



```r
echo = TRUE
```

## Loading libraries 


```r
library(plyr)
library(lattice)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```

## Loading and preprocessing the data


```r
unzip("activity.zip")
rdata <- read.csv("activity.csv")
rdata$date <- as.Date(rdata$date)
```

## What is mean total number of steps taken per day?

Plot the histogram of total number of steps taken per day:

```r
perday.steps <- tapply(rdata$steps, rdata$date, FUN=sum, na.rm=TRUE)
qplot(perday.steps, binwidth=1200, xlab = "Total Number of Steps Taken Per Day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

The mean is:

```r
mean(perday.steps, na.rm=TRUE)
```

```
## [1] 9354.23
```
The median is:

```r
median(perday.steps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Plot a time series:

```r
ndata <- na.omit(rdata)
raverage <- ddply(ndata, ~interval, summarize, mean=mean(steps))
qplot(x=interval, y=mean, data=raverage, geom="line", xlab="5-Minute Interval", ylab="Number of Steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

The 5-minute interval contains the maximum number of steps on average across all the days:

```r
raverage[which.max(raverage$mean), ]
```

```
##     interval     mean
## 104      835 206.1698
```

## Imputing missing values

How many missing values:

```r
sum(is.na(rdata))
```

```
## [1] 2304
```
Make a new histogram:

```r
raverage <- aggregate(x = list(steps = rdata$steps), by = list(interval=rdata$interval), FUN="mean", na.rm=TRUE)
names(raverage)[2] <- "meanstep"
nndata <- rdata
for (i in 1:nrow(nndata)) {
  if (is.na(nndata$steps[i])) {
    nndata$steps[i] <- raverage[which(nndata$interval[i] == raverage$interval), ]$meanstep
  }
}
perday.steps <- tapply(nndata$steps, nndata$date, FUN=sum)
qplot(perday.steps, binwidth=1200, xlab = "Total Number of Steps Taken Per Day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

Now the mean is:

```r
mean(perday.steps)
```

```
## [1] 10766.19
```
Now the median is:

```r
median(perday.steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

Create a factor variable:

```r
nndata$weektime <- factor(format(nndata$date, "%A"))
levels(nndata$weektime) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), weekend = c("Saturday", "Sunday"))
```

Make plots for weekdays and weekends:

```r
raverages <- aggregate(nndata$steps, list(interval=as.numeric(as.character(nndata$interval)), weektime = nndata$weektime), FUN="mean")
names(raverages)[3] <- "meanstep"
xyplot(raverages$meanstep ~ raverages$interval | raverages$weektime,
       type = 'l', layout = c(1,2),
       xlab = "5-Minute Interval",
       ylab = "Number of Steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

### Conclusion
* Yes, the plot reveals people's activity pattern in weekdays is different to that in weekends.
