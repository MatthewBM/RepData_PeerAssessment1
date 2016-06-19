Repoducibility Research Assignment 1
====================================

Load data


```r
act <- read.csv("activity.csv", header= T, na.strings = "NA")
```

Calculate the total number of steps per day


```r
actf <- act[act$steps != 0,]
actf <- na.omit(actf)
actf <- aggregate(actf$steps, by = list(actf$date), sum)
```

Produce a histogram of the total number of steps per day


```r
actf <- data.frame(as.character(actf$Group.1),actf$x)
colnames(actf)<- c("Date","TotalSteps")
hist(actf$TotalSteps, breaks = length(actf$Date),col = "gray", xlab="Total Number of Steps", main= "Total Number of Steps Per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Calculate the mean and median total number of steps

```r
print(paste("Mean Total Number of Steps per day: ", round(mean(actf$TotalSteps, na.rm = T), digits = 2)))
```

```
## [1] "Mean Total Number of Steps per day:  10766.19"
```

```r
print(paste("Median Total Number of Steps per day: ", median(actf$TotalSteps, na.rm = T)))
```

```
## [1] "Median Total Number of Steps per day:  10765"
```

Calculate and plot the activity level for the 5-minute intervals averaged across all days


```r
actf <- act[act$steps != 0,]
actf <- na.omit(actf)
actf <- aggregate(actf$steps, by = list(actf$interval), mean)
actf <- data.frame(as.character(actf$Group.1),actf$x)
colnames(actf)<- c("Interval","AverageNumberofSteps")
plot(as.numeric(as.character(actf$Interval)),actf$AverageNumberofStep,type="l", col = "darkgreen", ylab="Average Number of Steps", xlab= "Interval", main="Average Steps per Interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Show the interval with the maximum number of steps


```r
actfmax <- actf[actf$AverageNumberofSteps== max(actf$AverageNumberofSteps),]
print(as.character(actfmax[[1]][[1]]))
```

```
## [1] "835"
```

Report number of missing values


```r
sum(is.na(as.character(act$steps)))
```

```
## [1] 2304
```

Fill in missing values with average for the interval


```r
compact <- act
for(i in 1:nrow(act)){
  if(is.na(act$steps[i]) == T){
    compact$steps[i] <- actf[act$interval[i]==actf$Interval,]$AverageNumberofSteps[1]
  }
}
```

Produce a histogram of the total number of steps per day of complete data

```r
actf <- compact
actf <- na.omit(actf)
actf <- aggregate(actf$steps, by = list(actf$date), sum)
actf <- data.frame(as.character(actf$Group.1),actf$x)
colnames(actf)<- c("Date","TotalSteps")
hist(actf$TotalSteps, breaks = length(actf$Date),col = "gray", xlab="Total Number of Steps", main= "Total Number of Steps Per Day2")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

differences in activity patterns between weekdays and weekends plot

```r
compact$day <- as.factor(weekdays(as.Date(compact$date)))
compact$weekday <- ifelse(!(compact$day %in% c("Saturday","Sunday")), TRUE, FALSE) 
weekdays <- compact[compact$weekday,]
weekdays <- aggregate(weekdays$steps, by=list(interval=weekdays$interval), FUN=mean)
weekends <- compact[!compact$weekday,]
weekends <- aggregate(weekends$steps, by=list(interval=weekends$interval), FUN=mean)
colnames(weekdays) <- c("interval", "averagesteps")
colnames(weekends) <- c("interval", "averagesteps")
weekdays$day <- "Weekday"
weekends$day <- "Weekend"
week <- rbind(weekends,weekdays)
weekday <- as.factor(week$day)
library(lattice)
xyplot(averagesteps ~  interval | day, data = week, layout = c(1,2), type ="l", ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)



