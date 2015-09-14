---
output: html_document
---
#Reproducible Research
##Peer Assessment 1
###Loading and preprocessing the data
```{r, echo=TRUE}
# Load the data
sData <- read.csv("activity.csv")
# Process/transform the data
sData$date <- as.Date(sData$date)
```

###What is mean total number of steps taken per day?
```{r, echo=TRUE}
library(ggplot2)
#Calculate the total number of steps taken per day
dailyStepsCount <- aggregate(x = sData$steps , by = list(sData$date), FUN = sum ,na.rm=TRUE)
names(dailyStepsCount) <- c("date","steps")
#Make a histogram of the total number of steps taken each day
qplot(x = steps, data =dailyStepsCount , 
        main = "Daily steps count Histogram" ,
        xlab = "Steps (binwidth 2500)",
        geom = "histogram",
        binwidth = 2500
      )
#Calculate and report the mean and median of the total number of steps taken per day
#1. mean
mean(dailyStepsCount$steps , na.rm = TRUE)
#2.median
median(dailyStepsCount$steps , na.rm = TRUE)
```

###What is the average daily activity pattern?
```{r, echo= TRUE}
#1. Time series plot of 5-minute interval and the average number of steps taken, averaged across all days
stepsByIntervalAverage  <- aggregate(x = sData$steps , by = list(sData$interval), FUN = mean ,na.rm=TRUE)
names(stepsByIntervalAverage) <- c("interval","steps")
qplot(x=stepsByIntervalAverage$interval,
      y =stepsByIntervalAverage$steps,
      geom = "line",
      main = "Make a time series plot (Steps / Interval)",
        data= stepsByIntervalAverage,
      xlab = "Interval",
      ylab= "Steps"
      
      )

#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
stepsByIntervalAverage[which.max(stepsByIntervalAverage$steps),c("interval")]
```


###Imputing missing values
```{r, echo= TRUE}
#1. Calculate and report the total number of missing values in the dataset
nrow(sData[is.na(sData$steps),])

#2. imputing missing step values with mean step at time interval
sDataImputed <- merge(x = sData, y = stepsByIntervalAverage, by = "interval", all.x = TRUE)
sDataImputed[is.na(sDataImputed$steps.x),c("steps.x")] <- sDataImputed[is.na(sDataImputed$steps.x),c("steps.y")]

#3. cleaning data
sDataImputed$date <- as.Date(sDataImputed$date)
sDataImputed$date.x <- NULL
sDataImputed$Group.1 <- NULL
sDataImputed$steps <- sDataImputed$steps.x
sDataImputed$steps.x <- NULL
sDataImputed$steps.y <- NULL

#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
dailyStepsCount <- aggregate(x = sDataImputed$steps , by = list(sDataImputed$date), FUN = sum,na.rm=TRUE)
names(dailyStepsCount) <- c("date","steps")

qplot(dailyStepsCount$steps, 
      data = dailyStepsCount,
      geom = "histogram",
      binwidth = 2500,
      main = "Histogram of daily steps after imputation",
      xlab = "Steps (binwidth 2500)"
      )
```


###Are there differences in activity patterns between weekdays and weekends?
```{r, echo=TRUE}
#mean total number of steps taken per day
mean(dailyStepsCount$steps , na.rm = TRUE)
#median total number of steps taken per day
median(dailyStepsCount$steps , na.rm = TRUE)

#1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
sDataImputed$weekday <- as.factor(ifelse(weekdays(sDataImputed$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday")) 

stepsByIntervalAverageAndWeekType  <- aggregate(x = sDataImputed$steps , 
                                                    by = list(sDataImputed$interval,sDataImputed$weekday), FUN = mean ,na.rm=TRUE)
names(stepsByIntervalAverageAndWeekType) <- c("interval","weekday","steps")

#panel time series plot of the 5-minute interval and the average number of steps taken 
#averaged across all weekday days or weekend days.
ggplot(stepsByIntervalAverageAndWeekType,aes(interval,steps)) +
                 ggtitle("Time Series Plot of Average Steps by Interval after Imputation") +
                 facet_grid(. ~ weekday) +
                 geom_line(size = 1)
```