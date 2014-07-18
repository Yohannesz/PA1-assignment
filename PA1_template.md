R markdown written for Reproducible reaserach assignment 1

# Peer Assignment 1

### 1. Read data into R


```r
activity<-read.table("./activity.csv", sep=",", header=TRUE)
```

```
## Warning: cannot open file './activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
head(activity)
```

```
## Error: object 'activity' not found
```

Next let us plot histogram of total steps taken per day

### 1. Histogram od steps per day


```r
library(ggplot2)
Tsteps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
```

```
## Error: object 'activity' not found
```

```r
qplot(Tsteps, binwidth=800, xlab="Total number of steps taken per day", main="Histograms of Total number of steps")
```

```
## Error: object 'Tsteps' not found
```

### 2. Mean and Median of Steps

```r
mean(Tsteps, na.rm=TRUE)
```

```
## Error: object 'Tsteps' not found
```

```r
median(Tsteps, na.rm=TRUE)
```

```
## Error: object 'Tsteps' not found
```

## Average activity pattern 

### 1. Average daily activity pattern


```r
library(ggplot2)
average<- aggregate(x=list(s=activity$steps), by=list(i=activity$interval), FUN=mean, na.rm=TRUE)
```

```
## Error: object 'activity' not found
```

```r
ggplot(data=average, aes(x=i, y=s)) +
        geom_line() +
        xlab("5 minute interval") +
        ylab("average number of steps ") + 
        ggtitle("Average daily activity pattern for 5 min interval")
```

```
## Error: object 'average' not found
```

### 2. The maximum average number of steps taken on 

```r
average[which.max(average$s),]
```

```
## Error: object 'average' not found
```

## Imputting missing values

### 1. Total # of missing values

```r
missing <- is.na(activity$steps)
```

```
## Error: object 'activity' not found
```

```r
# Number of missing values Table
table(missing)
```

```
## Error: unique() applies only to vectors
```



Missing values will be filled in with mean value for that 5-minute
interval.


```r
# Replace each missing value with the mean value of its 5-minute interval
fill <- function(s, i) {
        filled <- NA
        if (!is.na(s))
                filled <- c(s)
        else
                filled <- (average[average$i==i, "s"])
        return(filled)
        }
# make a new data set form the activity data set and substitute the missing values
new_activity <- activity
```

```
## Error: object 'activity' not found
```

```r
new_activity$steps <- mapply(fill, new_activity$steps, new_activity$interval)
```

```
## Error: object 'new_activity' not found
```

```r
head(new_activity)
```

```
## Error: object 'new_activity' not found
```

### Histogram of total number of steps taken each day


```r
total.steps <- tapply(new_activity$steps, new_activity$date, FUN=sum)
```

```
## Error: object 'new_activity' not found
```

```r
qplot(total.steps, binwidth=800, xlab="Total number of steps taken each day")
```

```
## Error: object 'total.steps' not found
```

```r
mean(total.steps)
```

```
## Error: object 'total.steps' not found
```

```r
median(total.steps)
```

```
## Error: object 'total.steps' not found
```
The mean and median of the current data set is diffrent from the origional data set. Both mean and median are higher. For any NA score values, on the previous analysis the values were set to zero. But now when we replace the NA values with the mean values of each interval, the mean and median of the intervals tend to be higher. 


## Activity paterns

### 1. Create a new factor variable


```r
week <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                return("weekday")
        else if (day %in% c("Saturday", "Sunday"))
                return("weekend")
        else
                stop("invalid date")
        }
new_activity$date <- as.Date(new_activity$date)
```

```
## Error: object 'new_activity' not found
```

```r
new_activity$day <- sapply(new_activity$date, FUN=week)
```

```
## Error: object 'new_activity' not found
```

```r
## tabulate the day variable
table(new_activity$day)
```

```
## Error: object 'new_activity' not found
```
### 2. make panel plot of 5-minuite interval averaged over weekdays or weekends


```r
averages <- aggregate(steps ~ interval + day, data=new_activity, mean)
```

```
## Error: object 'new_activity' not found
```

```r
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Average number of steps") + ggtitle("Average number of steps on 5-minute interval")
```

```
## Error: object 'averages' not found
```



