Load the Data:

    activity <- read.csv("activity.csv")

What is the mean total number of steps taken per day
----------------------------------------------------

### Calculate total number of steps taken per day (here you can see the first 6 rows of the table dataframe created)

    activity1 <- activity[ with (activity, { !(is.na(steps)) } ), ]
    table_sum <- aggregate(activity1["steps"], by=activity1["date"], sum)
    head(table_sum)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

### Histogram of the total number of steps taken each day

    hist(table_sum$steps, main="Histogram of total number of steps per day", 
         xlab="Total number of steps in a day")

![](%5Bcoursera%5DReproducibleSearchW2_files/figure-markdown_strict/total2-1.png)

### Calculate and report the mean and median of the total number of steps taken per day

    neat_table <- matrix(c(mean(table_sum$steps),median(table_sum$steps)))
    rownames(neat_table) <- c("Mean","Median")
    colnames(neat_table) <- "Steps"
    as.table(neat_table)

    ##           Steps
    ## Mean   10766.19
    ## Median 10765.00

What is the average daily activity pattern?
-------------------------------------------

### Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    #to left side ~: what is being aggregated and to right side: what is being used to aggregate
    library(ggplot2)
    steps_by_interval <- aggregate(steps ~ interval, activity1, mean)
    ggplot(steps_by_interval,aes(x=interval,y=steps)) +
      ggtitle("Average number of steps across all days") +
      xlab("5-minute Interval") + ylab("Average number of steps") +
      geom_line() 

![](%5Bcoursera%5DReproducibleSearchW2_files/figure-markdown_strict/plot-1.png)
\#\#\#Which 5-minute interval, on average across all the days in the
dataset, contains the maximum number of steps?

    # find row with max of steps
    max_steps_row <- which.max(steps_by_interval$steps)

    # interval corresponding to max number of steps:
    answer <- steps_by_interval[max_steps_row, ]$interval

The 5-min interval which contains the maximum number of steps is 835

Total number missing values

    sum(is.na(activity$steps))

    ## [1] 2304

2. Devise a strategy for filling in all of the missing values in the dataset.
-----------------------------------------------------------------------------

Decided to use the mean for the steps of that day: get the day in
aggregate and pull out mean

    activity_mod <- activity

    # Loop through all the rows of activity, find the one with NA for steps.
    # For each identify the interval for that row
    # Then identify the avg steps for that interval in avg_steps_per_interval
    # Substitute the NA value with that value

    for (i in 1:nrow(activity_mod)) {
      if(is.na(activity_mod$steps[i])) {
        val <- steps_by_interval$steps[which(steps_by_interval$interval == activity$interval[i])]
        activity_mod$steps[i] <- val 
      }
    }

    # Aggregate the steps per day with the imputed values
    steps_per_day_impute <- aggregate(steps ~ date, activity_mod, sum)

    hist(steps_per_day_impute$steps, main="Histogram of total number of steps per day (NA filled in)", 
         xlab="Total number of steps in a day")

![](%5Bcoursera%5DReproducibleSearchW2_files/figure-markdown_strict/modif-1.png)

### Calculate and report the mean and median total number of steps taken per day.

### Do these values differ from the estimates from the first part of the assignment?

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

    neat_table2 <- matrix(c(mean(steps_per_day_impute$steps),median(steps_per_day_impute$steps)))
    rownames(neat_table2) <- c("Mean","Median")
    colnames(neat_table2) <- "Steps"
    as.table(neat_table2)

    ##           Steps
    ## Mean   10766.19
    ## Median 10766.19

Mean has not changed, Median increased slightly.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

### 2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

    #function check if day is weekday
    #convert to date bc weekdays() works with POSIXt or Date.

    week_day <- function(date_val) {
      wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
      if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
      } else {
        x <- 'Weekend'
      }
      x
    }


    # Apply the week_day function and add a new column called day_type to activity dataset
    activity_mod$day_type <- as.factor(sapply(activity_mod$date, week_day))

    #load the ggplot library
    library(ggplot2)

    # Create the aggregated data frame by intervals and day_type
    steps_per_day_impute <- aggregate(steps ~ interval+day_type, activity_mod, mean)

    # Create the plot facet_grid is to split it in 2 graphs. the . represents nothing in the columns view. 
    ggplot(steps_per_day_impute, aes(interval, steps)) +
      geom_line(aes(colour = day_type)) +
      facet_grid(day_type ~ .) +
      labs(x="Interval", y=expression("No of Steps")) +
      ggtitle("No of steps Per Interval by day type")

![](%5Bcoursera%5DReproducibleSearchW2_files/figure-markdown_strict/weekday-1.png)
