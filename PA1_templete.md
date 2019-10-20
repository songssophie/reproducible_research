### Loading and preprocessing the data
```{r global_options, include=TRUE}
library(ggplot2)
library(dplyr)
library(plyr)
df <- read.csv("activity.csv") #read data
```

### Task1: What is mean total number of steps taken per day?
```{r}
df1 <- df %>% filter(df$steps != "NA") %>% group_by(date) %>% dplyr::summarise(sum_step=sum(steps),mean_step=mean(steps),median_step=median(steps))#calculate the total number, the mean and the median of steps taken per day
df1 # display the total number, the mean and the median of steps taken per day

ggplot(df1,aes(x=sum_step))+geom_histogram(bins=20,fill="steelblue",color="white")+labs(title = "Histogram for the total No. of steps taken per  day")+theme(plot.title=element_text(hjust=0.5)) #draw histogram


```

  
### Task2: What is the average daily activity pattern?
```{r}
df2 <- df %>% filter(df$steps != "NA") %>% group_by(interval) %>% dplyr::summarise(mean_step = mean(steps)) #tidy the data
plot(mean_step~interval,data=df2,type="l") #draw the graph
df2[which.max(df2$mean_step),] #Find Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
```

  
### Task3: Imputing missing values
```{r}
df3 <- df
plyr::count(df[is.na(df$steps),]$steps) #calculate the total number of missing values

#filling in all of the missing values in the dataset with the mean for that 5-minute interval
for (i in 1:nrow(df3)) {
  if (is.na(df3$steps[i])){
    df3$steps[i] = df2$mean_step[which(df2$interval == df3$interval[i])]
  } 
}

#Calculate the mean and median total number of steps taken per day
df4 <- df3 %>% group_by(date) %>% dplyr::summarise(sum_step=sum(steps),mean_step=mean(steps),median_step=median(steps))
#report the mean and median total number of steps taken per day
df4

#draw histogram. Imputing missing data on the estimates of the total daily number of steps changes the shape of the graph
ggplot(df4,aes(x=sum_step))+geom_histogram(bins=20,fill="steelblue",color="white")+labs(title = "Histogram for the total No. of steps taken per  day")+theme(plot.title=element_text(hjust=0.5))

```

  
### Task 4: Are there differences in activity patterns between weekdays and weekends?
```{r}
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
df5 <- df3
df5$date <- as.Date(df5$date)
df5$week <- weekdays(df5$date)
df5$weekends_or_not <- ifelse(df5$week %in% c("Sunday","Saturday"),"weekend","weekday")

#calculate the average number of steps taken, averaged across all weekday days or weekend days
df6 <- df5 %>% group_by(interval,weekends_or_not) %>% dplyr::summarise(mean_step=mean(steps))

#Make the panel plot
ggplot(df6,aes(x=interval,y=mean_step))+geom_line()+facet_grid(weekends_or_not~.)
```
