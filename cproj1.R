##Load needed packages
library(readr)
library(dplyr)
library(ggplot2)

##Get and unzip data
path<-getwd()
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,file.path(path,"cp1.zip"))
unzip(zipfile="cp1.zip")

##Read in data
activityData<-read_csv("activity.csv")

##Condense data
byDate<-as.POSIXct(activityData$date, "%Y-%m-%d")
day <- weekdays(activityData$date)
activity<-cbind(activityData,day)

##What is mean total number of steps taken per day?
##Calculate the total number of steps taken per day
totalSteps_Day<-aggregate(activity$steps,
                          by=list(activity$date),
                          FUN=sum,
                          na.rm=TRUE)
names(totalSteps_Day)<-c("date","steps")

##Create a histogram depicting total nuber of steps taken per day
hist(totalSteps_Day$steps,
     main="Total Steps per Day",
     xlab="Steps",
     ylim=c(0,40),
     col="turquoise4")
dev.off()

##Calculate the mean and median of the total steps taken per day
meanSteps<-round(mean(totalSteps_Day$steps,na.rm=TRUE),digits=2)
medSteps<-median(totalSteps_Day$steps,NA.RM=TRUE)
print(paste("The mean number of steps taken per day is",meanSteps,sep=" "))
print(paste("The median number of steps taken per day is",medSteps,sep=" "))

##What is the average daily activity pattern?
##Make a time series plot of the intervals and the average steps taken per day
avgSteps_Int<-aggregate(activity$steps,
                        by=list(activity$interval),
                        FUN=mean,
                        na.rm=TRUE)
names(avgSteps_Int)<-c("interval","mean")

plot(avgSteps_Int$interval,avgSteps_Int$mean,
     type="l",
     col="turquoise4",
     lwd=2,
     xlab="5-minute Interval",
     ylab="Average Steps Taken",
     main="Average Steps Taken per 5-minute Interval")
dev.off()

##Which 5-minute interval, on average across all days, contains the max number of steps?
maxAvg<-avgSteps_Int[which.max(avgSteps_Int$mean),]$interval
print(paste("The 5-minute interval with the maximum number of steps is",maxAvg,sep=" "))

##Imputing missing values
##Calculate and report the total number of missing values in the dataset 
totalNA<-sum(is.na(activity$steps))
print(paste("The total number of NAs in the dataset is",totalNA,sep=" "))

##Fill in all of the missing values in the dataset.
##Create a new dataset that is equal to the original dataset but with the missing data filled in
activityImputed<-activity
activityNA<-is.na(activityImputed$steps)
intervalAvg<-tapply(activity$steps, activity$interval, mean, na.rm=TRUE, simplify=T)
activityImputed$steps[activityNA]<-intervalAvg[as.character(activityImputed$interval[activityNA])]

##Make a histogram of the total number of steps taken each day
totalSteps_Day_Im<-aggregate(activityImputed$steps,
                             by=list(activityImputed$date),
                             FUN=sum,
                             na.rm=TRUE)
names(totalSteps_Day_Im)<-c("date","steps")

hist(totalSteps_Day_Im$steps,
     main="Total Steps per Day",
     xlab="Steps",
     ylim=c(0,40),
     col="turquoise4")
dev.off()

##Calculate and report the mean and median total number of steps taken per day. 
meanStepsIm<-round(mean(totalSteps_Day_Im$steps,na.rm=TRUE),digits=2)
medStepsIm<-round(median(totalSteps_Day_Im$steps,NA.RM=TRUE),digits=2)
print(paste("For the imputed dataset,the mean number of steps taken per day is",meanStepsIm,sep=" "))
print(paste("For the imputed dataset,the median number of steps taken per day is",medStepsIm,sep=" "))

##Are there differences in activity patterns between weekdays and weekends?
##Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
dayType<-sapply(activity$day,function(x)
        {if(x=="Saturday"|x=="Sunday")
                {y<-"Weekend"} else
                {y<-"Weekday"}
                y
        })

##Make a panel plot containing a time series plot of interval vs avg number of steps, averaged across all weekday days or weekend days 
avgPerInt_dayType<-aggregate(steps~interval + dayType, activity, mean, na.rm = TRUE)
ggplot(avgPerInt_dayType,aes(x = interval , y = steps, color = dayType)) +
        geom_line() +
        scale_color_manual(values = c("turquoise4", "maroon4")) +
        labs(title = "Average steps per Interval by type of day", x = "Interval", y = "Average number of steps") +
        facet_wrap(~dayType, ncol = 1, nrow=2)
dev.off()





