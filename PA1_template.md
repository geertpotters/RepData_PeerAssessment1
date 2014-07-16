---
title: "Reproducible Research, Assignment 1"
author: "Geert Potters"
date: "Monday, July 14, 2014"
output: html_document
---
As provided in the assignment, we will "make use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day."

##Setting up the data

The first step in this analysis involves downloading and loading the proper dataset. The file is downloaded from the internet, saved and unzipped into the working directory (and there starts the code). 

```{r}
setwd("C:/R_exerc/repro")
unzip("activity.zip")
activity<-read.csv("activity.csv", header=TRUE, sep=",")
```

##What is the mean total number of steps taken per day?
The first assignment involved investigating how many steps the test person made per day on average. To investigate this, we created a histogram of the number of steps per day. 


```{r, results='asis'}
totalsteps<-tapply(activity$steps, activity$date, sum)
hist(totalsteps)
medianSteps<-median(totalsteps, na.rm=TRUE)
meanSteps<-as.integer(mean(totalsteps, na.rm=TRUE))
```

On average, the test person made `r meanSteps` steps. The corresponding median value is `r medianSteps`. All in all, a fairly good normal distribution.

##What is the test subject's average daily activity pattern?
In this section, we have a look at the pattern of steps throughout the day. First, we make a time plot. 

```{r}
ts<-as.matrix(tapply(activity$steps, activity$interval, mean, na.rm=TRUE))
ts2<-cbind(levels(as.factor(activity$interval)), ts)
plot(ts2, type="l", xlab="interval", ylab="average number of steps", col=4)
intMax<-as.numeric(ts2[which.max(ts2[,2]),1])
```
The interval in which the maximum number of steps were recorded was `r intMax` - half past nine, probably linked to his getting to work. There are little to no data between 11 pm and 5.30 am. This person enjoys short nights. The number of steps rises fast around 5.30 am, which is probably the moment his clock radio starts playing.


##How do we deal with missing values?
Of course, some data points are missing. But how many ? To find out, we answer the question how many rows actually have the value NA. 

```{r}
# Calculate the number of rows with missing values.
numNA<-length(activity$steps[is.na(activity$steps)])
```

The number of NAs in the data set is `r numNA`. Rather a lot. Now, to deal with missing values, we have three options: change all NAs into 0, replacing them by the value of the original mean, or by the value of the original median. We will go through these three possibilities and create a histogram for each option. Finally, we create a table to compare these choices. 

```{r}
# Replace all NA's with 0
activity2 <- replace(activity, is.na(activity), 0)
totalsteps2<-tapply(activity2$steps, activity2$date, sum)
hist(totalsteps2, xlab="steps", ylab="frequency", 
     main="Steps histogram, NA set to 0")
medianSteps2<-median(totalsteps2, na.rm=TRUE)
meanSteps2<-mean(totalsteps2, na.rm=TRUE)


# Replace all NA's with the value of the original mean
activity3 <- replace(activity, is.na(activity), meanSteps)
totalsteps3<-tapply(activity3$steps, activity3$date, sum)
hist(totalsteps3, xlab="steps", ylab="frequency", 
     main="Steps histogram, NA set to mean")
medianSteps3<-median(totalsteps3, na.rm=TRUE)
meanSteps3<-mean(totalsteps3, na.rm=TRUE)

# Replace all NA's with the value of the original median
activity4 <- replace(activity, is.na(activity), medianSteps)
totalsteps4<-tapply(activity4$steps, activity4$date, sum)
hist(totalsteps4, xlab="steps", ylab="frequency", 
     main="Steps histogram, NA set to median")
medianSteps4<-median(totalsteps4)
meanSteps4<-mean(totalsteps4)

```

Now we can make a comparison between these four analyses. 

```{r, results='asis'}
library(xtable)
r1<-c("with NAs", "NAs set to 0", "NAs set to mean", "NAs set to median")
r2<-c(medianSteps, medianSteps2, medianSteps3, medianSteps4)
r3<-c(meanSteps,meanSteps2,meanSteps3,meanSteps4)
table<-rbind(r2, r3)
colnames(table)<-r1
rownames(table)<- c("median", "mean")
print(xtable(table), type="html")
```

Given the totally distorted distribution of the number of steps when we replace NA by either the mean or the median, we conclude that only replacing the NA with 0 is an acceptable solution.

##Are there differences between weekdays and weekends?
The final question is, whether the guy walks more or less during the weekends as compared to during the working week. Let's find out, by plotting his stepping pattern for both kinds of days. 

```{r}
#First we separate weekday data from weekend data
activity2$date<-strptime(activity2$date, format="%Y-%m-%d")
activity2$date<-weekdays(activity2$date)
days<-levels(as.factor(activity2$date))
weekdays<-days[1:5]
weekend<-days[6:7]
weekdayTs<-c(NULL, NULL)
weekendTs<-c(NULL, NULL)
for (i in 1:length(activity2$date)){
     if (any(weekdays == activity2$date[[i]])){
          weekdayTs<-rbind(weekdayTs, activity2[i,])
     };
     if (any(weekend == activity2$date[[i]])){
          weekendTs<-rbind(weekendTs, activity2[i,])
     }
}

# Now we average the number of steps per interval for the weekdays...
colnames(weekdayTs)<-colnames(activity2)
tswd<-as.matrix(tapply(weekdayTs$steps, weekdayTs$interval, mean))

#... and the same for the weekend data:
colnames(weekendTs)<-colnames(activity2)
tswe<-as.matrix(tapply(weekendTs$steps, weekendTs$interval, mean))

#We plot both data sets on one figure:
par(mfrow=c(2,1))
plot(levels(as.factor(weekendTs$interval)), tswe, type="l", main="weekend", 
     xlab="interval", ylab="average number of steps", col=4)
plot(levels(as.factor(weekdayTs$interval)), tswd, type="l", main="weekday", 
     xlab="interval", ylab="average number of steps", col=4)
par(mfrow=c(1,1))
```
We note that while on weekdays, there is a large peak of steps (indeed, the maximum) in the morning, there is a much more even distribution during the weekend. Also, he gets up later during the weekend. This concludes the analysis. 