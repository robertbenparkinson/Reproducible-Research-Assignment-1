---
title: "Reproducible Research Assignment 1"
author: "Robert Ben Parkinson"
date: "24 Febraury 2016"
output: html_document
---
1: Code for reading Activity Monitor Data

```{r}
step <-   read.csv("data/activity.csv", sep =",")

library(dplyr)

uni <- unique(step$interval)

df <- data.frame(s.Interval=integer(),
                 s.Total=integer(),
                 s.Mean=integer(),
                 s.Median=integer(),
                 s.NA=character(),
                 stringsAsFactors = FALSE)

for (i in uni){
        x <- subset(step, interval==i)
        y <- sum(is.na(x$steps))
        x <- na.omit(x)
        df[nrow(df)+1,] <- c(i, sum(x$steps), mean(x$steps), median(x$steps), y)
              }

df <- na.omit(df)


```
2: Historgram of the Total Number of Step Taken Each Day

```{r, echo=TRUE}

hist(df$s.Total, col = "green", main = "Number of Steps", xlab = "Steps")


```

3: Mean and Median Number of Step 
Limited to the top 50

```{r}
head(df[, 3:4], n=50)
    

```

4: Time Series Plot set at Five Minute Intervals

```{r, echo=TRUE}
plot(df$s.Interval, df$s.Total, type="l", 
col="green", main = "Number of Steps Taken",
xlab = "Five Minute Intervals",
ylab = "Steps")


```

5: The 5-Minute Interval with the Maxium Steps. Top 5

```{r}
library(dplyr)
maxsteps <- arrange(step, desc(steps))
head(maxsteps, n=5)

```
6: Strategy for Inputing Missing Data
I took the average number of steps per interval and across the whole study.
I then subsituted in the figure (~37.1 steps) for all of the missing values.
This figures is probally high.

```{r}
step2 <- step
step2[is.na(step2)] <- mean(df$s.Mean)

```
7: Historgram of the Total Number of Step Taken Each Day With Missing Data

```{r, echo=TRUE}
for (i in uni){
  
  x <- subset(step2, interval==i)
  y <- sum(is.na(x$steps))
  x <- na.omit(x)
  df[nrow(df)+1,] <- c(i, sum(x$steps), mean(x$steps), median(x$steps), y)
}

df <- na.omit(df)

hist(df$s.Total, col = "green", main = "Number of Steps", xlab = "Steps")


```
His is the Adjusted Mean and Median Number of Step with the extra 37.1 steps per interval added in.
The figures are limited to the first 50.

```{r}
head(df[, 3:4], n=50)
```


8: Average Number of Steps taken on a Weekday and Weekend


```{r, echo=TRUE}
step2 <- mutate(step2, dayofweek=(weekdays(as.Date(step2$date, "%Y-%m-%d"))))

s.weekend <- subset(step2, dayofweek == c("Saturday","Sunday"))
s.weekday <- subset(step2, dayofweek != c("Saturday","Sunday"))

for (i in uni){
  
  x <- subset(s.weekend, interval==i)
  y <- sum(is.na(x$steps))
  x <- na.omit(x)
  df.weekend <- df
  df.weekend[nrow(df.weekend)+1,] <- c(i, sum(x$steps), mean(x$steps), median(x$steps), y)
}


df.weekend <- na.omit(df.weekend)

for (i in uni){
  
  x <- subset(s.weekday, interval==i)
  y <- sum(is.na(x$steps))
  x <- na.omit(x)
  df.weekday <- df
  df.weekday[nrow(df.weekday)+1,] <- c(i, sum(x$steps), mean(x$steps), median(x$steps), y)
}


df.weekday <- na.omit(df.weekday)


par(mfrow = c(2,1), mar= c(2, 4, 2, 2))
plot(df.weekend$s.Interval, df.weekend$s.Total  , type="l", col="green", 
     main = "Number of Steps Taken on a Weekend",
     xlab = "Five Minute Intervals",
     ylab = "Average Steps taken on a Weekday")

plot(df.weekday$s.Interval, df.weekday$s.Total  , type="l", col="green", 
     main = "Number of Steps Taken on a Weekday",
     xlab = "Five Minute Intervals",
     ylab = "Average Steps taken on a Weekday")


```

9: All Code used for this Assignement.

```{r, echo=TRUE}
step <-   read.csv("data/activity.csv", sep =",")

library(dplyr)

uni <- unique(step$interval)

df <- data.frame(s.Interval=integer(),
                 s.Total=integer(),
                 s.Mean=integer(),
                 s.Median=integer(),
                 s.NA=character(),
                 stringsAsFactors = FALSE)

for (i in uni){
  
        x <- subset(step, interval==i)
        y <- sum(is.na(x$steps))
        x <- na.omit(x)
        df[nrow(df)+1,] <- c(i, sum(x$steps), mean(x$steps), median(x$steps), y)
              }


df <- na.omit(df)

hist(df$s.Total, col = "green", main = "Number of Steps", xlab = "Steps")

df[, 3:4]


plot(df$s.Interval, df$s.Total  , type="l", col="green", 
     main = "Number of Steps Taken",
     xlab = "Five Minute Intervals",
     ylab = "Steps")

library(dplyr)
maxsteps <- arrange(step, desc(steps))
head(maxsteps, n=5)

step2 <- step
step2[is.na(step2)] <- mean(df$s.Mean)


for (i in uni){
  
  x <- subset(step2, interval==i)
  y <- sum(is.na(x$steps))
  x <- na.omit(x)
  df[nrow(df)+1,] <- c(i, sum(x$steps), mean(x$steps), median(x$steps), y)
}


df <- na.omit(df)

hist(df$s.Total, col = "green", main = "Number of Steps", xlab = "Steps")

step2 <- mutate(step2, dayofweek=(weekdays(as.Date(step2$date, "%Y-%m-%d"))))

s.weekend <- subset(step2, dayofweek == c("Saturday","Sunday"))
s.weekday <- subset(step2, dayofweek != c("Saturday","Sunday"))

for (i in uni){
  
  x <- subset(s.weekend, interval==i)
  y <- sum(is.na(x$steps))
  x <- na.omit(x)
  df.weekend <- df
  df.weekend[nrow(df.weekend)+1,] <- c(i, sum(x$steps), mean(x$steps), median(x$steps), y)
}


df.weekend <- na.omit(df.weekend)

for (i in uni){
  
  x <- subset(s.weekday, interval==i)
  y <- sum(is.na(x$steps))
  x <- na.omit(x)
  df.weekday <- df
  df.weekday[nrow(df.weekday)+1,] <- c(i, sum(x$steps), mean(x$steps), median(x$steps), y)
}


df.weekday <- na.omit(df.weekday)


par(mfrow = c(2,1), mar= c(2, 4, 2, 2))
plot(df.weekend$s.Interval, df.weekend$s.Total  , type="l", col="green", 
     main = "Number of Steps Taken on a Weekend",
     xlab = "Five Minute Intervals",
     ylab = "Average Steps taken on a Weekday")

plot(df.weekday$s.Interval, df.weekday$s.Total  , type="l", col="green", 
     main = "Number of Steps Taken on a Weekday",
     xlab = "Five Minute Intervals",
     ylab = "Average Steps taken on a Weekday")


```

FIN