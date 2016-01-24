# Reproducible Research Assignment 1
Robert Ben Parkinson  
24-Feb-16  
1: Code for reading Activity Monitor Data


```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
con <- unz(temp, "activity.csv")
step <- read.csv(con, sep =",")
unlink(temp)


library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
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


```r
hist(df$s.Total, col = "green", main = "Number of Steps", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

3: Mean and Median Number of Step 
Limited to the top 50


```r
head(df[, 3:4], n=50)
```

```
##       s.Mean s.Median
## 1  1.7169811        0
## 2  0.3396226        0
## 3  0.1320755        0
## 4  0.1509434        0
## 5  0.0754717        0
## 6  2.0943396        0
## 7  0.5283019        0
## 8  0.8679245        0
## 9  0.0000000        0
## 10 1.4716981        0
## 11 0.3018868        0
## 12 0.1320755        0
## 13 0.3207547        0
## 14 0.6792453        0
## 15 0.1509434        0
## 16 0.3396226        0
## 17 0.0000000        0
## 18 1.1132075        0
## 19 1.8301887        0
## 20 0.1698113        0
## 21 0.1698113        0
## 22 0.3773585        0
## 23 0.2641509        0
## 24 0.0000000        0
## 25 0.0000000        0
## 26 0.0000000        0
## 27 1.1320755        0
## 28 0.0000000        0
## 29 0.0000000        0
## 30 0.1320755        0
## 31 0.0000000        0
## 32 0.2264151        0
## 33 0.0000000        0
## 34 0.0000000        0
## 35 1.5471698        0
## 36 0.9433962        0
## 37 0.0000000        0
## 38 0.0000000        0
## 39 0.0000000        0
## 40 0.0000000        0
## 41 0.2075472        0
## 42 0.6226415        0
## 43 1.6226415        0
## 44 0.5849057        0
## 45 0.4905660        0
## 46 0.0754717        0
## 47 0.0000000        0
## 48 0.0000000        0
## 49 1.1886792        0
## 50 0.9433962        0
```

4: Time Series Plot set at Five Minute Intervals


```r
plot(df$s.Interval, df$s.Total, type="l", 
col="green", main = "Number of Steps Taken",
xlab = "Five Minute Intervals",
ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

5: The 5-Minute Interval with the Maxium Steps. Top 5


```r
library(dplyr)
maxsteps <- arrange(step, desc(steps))
head(maxsteps, n=5)
```

```
##   steps       date interval
## 1   806 2012-11-27      615
## 2   802 2012-10-12      900
## 3   794 2012-11-27      550
## 4   789 2012-11-19      720
## 5   786 2012-10-15      835
```
6: Strategy for Inputing Missing Data
I took the average number of steps per interval and across the whole study.
I then subsituted in the figure (~37.1 steps) for all of the missing values.
This figures is probally high.


```r
step2 <- step
step2[is.na(step2)] <- mean(df$s.Mean)
```
7: Historgram of the Total Number of Step Taken Each Day With Missing Data


```r
for (i in uni){
  
  x <- subset(step2, interval==i)
  y <- sum(is.na(x$steps))
  x <- na.omit(x)
  df[nrow(df)+1,] <- c(i, sum(x$steps), mean(x$steps), median(x$steps), y)
}

df <- na.omit(df)

hist(df$s.Total, col = "green", main = "Number of Steps", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
His is the Adjusted Mean and Median Number of Step with the extra 37.1 steps per interval added in.
The figures are limited to the first 50.


```r
head(df[, 3:4], n=50)
```

```
##       s.Mean s.Median
## 1  1.7169811        0
## 2  0.3396226        0
## 3  0.1320755        0
## 4  0.1509434        0
## 5  0.0754717        0
## 6  2.0943396        0
## 7  0.5283019        0
## 8  0.8679245        0
## 9  0.0000000        0
## 10 1.4716981        0
## 11 0.3018868        0
## 12 0.1320755        0
## 13 0.3207547        0
## 14 0.6792453        0
## 15 0.1509434        0
## 16 0.3396226        0
## 17 0.0000000        0
## 18 1.1132075        0
## 19 1.8301887        0
## 20 0.1698113        0
## 21 0.1698113        0
## 22 0.3773585        0
## 23 0.2641509        0
## 24 0.0000000        0
## 25 0.0000000        0
## 26 0.0000000        0
## 27 1.1320755        0
## 28 0.0000000        0
## 29 0.0000000        0
## 30 0.1320755        0
## 31 0.0000000        0
## 32 0.2264151        0
## 33 0.0000000        0
## 34 0.0000000        0
## 35 1.5471698        0
## 36 0.9433962        0
## 37 0.0000000        0
## 38 0.0000000        0
## 39 0.0000000        0
## 40 0.0000000        0
## 41 0.2075472        0
## 42 0.6226415        0
## 43 1.6226415        0
## 44 0.5849057        0
## 45 0.4905660        0
## 46 0.0754717        0
## 47 0.0000000        0
## 48 0.0000000        0
## 49 1.1886792        0
## 50 0.9433962        0
```


8: Average Number of Steps taken on a Weekday and Weekend



```r
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

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

9: All Code used for this Assignement.


```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
con <- unz(temp, "activity.csv")
step <- read.csv(con, sep =",")
unlink(temp)

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
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
df[, 3:4]
```

```
##          s.Mean s.Median
## 1     1.7169811        0
## 2     0.3396226        0
## 3     0.1320755        0
## 4     0.1509434        0
## 5     0.0754717        0
## 6     2.0943396        0
## 7     0.5283019        0
## 8     0.8679245        0
## 9     0.0000000        0
## 10    1.4716981        0
## 11    0.3018868        0
## 12    0.1320755        0
## 13    0.3207547        0
## 14    0.6792453        0
## 15    0.1509434        0
## 16    0.3396226        0
## 17    0.0000000        0
## 18    1.1132075        0
## 19    1.8301887        0
## 20    0.1698113        0
## 21    0.1698113        0
## 22    0.3773585        0
## 23    0.2641509        0
## 24    0.0000000        0
## 25    0.0000000        0
## 26    0.0000000        0
## 27    1.1320755        0
## 28    0.0000000        0
## 29    0.0000000        0
## 30    0.1320755        0
## 31    0.0000000        0
## 32    0.2264151        0
## 33    0.0000000        0
## 34    0.0000000        0
## 35    1.5471698        0
## 36    0.9433962        0
## 37    0.0000000        0
## 38    0.0000000        0
## 39    0.0000000        0
## 40    0.0000000        0
## 41    0.2075472        0
## 42    0.6226415        0
## 43    1.6226415        0
## 44    0.5849057        0
## 45    0.4905660        0
## 46    0.0754717        0
## 47    0.0000000        0
## 48    0.0000000        0
## 49    1.1886792        0
## 50    0.9433962        0
## 51    2.5660377        0
## 52    0.0000000        0
## 53    0.3396226        0
## 54    0.3584906        0
## 55    4.1132075        0
## 56    0.6603774        0
## 57    3.4905660        0
## 58    0.8301887        0
## 59    3.1132075        0
## 60    1.1132075        0
## 61    0.0000000        0
## 62    1.5660377        0
## 63    3.0000000        0
## 64    2.2452830        0
## 65    3.3207547        0
## 66    2.9622642        0
## 67    2.0943396        0
## 68    6.0566038        0
## 69   16.0188679        0
## 70   18.3396226        0
## 71   39.4528302        0
## 72   44.4905660        0
## 73   31.4905660        0
## 74   49.2641509        0
## 75   53.7735849        0
## 76   63.4528302        0
## 77   49.9622642        0
## 78   47.0754717        0
## 79   52.1509434        0
## 80   39.3396226        0
## 81   44.0188679        0
## 82   44.1698113        0
## 83   37.3584906        8
## 84   49.0377358       13
## 85   43.8113208        7
## 86   44.3773585       13
## 87   50.5094340       14
## 88   54.5094340        0
## 89   49.9245283        0
## 90   50.9811321       12
## 91   55.6792453        0
## 92   44.3207547        0
## 93   52.2641509       15
## 94   69.5471698       19
## 95   57.8490566       19
## 96   56.1509434       28
## 97   73.3773585       41
## 98   68.2075472       25
## 99  129.4339623       32
## 100 157.5283019       13
## 101 171.1509434       45
## 102 155.3962264       33
## 103 177.3018868       37
## 104 206.1698113       19
## 105 195.9245283       51
## 106 179.5660377       60
## 107 183.3962264       16
## 108 167.0188679       43
## 109 143.4528302       20
## 110 124.0377358        8
## 111 109.1132075       31
## 112 108.1132075       15
## 113 103.7169811       16
## 114  95.9622642        0
## 115  66.2075472        0
## 116  45.2264151        0
## 117  24.7924528        0
## 118  38.7547170        0
## 119  34.9811321        0
## 120  21.0566038        0
## 121  40.5660377        0
## 122  26.9811321        0
## 123  42.4150943        0
## 124  52.6603774        0
## 125  38.9245283        0
## 126  50.7924528        0
## 127  44.2830189        0
## 128  37.4150943        0
## 129  34.6981132        0
## 130  28.3396226        0
## 131  25.0943396        0
## 132  31.9433962        0
## 133  31.3584906        0
## 134  29.6792453        0
## 135  21.3207547        0
## 136  25.5471698        0
## 137  28.3773585        0
## 138  26.4716981        0
## 139  33.4339623        0
## 140  49.9811321        0
## 141  42.0377358        0
## 142  44.6037736        0
## 143  46.0377358        0
## 144  59.1886792        0
## 145  63.8679245        0
## 146  87.6981132        0
## 147  94.8490566        6
## 148  92.7735849       10
## 149  63.3962264        0
## 150  50.1698113        0
## 151  54.4716981        0
## 152  32.4150943        0
## 153  26.5283019        0
## 154  37.7358491        0
## 155  45.0566038        0
## 156  67.2830189        0
## 157  42.3396226        0
## 158  39.8867925        0
## 159  43.2641509        0
## 160  40.9811321        0
## 161  46.2452830        0
## 162  56.4339623        0
## 163  42.7547170        0
## 164  25.1320755        0
## 165  39.9622642        0
## 166  53.5471698        0
## 167  47.3207547        0
## 168  60.8113208        0
## 169  55.7547170        0
## 170  51.9622642        0
## 171  43.5849057        0
## 172  48.6981132        0
## 173  35.4716981        0
## 174  37.5471698        0
## 175  41.8490566        0
## 176  27.5094340        0
## 177  17.1132075        0
## 178  26.0754717        0
## 179  43.6226415        0
## 180  43.7735849        0
## 181  30.0188679        0
## 182  36.0754717        0
## 183  35.4905660        0
## 184  38.8490566        0
## 185  45.9622642        0
## 186  47.7547170        0
## 187  48.1320755        0
## 188  65.3207547        0
## 189  82.9056604        0
## 190  98.6603774        0
## 191 102.1132075        0
## 192  83.9622642        0
## 193  62.1320755        0
## 194  64.1320755        0
## 195  74.5471698        0
## 196  63.1698113        0
## 197  56.9056604        0
## 198  59.7735849        0
## 199  43.8679245        0
## 200  38.5660377        0
## 201  44.6603774        0
## 202  45.4528302        0
## 203  46.2075472        0
## 204  43.6792453        0
## 205  46.6226415        0
## 206  56.3018868        0
## 207  50.7169811        0
## 208  61.2264151        7
## 209  72.7169811        7
## 210  78.9433962        0
## 211  68.9433962        7
## 212  59.6603774        7
## 213  75.0943396       26
## 214  56.5094340        7
## 215  34.7735849        0
## 216  37.4528302       10
## 217  40.6792453       15
## 218  58.0188679       18
## 219  74.6981132       26
## 220  85.3207547       25
## 221  59.2641509       24
## 222  67.7735849        9
## 223  77.6981132       33
## 224  74.2452830       26
## 225  85.3396226       34
## 226  99.4528302       42
## 227  86.5849057       33
## 228  85.6037736       30
## 229  84.8679245       33
## 230  77.8301887       30
## 231  58.0377358        8
## 232  53.3584906        8
## 233  36.3207547        7
## 234  20.7169811        0
## 235  27.3962264        0
## 236  40.0188679        0
## 237  30.2075472        0
## 238  25.5471698        0
## 239  45.6603774        0
## 240  33.5283019        0
## 241  19.6226415        0
## 242  19.0188679        0
## 243  19.3396226        0
## 244  33.3396226        0
## 245  26.8113208        0
## 246  21.1698113        0
## 247  27.3018868        0
## 248  21.3396226        0
## 249  19.5471698        0
## 250  21.3207547        0
## 251  32.3018868        0
## 252  20.1509434        0
## 253  15.9433962        0
## 254  17.2264151        0
## 255  23.4528302        0
## 256  19.2452830        0
## 257  12.4528302        0
## 258   8.0188679        0
## 259  14.6603774        0
## 260  16.3018868        0
## 261   8.6792453        0
## 262   7.7924528        0
## 263   8.1320755        0
## 264   2.6226415        0
## 265   1.4528302        0
## 266   3.6792453        0
## 267   4.8113208        0
## 268   8.5094340        0
## 269   7.0754717        0
## 270   8.6981132        0
## 271   9.7547170        0
## 272   2.2075472        0
## 273   0.3207547        0
## 274   0.1132075        0
## 275   1.6037736        0
## 276   4.6037736        0
## 277   3.3018868        0
## 278   2.8490566        0
## 279   0.0000000        0
## 280   0.8301887        0
## 281   0.9622642        0
## 282   1.5849057        0
## 283   2.6037736        0
## 284   4.6981132        0
## 285   3.3018868        0
## 286   0.6415094        0
## 287   0.2264151        0
## 288   1.0754717        0
```

```r
plot(df$s.Interval, df$s.Total  , type="l", col="green", 
     main = "Number of Steps Taken",
     xlab = "Five Minute Intervals",
     ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-2.png) 

```r
library(dplyr)
maxsteps <- arrange(step, desc(steps))
head(maxsteps, n=5)
```

```
##   steps       date interval
## 1   806 2012-11-27      615
## 2   802 2012-10-12      900
## 3   794 2012-11-27      550
## 4   789 2012-11-19      720
## 5   786 2012-10-15      835
```

```r
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
```

![](PA1_template_files/figure-html/unnamed-chunk-10-3.png) 

```r
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

![](PA1_template_files/figure-html/unnamed-chunk-10-4.png) 

FIN
