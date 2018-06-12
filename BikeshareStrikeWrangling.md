---
title: "BikeshareStrikeWrangling"
author: "Daniel Fuller"
date: '2018-04-25'
output:
      html_document:
        keep_md: true
---



## Merging the data from each city


```r
library(stringr)
library(knitr)
library(dplyr)
library(lubridate)
library(magrittr)
library(purrr)

## Chicago - Divvy
divvy = list.files(path = "./Bikeshare Data/", pattern = "Divvy.*\\.csv")

### Washington - Cabi
cabi = list.files(path = "./Bikeshare Data/", pattern = "Cabi.*\\.csv")

### Philly - Indego
indeg = list.files(path = "./Bikeshare Data/", pattern = "Indego.*\\.csv")

### Boston - Hubway
hubway = list.files(path = "./Bikeshare Data/", pattern = "Hubway.*\\.csv")
```

## Chicago


```r
chicago <- divvy %>% map(function(x) {
    read.csv(paste0("./Bikeshare Data/", x))
}) %>% reduce(rbind)

chicago$city <- "Chicago"
head(chicago)
```

```
##   trip_id       starttime      stoptime bikeid tripduration
## 1 9379901 4/30/2016 23:59 5/1/2016 0:11     21          733
## 2 9379900 4/30/2016 23:58 5/1/2016 0:07   3583          556
## 3 9379899 4/30/2016 23:58 5/1/2016 0:02   4557          253
## 4 9379898 4/30/2016 23:54 5/1/2016 0:08   2443          802
## 5 9379897 4/30/2016 23:52 5/1/2016 0:11     50         1146
## 6 9379896 4/30/2016 23:49 5/1/2016 0:11   1676         1291
##   from_station_id              from_station_name to_station_id
## 1             123 California Ave & Milwaukee Ave           374
## 2             349    Halsted St & Wrightwood Ave           165
## 3              59      Wabash Ave & Roosevelt Rd           273
## 4             289          Wells St & Concord Ln           199
## 5             239       Western Ave & Leland Ave           227
## 6             239       Western Ave & Leland Ave           227
##                to_station_name   usertype gender birthyear    city
## 1      Western Ave & Walton St Subscriber   Male      1982 Chicago
## 2          Clark St & Grace St Subscriber   Male      1991 Chicago
## 3       Michigan Ave & 18th St Subscriber   Male      1984 Chicago
## 4       Wabash Ave & Grand Ave Subscriber   Male      1978 Chicago
## 5 Southport Ave & Waveland Ave   Customer               NA Chicago
## 6 Southport Ave & Waveland Ave   Customer               NA Chicago
```

```r
chicagoClean <- subset(chicago, select = c(trip_id, tripduration, starttime, 
    stoptime, usertype, city), stringsAsFactors = FALSE)
names(chicagoClean) <- c("trip_id", "duration", "start_time", "end_time", "passholder_type", 
    "city")
head(chicagoClean)
```

```
##   trip_id duration      start_time      end_time passholder_type    city
## 1 9379901      733 4/30/2016 23:59 5/1/2016 0:11      Subscriber Chicago
## 2 9379900      556 4/30/2016 23:58 5/1/2016 0:07      Subscriber Chicago
## 3 9379899      253 4/30/2016 23:58 5/1/2016 0:02      Subscriber Chicago
## 4 9379898      802 4/30/2016 23:54 5/1/2016 0:08      Subscriber Chicago
## 5 9379897     1146 4/30/2016 23:52 5/1/2016 0:11        Customer Chicago
## 6 9379896     1291 4/30/2016 23:49 5/1/2016 0:11        Customer Chicago
```

```r
## Clean Start Time
chicagoClean$start_time <- gsub(" .*", "", chicagoClean$start_time)
chicagoClean$start_time <- mdy(chicagoClean$start_time)

## Clean End Time
chicagoClean$end_time <- gsub(" .*", "", chicagoClean$end_time)
chicagoClean$end_time <- mdy(chicagoClean$end_time)

head(chicagoClean)
```

```
##   trip_id duration start_time   end_time passholder_type    city
## 1 9379901      733 2016-04-30 2016-05-01      Subscriber Chicago
## 2 9379900      556 2016-04-30 2016-05-01      Subscriber Chicago
## 3 9379899      253 2016-04-30 2016-05-01      Subscriber Chicago
## 4 9379898      802 2016-04-30 2016-05-01      Subscriber Chicago
## 5 9379897     1146 2016-04-30 2016-05-01        Customer Chicago
## 6 9379896     1291 2016-04-30 2016-05-01        Customer Chicago
```

## Boston


```r
boston <- hubway %>% map(function(x) {
    read.csv(paste0("./Bikeshare Data/", x))
}) %>% reduce(rbind)

boston$city <- "Boston"
boston$trip_id <- seq.int(nrow(boston))
head(boston)
```

```
##   tripduration           starttime            stoptime start.station.id
## 1         1939 2016-01-01 00:08:07 2016-01-01 00:40:26               36
## 2         1927 2016-01-01 00:08:19 2016-01-01 00:40:27               36
## 3         1813 2016-01-01 00:10:15 2016-01-01 00:40:29               36
## 4         1788 2016-01-01 00:10:22 2016-01-01 00:40:11               36
## 5          941 2016-01-01 00:15:36 2016-01-01 00:31:17               36
## 6          323 2016-01-01 00:17:15 2016-01-01 00:22:39              110
##                                       start.station.name
## 1               Boston Public Library - 700 Boylston St.
## 2               Boston Public Library - 700 Boylston St.
## 3               Boston Public Library - 700 Boylston St.
## 4               Boston Public Library - 700 Boylston St.
## 5               Boston Public Library - 700 Boylston St.
## 6 Harvard University Gund Hall at Quincy St / Kirkland S
##   start.station.latitude start.station.longitude end.station.id
## 1                  42.35                  -71.08             19
## 2                  42.35                  -71.08             19
## 3                  42.35                  -71.08             19
## 4                  42.35                  -71.08             19
## 5                  42.35                  -71.08             67
## 6                  42.38                  -71.11             88
##                                end.station.name end.station.latitude
## 1                       Buswell St. at Park Dr.                42.35
## 2                       Buswell St. at Park Dr.                42.35
## 3                       Buswell St. at Park Dr.                42.35
## 4                       Buswell St. at Park Dr.                42.35
## 5                  MIT at Mass Ave / Amherst St                42.36
## 6 Inman Square at Vellucci Plaza / Hampshire St                42.37
##   end.station.longitude bikeid   usertype birth.year gender   city trip_id
## 1                -71.11   1406 Subscriber        \\N      0 Boston       1
## 2                -71.11   1121 Subscriber        \\N      0 Boston       2
## 3                -71.11    474 Subscriber        \\N      0 Boston       3
## 4                -71.11   1504 Subscriber        \\N      0 Boston       4
## 5                -71.09    913   Customer       1990      1 Boston       5
## 6                -71.10    620 Subscriber       1993      1 Boston       6
```

```r
bostonClean <- subset(boston, select = c(trip_id, tripduration, starttime, stoptime, 
    usertype, city), stringsAsFactors = FALSE)
names(bostonClean) <- c("trip_id", "duration", "start_time", "end_time", "passholder_type", 
    "city")
head(bostonClean)
```

```
##   trip_id duration          start_time            end_time passholder_type
## 1       1     1939 2016-01-01 00:08:07 2016-01-01 00:40:26      Subscriber
## 2       2     1927 2016-01-01 00:08:19 2016-01-01 00:40:27      Subscriber
## 3       3     1813 2016-01-01 00:10:15 2016-01-01 00:40:29      Subscriber
## 4       4     1788 2016-01-01 00:10:22 2016-01-01 00:40:11      Subscriber
## 5       5      941 2016-01-01 00:15:36 2016-01-01 00:31:17        Customer
## 6       6      323 2016-01-01 00:17:15 2016-01-01 00:22:39      Subscriber
##     city
## 1 Boston
## 2 Boston
## 3 Boston
## 4 Boston
## 5 Boston
## 6 Boston
```

```r
## Clean Start Time
bostonClean$start_time <- gsub(" .*", "", bostonClean$start_time)
bostonClean$start_time <- ymd(bostonClean$start_time)

## Clean End Time
bostonClean$end_time <- gsub(" .*", "", bostonClean$end_time)
bostonClean$end_time <- ymd(bostonClean$end_time)

head(bostonClean)
```

```
##   trip_id duration start_time   end_time passholder_type   city
## 1       1     1939 2016-01-01 2016-01-01      Subscriber Boston
## 2       2     1927 2016-01-01 2016-01-01      Subscriber Boston
## 3       3     1813 2016-01-01 2016-01-01      Subscriber Boston
## 4       4     1788 2016-01-01 2016-01-01      Subscriber Boston
## 5       5      941 2016-01-01 2016-01-01        Customer Boston
## 6       6      323 2016-01-01 2016-01-01      Subscriber Boston
```

##Philly

I had to import the files individually because R was reading in the variables as different types and it was messing up the `rbind` call. I ran this code to diagnose the problem:

```
cbind(
    philly1 = sapply(philly1, class),
    philly2 = sapply(philly2, class),
    philly3 = sapply(philly3, class),
    philly4 = sapply(philly4, class)
)
```

From this article [https://stackoverflow.com/questions/1632772/appending-rows-to-a-dataframe-the-factor-problem]


```r
philly1 <- read.csv("./Bikeshare Data/Indego_Trips_2016Q1.csv")
philly1 <- subset(philly1, select = c(trip_id, duration, start_time, end_time, 
    passholder_type), stringsAsFactors = FALSE)

philly2 <- read.csv("./Bikeshare Data/Indego_Trips_2016Q2.csv")
philly2 <- subset(philly2, select = c(trip_id, duration, start_time, end_time, 
    passholder_type), stringsAsFactors = FALSE)

philly3 <- read.csv("./Bikeshare Data/Indego_Trips_Q3_2016_trips.csv")
philly3 <- subset(philly3, select = c(trip_id, duration, start_time, end_time, 
    passholder_type), stringsAsFactors = FALSE)

philly4 <- read.csv("./Bikeshare Data/Indego_trips_Q4_2016.csv")
philly4 <- subset(philly4, select = c(trip_id, duration, start_time, end_time, 
    passholder_type), stringsAsFactors = FALSE)

philly <- bind_rows(philly1, philly2, philly3, philly4)

philly$city <- "Philly"

phillyClean <- subset(philly, select = c(trip_id, duration, start_time, end_time, 
    passholder_type, city), stringsAsFactors = FALSE)
names(phillyClean) <- c("trip_id", "duration", "start_time", "end_time", "passholder_type", 
    "city")

## Clean Start Time
phillyClean$start_time <- gsub(" .*", "", phillyClean$start_time)
phillyClean$start_time <- mdy(phillyClean$start_time)

## Clean End Time
phillyClean$end_time <- gsub(" .*", "", phillyClean$end_time)
phillyClean$end_time <- mdy(phillyClean$end_time)

## Add records for the missing two days
phillyClean[365, "start_time"] <- "2016-01-23"
phillyClean[366, "start_time"] <- "2016-01-24"

head(phillyClean)
```

```
##   trip_id duration start_time   end_time passholder_type   city
## 1 4516691      600 2016-01-01 2016-01-01        Indego30 Philly
## 2 4516692     1320 2016-01-01 2016-01-01        Indego30 Philly
## 3 4516693      780 2016-01-01 2016-01-01        Indego30 Philly
## 4 4516694     1320 2016-01-01 2016-01-01         Walk-up Philly
## 5 4516695      600 2016-01-01 2016-01-01        Indego30 Philly
## 6 4516696     1260 2016-01-01 2016-01-01         Walk-up Philly
```

##Washington 

```r
washington <- cabi %>% map(function(x) {
    read.csv(paste0("./Bikeshare Data/", x))
}) %>% reduce(rbind)

washington$city <- "Washington"
washington$duration <- as.integer(washington$duration)
head(washington)
```

```
##   trip_id duration      start_time         end_time passholder_type
## 1 5000000   301295 3/31/2016 23:59 04/01/2016 00:04      Registered
## 2 5000001   557887 3/31/2016 23:59 04/01/2016 00:08      Registered
## 3 5000002   555944 3/31/2016 23:59 04/01/2016 00:08      Registered
## 4 5000003   766916 3/31/2016 23:57 04/01/2016 00:09      Registered
## 5 5000004   139656 3/31/2016 23:57  3/31/2016 23:59      Registered
## 6 5000005   967713 3/31/2016 23:57 04/01/2016 00:13          Casual
##         city
## 1 Washington
## 2 Washington
## 3 Washington
## 4 Washington
## 5 Washington
## 6 Washington
```

```r
washingtonClean <- subset(washington, select = c(trip_id, duration, start_time, 
    end_time, passholder_type, city), stringsAsFactors = FALSE)
names(washingtonClean) <- c("trip_id", "duration", "start_time", "end_time", 
    "passholder_type", "city")
head(washingtonClean)
```

```
##   trip_id duration      start_time         end_time passholder_type
## 1 5000000   301295 3/31/2016 23:59 04/01/2016 00:04      Registered
## 2 5000001   557887 3/31/2016 23:59 04/01/2016 00:08      Registered
## 3 5000002   555944 3/31/2016 23:59 04/01/2016 00:08      Registered
## 4 5000003   766916 3/31/2016 23:57 04/01/2016 00:09      Registered
## 5 5000004   139656 3/31/2016 23:57  3/31/2016 23:59      Registered
## 6 5000005   967713 3/31/2016 23:57 04/01/2016 00:13          Casual
##         city
## 1 Washington
## 2 Washington
## 3 Washington
## 4 Washington
## 5 Washington
## 6 Washington
```

```r
## Clean Start Time
washingtonClean$start_time <- gsub(" .*", "", washingtonClean$start_time)
washingtonClean$start_time <- mdy(washingtonClean$start_time)

## Clean End Time
washingtonClean$end_time <- gsub(" .*", "", washingtonClean$end_time)
washingtonClean$end_time <- mdy(washingtonClean$end_time)

## Fill in Missing Days for Washington
washingtonClean[363, "start_time"] <- "2016-01-23"
washingtonClean[364, "start_time"] <- "2016-01-24"
washingtonClean[365, "start_time"] <- "2016-01-25"
washingtonClean[366, "start_time"] <- "2016-01-26"

head(washingtonClean)
```

```
##   trip_id duration start_time   end_time passholder_type       city
## 1 5000000   301295 2016-03-31 2016-04-01      Registered Washington
## 2 5000001   557887 2016-03-31 2016-04-01      Registered Washington
## 3 5000002   555944 2016-03-31 2016-04-01      Registered Washington
## 4 5000003   766916 2016-03-31 2016-04-01      Registered Washington
## 5 5000004   139656 2016-03-31 2016-03-31      Registered Washington
## 6 5000005   967713 2016-03-31 2016-04-01          Casual Washington
```

## Appending data

```r
strikeData <- rbind(chicagoClean, phillyClean, washingtonClean, bostonClean)
strikeData$month <- month(strikeData$start_time)
strikeData$day <- day(strikeData$start_time)
strikeData$year <- year(strikeData$start_time)

strikeData <- filter(strikeData, year > 2015)

head(strikeData)
```

```
##   trip_id duration start_time   end_time passholder_type    city month day
## 1 9379901      733 2016-04-30 2016-05-01      Subscriber Chicago     4  30
## 2 9379900      556 2016-04-30 2016-05-01      Subscriber Chicago     4  30
## 3 9379899      253 2016-04-30 2016-05-01      Subscriber Chicago     4  30
## 4 9379898      802 2016-04-30 2016-05-01      Subscriber Chicago     4  30
## 5 9379897     1146 2016-04-30 2016-05-01        Customer Chicago     4  30
## 6 9379896     1291 2016-04-30 2016-05-01        Customer Chicago     4  30
##   year
## 1 2016
## 2 2016
## 3 2016
## 4 2016
## 5 2016
## 6 2016
```

```r
write.csv(strikeData, file = "strikeData.csv")
```

## Cleaning and recoding


```r
# add dummy to CityColl

strikeData$dummy <- ifelse(strikeData$start_time >= as.Date("2016/11/01", format = "%Y/%m/%d"), 
    1, 0)

strikeData$memberType <- recode_factor(strikeData$passholder_type, Customer = "shortterm", 
    Subscriber = "member", Indego30 = "member", IndegoFlex = "shortterm", `Walk-up` = "shortterm", 
    Registered = "member", Casual = "shortterm", Dependent = "shortterm", Member = "member")

table(strikeData$memberType, strikeData$passholder_type)
```

```
##            
##             Customer Subscriber Dependent Indego30 IndegoFlex Walk-up
##   shortterm  1110735          0        40        0       8937  108157
##   member           0    3720807         0   537964          0       0
##            
##              Casual Registered
##   shortterm  732265          0
##   member          0    2601521
```

```r
table(strikeData$passholder_type)
```

```
## 
##   Customer Subscriber  Dependent   Indego30 IndegoFlex    Walk-up 
##    1110735    3720807         40     537964       8937     108157 
##     Casual Registered 
##     732265    2601521
```


```r
city_day_sum <- strikeData %>% group_by(year, month, day, city) %>% mutate(mDuration = mean(duration, 
    na.rm = TRUE), number_trips = n()) %>% distinct(year, month, day, city, 
    .keep_all = TRUE)

## Normalize by city pop Boston - 667137 (6.67137) Chicago - 2695598
## (26.95598) Washington - 672228 (6.72228) Philly - 1567872 (15.67872)

city_day_sum$by100000 <- ifelse(city_day_sum$city == "Philly", city_day_sum$number_trips/15.67872, 
    NA) %>% ifelse(city_day_sum$city == "Washington", city_day_sum$number_trips/6.72228, 
    .) %>% ifelse(city_day_sum$city == "Chicago", city_day_sum$number_trips/26.95598, 
    .) %>% ifelse(city_day_sum$city == "Boston", city_day_sum$number_trips/6.67137, 
    .)

write.csv(city_day_sum, "city_day_sum.csv")
```
