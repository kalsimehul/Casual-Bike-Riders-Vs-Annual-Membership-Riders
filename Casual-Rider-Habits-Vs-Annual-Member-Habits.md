Casual Rider Habits Vs Annual Member Rider Habits
================
Mehul Kalsi

\#Summary: Extracted previous 12 months of Cyclistic Bike Trip data from
09/2021 To 08/2022. Organized, transformed, and cleaned data in excel
and converted to csv format. Wrangled data and combined into a single
dataframe. Analyzed average ride_length,and the number of rides per day.
Visualized data to identify pattern and relationships between
attributes.

``` r
library("tidyverse")
library(lubridate)  #helps wrangle date attributes.
library(ggplot2)  # helps visualize data.
```

\#=====================

# STEP 1: COLLECT DATA

\#=====================

``` r
YM202109 <- read_csv("202109-divvy-tripdata.csv")
YM202110 <- read_csv("202110-divvy-tripdata.csv")
YM202111 <- read_csv("202111-divvy-tripdata.csv")
YM202112 <- read_csv("202112-divvy-tripdata.csv")
YM202201 <- read_csv("202201-divvy-tripdata.csv")
YM202202 <- read_csv("202202-divvy-tripdata.csv")
YM202203 <- read_csv("202203-divvy-tripdata.csv")
YM202204 <- read_csv("202204-divvy-tripdata.csv")
YM202205 <- read_csv("202205-divvy-tripdata.csv")
YM202206 <- read_csv("202206-divvy-tripdata.csv")
YM202207 <- read_csv("202207-divvy-tripdata.csv")
YM202208 <- read_csv("202208-divvy-tripdata.csv")
```

\#====================================================

# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE

\#====================================================

``` r
colnames(YM202208) #column names.
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "ride_length"        "day_of_week"       
    ##  [7] "start_station_name" "start_station_id"   "end_station_name"  
    ## [10] "end_station_id"     "start_lat"          "start_lng"         
    ## [13] "end_lat"            "end_lng"            "member_casual"

``` r
str(YM202208) ## Inspect the dataframes and look for incongruence.
```

    ## spec_tbl_df [785,932 × 15] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:785932] "550CF7EFEAE0C618" "DAD198F405F9C5F5" "E6F2BC47B65CB7FD" "F597830181C2E13C" ...
    ##  $ rideable_type     : chr [1:785932] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
    ##  $ started_at        : chr [1:785932] "8/7/22 21:34" "8/8/22 14:39" "8/8/22 15:29" "8/8/22 2:43" ...
    ##  $ ended_at          : chr [1:785932] "8/7/22 21:41" "8/8/22 14:53" "8/8/22 15:40" "8/8/22 2:58" ...
    ##  $ ride_length       : 'hms' num [1:785932] 00:07:31 00:14:02 00:10:44 00:15:03 ...
    ##   ..- attr(*, "units")= chr "secs"
    ##  $ day_of_week       : num [1:785932] 1 2 2 2 1 2 2 1 1 1 ...
    ##  $ start_station_name: chr [1:785932] NA NA NA NA ...
    ##  $ start_station_id  : chr [1:785932] NA NA NA NA ...
    ##  $ end_station_name  : chr [1:785932] NA NA NA NA ...
    ##  $ end_station_id    : chr [1:785932] NA NA NA NA ...
    ##  $ start_lat         : num [1:785932] 41.9 41.9 42 41.9 41.9 ...
    ##  $ start_lng         : num [1:785932] -87.7 -87.6 -87.7 -87.7 -87.7 ...
    ##  $ end_lat           : num [1:785932] 41.9 41.9 42 42 41.8 ...
    ##  $ end_lng           : num [1:785932] -87.7 -87.6 -87.7 -87.7 -87.7 ...
    ##  $ member_casual     : chr [1:785932] "casual" "casual" "casual" "casual" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   ride_id = col_character(),
    ##   ..   rideable_type = col_character(),
    ##   ..   started_at = col_character(),
    ##   ..   ended_at = col_character(),
    ##   ..   ride_length = col_time(format = ""),
    ##   ..   day_of_week = col_double(),
    ##   ..   start_station_name = col_character(),
    ##   ..   start_station_id = col_character(),
    ##   ..   end_station_name = col_character(),
    ##   ..   end_station_id = col_character(),
    ##   ..   start_lat = col_double(),
    ##   ..   start_lng = col_double(),
    ##   ..   end_lat = col_double(),
    ##   ..   end_lng = col_double(),
    ##   ..   member_casual = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
# Stack individual quarter's data frames into one big data frame.
all_trips <- bind_rows(YM202109,YM202110,YM202111,YM202112,YM202201,YM202202,YM202203,YM202204,YM202205,YM202206,YM202207,YM202208)
```

\#======================================================

# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

\#======================================================

``` r
#Convert ride length from HH:MM to Seconds.
all_trips$ride_lengthv2 <- hms(all_trips$ride_length)
all_trips$ride_lengthv3 <- period_to_seconds(all_trips$ride_lengthv2)
#Dropped rows with null values.
 all_trips<-all_trips %>% 
   drop_na(ride_lengthv3)
 summary(all_trips$ride_lengthv3)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0     363     643    1102    1160  358550

``` r
#Dropped all rows with ride length = 0 | > 86400
all_trips <- all_trips[all_trips$ride_lengthv3 != 0, ]
all_trips <- all_trips[all_trips$ride_lengthv3 <= 86400, ]
#removed temporary column
all_trips <- all_trips %>% select(-c(ride_lengthv2))

 library(lubridate)
 #new row that contains the start time for the trip.
all_trips$start_date <- as.Date(all_trips$started_at, format = "%m/%d/%y")
```

\#=====================================

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS

\#=====================================

``` r
#Average ride_length for casual and member riders.
aggregate(all_trips$ride_lengthv3 ~ all_trips$member_casual, FUN = mean)
```

    ##   all_trips$member_casual all_trips$ride_lengthv3
    ## 1                  casual               1388.1643
    ## 2                  member                754.8808

``` r
#Median ride_length for casual and member riders.
aggregate(all_trips$ride_lengthv3 ~ all_trips$member_casual, FUN = median)
```

    ##   all_trips$member_casual all_trips$ride_lengthv3
    ## 1                  casual                     833
    ## 2                  member                     538

``` r
#Max Ride_length.
aggregate(all_trips$ride_lengthv3 ~ all_trips$member_casual, FUN = max)
```

    ##   all_trips$member_casual all_trips$ride_lengthv3
    ## 1                  casual                   86395
    ## 2                  member                   86397

``` r
#Min Ride_length.
aggregate(all_trips$ride_lengthv3 ~ all_trips$member_casual, FUN = min)
```

    ##   all_trips$member_casual all_trips$ride_lengthv3
    ## 1                  casual                       1
    ## 2                  member                       1

``` r
# Looking at the average ride time by each day for members vs casual users.
aggregate(all_trips$ride_lengthv3 ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
```

    ##    all_trips$member_casual all_trips$day_of_week all_trips$ride_lengthv3
    ## 1                   casual                     1               1602.7337
    ## 2                   member                     1                842.1724
    ## 3                   casual                     2               1423.7808
    ## 4                   member                     2                732.0250
    ## 5                   casual                     3               1228.6598
    ## 6                   member                     3                714.9347
    ## 7                   casual                     4               1192.6363
    ## 8                   member                     4                718.6201
    ## 9                   casual                     5               1229.7156
    ## 10                  member                     5                725.0591
    ## 11                  casual                     6               1296.4928
    ## 12                  member                     6                740.2390
    ## 13                  casual                     7               1542.1473
    ## 14                  member                     7                841.6563

``` r
# Analyzing ridership data by type and weekday.
all_trips %>% mutate(weekday = wday(start_date, label = TRUE)) %>% group_by(member_casual, weekday) %>% summarise(number_of_rides = n(),average_duration = mean(ride_lengthv3)) %>% arrange(member_casual, weekday)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 14 × 4
    ## # Groups:   member_casual [2]
    ##    member_casual weekday number_of_rides average_duration
    ##    <chr>         <ord>             <int>            <dbl>
    ##  1 casual        Sun              436448            1603.
    ##  2 casual        Mon              291609            1424.
    ##  3 casual        Tue              277485            1229.
    ##  4 casual        Wed              292751            1193.
    ##  5 casual        Thu              310919            1230.
    ##  6 casual        Fri              345330            1296.
    ##  7 casual        Sat              509071            1542.
    ##  8 member        Sun              404361             842.
    ##  9 member        Mon              474709             732.
    ## 10 member        Tue              536358             715.
    ## 11 member        Wed              546896             719.
    ## 12 member        Thu              525348             725.
    ## 13 member        Fri              472067             740.
    ## 14 member        Sat              453900             842.

``` r
#Visualizing the number of rides by rider type.
all_trips %>% mutate(weekday = wday(start_date, label = TRUE)) %>% group_by(member_casual, weekday) %>% summarise(number_of_rides = n(),average_duration = mean(ride_lengthv3)) %>% arrange(member_casual, weekday) %>% ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + 
geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](Casual-Rider-Habits-Vs-Annual-Member-Habits_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#Visualizing the average duration for each member by weekday.
all_trips %>% mutate(weekday = wday(start_date, label = TRUE)) %>% group_by(member_casual, weekday) %>% summarise(number_of_rides = n(),average_duration = mean(ride_lengthv3)) %>% arrange(member_casual, weekday) %>% ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](Casual-Rider-Habits-Vs-Annual-Member-Habits_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->
