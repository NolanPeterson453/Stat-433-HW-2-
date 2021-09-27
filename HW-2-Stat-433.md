HW 2 Stat 433
================
Nolan Peterson
9/26/2021

## Question 1

``` r
## Select and name data of interest
flights <- nycflights13::flights

## Find the number of flights that have missing dep_time
missing_dep_time <- flights %>% filter(is.na(dep_time)) %>% nrow()

## Print the number of missing flights
cat(paste("The number of flights with missing departure time is ", 
    missing_dep_time, ".", sep = ""))
```

    The number of flights with missing departure time is 8255.

``` r
## Find what other variables are missing
flights %>% filter(is.na(dep_time)) %>% head() %>% knitr::kable()
```

| year | month | day | dep\_time | sched\_dep\_time | dep\_delay | arr\_time | sched\_arr\_time | arr\_delay | carrier | flight | tailnum | origin | dest | air\_time | distance | hour | minute | time\_hour          |
| ---: | ----: | --: | --------: | ---------------: | ---------: | --------: | ---------------: | ---------: | :------ | -----: | :------ | :----- | :--- | --------: | -------: | ---: | -----: | :------------------ |
| 2013 |     1 |   1 |        NA |             1630 |         NA |        NA |             1815 |         NA | EV      |   4308 | N18120  | EWR    | RDU  |        NA |      416 |   16 |     30 | 2013-01-01 16:00:00 |
| 2013 |     1 |   1 |        NA |             1935 |         NA |        NA |             2240 |         NA | AA      |    791 | N3EHAA  | LGA    | DFW  |        NA |     1389 |   19 |     35 | 2013-01-01 19:00:00 |
| 2013 |     1 |   1 |        NA |             1500 |         NA |        NA |             1825 |         NA | AA      |   1925 | N3EVAA  | LGA    | MIA  |        NA |     1096 |   15 |      0 | 2013-01-01 15:00:00 |
| 2013 |     1 |   1 |        NA |              600 |         NA |        NA |              901 |         NA | B6      |    125 | N618JB  | JFK    | FLL  |        NA |     1069 |    6 |      0 | 2013-01-01 06:00:00 |
| 2013 |     1 |   2 |        NA |             1540 |         NA |        NA |             1747 |         NA | EV      |   4352 | N10575  | EWR    | CVG  |        NA |      569 |   15 |     40 | 2013-01-02 15:00:00 |
| 2013 |     1 |   2 |        NA |             1620 |         NA |        NA |             1746 |         NA | EV      |   4406 | N13949  | EWR    | PIT  |        NA |      319 |   16 |     20 | 2013-01-02 16:00:00 |

Any variable that has to do with a real world quantity that could be
recorded from an actual flight is missing. This indicate that the rows
with departure time missing represent the rows where flights were
canceled.

## Question 2

``` r
## Write a function which converts from (H)HMM format to
## minutes after midnight
HM_convert <- function(HHMM) {
    require(dplyr)
    MMMM <- 0
    if (HHMM <= 60) {
        MMMM <- HHMM
    } else {
        HHMM_vec <- HHMM %>% as.character() %>% strsplit("") %>% 
            unlist() %>% as.numeric() %>% as.vector()
        
        ifelse(length(HHMM_vec) == 3, MMMM <- HHMM_vec[1] * 60 + 
            HHMM_vec[2] * 10 + HHMM_vec[3], MMMM <- HHMM_vec[1] * 
            600 + HHMM_vec[2] * 60 + HHMM_vec[3] * 10 + HHMM_vec[4])
    }
    
    return(MMMM)
    
}

## Filter out NAs
flights_new <- flights %>% filter(!is.na(dep_time) & !is.na(sched_dep_time))

## convert the HHMM time to minutes past midnight
flights_new <- flights_new %>% mutate(dep_time_MPM = sapply(flights_new$dep_time, 
    HM_convert), sched_dep_time_MPM = sapply(flights_new$sched_dep_time, 
    HM_convert))

## display a part of the data to prove that it worked
flights_new[, 16:21] %>% head() %>% knitr::kable()
```

| distance | hour | minute | time\_hour          | dep\_time\_MPM | sched\_dep\_time\_MPM |
| -------: | ---: | -----: | :------------------ | -------------: | --------------------: |
|     1400 |    5 |     15 | 2013-01-01 05:00:00 |            317 |                   315 |
|     1416 |    5 |     29 | 2013-01-01 05:00:00 |            333 |                   329 |
|     1089 |    5 |     40 | 2013-01-01 05:00:00 |            342 |                   340 |
|     1576 |    5 |     45 | 2013-01-01 05:00:00 |            344 |                   345 |
|      762 |    6 |      0 | 2013-01-01 06:00:00 |            354 |                   360 |
|      719 |    5 |     58 | 2013-01-01 05:00:00 |            354 |                   358 |

## Question 3

``` r
## Create a variable called canceled that indicates 0 if not
## canceled and 1 if canceled
flights %>% mutate(canceled = ifelse(is.na(flights$dep_time), 
    1, 0)) %>% 
## Group by day
group_by(day) %>% 
## find mean daily delay and proportion of daily flights
## canceled
summarise(delay_mean = mean(dep_delay, na.rm = TRUE), prop_canceled = mean(canceled)) %>% 
    
## create a scatter plot of mean daily delay vs prop of daily
## canceled flights
ggplot(aes(x = delay_mean, y = prop_canceled)) + geom_point() + 
    labs(x = "Average Daily Delay", y = "Proportion of Daily Flights canceled") + 
    theme_minimal()
```

    `summarise()` ungrouping output (override with `.groups` argument)

![](HW-2-Stat-433_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
