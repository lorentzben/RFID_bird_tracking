######## Test RFID FUNCTIONS ########

source("./rfid_functions.R")

library(xts)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tsibble)
library(testthat)

### DUPLICATE DAT ###

# TODO possibly Deprecated 

### OUTSIDE CUTOFF ###

oc <- read.csv("../data/test_data/outside_cutoff.csv")

bird_ids_oc <- unique(oc$tagname)
bird_ids_oc <- na.trim(sort(bird_ids_oc))

oc["DateTime"] <- as.POSIXct(oc$access, origin="1970-01-01", tz="GMT")

oc$accessdate <- ymd_hms(oc$DateTime)

oc_struct <- oc |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

oc_all_analysis <- oc_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

expect_equal(oc_all_analysis$slicedTsibble[[1]], NA, label='outside cutoff values')

### ONE DAY ###

# One Day 0 Trans

d1t0 <- read.csv("../data/test_data/one_day_zero_trans_r2.csv")

bird_ids_d1t0 <- unique(d1t0$tagname)
bird_ids_d1t0 <- na.trim(sort(bird_ids_d1t0))

d1t0["DateTime"] <- as.POSIXct(d1t0$access, origin="1970-01-01", tz="GMT")

print("what makes up subzone col")
unique(d1t0$subzone)

d1t0$subzone[d1t0$subzone == "Bottom"] <- "bottom"
d1t0$subzone[d1t0$subzone == "Middle"] <- "middle"
d1t0$subzone[d1t0$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(d1t0$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(d1t0$DateTime))
sum(is.na(d1t0$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
d1t0$accessdate <- ymd_hms(d1t0$DateTime)

d1t0_struct <- d1t0 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(id_dupes = map(data ,~identify_duplicate_records(.x))) |>
 mutate(cleaned = map(id_dupes, ~.x[! .x$duplicate == 1,])) |>
 mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d1t0_all_analysis <- d1t0_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are zero trans
expect_equal(unique(d1t0_all_analysis$slicedTsibble[[1]]$value),"bottom", label='d1t0 sliced tsibble valuecol')

d1t0_regular <- d1t0_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 

# # TODO does this even matter? check that the interval is actually 5 sec

# room_2_summary <- room_2 |> nest(data = - tagname) |> 
#  na.exclude() |>
#  mutate(id_dupes = map(data ,~identify_duplicate_records(.x))) |>
#  mutate(cleaned = map(id_dupes, ~.x[! .x$duplicate == 1,])) |>
#  mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) )) |>
#  mutate(intervals_s = map(tsibble, ~ as.numeric(difftime(.x$datetime[1:(length(.x$datetime)-1)], .x$datetime[2:length(.x$datetime)],units='secs') ))) 

# first item to compare to day+night
d1t0_overall_interval <- d1t0_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x)))

d1t0_all_room_time_budget <- d1t0_overall_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

d1t0_overall_tb <- d1t0_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d1t0_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d1t0_overall_interval$interval[[1]],n=1)$t2),origin=origin)))

expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(1,0,0), ncol=3)))

# check that time budget says that it spent 100% in bottom
expect_equal(d1t0_overall_tb, expected_res, label='d1t0 overall time budget')



# One Day 1 Trans

# One Day 2 Trans

# One Day 3 Trans



### TWO DAYS ###

# Two Day 0 Trans

# Two Day 1 Trans

# Two Day 2 Trans

# Two Day 3 Trans



### THREE DAYS ###

# Three Day 0 Trans

# Three Day 1 Trans

# Three Day 2 Trans

# Three Day 3 Trans

### Duplicate entries ###

# One day one trans one dupe

# One day one trans multiple dupes