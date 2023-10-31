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

# TODO change this block to remove the duplicate check
d1t0_struct <- d1t0 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d1t0_all_analysis <- d1t0_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are zero trans
expect_equal(unique(d1t0_all_analysis$slicedTsibble[[1]]$value),"bottom", label='d1t0 sliced tsibble valuecol')

# TODO can we delete the sampled?
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

# check overall time budget
expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(1,0,0), ncol=3)))

# check that time budget says that it spent 100% in bottom
expect_equal(d1t0_overall_tb, expected_res, label='d1t0 overall time budget')

# TODO change the code to be slicedTsibble as opposed to sampled
d1t0_all_room_day <- d1t0_overall_interval |>
  mutate(day = map(slicedTsibble, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(slicedTsibble, ~ getNightRecords(.x,"05:00","22:00"))) 

# check n day records

expect_equal(length(d1t0_all_room_day$day[[1]]$day), sum(d1t0$characteristic == "day"), label='d1t0 num of day records')

# check n night records

expect_equal(length(d1t0_all_room_day$night[[1]]$day), sum(d1t0$characteristic == "night"), label='d1t0 num of night records')

# check that rejoining day and night gives you the overall table (Will break if Regmi wants to have a hour deadband for night)

expect_equal( bind_rows(d1t0_all_room_day$day[[1]],d1t0_all_room_day$night[[1]])[,1:2], d1t0_overall_interval$slicedTsibble[[1]], label='d1t0 day+night == overall')

# check range of dos

expect_equal(unique(d1t0_all_room_day$day[[1]]$dos), 1 , label='d1t0 unique dos counts')

# check range of wos

expect_equal(unique(d1t0_all_room_day$day[[1]]$wos), 1 , label='d1t0 unique wos counts')

d1t0_all_room_day <- d1t0_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))

# check 0 trans in day

n_trans <- length(d1t0_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)-1

expect_equal(n_trans, 0 , label='d1t0 expect 0 trans in day')

# check 0 trans in night

n_trans <- length(d1t0_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)-1

expect_equal(n_trans, 0 , label='d1t0 expect 0 trans in night')

# check start and end day

expect_equal(as.numeric(head(d1t0_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d1t0_all_room_day$day[[1]]$datetime,n=1)) , label='d1t0 expect first record t1 is beginning of day')
expect_equal(as.numeric(tail(d1t0_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t2), as.numeric(tail(d1t0_all_room_day$day[[1]]$datetime,n=1)) , label='d1t0 expect last record t2 is beginning of day')

# check start and end night

expect_equal(as.numeric(head(d1t0_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d1t0_all_room_day$night[[1]]$datetime,n=1)) , label='d1t0 expect first record t1 is beginning of night')
expect_equal(as.numeric(tail(d1t0_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t2), as.numeric(tail(d1t0_all_room_day$night[[1]]$datetime,n=1)) , label='d1t0 expect last record t2 is beginning of night')

# check n records day+night - 1 == n records overall 

n_day_trans <- length(d1t0_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)-1
n_night_trans <- length(d1t0_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)-1

n_trans_overall <- length(d1t0_overall_interval$interval[[1]]$to_zone) - 1

expect_equal(as.numeric(n_day_trans+n_night_trans), as.numeric(n_trans_overall) , label='d1t0 expect nTransDay+nTransNight == nTransOverall')

d1t0_all_room_time_budget <- d1t0_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))

# check day time budget

d1t0_day_tb <- tibble(d1t0_all_room_time_budget$daily_tb[[1]][[1]])

Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d1t0_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d1t0_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t2),origin=origin)))

expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(1,0,0), ncol=3)))

expect_equal(d1t0_day_tb, expected_res, label='d1t0 expect day tb 100% bottom')

# check night time budget

d1t0_night_tb <- tibble(d1t0_all_room_time_budget$night_tb[[1]][[1]])

Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d1t0_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d1t0_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t2),origin=origin)))

expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(1,0,0), ncol=3)))

expect_equal(d1t0_night_tb, expected_res, label='d1t0 expect day tb 100% bottom')

### END ONE DAY 0 TRANS ###

# One Day 1 Trans

d1t1 <- read.csv("../data/test_data/one_day_one_trans_r2.csv")

bird_ids_d1t1 <- unique(d1t1$tagname)
bird_ids_d1t1 <- na.trim(sort(bird_ids_d1t1))

d1t1["DateTime"] <- as.POSIXct(d1t1$access, origin="1970-01-01", tz="GMT")

print("what makes up subzone col")
unique(d1t1$subzone)

d1t1$subzone[d1t1$subzone == "Bottom"] <- "bottom"
d1t1$subzone[d1t1$subzone == "Middle"] <- "middle"
d1t1$subzone[d1t1$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(d1t1$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(d1t1$DateTime))
sum(is.na(d1t1$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
d1t1$accessdate <- ymd_hms(d1t1$DateTime)

d1t1_struct <- d1t1 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d1t1_all_analysis <- d1t1_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are two zones observed
expect_equal(unique(d1t1_all_analysis$slicedTsibble[[1]]$value),c("bottom","top"), label='d1t1 sliced tsibble valuecol')

# TODO can we delete the sampled?
d1t1_regular <- d1t1_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 


# first item to compare to day+night
d1t1_overall_interval <- d1t1_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x)))

d1t1_all_room_time_budget <- d1t1_overall_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

#TODO can compare this later but not functional right now
d1t1_overall_tb <- d1t1_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d1t1_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d1t1_overall_interval$interval[[1]],n=1)$t2),origin=origin)))

# # check overall time budget
# bot_time <- sum(d1t1$subzone == "bottom")/length(d1t1$subzone)
# mid_time <- sum(d1t1$subzone == "middle")/length(d1t1$subzone)
# top_time <- sum(d1t1$subzone == "top")/length(d1t1$subzone)
# expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(bot_time,mid_time,top_time), ncol=3)))

# # check that time budget says that it spent
# expect_equal(d1t1_overall_tb, expected_res, label='d1t1 overall time budget')

# TODO change the code to be slicedTsibble as opposed to sampled
d1t1_all_room_day <- d1t1_overall_interval |>
  mutate(day = map(slicedTsibble, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(slicedTsibble, ~ getNightRecords(.x,"05:00","22:00"))) 

# check n day records

expect_equal(length(d1t1_all_room_day$day[[1]]$day), sum(d1t1$characteristic == "day"), label='d1t1 num of day records')

# check n night records

expect_equal(length(d1t1_all_room_day$night[[1]]$day), sum(d1t1$characteristic == "night"), label='d1t1 num of night records')

# check that rejoining day and night gives you the overall table (Will break if Regmi wants to have a hour deadband for night)

expect_equal( bind_rows(d1t1_all_room_day$day[[1]],d1t1_all_room_day$night[[1]])[,1:2], d1t1_overall_interval$slicedTsibble[[1]], label='d1t1 day+night == overall')

# check range of dos

expect_equal(unique(d1t1_all_room_day$day[[1]]$dos), 1 , label='d1t1 unique dos counts')
expect_equal(unique(d1t1_all_room_day$night[[1]]$dos), 1 , label='d1t1 unique dos night counts')

# check range of wos

expect_equal(unique(d1t1_all_room_day$day[[1]]$wos), 1 , label='d1t1 unique wos counts')

d1t1_all_room_day <- d1t1_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))

# check 1 trans in day

n_trans <- length(d1t1_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)-1

expect_equal(n_trans, 1 , label='d1t1 expect 1 trans in day')

# check 1 trans in night

n_trans <- length(d1t1_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)-1

expect_equal(n_trans, 0 , label='d1t1 expect 0 trans in night')

# check start and end day

expect_equal(as.numeric(head(d1t1_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d1t1_all_room_day$day[[1]]$datetime,n=1)) , label='d1t1 expect first record t1 is beginning of day')
expect_equal(as.numeric(tail(d1t1_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t2), as.numeric(tail(d1t1_all_room_day$day[[1]]$datetime,n=1)) , label='d1t1 expect last record t2 is beginning of day')

# check start and end night

expect_equal(as.numeric(head(d1t1_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d1t1_all_room_day$night[[1]]$datetime,n=1)) , label='d1t1 expect first record t1 is beginning of night')
expect_equal(as.numeric(tail(d1t1_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t2), as.numeric(tail(d1t1_all_room_day$night[[1]]$datetime,n=1)) , label='d1t1 expect last record t2 is beginning of night')

# check n records day+night - 1 == n records overall 

n_day_trans <- length(d1t1_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)-1
n_night_trans <- length(d1t1_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)-1

n_trans_overall <- length(d1t1_overall_interval$interval[[1]]$to_zone) - 1

expect_equal(as.numeric(n_day_trans+n_night_trans), as.numeric(n_trans_overall) , label='d1t1 expect nTransDay+nTransNight == nTransOverall')

d1t1_all_room_time_budget <- d1t1_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))


# d1t1_day_tb <- tibble(d1t1_all_room_time_budget$daily_tb[[1]][[1]])

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d1t1_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d1t1_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t2),origin=origin)))


# d1t1_night_tb <- tibble(d1t1_all_room_time_budget$night_tb[[1]][[1]])

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d1t1_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d1t1_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t2),origin=origin)))

# # check day tb + night tb  == overall tb

# day <-as.numeric(d1t1_day_tb[,2] - d1t1_day_tb[,1])
# night <- as.numeric(d1t1_night_tb[,2] - d1t1_night_tb[,1])
# total <- as.numeric(day+night)
# d1t1_day_tb[,3:5]*(as.numeric(day)/as.numeric(total)) + d1t1_night_tb[,3:5]*(as.numeric(night)/as.numeric(total))
# # close but not exact so I don't want to use it
# expect_equal((d1t1_day_tb[,3:5] + d1t1_night_tb[,3:5])/2,d1t1_overall_tb[,3:5])

### END OF ONE DAY ONE TRANS ###

# One Day 2 Trans

d1t2 <- read.csv("../data/test_data/one_day_two_trans_r3.csv")

d1t2$tagname <- d1t2$LegBand

bird_ids_d1t2 <- unique(d1t2$tagname)
bird_ids_d1t2 <- na.trim(sort(bird_ids_d1t2))

d1t2["DateTime"] <- as.POSIXct(d1t2$access, origin="1970-01-01", tz="GMT")

print("what makes up subzone col")
unique(d1t2$subzone)

d1t2$subzone[d1t2$subzone == "Bottom"] <- "bottom"
d1t2$subzone[d1t2$subzone == "Middle"] <- "middle"
d1t2$subzone[d1t2$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(d1t2$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(d1t2$DateTime))
sum(is.na(d1t2$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
d1t2$accessdate <- ymd_hms(d1t2$DateTime)

d1t2_struct <- d1t2 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d1t2_all_analysis <- d1t2_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are two zones observed
expect_equal(unique(d1t2_all_analysis$slicedTsibble[[1]]$value),c("middle","bottom"), label='d1t2 sliced tsibble valuecol')

# TODO can we delete the sampled?
d1t2_regular <- d1t2_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 


# first item to compare to day+night
d1t2_overall_interval <- d1t2_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x)))



d1t2_all_room_time_budget <- d1t2_overall_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

#TODO can compare this later but not functional right now
d1t2_overall_tb <- d1t2_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d1t2_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d1t2_overall_interval$interval[[1]],n=1)$t2),origin=origin)))

# # check overall time budget
# bot_time <- sum(d1t2$subzone == "bottom")/length(d1t2$subzone)
# mid_time <- sum(d1t2$subzone == "middle")/length(d1t2$subzone)
# top_time <- sum(d1t2$subzone == "top")/length(d1t2$subzone)
# expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(bot_time,mid_time,top_time), ncol=3)))

# # check that time budget says that it spent
# expect_equal(d1t2_overall_tb, expected_res, label='d1t2 overall time budget')

# TODO change the code to be slicedTsibble as opposed to sampled
d1t2_all_room_day <- d1t2_overall_interval |>
  mutate(day = map(slicedTsibble, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(slicedTsibble, ~ getNightRecords(.x,"05:00","22:00"))) 

# check n day records

expect_equal(length(d1t2_all_room_day$day[[1]]$day), sum(d1t2$characteristic == "day"), label='d1t2 num of day records')

# check n night records

expect_equal(length(d1t2_all_room_day$night[[1]]$day), sum(d1t2$characteristic == "night"), label='d1t2 num of night records')

# check that rejoining day and night gives you the overall table (Will break if Regmi wants to have a hour deadband for night)

expect_equal( bind_rows(d1t2_all_room_day$day[[1]],d1t2_all_room_day$night[[1]])[,1:2], d1t2_overall_interval$slicedTsibble[[1]], label='d1t2 day+night == overall')

# check range of dos

expect_equal(unique(d1t2_all_room_day$day[[1]]$dos), 1 , label='d1t2 unique dos counts')
expect_equal(unique(d1t2_all_room_day$night[[1]]$dos), 1 , label='d1t2 unique dos night counts')

# check range of wos

expect_equal(unique(d1t2_all_room_day$day[[1]]$wos), 1 , label='d1t2 unique wos counts')

d1t2_all_room_day <- d1t2_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))

# check 1 trans in day

n_trans <- length(d1t2_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)-1

# The second trans happens right at day to night
expect_equal(n_trans, 1 , label='d1t2 expect 1 trans in day')

# check 1 trans in night

n_trans <- length(d1t2_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)-1

expect_equal(n_trans, 0 , label='d1t2 expect 0 trans in night')

# check start and end day

expect_equal(as.numeric(head(d1t2_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d1t2_all_room_day$day[[1]]$datetime,n=1)) , label='d1t2 expect first record t1 is beginning of day')
expect_equal(as.numeric(tail(d1t2_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t2), as.numeric(tail(d1t2_all_room_day$day[[1]]$datetime,n=1)) , label='d1t2 expect last record t2 is beginning of day')

# check start and end night

expect_equal(as.numeric(head(d1t2_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d1t2_all_room_day$night[[1]]$datetime,n=1)) , label='d1t2 expect first record t1 is beginning of night')
expect_equal(as.numeric(tail(d1t2_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t2), as.numeric(tail(d1t2_all_room_day$night[[1]]$datetime,n=1)) , label='d1t2 expect last record t2 is beginning of night')

# check n records day+night - 1 == n records overall 

n_day_trans <- length(d1t2_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)-1
n_night_trans <- length(d1t2_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)-1

n_trans_overall <- length(d1t2_overall_interval$interval[[1]]$to_zone) - 1 

if(n_trans_overall != sum(n_day_trans,n_night_trans)){ inbetween_trans = (n_trans_overall - sum(n_day_trans,n_night_trans))} else { inbetween_trans = 0}

# Mismatch error because there are transitions at day and night
expect_equal(as.numeric(n_day_trans+n_night_trans + inbetween_trans), as.numeric(n_trans_overall) , label='d1t2 expect nTransDay+nTransNight == nTransOverall')

d1t2_all_room_time_budget <- d1t2_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))

### END OF ONE DAY 2 TRANS ###

# One Day 3 Trans

d1t3 <- read.csv("../data/test_data/one_day_three_trans_r3.csv")

d1t3$tagname <- d1t3$LegBand

bird_ids_d1t3 <- unique(d1t3$tagname)
bird_ids_d1t3 <- na.trim(sort(bird_ids_d1t3))

d1t3["DateTime"] <- as.POSIXct(d1t3$access, origin="1970-01-01", tz="GMT")

print("what makes up subzone col")
unique(d1t3$subzone)

d1t3$subzone[d1t3$subzone == "Bottom"] <- "bottom"
d1t3$subzone[d1t3$subzone == "Middle"] <- "middle"
d1t3$subzone[d1t3$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(d1t3$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(d1t3$DateTime))
sum(is.na(d1t3$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
d1t3$accessdate <- ymd_hms(d1t3$DateTime)

d1t3_struct <- d1t3 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d1t3_all_analysis <- d1t3_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are two zones observed
expect_equal(unique(d1t3_all_analysis$slicedTsibble[[1]]$value),c("middle","top"), label='d1t3 sliced tsibble valuecol')

# TODO can we delete the sampled?
d1t3_regular <- d1t3_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 


# first item to compare to day+night
d1t3_overall_interval <- d1t3_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x)))

d1t3_all_room_time_budget <- d1t3_overall_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

#TODO can compare this later but not functional right now
d1t3_overall_tb <- d1t3_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d1t3_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d1t3_overall_interval$interval[[1]],n=1)$t2),origin=origin)))

# # check overall time budget
# bot_time <- sum(d1t3$subzone == "bottom")/length(d1t3$subzone)
# mid_time <- sum(d1t3$subzone == "middle")/length(d1t3$subzone)
# top_time <- sum(d1t3$subzone == "top")/length(d1t3$subzone)
# expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(bot_time,mid_time,top_time), ncol=3)))

# # check that time budget says that it spent
# expect_equal(d1t3_overall_tb, expected_res, label='d1t3 overall time budget')

# TODO change the code to be slicedTsibble as opposed to sampled
d1t3_all_room_day <- d1t3_overall_interval |>
  mutate(day = map(slicedTsibble, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(slicedTsibble, ~ getNightRecords(.x,"05:00","22:00"))) 

# check n day records

expect_equal(length(d1t3_all_room_day$day[[1]]$day), sum(d1t3$characteristic == "day"), label='d1t3 num of day records')

# check n night records

expect_equal(length(d1t3_all_room_day$night[[1]]$day), sum(d1t3$characteristic == "night"), label='d1t3 num of night records')

# check that rejoining day and night gives you the overall table (Will break if Regmi wants to have a hour deadband for night)

expect_equal( bind_rows(d1t3_all_room_day$day[[1]],d1t3_all_room_day$night[[1]])[,1:2], d1t3_overall_interval$slicedTsibble[[1]], label='d1t3 day+night == overall')

# check range of dos

expect_equal(unique(d1t3_all_room_day$day[[1]]$dos), 1 , label='d1t3 unique dos counts')
expect_equal(unique(d1t3_all_room_day$night[[1]]$dos), 1 , label='d1t3 unique dos night counts')

# check range of wos

expect_equal(unique(d1t3_all_room_day$day[[1]]$wos), 1 , label='d1t3 unique wos counts')

d1t3_all_room_day <- d1t3_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))

# check 1 trans in day

n_trans <- length(d1t3_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)-1

expect_equal(n_trans, 2 , label='d1t3 expect 2 trans in day')

# check 1 trans in night

n_trans <- length(d1t3_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)-1

expect_equal(n_trans, 1 , label='d1t3 expect 1 trans in night')

# check start and end day

expect_equal(as.numeric(head(d1t3_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d1t3_all_room_day$day[[1]]$datetime,n=1)) , label='d1t3 expect first record t1 is beginning of day')
expect_equal(as.numeric(tail(d1t3_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t2), as.numeric(tail(d1t3_all_room_day$day[[1]]$datetime,n=1)) , label='d1t3 expect last record t2 is beginning of day')

# check start and end night

expect_equal(as.numeric(head(d1t3_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d1t3_all_room_day$night[[1]]$datetime,n=1)) , label='d1t3 expect first record t1 is beginning of night')
expect_equal(as.numeric(tail(d1t3_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t2), as.numeric(tail(d1t3_all_room_day$night[[1]]$datetime,n=1)) , label='d1t3 expect last record t2 is beginning of night')

# check n records day+night - 1 == n records overall 

n_day_trans <- length(d1t3_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)-1
n_night_trans <- length(d1t3_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)-1

n_trans_overall <- length(d1t3_overall_interval$interval[[1]]$to_zone) - 1

# Mismatch error because there are transitions at day and night
expect_equal(as.numeric(n_day_trans+n_night_trans), as.numeric(n_trans_overall) , label='d1t3 expect nTransDay+nTransNight == nTransOverall')

d1t3_all_room_time_budget <- d1t3_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))

### END OF ONE DAY THREE TRANS


### TWO DAYS ###


# Two Day 0 Trans

d2t0 <- read.csv("../data/test_data/two_day_zero_trans_r8.csv")

#d2t0$tagname <- d2t0$LegBand

bird_ids_d2t0 <- unique(d2t0$tagname)
bird_ids_d2t0 <- na.trim(sort(bird_ids_d2t0))

d2t0["DateTime"] <- as.POSIXct(d2t0$access, origin="1970-01-01", tz="GMT")

print("what makes up subzone col")
unique(d2t0$subzone)

d2t0$subzone[d2t0$subzone == "Bottom"] <- "bottom"
d2t0$subzone[d2t0$subzone == "Middle"] <- "middle"
d2t0$subzone[d2t0$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(d2t0$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(d2t0$DateTime))
sum(is.na(d2t0$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
d2t0$accessdate <- ymd_hms(d2t0$DateTime)

d2t0_struct <- d2t0 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d2t0_all_analysis <- d2t0_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are two zones observed
expect_equal(unique(d2t0_all_analysis$slicedTsibble[[1]]$value),c("bottom"), label='d2t0 sliced tsibble valuecol')

# TODO can we delete the sampled?
d2t0_regular <- d2t0_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 


# first item to compare to day+night
d2t0_overall_interval <- d2t0_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x)))

d2t0_all_room_time_budget <- d2t0_overall_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

#TODO can compare this later but not functional right now
d2t0_overall_tb <- d2t0_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d2t0_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d2t0_overall_interval$interval[[1]],n=1)$t2),origin=origin)))

# # check overall time budget
# bot_time <- sum(d2t0$subzone == "bottom")/length(d2t0$subzone)
# mid_time <- sum(d2t0$subzone == "middle")/length(d2t0$subzone)
# top_time <- sum(d2t0$subzone == "top")/length(d2t0$subzone)
# expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(bot_time,mid_time,top_time), ncol=3)))

# # check that time budget says that it spent
# expect_equal(d2t0_overall_tb, expected_res, label='d2t0 overall time budget')

# TODO change the code to be slicedTsibble as opposed to sampled
d2t0_all_room_day <- d2t0_overall_interval |>
  mutate(day = map(slicedTsibble, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(slicedTsibble, ~ getNightRecords(.x,"05:00","22:00"))) 

# check n day records

expect_equal(length(d2t0_all_room_day$day[[1]]$day), sum(d2t0$characteristic == "day"), label='d2t0 num of day records')

# check n night records

expect_equal(length(d2t0_all_room_day$night[[1]]$day), sum(d2t0$characteristic == "night"), label='d2t0 num of night records')

# check that rejoining day and night gives you the overall table (Will break if Regmi wants to have a hour deadband for night)

expect_equal( bind_rows(d2t0_all_room_day$day[[1]],d2t0_all_room_day$night[[1]])[,1:2], d2t0_overall_interval$slicedTsibble[[1]], label='d2t0 day+night == overall')

# check range of dos

expect_equal(unique(d2t0_all_room_day$day[[1]]$dos), c(1,2) , label='d2t0 unique dos counts')
expect_equal(unique(d2t0_all_room_day$night[[1]]$dos), c(1,2) , label='d2t0 unique dos night counts')

# check range of wos

expect_equal(unique(d2t0_all_room_day$day[[1]]$wos), 1 , label='d2t0 unique wos counts')

d2t0_all_room_day <- d2t0_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))

# check 1 trans in day

n_trans <- length(d2t0_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d2t0_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 0 , label='d2t0 expect 0 trans in day')

# check 1 trans in night

n_trans <- length(d2t0_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d2t0_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 0 , label='d2t0 expect 1 trans in night')

# check start and end day

expect_equal(as.numeric(head(d2t0_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d2t0_all_room_day$day[[1]]$datetime,n=1)) , label='d2t0 expect first record t1 is beginning of day')
expect_equal(as.numeric(tail(d2t0_all_room_day$day_int[[1]]$daily_int[[2]],n=1)$t2), as.numeric(tail(d2t0_all_room_day$day[[1]]$datetime,n=1)) , label='d2t0 expect last record t2 is beginning of day')

# check start and end night

expect_equal(as.numeric(head(d2t0_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d2t0_all_room_day$night[[1]]$datetime,n=1)) , label='d2t0 expect first record t1 is beginning of night')
expect_equal(as.numeric(tail(d2t0_all_room_day$night_int[[1]]$daily_int[[2]],n=1)$t2), as.numeric(tail(d2t0_all_room_day$night[[1]]$datetime,n=1)) , label='d2t0 expect last record t2 is beginning of night')

# check n records day+night - 1 == n records overall 

n_day_trans <- length(d2t0_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d2t0_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)-2
n_night_trans <- length(d2t0_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d2t0_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)-2

n_trans_overall <- length(d2t0_overall_interval$interval[[1]]$to_zone)-1

# Mismatch error because there are transitions at day and night
expect_equal(as.numeric(n_day_trans+n_night_trans), as.numeric(n_trans_overall) , label='d2t0 expect nTransDay+nTransNight == nTransOverall')

d2t0_all_room_time_budget <- d2t0_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))

### END OF TWO DAY ZERO TRANS

# Two Day 1 Trans

d2t1 <- read.csv("../data/test_data/two_day_one_trans_r8.csv")

#d2t1$tagname <- d2t1$LegBand

bird_ids_d2t1 <- unique(d2t1$tagname)
bird_ids_d2t1 <- na.trim(sort(bird_ids_d2t1))

d2t1["DateTime"] <- as.POSIXct(d2t1$access, origin="1970-01-01", tz="GMT")

print("what makes up subzone col")
unique(d2t1$subzone)

d2t1$subzone[d2t1$subzone == "Bottom"] <- "bottom"
d2t1$subzone[d2t1$subzone == "Middle"] <- "middle"
d2t1$subzone[d2t1$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(d2t1$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(d2t1$DateTime))
sum(is.na(d2t1$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
d2t1$accessdate <- ymd_hms(d2t1$DateTime)

d2t1_struct <- d2t1 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d2t1_all_analysis <- d2t1_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are two zones observed
expect_equal(unique(d2t1_all_analysis$slicedTsibble[[1]]$value),c("top","middle"), label='d2t1 sliced tsibble valuecol')

# TODO can we delete the sampled?
d2t1_regular <- d2t1_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 


# first item to compare to day+night
d2t1_overall_interval <- d2t1_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x)))

d2t1_all_room_time_budget <- d2t1_overall_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

#TODO can compare this later but not functional right now
d2t1_overall_tb <- d2t1_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d2t1_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d2t1_overall_interval$interval[[1]],n=1)$t2),origin=origin)))

# # check overall time budget
# bot_time <- sum(d2t1$subzone == "bottom")/length(d2t1$subzone)
# mid_time <- sum(d2t1$subzone == "middle")/length(d2t1$subzone)
# top_time <- sum(d2t1$subzone == "top")/length(d2t1$subzone)
# expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(bot_time,mid_time,top_time), ncol=3)))

# # check that time budget says that it spent
# expect_equal(d2t1_overall_tb, expected_res, label='d2t1 overall time budget')

# TODO change the code to be slicedTsibble as opposed to sampled
d2t1_all_room_day <- d2t1_overall_interval |>
  mutate(day = map(slicedTsibble, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(slicedTsibble, ~ getNightRecords(.x,"05:00","22:00"))) 

# check n day records

expect_equal(length(d2t1_all_room_day$day[[1]]$day), sum(d2t1$characteristic == "day"), label='d2t1 num of day records')

# check n night records

expect_equal(length(d2t1_all_room_day$night[[1]]$day), sum(d2t1$characteristic == "night"), label='d2t1 num of night records')

# check that rejoining day and night gives you the overall table (Will break if Regmi wants to have a hour deadband for night)

expect_equal( bind_rows(d2t1_all_room_day$day[[1]],d2t1_all_room_day$night[[1]])[,1:2], d2t1_overall_interval$slicedTsibble[[1]], label='d2t1 day+night == overall')

# check range of dos

expect_equal(unique(d2t1_all_room_day$day[[1]]$dos), c(1,2) , label='d2t1 unique dos counts')
expect_equal(unique(d2t1_all_room_day$night[[1]]$dos), c(1,2) , label='d2t1 unique dos night counts')

# check range of wos

expect_equal(unique(d2t1_all_room_day$day[[1]]$wos), 1 , label='d2t1 unique wos counts')

d2t1_all_room_day <- d2t1_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))

# check 1 trans in day

n_trans <- length(d2t1_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d2t1_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 1 , label='d2t1 expect 1 trans in day')

# check 1 trans in night

n_trans <- length(d2t1_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d2t1_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 0 , label='d2t1 expect 0 trans in night')

# check start and end day

expect_equal(as.numeric(head(d2t1_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d2t1_all_room_day$day[[1]]$datetime,n=1)) , label='d2t1 expect first record t1 is beginning of day')
expect_equal(as.numeric(tail(d2t1_all_room_day$day_int[[1]]$daily_int[[2]],n=1)$t2), as.numeric(tail(d2t1_all_room_day$day[[1]]$datetime,n=1)) , label='d2t1 expect last record t2 is beginning of day')

# check start and end night

expect_equal(as.numeric(head(d2t1_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d2t1_all_room_day$night[[1]]$datetime,n=1)) , label='d2t1 expect first record t1 is beginning of night')
expect_equal(as.numeric(tail(d2t1_all_room_day$night_int[[1]]$daily_int[[2]],n=1)$t2), as.numeric(tail(d2t1_all_room_day$night[[1]]$datetime,n=1)) , label='d2t1 expect last record t2 is beginning of night')

# check n records day+night - 1 == n records overall 

n_day_trans <- length(d2t1_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d2t1_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)-2
n_night_trans <- length(d2t1_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d2t1_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)-2

n_trans_overall <- length(d2t1_overall_interval$interval[[1]]$to_zone)-1

# Mismatch error because there are transitions at day and night
expect_equal(as.numeric(n_day_trans+n_night_trans), as.numeric(n_trans_overall) , label='d2t1 expect nTransDay+nTransNight == nTransOverall')

d2t1_all_room_time_budget <- d2t1_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))

### END OF TWO DAY ONE TRANS

# Two Day 2 Trans

d2t2 <- read.csv("../data/test_data/two_day_two_trans_r8.csv")

#d2t2$tagname <- d2t2$LegBand

bird_ids_d2t2 <- unique(d2t2$tagname)
bird_ids_d2t2 <- na.trim(sort(bird_ids_d2t2))

d2t2["DateTime"] <- as.POSIXct(d2t2$access, origin="1970-01-01", tz="GMT")

print("what makes up subzone col")
unique(d2t2$subzone)

d2t2$subzone[d2t2$subzone == "Bottom"] <- "bottom"
d2t2$subzone[d2t2$subzone == "Middle"] <- "middle"
d2t2$subzone[d2t2$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(d2t2$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(d2t2$DateTime))
sum(is.na(d2t2$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
d2t2$accessdate <- ymd_hms(d2t2$DateTime)

d2t2_struct <- d2t2 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d2t2_all_analysis <- d2t2_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are two zones observed
expect_equal(unique(d2t2_all_analysis$slicedTsibble[[1]]$value),c('top',"middle","bottom"), label='d2t2 sliced tsibble valuecol')

# TODO can we delete the sampled?
d2t2_regular <- d2t2_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 


# first item to compare to day+night
d2t2_overall_interval <- d2t2_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x)))

d2t2_all_room_time_budget <- d2t2_overall_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

#TODO can compare this later but not functional right now
d2t2_overall_tb <- d2t2_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d2t2_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d2t2_overall_interval$interval[[1]],n=1)$t2),origin=origin)))

# # check overall time budget
# bot_time <- sum(d2t2$subzone == "bottom")/length(d2t2$subzone)
# mid_time <- sum(d2t2$subzone == "middle")/length(d2t2$subzone)
# top_time <- sum(d2t2$subzone == "top")/length(d2t2$subzone)
# expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(bot_time,mid_time,top_time), ncol=3)))

# # check that time budget says that it spent
# expect_equal(d2t2_overall_tb, expected_res, label='d2t2 overall time budget')

# TODO change the code to be slicedTsibble as opposed to sampled
d2t2_all_room_day <- d2t2_overall_interval |>
  mutate(day = map(slicedTsibble, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(slicedTsibble, ~ getNightRecords(.x,"05:00","22:00"))) 

# check n day records

expect_equal(length(d2t2_all_room_day$day[[1]]$day), sum(d2t2$characteristic == "day"), label='d2t2 num of day records')

# check n night records

expect_equal(length(d2t2_all_room_day$night[[1]]$day), sum(d2t2$characteristic == "night"), label='d2t2 num of night records')

# check that rejoining day and night gives you the overall table (Will break if Regmi wants to have a hour deadband for night)

expect_equal( bind_rows(d2t2_all_room_day$day[[1]],d2t2_all_room_day$night[[1]])[,1:2], d2t2_overall_interval$slicedTsibble[[1]], label='d2t2 day+night == overall')

# check range of dos

expect_equal(unique(d2t2_all_room_day$day[[1]]$dos), c(1,2) , label='d2t2 unique dos counts')
expect_equal(unique(d2t2_all_room_day$night[[1]]$dos), c(1,2) , label='d2t2 unique dos night counts')

# check range of wos

expect_equal(unique(d2t2_all_room_day$day[[1]]$wos), 1 , label='d2t2 unique wos counts')

d2t2_all_room_day <- d2t2_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))

# check 1 trans in day

n_trans <- length(d2t2_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d2t2_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 0 , label='d2t2 expect 0 trans in day')

# check 1 trans in night

n_trans <- length(d2t2_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d2t2_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 2 , label='d2t2 expect 2 trans in night')

# TODO update check start and end day

expect_equal(as.numeric(head(d2t2_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d2t2_all_room_day$day[[1]]$datetime,n=1)) , label='d2t2 expect first record t1 is beginning of day')
expect_equal(as.numeric(tail(d2t2_all_room_day$day_int[[1]]$daily_int[[2]],n=1)$t2), as.numeric(tail(d2t2_all_room_day$day[[1]]$datetime,n=1)) , label='d2t2 expect last record t2 is beginning of day')

#  TODO update check start and end night

expect_equal(as.numeric(head(d2t2_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d2t2_all_room_day$night[[1]]$datetime,n=1)) , label='d2t2 expect first record t1 is beginning of night')
expect_equal(as.numeric(tail(d2t2_all_room_day$night_int[[1]]$daily_int[[2]],n=1)$t2), as.numeric(tail(d2t2_all_room_day$night[[1]]$datetime,n=1)) , label='d2t2 expect last record t2 is beginning of night')

# check n records day+night - 1 == n records overall 

n_day_trans <- length(d2t2_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d2t2_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)-2
n_night_trans <- length(d2t2_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d2t2_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)-2

n_trans_overall <- length(d2t2_overall_interval$interval[[1]]$to_zone)-1

# Mismatch error because there are transitions at day and night
expect_equal(as.numeric(n_day_trans+n_night_trans), as.numeric(n_trans_overall) , label='d2t2 expect nTransDay+nTransNight == nTransOverall')

d2t2_all_room_time_budget <- d2t2_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))

### END OF TWO DAY TWO TRANS

# Two Day 3 Trans

d2t3 <- read.csv("../data/test_data/two_day_three_trans_r8.csv")

#d2t3$tagname <- d2t3$LegBand

bird_ids_d2t3 <- unique(d2t3$tagname)
bird_ids_d2t3 <- na.trim(sort(bird_ids_d2t3))

d2t3["DateTime"] <- as.POSIXct(d2t3$access, origin="1970-01-01", tz="GMT")

print("what makes up subzone col")
unique(d2t3$subzone)

d2t3$subzone[d2t3$subzone == "Bottom"] <- "bottom"
d2t3$subzone[d2t3$subzone == "Middle"] <- "middle"
d2t3$subzone[d2t3$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(d2t3$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(d2t3$DateTime))
sum(is.na(d2t3$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
d2t3$accessdate <- ymd_hms(d2t3$DateTime)

d2t3_struct <- d2t3 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d2t3_all_analysis <- d2t3_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are two zones observed
expect_equal(unique(d2t3_all_analysis$slicedTsibble[[1]]$value),c('middle',"top","bottom"), label='d2t3 sliced tsibble valuecol')

# TODO can we delete the sampled?
d2t3_regular <- d2t3_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 


# first item to compare to day+night
d2t3_overall_interval <- d2t3_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x)))

d2t3_all_room_time_budget <- d2t3_overall_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

#TODO can compare this later but not functional right now
d2t3_overall_tb <- d2t3_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d2t3_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d2t3_overall_interval$interval[[1]],n=1)$t2),origin=origin)))

# # check overall time budget
# bot_time <- sum(d2t3$subzone == "bottom")/length(d2t3$subzone)
# mid_time <- sum(d2t3$subzone == "middle")/length(d2t3$subzone)
# top_time <- sum(d2t3$subzone == "top")/length(d2t3$subzone)
# expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(bot_time,mid_time,top_time), ncol=3)))

# # check that time budget says that it spent
# expect_equal(d2t3_overall_tb, expected_res, label='d2t3 overall time budget')

# TODO change the code to be slicedTsibble as opposed to sampled
d2t3_all_room_day <- d2t3_overall_interval |>
  mutate(day = map(slicedTsibble, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(slicedTsibble, ~ getNightRecords(.x,"05:00","22:00"))) 

# check n day records

expect_equal(length(d2t3_all_room_day$day[[1]]$day), sum(d2t3$characteristic == "day"), label='d2t3 num of day records')

# check n night records

expect_equal(length(d2t3_all_room_day$night[[1]]$day), sum(d2t3$characteristic == "night"), label='d2t3 num of night records')

# check that rejoining day and night gives you the overall table (Will break if Regmi wants to have a hour deadband for night)

expect_equal( bind_rows(d2t3_all_room_day$day[[1]],d2t3_all_room_day$night[[1]])[,1:2], d2t3_overall_interval$slicedTsibble[[1]], label='d2t3 day+night == overall')

# check range of dos

expect_equal(unique(d2t3_all_room_day$day[[1]]$dos), c(1,2) , label='d2t3 unique dos counts')
expect_equal(unique(d2t3_all_room_day$night[[1]]$dos), c(1,2) , label='d2t3 unique dos night counts')

# check range of wos

expect_equal(unique(d2t3_all_room_day$day[[1]]$wos), 1 , label='d2t3 unique wos counts')

d2t3_all_room_day <- d2t3_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))

# check 1 trans in day

n_trans <- length(d2t3_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d2t3_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 1 , label='d2t3 expect 1 trans in day')

# check 1 trans in night

n_trans <- length(d2t3_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d2t3_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 2 , label='d2t3 expect 2 trans in night')

# TODO update check start and end day

expect_equal(as.numeric(head(d2t3_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d2t3_all_room_day$day[[1]]$datetime,n=1)) , label='d2t3 expect first record t1 is beginning of day')
expect_equal(as.numeric(tail(d2t3_all_room_day$day_int[[1]]$daily_int[[2]],n=1)$t2), as.numeric(tail(d2t3_all_room_day$day[[1]]$datetime,n=1)) , label='d2t3 expect last record t2 is beginning of day')

#  TODO update check start and end night

expect_equal(as.numeric(head(d2t3_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d2t3_all_room_day$night[[1]]$datetime,n=1)) , label='d2t3 expect first record t1 is beginning of night')
expect_equal(as.numeric(tail(d2t3_all_room_day$night_int[[1]]$daily_int[[2]],n=1)$t2), as.numeric(tail(d2t3_all_room_day$night[[1]]$datetime,n=1)) , label='d2t3 expect last record t2 is beginning of night')

# check n records day+night - 1 == n records overall 

n_day_trans <- length(d2t3_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d2t3_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)-2
n_night_trans <- length(d2t3_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d2t3_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)-2

n_trans_overall <- length(d2t3_overall_interval$interval[[1]]$to_zone)-1

# Mismatch error because there are transitions at day and night
expect_equal(as.numeric(n_day_trans+n_night_trans), as.numeric(n_trans_overall) , label='d2t3 expect nTransDay+nTransNight == nTransOverall')

d2t3_all_room_time_budget <- d2t3_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))

### END OF TWO DAY THREE TRANS

### THREE DAYS ###

# Three Day 0 Trans

d3t0 <- read.csv("../data/test_data/three_day_zero_trans_r11.csv")

#d3t0$tagname <- d3t0$LegBand

bird_ids_d3t0 <- unique(d3t0$tagname)
bird_ids_d3t0 <- na.trim(sort(bird_ids_d3t0))

d3t0["DateTime"] <- as.POSIXct(d3t0$access, origin="1970-01-01", tz="GMT")

print("what makes up subzone col")
unique(d3t0$subzone)

d3t0$subzone[d3t0$subzone == "Bottom"] <- "bottom"
d3t0$subzone[d3t0$subzone == "Middle"] <- "middle"
d3t0$subzone[d3t0$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(d3t0$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(d3t0$DateTime))
sum(is.na(d3t0$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
d3t0$accessdate <- ymd_hms(d3t0$DateTime)

d3t0_struct <- d3t0 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d3t0_all_analysis <- d3t0_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are two zones observed
expect_equal(unique(d3t0_all_analysis$slicedTsibble[[1]]$value),c("middle"), label='d3t0 sliced tsibble valuecol')

# TODO can we delete the sampled?
d3t0_regular <- d3t0_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 


# first item to compare to day+night
d3t0_overall_interval <- d3t0_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x)))

d3t0_all_room_time_budget <- d3t0_overall_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

#TODO can compare this later but not functional right now
d3t0_overall_tb <- d3t0_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d3t0_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d3t0_overall_interval$interval[[1]],n=1)$t2),origin=origin)))

# # check overall time budget
# bot_time <- sum(d3t0$subzone == "bottom")/length(d3t0$subzone)
# mid_time <- sum(d3t0$subzone == "middle")/length(d3t0$subzone)
# top_time <- sum(d3t0$subzone == "top")/length(d3t0$subzone)
# expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(bot_time,mid_time,top_time), ncol=3)))

# # check that time budget says that it spent
# expect_equal(d3t0_overall_tb, expected_res, label='d3t0 overall time budget')

# TODO change the code to be slicedTsibble as opposed to sampled
d3t0_all_room_day <- d3t0_overall_interval |>
  mutate(day = map(slicedTsibble, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(slicedTsibble, ~ getNightRecords(.x,"05:00","22:00"))) 

# check n day records

expect_equal(length(d3t0_all_room_day$day[[1]]$day), sum(d3t0$characteristic == "day"), label='d3t0 num of day records')

# check n night records

expect_equal(length(d3t0_all_room_day$night[[1]]$day), sum(d3t0$characteristic == "night"), label='d3t0 num of night records')

# check that rejoining day and night gives you the overall table (Will break if Regmi wants to have a hour deadband for night)

expect_equal( bind_rows(d3t0_all_room_day$day[[1]],d3t0_all_room_day$night[[1]])[,1:2], d3t0_overall_interval$slicedTsibble[[1]], label='d3t0 day+night == overall')

# check range of dos

expect_equal(unique(d3t0_all_room_day$day[[1]]$dos), c(1,2,3) , label='d3t0 unique dos counts')
expect_equal(unique(d3t0_all_room_day$night[[1]]$dos), c(1,2,3) , label='d3t0 unique dos night counts')

# check range of wos

expect_equal(unique(d3t0_all_room_day$day[[1]]$wos), 1 , label='d3t0 unique wos counts')

d3t0_all_room_day <- d3t0_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))

# check 0 trans in day

n_trans <- length(d3t0_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d3t0_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 0 , label='d3t0 expect 0 trans in day')

# check 0 trans in night

n_trans <- length(d3t0_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d3t0_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 0 , label='d3t0 expect 0 trans in night')

# TODO update check start and end day

expect_equal(as.numeric(head(d3t0_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d3t0_all_room_day$day[[1]]$datetime,n=1)) , label='d3t0 expect first record t1 is beginning of day')
expect_equal(as.numeric(tail(d3t0_all_room_day$day_int[[1]]$daily_int[[3]],n=1)$t2), as.numeric(tail(d3t0_all_room_day$day[[1]]$datetime,n=1)) , label='d3t0 expect last record t2 is beginning of day')

#  TODO update check start and end night

expect_equal(as.numeric(head(d3t0_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d3t0_all_room_day$night[[1]]$datetime,n=1)) , label='d3t0 expect first record t1 is beginning of night')
expect_equal(as.numeric(tail(d3t0_all_room_day$night_int[[1]]$daily_int[[3]],n=1)$t2), as.numeric(tail(d3t0_all_room_day$night[[1]]$datetime,n=1)) , label='d3t0 expect last record t2 is beginning of night')

# check n records day+night - 1 == n records overall 

n_day_trans <- length(d3t0_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d3t0_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)+length(d3t0_all_room_day$day_int[[1]]$daily_int[[3]]$to_zone)-3
n_night_trans <- length(d3t0_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d3t0_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)+length(d3t0_all_room_day$night_int[[1]]$daily_int[[3]]$to_zone)-3

n_trans_overall <- length(d3t0_overall_interval$interval[[1]]$to_zone)-1

# Mismatch error because there are transitions at day and night
expect_equal(as.numeric(n_day_trans+n_night_trans), as.numeric(n_trans_overall) , label='d3t0 expect nTransDay+nTransNight == nTransOverall')

d3t0_all_room_time_budget <- d3t0_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))

### END OF THREE DAY ZERO TRANS

# Three Day 1 Trans

d3t1 <- read.csv("../data/test_data/three_day_one_trans_r11.csv")

#d3t1$tagname <- d3t1$LegBand

bird_ids_d3t1 <- unique(d3t1$tagname)
bird_ids_d3t1 <- na.trim(sort(bird_ids_d3t1))

d3t1["DateTime"] <- as.POSIXct(d3t1$access, origin="1970-01-01", tz="GMT")

print("what makes up subzone col")
unique(d3t1$subzone)

d3t1$subzone[d3t1$subzone == "Bottom"] <- "bottom"
d3t1$subzone[d3t1$subzone == "Middle"] <- "middle"
d3t1$subzone[d3t1$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(d3t1$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(d3t1$DateTime))
sum(is.na(d3t1$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
d3t1$accessdate <- ymd_hms(d3t1$DateTime)

d3t1_struct <- d3t1 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

d3t1_all_analysis <- d3t1_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-19 T05:00:00", "2021-05-06 T22:00:00")))

# check that there are two zones observed
expect_equal(unique(d3t1_all_analysis$slicedTsibble[[1]]$value),c("top","bottom"), label='d3t1 sliced tsibble valuecol')

# TODO can we delete the sampled?
d3t1_regular <- d3t1_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 


# first item to compare to day+night
d3t1_overall_interval <- d3t1_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x)))

d3t1_all_room_time_budget <- d3t1_overall_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

#TODO can compare this later but not functional right now
d3t1_overall_tb <- d3t1_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

# Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d3t1_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d3t1_overall_interval$interval[[1]],n=1)$t2),origin=origin)))

# # check overall time budget
# bot_time <- sum(d3t1$subzone == "bottom")/length(d3t1$subzone)
# mid_time <- sum(d3t1$subzone == "middle")/length(d3t1$subzone)
# top_time <- sum(d3t1$subzone == "top")/length(d3t1$subzone)
# expected_res <- tibble(data.frame(Interval[1],Interval[2],matrix(c(bot_time,mid_time,top_time), ncol=3)))

# # check that time budget says that it spent
# expect_equal(d3t1_overall_tb, expected_res, label='d3t1 overall time budget')

# TODO change the code to be slicedTsibble as opposed to sampled
d3t1_all_room_day <- d3t1_overall_interval |>
  mutate(day = map(slicedTsibble, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(slicedTsibble, ~ getNightRecords(.x,"05:00","22:00"))) 

# check n day records

expect_equal(length(d3t1_all_room_day$day[[1]]$day), sum(d3t1$characteristic == "day"), label='d3t1 num of day records')

# check n night records

expect_equal(length(d3t1_all_room_day$night[[1]]$day), sum(d3t1$characteristic == "night"), label='d3t1 num of night records')

# check that rejoining day and night gives you the overall table (Will break if Regmi wants to have a hour deadband for night)

expect_equal( bind_rows(d3t1_all_room_day$day[[1]],d3t1_all_room_day$night[[1]])[,1:2], d3t1_overall_interval$slicedTsibble[[1]], label='d3t1 day+night == overall')

# check range of dos

expect_equal(unique(d3t1_all_room_day$day[[1]]$dos), c(1,2,3) , label='d3t1 unique dos counts')
expect_equal(unique(d3t1_all_room_day$night[[1]]$dos), c(1,2,3) , label='d3t1 unique dos night counts')

# check range of wos

expect_equal(unique(d3t1_all_room_day$day[[1]]$wos), 1 , label='d3t1 unique wos counts')

d3t1_all_room_day <- d3t1_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))

# check 0 trans in day

n_trans <- length(d3t1_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d3t1_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 0 , label='d3t1 expect 0 trans in day')

# check 0 trans in night

n_trans <- length(d3t1_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d3t1_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)-2

expect_equal(n_trans, 0 , label='d3t1 expect 0 trans in night')

# TODO update check start and end day

expect_equal(as.numeric(head(d3t1_all_room_day$day_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d3t1_all_room_day$day[[1]]$datetime,n=1)) , label='d3t1 expect first record t1 is beginning of day')
expect_equal(as.numeric(tail(d3t1_all_room_day$day_int[[1]]$daily_int[[3]],n=1)$t2), as.numeric(tail(d3t1_all_room_day$day[[1]]$datetime,n=1)) , label='d3t1 expect last record t2 is beginning of day')

#  TODO update check start and end night

expect_equal(as.numeric(head(d3t1_all_room_day$night_int[[1]]$daily_int[[1]],n=1)$t1), as.numeric(head(d3t1_all_room_day$night[[1]]$datetime,n=1)) , label='d3t1 expect first record t1 is beginning of night')
expect_equal(as.numeric(tail(d3t1_all_room_day$night_int[[1]]$daily_int[[3]],n=1)$t2), as.numeric(tail(d3t1_all_room_day$night[[1]]$datetime,n=1)) , label='d3t1 expect last record t2 is beginning of night')

# check n records day+night - 1 == n records overall 

n_day_trans <- length(d3t1_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)+length(d3t1_all_room_day$day_int[[1]]$daily_int[[2]]$to_zone)+length(d3t1_all_room_day$day_int[[1]]$daily_int[[3]]$to_zone)-3
n_night_trans <- length(d3t1_all_room_day$night_int[[1]]$daily_int[[1]]$to_zone)+length(d3t1_all_room_day$night_int[[1]]$daily_int[[2]]$to_zone)+length(d3t1_all_room_day$night_int[[1]]$daily_int[[3]]$to_zone)-3

n_trans_overall <- length(d3t1_overall_interval$interval[[1]]$to_zone)-1

# Mismatch error because there are transitions at day and night
expect_equal(as.numeric(n_day_trans+n_night_trans), as.numeric(n_trans_overall) , label='d3t1 expect nTransDay+nTransNight == nTransOverall')

d3t1_all_room_time_budget <- d3t1_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))

### END OF THREE DAY ONE TRANS

# Three Day 2 Trans

# Three Day 3 Trans

### Duplicate entries ###

# One day one trans one dupe

# One day one trans multiple dupes