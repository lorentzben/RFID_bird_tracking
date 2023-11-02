#### All Room: 2,3,8,11 Joint analysis ####

source("./rfid_functions.R")

# Generate Transition tables from Room 11
library(xts)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tsibble)
library(testthat)

# read in room 2 tables
library(readr)
library(ggplot2)

rm_2_overall_int <- Sys.glob("../intermediate/all_rooms/overall_interval/room_2_interval_*")

rm_2_day_int <- Sys.glob("../intermediate/all_rooms/daily_interval/room_2_interval_*")

rm_2_night_int <- Sys.glob("../intermediate/all_rooms/nightly_interval/room_2_interval_*")

rm_2_overall_df <- read_csv(rm_2_overall_int)

rm_2_nest_overall_int <- rm_2_overall_df |>
  nest(data = -tagname)

rm_2_day_int_df <- read_csv(rm_2_day_int)

rm_2_nest_day_int <- rm_2_day_int_df |>
  nest(data = -tagname)

rm_2_night_int_df <- read_csv(rm_2_night_int)

rm_2_nest_night_int <- rm_2_night_int_df |>
   nest(data = - tagname)

# # read in room 3 tables

# day_tbs <- Sys.glob("../intermediate/all_rooms/room_3_day_time_budget_*")

# night_tbs <- Sys.glob("../intermediate/all_rooms/room_3_night_time_budget_*")

# day_tbs_df <- read_csv(day_tbs)

# nest_day_tbs <- day_tbs_df |>
#   nest(data = -tagname)

# night_tbs_df <- read_csv(night_tbs)

# nest_night_tbs <- night_tbs_df |>
#    nest(data = - tagname)

# # read in room 8 tables

# day_tbs <- Sys.glob("../intermediate/all_rooms/room_8_day_time_budget_*")

# night_tbs <- Sys.glob("../intermediate/all_rooms/room_8_night_time_budget_*")

# day_tbs_df <- read_csv(day_tbs)

# nest_day_tbs <- day_tbs_df |>
#   nest(data = -tagname)

# night_tbs_df <- read_csv(night_tbs)

# nest_night_tbs <- night_tbs_df |>
#    nest(data = - tagname)

# # read in room 11 tables

# day_tbs <- Sys.glob("../intermediate/all_rooms/room_11_day_time_budget_*")

# night_tbs <- Sys.glob("../intermediate/all_rooms/room_11_night_time_budget_*")

# day_tbs_df <- read_csv(day_tbs)

# nest_day_tbs <- day_tbs_df |>
#   nest(data = -tagname)

# night_tbs_df <- read_csv(night_tbs)

# nest_night_tbs <- night_tbs_df |>
#    nest(data = - tagname)

# calc num trans in room 2

rm_2_overall <- rm_2_nest_overall_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('overall summary: '))
print(summary(rm_2_overall$ntrans))

rm_2_day <- rm_2_nest_day_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('day summary: '))
print(summary(rm_2_day$ntrans))


rm_2_night <- rm_2_nest_night_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('night summary: '))
print(summary(rm_2_night$ntrans))

summary_rm_2 <- summary(rm_2_overall$ntrans)

l2m <- summary_rm_2[2]

m2h <- summary_rm_2[5]

rm_2_low_act <- rm_2_overall[rm_2_overall$ntrans < l2m,]
rm_2_low_act$activity <- rep("low",length(rm_2_low_act$ntrans))

rm_2_med_act <- rm_2_overall[(l2m < rm_2_overall$ntrans) & (rm_2_overall$ntrans <= m2h),]
rm_2_med_act$activity <- rep("medium",length(rm_2_med_act$ntrans))

rm_2_high_act <- rm_2_overall[(rm_2_overall$ntrans > m2h),]
rm_2_high_act$activity <- rep("high",length(rm_2_high_act$ntrans))

rm_2_org_overall <- bind_rows(rm_2_low_act,rm_2_med_act, rm_2_high_act)

expect_equal(length(rm_2_org_overall$ntrans), length(rm_2_overall$ntrans))



# # calc num trans room 3

rm_3_overall_int <- Sys.glob("../intermediate/all_rooms/overall_interval/room_3_interval_*")

rm_3_day_int <- Sys.glob("../intermediate/all_rooms/daily_interval/room_3_interval_*")

rm_3_night_int <- Sys.glob("../intermediate/all_rooms/nightly_interval/room_3_interval_*")

rm_3_overall_df <- read_csv(rm_3_overall_int)

rm_3_nest_overall_int <- rm_3_overall_df |>
  nest(data = -tagname)

rm_3_day_int_df <- read_csv(rm_3_day_int)

rm_3_nest_day_int <- rm_3_day_int_df |>
  nest(data = -tagname)

rm_3_night_int_df <- read_csv(rm_3_night_int)

rm_3_nest_night_int <- rm_3_night_int_df |>
   nest(data = - tagname)

rm_3_overall <- rm_3_nest_overall_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('overall summary: '))
print(summary(rm_3_overall$ntrans))

rm_3_day <- rm_3_nest_day_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('day summary: '))
print(summary(rm_3_day$ntrans))


rm_3_night <- rm_3_nest_night_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('night summary: '))
print(summary(rm_3_night$ntrans))

summary_rm_3 <- summary(rm_3_overall$ntrans)

l2m <- summary_rm_3[2]

m2h <- summary_rm_3[5]

rm_3_low_act <- rm_3_overall[rm_3_overall$ntrans < l2m,]
rm_3_low_act$activity <- rep("low",length(rm_3_low_act$ntrans))

rm_3_med_act <- rm_3_overall[(l2m < rm_3_overall$ntrans) & (rm_3_overall$ntrans <= m2h),]
rm_3_med_act$activity <- rep("medium",length(rm_3_med_act$ntrans))

rm_3_high_act <- rm_3_overall[(rm_3_overall$ntrans > m2h),]
rm_3_high_act$activity <- rep("high",length(rm_3_high_act$ntrans))

rm_3_org_overall <- bind_rows(rm_3_low_act,rm_3_med_act, rm_3_high_act)

expect_equal(length(rm_3_org_overall$ntrans), length(rm_3_overall$ntrans))

# # calc num trans room 8

rm_8_overall_int <- Sys.glob("../intermediate/all_rooms/overall_interval/room_8_interval_*")

rm_8_day_int <- Sys.glob("../intermediate/all_rooms/daily_interval/room_8_interval_*")

rm_8_night_int <- Sys.glob("../intermediate/all_rooms/nightly_interval/room_8_interval_*")

rm_8_overall_df <- read_csv(rm_8_overall_int)

rm_8_nest_overall_int <- rm_8_overall_df |>
  nest(data = -tagname)

rm_8_day_int_df <- read_csv(rm_8_day_int)

rm_8_nest_day_int <- rm_8_day_int_df |>
  nest(data = -tagname)

rm_8_night_int_df <- read_csv(rm_8_night_int)

rm_8_nest_night_int <- rm_8_night_int_df |>
   nest(data = - tagname)

rm_8_overall <- rm_8_nest_overall_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('overall summary: '))
print(summary(rm_8_overall$ntrans))

rm_8_day <- rm_8_nest_day_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('day summary: '))
print(summary(rm_8_day$ntrans))


rm_8_night <- rm_8_nest_night_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('night summary: '))
print(summary(rm_8_night$ntrans))

summary_rm_8 <- summary(rm_8_overall$ntrans)

l2m <- summary_rm_8[2]

m2h <- summary_rm_8[5]

rm_8_low_act <- rm_8_overall[rm_8_overall$ntrans < l2m,]
rm_8_low_act$activity <- rep("low",length(rm_8_low_act$ntrans))

rm_8_med_act <- rm_8_overall[(l2m < rm_8_overall$ntrans) & (rm_8_overall$ntrans <= m2h),]
rm_8_med_act$activity <- rep("medium",length(rm_8_med_act$ntrans))

rm_8_high_act <- rm_8_overall[(rm_8_overall$ntrans > m2h),]
rm_8_high_act$activity <- rep("high",length(rm_8_high_act$ntrans))

rm_8_org_overall <- bind_rows(rm_8_low_act,rm_8_med_act, rm_8_high_act)

expect_equal(length(rm_8_org_overall$ntrans), length(rm_8_overall$ntrans))

# # calc num trans room 11

rm_11_overall_int <- Sys.glob("../intermediate/all_rooms/overall_interval/room_11_interval_*")

rm_11_day_int <- Sys.glob("../intermediate/all_rooms/daily_interval/room_11_interval_*")

rm_11_night_int <- Sys.glob("../intermediate/all_rooms/nightly_interval/room_11_interval_*")

rm_11_overall_df <- read_csv(rm_11_overall_int)

rm_11_nest_overall_int <- rm_11_overall_df |>
  nest(data = -tagname)

rm_11_day_int_df <- read_csv(rm_11_day_int)

rm_11_nest_day_int <- rm_11_day_int_df |>
  nest(data = -tagname)

rm_11_night_int_df <- read_csv(rm_11_night_int)

rm_11_nest_night_int <- rm_11_night_int_df |>
   nest(data = - tagname)

rm_11_overall <- rm_11_nest_overall_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('overall summary: '))
print(summary(rm_11_overall$ntrans))

rm_11_day <- rm_11_nest_day_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('day summary: '))
print(summary(rm_11_day$ntrans))


rm_11_night <- rm_11_nest_night_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('night summary: '))
print(summary(rm_11_night$ntrans))

summary_rm_11 <- summary(rm_11_overall$ntrans)

l2m <- summary_rm_11[2]

m2h <- summary_rm_11[5]

rm_11_low_act <- rm_11_overall[rm_11_overall$ntrans < l2m,]
rm_11_low_act$activity <- rep("low",length(rm_11_low_act$ntrans))

rm_11_med_act <- rm_11_overall[(l2m < rm_11_overall$ntrans) & (rm_11_overall$ntrans <= m2h),]
rm_11_med_act$activity <- rep("medium",length(rm_11_med_act$ntrans))

rm_11_high_act <- rm_11_overall[(rm_11_overall$ntrans > m2h),]
rm_11_high_act$activity <- rep("high",length(rm_11_high_act$ntrans))

rm_11_org_overall <- bind_rows(rm_11_low_act,rm_11_med_act, rm_11_high_act)

expect_equal(length(rm_11_org_overall$ntrans), length(rm_11_overall$ntrans))

# # concat all num trans tables

rm_2_overall$rm <- 2

rm_3_overall$rm <- 3

rm_8_overall$rm <- 8

rm_11_overall$rm <- 11

overall_table <- bind_rows(rm_2_overall, rm_3_overall, rm_8_overall, rm_11_overall)


# # separate out low medium high activity

summary_overall <- summary(overall_table$ntrans)

l2m <- summary_overall[2]

m2h <- summary_overall[5]

overall_low_act <- overall_table[overall_table$ntrans < l2m,]
overall_low_act$activity <- rep("low",length(overall_low_act$ntrans))

overall_med_act <- overall_table[(l2m < overall_table$ntrans) & (overall_table$ntrans <= m2h),]
overall_med_act$activity <- rep("medium",length(overall_med_act$ntrans))

overall_high_act <- overall_table[(overall_table$ntrans > m2h),]
overall_high_act$activity <- rep("high",length(overall_high_act$ntrans))

overall_org_table <- bind_rows(overall_low_act,overall_med_act, overall_high_act)

overall_org_table <- overall_org_table[order(overall_org_table$ntrans),]