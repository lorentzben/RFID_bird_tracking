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

rm_2_low_act <- rm_2_overall[rm_2_overall$ntrans <= l2m,]
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

rm_3_low_act <- rm_3_overall[rm_3_overall$ntrans <= l2m,]
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

rm_8_low_act <- rm_8_overall[rm_8_overall$ntrans <= l2m,]
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

rm_11_low_act <- rm_11_overall[rm_11_overall$ntrans <= l2m,]
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

overall_low_act <- overall_table[overall_table$ntrans <= l2m,]
overall_low_act$activity <- rep("low",length(overall_low_act$ntrans))

overall_med_act <- overall_table[(l2m < overall_table$ntrans) & (overall_table$ntrans <= m2h),]
overall_med_act$activity <- rep("medium",length(overall_med_act$ntrans))

overall_high_act <- overall_table[(overall_table$ntrans > m2h),]
overall_high_act$activity <- rep("high",length(overall_high_act$ntrans))

overall_org_table <- bind_rows(overall_low_act,overall_med_act, overall_high_act)

overall_org_table <- overall_org_table[order(overall_org_table$ntrans),]

### Compare overall low med high activity to day time classification ###

rm_2_day$rm <- 2

rm_3_day$rm <- 3

rm_8_day$rm <- 8

rm_11_day$rm <- 11

day_table <- bind_rows(rm_2_day, rm_3_day, rm_8_day, rm_11_day)

summary_day <- summary(day_table$ntrans)

l2m <- summary_day[2]

m2h <- summary_day[5]

day_low_act <- day_table[day_table$ntrans <= l2m,]
day_low_act$activity <- rep("low",length(day_low_act$ntrans))

day_med_act <- day_table[(l2m < day_table$ntrans) & (day_table$ntrans <= m2h),]
day_med_act$activity <- rep("medium",length(day_med_act$ntrans))

day_high_act <- day_table[(day_table$ntrans > m2h),]
day_high_act$activity <- rep("high",length(day_high_act$ntrans))

day_org_table <- bind_rows(day_low_act,day_med_act, day_high_act)

day_org_table <- day_org_table[order(day_org_table$ntrans),]

sorted_overall_org <- overall_org_table[order(overall_org_table$activity, overall_org_table$tagname),]

sorted_day_org <- day_org_table[order(day_org_table$activity, day_org_table$tagname),]

# Differences

(diff <- setdiff( sorted_overall_org[,c(1,4)], sorted_day_org[,c(1,4)]))
same <- intersect( sorted_overall_org[,c(1,4)], sorted_day_org[,c(1,4)])

# where they are in overall
(overall_org_table[overall_org_table$tagname %in% diff$tagname,])
# where they are in day
(day_org_table[day_org_table$tagname %in% diff$tagname,])

### Compare overall low med high act to night time classification ###

rm_2_night$rm <- 2

rm_3_night$rm <- 3

rm_8_night$rm <- 8

rm_11_night$rm <- 11

night_table <- bind_rows(rm_2_night, rm_3_night, rm_8_night, rm_11_night)

summary_night <- summary(night_table$ntrans)

l2m <- summary_night[2]

m2h <- summary_night[5]

night_low_act <- night_table[night_table$ntrans <= l2m,]
night_low_act$activity <- rep("low",length(night_low_act$ntrans))

night_med_act <- night_table[(l2m < night_table$ntrans) & (night_table$ntrans <= m2h),]
night_med_act$activity <- rep("medium",length(night_med_act$ntrans))

night_high_act <- night_table[(night_table$ntrans > m2h),]
night_high_act$activity <- rep("high",length(night_high_act$ntrans))

night_org_table <- bind_rows(night_low_act,night_med_act, night_high_act)

night_org_table <- night_org_table[order(night_org_table$ntrans),]

sorted_overall_org <- overall_org_table[order(overall_org_table$activity, overall_org_table$tagname),]

sorted_night_org <- night_org_table[order(night_org_table$activity, night_org_table$tagname),]

# Differences

(diff <- setdiff( sorted_overall_org[,c(1,4)], sorted_night_org[,c(1,4)]))
same <- intersect( sorted_overall_org[,c(1,4)], sorted_night_org[,c(1,4)])

# where they are in overall
(overall_org_table[overall_org_table$tagname %in% diff$tagname,])
# where they are in day
(night_org_table[night_org_table$tagname %in% diff$tagname,])


### Where do the high low medium birds nest at night? ### 

# read in time budget tables

rm_2_day_tb <- Sys.glob("../intermediate/all_rooms/room_2_day_time_budget_*")
rm_2_night_tb <- Sys.glob("../intermediate/all_rooms/room_2_night_time_budget_*")

rm_3_day_tb <- Sys.glob("../intermediate/all_rooms/room_3_day_time_budget_*")
rm_3_night_tb <- Sys.glob("../intermediate/all_rooms/room_3_night_time_budget_*")

rm_8_day_tb <- Sys.glob("../intermediate/all_rooms/room_8_day_time_budget_*")
rm_8_night_tb <- Sys.glob("../intermediate/all_rooms/room_8_night_time_budget_*")

rm_11_day_tb <- Sys.glob("../intermediate/all_rooms/room_11_day_time_budget_*")
rm_11_night_tb <- Sys.glob("../intermediate/all_rooms/room_11_night_time_budget_*")

day_tb_df <- read_csv(c(rm_2_day_tb, rm_3_day_tb, rm_8_day_tb, rm_11_day_tb))

nest_day_tb <- day_tb_df |>
  nest(data = -tagname)

night_tb_df <- read_csv(c(rm_2_night_tb, rm_3_night_tb, rm_8_night_tb, rm_11_night_tb))

nest_night_tb <- night_tb_df |>
   nest(data = - tagname)

# select high activity birds
overall_high <- overall_org_table[overall_org_table$activity == "high",]

# select high time budgets based on ids

high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_high$tagname,]

high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))

# select med activity birds
overall_med <- overall_org_table[overall_org_table$activity == "medium",]

# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_med$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))

# select low activity birds
overall_low <- overall_org_table[overall_org_table$activity == "low",]

# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_low$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the low activity birds nest at night: ")
(sort(table(low_act_nest$nest),decreasing=T))

### does the weekly time budget differ from feb to april ### 

day_tb_df$week <- week(day_tb_df$interval1)

day_bottom_sum <- day_tb_df |>
  group_by(tagname,week) |>
  summarize(bottom_mean = mean(Bottom)) 

day_middle_sum <- day_tb_df |>
  group_by(tagname,week) |>
  summarize(middle_mean = mean(Middle)) 

day_top_sum <- day_tb_df |>
  group_by(tagname,week) |>
  summarize(top_mean = mean(Top))

(overall_day_summary <- cbind(day_bottom_sum,day_middle_sum[,3],day_top_sum[,3]))
(unique(day_tb_df$interval1))
(unique(day_tb_df$interval2))
