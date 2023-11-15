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

### END Activity classification with overall table ###


### Activity Classification of room 2 ###

rm_2 <- overall_table[overall_table$rm == 2,]

summary_rm_2 <- summary(rm_2$ntrans)

l2m <- summary_rm_2[2]

m2h <- summary_rm_2[5]

rm_2_low_act <- rm_2[rm_2$ntrans <= l2m,]
rm_2_low_act$activity <- rep("low",length(rm_2_low_act$ntrans))

rm_2_med_act <- rm_2[(l2m < rm_2$ntrans) & (rm_2$ntrans <= m2h),]
rm_2_med_act$activity <- rep("medium",length(rm_2_med_act$ntrans))

rm_2_high_act <- rm_2[(rm_2$ntrans > m2h),]
rm_2_high_act$activity <- rep("high",length(rm_2_high_act$ntrans))

rm_2_org_table <- bind_rows(rm_2_low_act,rm_2_med_act, rm_2_high_act)

rm_2_org_table <- rm_2_org_table[order(rm_2_org_table$ntrans),]

# select high time budgets based on ids

high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_2_high_act$tagname,]

high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_2_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_2_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the low activity birds nest at night: ")
(sort(table(low_act_nest$nest),decreasing=T))

### END Activity Class of room 2 ###

### Activity Classification of room 3 ###

rm_3 <- overall_table[overall_table$rm == 3,]

summary_rm_3 <- summary(rm_3$ntrans)

l2m <- summary_rm_3[2]

m2h <- summary_rm_3[5]

rm_3_low_act <- rm_3[rm_3$ntrans <= l2m,]
rm_3_low_act$activity <- rep("low",length(rm_3_low_act$ntrans))

rm_3_med_act <- rm_3[(l2m < rm_3$ntrans) & (rm_3$ntrans <= m2h),]
rm_3_med_act$activity <- rep("medium",length(rm_3_med_act$ntrans))

rm_3_high_act <- rm_3[(rm_3$ntrans > m2h),]
rm_3_high_act$activity <- rep("high",length(rm_3_high_act$ntrans))

rm_3_org_table <- bind_rows(rm_3_low_act,rm_3_med_act, rm_3_high_act)

rm_3_org_table <- rm_3_org_table[order(rm_3_org_table$ntrans),]

# select high time budgets based on ids

high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_3_high_act$tagname,]

high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_3_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_3_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the low activity birds nest at night: ")
(sort(table(low_act_nest$nest),decreasing=T))

### END Activity Class of room 3 ###

### Activity Classification of room 8 ###

rm_8 <- overall_table[overall_table$rm == 8,]

summary_rm_8 <- summary(rm_8$ntrans)

l2m <- summary_rm_8[2]

m2h <- summary_rm_8[5]

rm_8_low_act <- rm_8[rm_8$ntrans <= l2m,]
rm_8_low_act$activity <- rep("low",length(rm_8_low_act$ntrans))

rm_8_med_act <- rm_8[(l2m < rm_8$ntrans) & (rm_8$ntrans <= m2h),]
rm_8_med_act$activity <- rep("medium",length(rm_8_med_act$ntrans))

rm_8_high_act <- rm_8[(rm_8$ntrans > m2h),]
rm_8_high_act$activity <- rep("high",length(rm_8_high_act$ntrans))

rm_8_org_table <- bind_rows(rm_8_low_act,rm_8_med_act, rm_8_high_act)

rm_8_org_table <- rm_8_org_table[order(rm_8_org_table$ntrans),]

# select high time budgets based on ids

high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_8_high_act$tagname,]

high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_8_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_8_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the low activity birds nest at night: ")
(sort(table(low_act_nest$nest),decreasing=T))

### END Activity Class of room 8 ###

### Activity Classification of room 11 ###

rm_11 <- overall_table[overall_table$rm == 11,]

summary_rm_11 <- summary(rm_11$ntrans)

l2m <- summary_rm_11[2]

m2h <- summary_rm_11[5]

rm_11_low_act <- rm_11[rm_11$ntrans <= l2m,]
rm_11_low_act$activity <- rep("low",length(rm_11_low_act$ntrans))

rm_11_med_act <- rm_11[(l2m < rm_11$ntrans) & (rm_11$ntrans <= m2h),]
rm_11_med_act$activity <- rep("medium",length(rm_11_med_act$ntrans))

rm_11_high_act <- rm_11[(rm_11$ntrans > m2h),]
rm_11_high_act$activity <- rep("high",length(rm_11_high_act$ntrans))

rm_11_org_table <- bind_rows(rm_11_low_act,rm_11_med_act, rm_11_high_act)

rm_11_org_table <- rm_11_org_table[order(rm_11_org_table$ntrans),]

# select high time budgets based on ids

high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_11_high_act$tagname,]

high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_11_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_11_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the low activity birds nest at night: ")
(sort(table(low_act_nest$nest),decreasing=T))

### END Activity Class of room 11 ###

### END Activity Class for each room ### 



### does the weekly time budget differ from feb to april ### 

library(emmeans)
library(lme4)
library(lmerTest)

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

overall_day_summary <- cbind(day_bottom_sum,day_middle_sum[,3],day_top_sum[,3])
overall_day_summary <- data.frame(merge(overall_day_summary,overall_org_table[,c(1,4)], by="tagname"))
overall_day_summary$activity <- factor(overall_day_summary$activity, levels=c("low","medium","high"))
overall_day_summary$weekFac <- factor(overall_day_summary$week)
(unique(day_tb_df$interval1))
(unique(day_tb_df$interval2))

m1 <- lmer(bottom_mean ~ weekFac + activity + weekFac:activity + (1|tagname), overall_day_summary)
summary(m1)
anova(m1)
m1.res <- resid(m1)

# generate and save residual plot of model to check assumptions

png("../figures/all_day/model_diag/bottom_mean_resid.png")
plot(fitted(m1),m1.res)
abline(0,0)
dev.off()

# generate and save Q-Q normal plot to check assumptions

png("../figures/all_day/model_diag/bottom_mean_qq.png")
qqnorm(m1.res)
qqline(m1.res)
dev.off()


# Get estimations of bottom time spent

m1.bottom.means <- emmeans(m1, specs=list(weekMeans = ~weekFac,
actMeans = ~activity,
jointMeans=~weekFac:activity))

 
# interaction plot of week on x
png("../figures/all_day/model_diag/bottom_interaction_act_week.png")
emmip(m1.bottom.means$jointMeans, activity~weekFac)
dev.off()

# interaction plot of activity on x
png("../figures/all_day/model_diag/bottom_interaction_week_act.png")
emmip(m1.bottom.means$jointMeans, weekFac~activity)
dev.off()


# Some evidence of interaction weeks 12 to 16 flip flop

contrast(m1.bottom.means$jointMeans, method=list(
  low.vs.medHigh = c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18,-1/18),
  med.vs.high = c(0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1),
  first.vs.last = c(1,0,0,0,0,0,0,0,-1,1,0,0,0,0,0,0,0,-1,1,0,0,0,0,0,0,0,-1)), adjust="bonferroni")

# can we choose a slice of does low activity etc have linear or non-linear 
orthPolyCon <- contr.poly(9)

# bottom low activity linear effect
contrast(m1.bottom.means$jointMeans,simple='weekFac',"poly")[1]

# bottom low activity non-linear effect
test(contrast(m1.bottom.means$jointMeans,simple='weekFac',"poly")[2:6],joint=TRUE)

# bottom med activity linear effect
contrast(m1.bottom.means$jointMeans,simple='weekFac',"poly")[7]

# bottom med activity non-linear effect
test(contrast(m1.bottom.means$jointMeans,simple='weekFac',"poly")[8:12],joint=TRUE)

# bottom high activity linear effect
contrast(m1.bottom.means$jointMeans,simple='weekFac',"poly")[13]

# bottom high activity non-linear effect 
test(contrast(m1.bottom.means$jointMeans,simple='weekFac',"poly")[13:17],joint=TRUE)



# linear vs non-linear inside low activity birds
cl1 <- c(orthPolyCon[,1],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
cl2 <- c(orthPolyCon[,2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
cl3 <- c(orthPolyCon[,3],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
cl4 <- c(orthPolyCon[,4],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
cl5 <- c(orthPolyCon[,5],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
cl6 <- c(orthPolyCon[,6],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
cl7 <- c(orthPolyCon[,7],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
cl8 <- c(orthPolyCon[,8],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

# linear vs non-linear inside low activity birds
cm1 <- c(0,0,0,0,0,0,0,0,0,orthPolyCon[,1],0,0,0,0,0,0,0,0,0)
cm2 <- c(0,0,0,0,0,0,0,0,0,orthPolyCon[,2],0,0,0,0,0,0,0,0,0)
cm3 <- c(0,0,0,0,0,0,0,0,0,orthPolyCon[,3],0,0,0,0,0,0,0,0,0)
cm4 <- c(0,0,0,0,0,0,0,0,0,orthPolyCon[,4],0,0,0,0,0,0,0,0,0)
cm5 <- c(0,0,0,0,0,0,0,0,0,orthPolyCon[,5],0,0,0,0,0,0,0,0,0)
cm6 <- c(0,0,0,0,0,0,0,0,0,orthPolyCon[,6],0,0,0,0,0,0,0,0,0)
cm7 <- c(0,0,0,0,0,0,0,0,0,orthPolyCon[,7],0,0,0,0,0,0,0,0,0)
cm8 <- c(0,0,0,0,0,0,0,0,0,orthPolyCon[,8],0,0,0,0,0,0,0,0,0)

# linear vs non-linear inside low activity birds
ch1 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,orthPolyCon[,1])
ch2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,orthPolyCon[,2])
ch3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,orthPolyCon[,3])
ch4 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,orthPolyCon[,4])
ch5 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,orthPolyCon[,5])
ch6 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,orthPolyCon[,6])
ch7 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,orthPolyCon[,7])
ch8 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,orthPolyCon[,8])

contrast(m1.bottom.means$jointMeans, method=list(
  low.lin=cl1))

test(contrast(m1.bottom.means$jointMeans, method=list(
  low.2=cl2,
  low.3=cl3,
  low.4=cl4,
  low.5=cl5,
  low.6=cl6,
  low.7=cl7,
  low.8=cl8)),joint=T)



m2 <- lmer(middle_mean ~ week + activity + week:activity + (1|tagname), overall_day_summary)
summary(m2)
anova(m2)
m2.res <- resid(m2)


# generate and save residual plot of model to check assumptions

png("../figures/all_day/model_diag/bottom_mean_resid.png")
plot(fitted(m1),m1.res)
abline(0,0)
dev.off()

# generate and save Q-Q normal plot to check assumptions

png("../figures/all_day/model_diag/bottom_mean_qq.png")
qqnorm(m1.res)
qqline(m1.res)
dev.off()

# Get estimations of bottom time spent

m1.bottom.means <- emmeans(m1, specs=~week)

m3 <- lmer(top_mean ~ week + activity + week:activity + (1|tagname), overall_day_summary)
summary(m3)
anova(m3)


m1 <- aov(bottom_mean ~ activity + week + activity:week + (1|tagname), overall_day_summary)

### End weekly time budget differ march to april ###

### Does Keel Score differ based on activity level and week ###

keel_score <- read_csv('../data/keel_score/rfid_keel_scores_old_classification.csv')

### End does Keel Score Differ based on activity level and week ###