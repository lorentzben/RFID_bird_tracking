#### All Room: 2,3,8,11 Joint analysis ####

source("./rfid_functions.R")

# Generate Transition tables from Room 11
library(xts)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tsibble)

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

# # calc num trans in room 2

# # calc num trans room 3

# # calc num trans room 8

# # calc num trans room 11

# # concat all num trans tables

# # separate out low medium high activity