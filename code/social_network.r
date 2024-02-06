#### All Room: 2,3,8,11 Social Network Analysis ####

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

rm_2_records <- Sys.glob("../intermediate/all_rooms/room_2_tsibble_*")

rm_2 <- rm_2_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")

# read in room 3 tables

rm_3_records <- Sys.glob("../intermediate/all_rooms/room_3_tsibble_*")

rm_3 <- rm_3_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")

# read in room 8 tables

rm_8_records <- Sys.glob("../intermediate/all_rooms/room_8_tsibble_*")

rm_8 <- rm_8_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")

# read in room 11 tables

rm_11_records <- Sys.glob("../intermediate/all_rooms/room_11_tsibble_*")

rm_11 <- rm_11_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")
