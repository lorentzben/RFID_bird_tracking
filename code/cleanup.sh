#!/usr/bin/env bash

BASEDIR=$PWD

echo $BASEDIR

# remove figure subdirs 

if [ -d "../figures/all_day" ]; then
    rm -rf ../figures/all_day
    rm -rf ../figures/all_day/room2
    rm -rf ../figures/all_day/room3
    rm -rf ../figures/all_day/room8
    rm -rf ../figures/all_day/room11
    rm -rf ../figures/all_day/all_rooms
fi


# remove intermediate dir

if [ -d "../intermediate" ]; then
    rm -rf ../intermediate/all_rooms
    rm -rf ../intermediate/no_room_3
    rm -rf ../intermediate/all_rooms/overall_interval
    rm -rf ../intermediate/all_rooms/daily_interval
    rm -rf ../intermediate/all_rooms/nightly_interval
fi

# remove output dir

if [ -d "../output" ]; then
    rm -rf ../output/all_rooms
    rm -rf ../output/no_room_3
fi