#!/usr/bin/env bash

TS=`date +%Y-%m-%d_%H-%M-%S`

BASEDIR=$PWD

echo $BASEDIR

if [ ! -d "../logs" ]; then
    mkdir -p ../logs
fi

# make figure subdirs 

if [ ! -d "../figures/all_day" ]; then
    mkdir -p ../figures/all_day
    mkdir -p ../figures/all_day/room2
    mkdir -p ../figures/all_day/room3
    mkdir -p ../figures/all_day/room8
    mkdir -p ../figures/all_day/room11
    mkdir -p ../figures/all_day/all_rooms
fi


# make intermediate dir

if [ ! -d "../intermediate/all_rooms" ]; then
    mkdir -p ../intermediate/all_rooms
    mkdir -p ../intermediate/no_room_3
fi

# make output dir

if [ ! -d "../output/all_rooms" ]; then
    mkdir -p ../output/all_rooms
    mkdir -p ../output/no_room_3
fi

# # Run Room 2 Individual Analysis

# Rscript room_2_analysis.r > ../logs/"$TS"_rm2.log

# # Run Room 3 Individual Analysis 

# Rscript room_3_analysis.r > ../logs/"$TS"_rm3.log

# Run Room 8 Individual Analysis

Rscript room_8_analysis.r > ../logs/"$TS"_rm8.log

# # Run Room 11 Individual Analysis

# Rscript room_11_analysis.r > ../logs/"$TS"_rm11.log

# # Run All Room Joint Analysis

# Rscript joint_analysis.r > ../logs/"$TS"_joint.log

echo "Script finished"
