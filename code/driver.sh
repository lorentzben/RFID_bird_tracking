#!/usr/bin/env bash

#SBATCH --partition=batch
#SBATCH --job-name=RFID
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --nodes=1
#SBATCH --time=96:00:00
#SBATCH --mem=16gb

#Replace this with your UGA email to get notified on completion
#SBATCH --mail-user="bjl34716@uga.edu"
#SBATCH --mail-type=BEGIN,END

cd /scratch/bjl34716/RFID_bird_tracking/code

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
    mkdir -p ../figures/all_day/model_diag
    mkdir -p ../figures/all_day/transition_plots
fi


# make intermediate dir

if [ ! -d "../intermediate/all_rooms" ]; then
    mkdir -p ../intermediate/all_rooms
    mkdir -p ../intermediate/no_room_3
    mkdir -p ../intermediate/all_rooms/overall_interval
    mkdir -p ../intermediate/all_rooms/daily_interval
    mkdir -p ../intermediate/all_rooms/nightly_interval
fi

# make output dir

if [ ! -d "../output/all_rooms" ]; then
    mkdir -p ../output/all_rooms
    mkdir -p ../output/no_room_3
fi

# # Run Room 2 Individual Analysis

# singularity run docker://lorentzb/rfid:2.2 Rscript room_2_analysis.r > ../logs/"$TS"_rm2.log

# # Run Room 3 Individual Analysis 

# singularity run docker://lorentzb/rfid:2.2 Rscript room_3_analysis.r > ../logs/"$TS"_rm3.log

# # Run Room 8 Individual Analysis

# singularity run docker://lorentzb/rfid:2.2 Rscript room_8_analysis.r > ../logs/"$TS"_rm8.log

# # Run Room 11 Individual Analysis

# singularity run docker://lorentzb/rfid:2.2 Rscript room_11_analysis.r > ../logs/"$TS"_rm11.log

# # Run All Room Joint Analysis

# singularity run docker://lorentzb/rfid:2.2 Rscript joint_analysis.r > ../logs/"$TS"_joint.log

# Run Social Network Analysis

singularity run docker://lorentzb/rfid:2.2 Rscript social_network.r > ../logs/"$TS"_network.log

echo "Script finished"
