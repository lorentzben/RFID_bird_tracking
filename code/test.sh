#!/usr/bin/env bash

TS=`date +%Y-%m-%d_%H-%M-%S`

BASEDIR=$PWD

if [ ! -d "../logs" ]; then
    mkdir -p ../logs
fi

# Run Room 2 Individual Analysis

Rscript testRfid.r > ../logs/"$TS"_test.log