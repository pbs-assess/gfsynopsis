#!/bin/bash

cd "$(dirname "$0")" || exit 1

values=$(seq 1 123)
NUM_JOBS=6
R_SCRIPT="make.R"
parallel -j $NUM_JOBS Rscript $R_SCRIPT {} ::: $values
