#!/bin/bash

values=$(seq 1 116)
NUM_JOBS=5
R_SCRIPT="make.R"
parallel -j $NUM_JOBS Rscript $R_SCRIPT {} ::: $values
