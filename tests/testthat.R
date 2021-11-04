# Project:   ordinality
# Objective: set up and run tests
# Author:    Edoardo Costantini
# Created:   2021-11-02
# Modified:  2021-11-02

rm(list = ls())

## Initialize the environment:
source("./init.R")

## Prepare storing results
source("./fs.R")

test_dir("../tests/testthat")
