# MI-PCR Comparison
Simulation study to compare the out-of-sample prediction performance of Principal Component Regression 
with discrete data as input data. 

# Repository structure
This directory contains the following main subfolders:
- code: the main software to run the study
  - functions
  - helper
  - plots
  - subroutines
  - main R scripts
- graphs: folder to store plots
- output: folder where the results of scripts located in code are stored
- tests: folder to store testing scripts

# How to replicate results

## Running the simulation on a PC / Mac

You can also replicate the simulation on a personal computer by following these steps: 

### 1. Preparation
- Check the script `code/init.R` - 
  This script contains the information of how everything in the simulation study is set up. 
  It stores the definition of the fixed and experimental factors. Before doing anything else,
  check that every value here is what you want it to be.
- Run `generateBreaks.R` - This scripts creates a batch of breaks to be used in discretizing 
  data in the ordinal scale conditions (the ones for which `interval = FALSE`). This step can
  be done skipped if you already have the `breaks_batch.rds` file in the input folder. 

### 2. Running the simulation
- Open the script `code/run_sim.R`
- Define the number of desired repetitions by changing the parameter `reps`
- Define the number of clusters for parallelization by changing the parameter `clus`
- Run the script `code/run_sim.R`
- results are then pooled by the `code/script_pooling.R` script.