# MI-PCR Comparison
Simulation study to compare the out-of-sample prediction performance of Principal Component Regression 
with discrete data as input data.
Detailed notes on the project goal, set up, and results can be found 
[here](https://lavish-hollyhock-981.notion.site/PCR-with-discrete-data-ed2f8dac46a7446b9e9fac5aed9aa99b).

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

### 2. Running the simulation
- Open the script `code/run_sim.R` and set your working directory to its location
- Define the number of desired repetitions by changing the parameter `reps`
- Define the number of clusters for parallelization by changing the parameter `clus`
- Run the script `code/run_sim.R`

### 3. Getting the results
- Open the script `code/script_pooling.R` and run it to pool the results. 
  Pay attention that the script is reading the latest file saved in the 
  output folder.
- Open the script `code/script_analysis.R` to obtain the plots

# Understanding the codebase
If you want to play around with this simulation study and 
include conditions of your liking keep in mind the following simulation structure:
- Fixed and experimental factors are provided exclusively by in the
  `init.R`.
- `run_sim.R` is a script that runs in parallel different calls of 
  the subroutine `doRep()` (located: `code/subroutines/doRep.R()`).
  The script calls one instance of `doRep()` for every repetition 
  desired. A *repetition* here is a cycle through all the conditions.
- `doRep()` is a subroutine that calls `runCell()` for every condition 
  in a sequential loop. 
  In this set up, parallelization happens at the level of the repetitions,
  not at the level of the conditions.
- `runCell()` is a subroutine calling a collection of functions to
  actually perform the steps of the simulation:
  1. Generation of X - `generateXTP()` generates the true principal components (T) 
     and observed items (X)
  2. Generation of y - `generateDV()` generates the dependent variable as a linear 
     combination of the true components
  3. Discretization of (part of) X - `disData()` discretizes a condition specific 
     proportion of variables in X
  4. Preparation of different version of X - Disjunction table and dummy coded representations
     of X are created
  5. PC extraction - a collection of `extractPC**()` functions performs PCA according to the 
     various approaches
  6. Outcome measures are computed - `extractMSE()` the MSE and other desired outcomes are
     extracted from previously created objects
  7. Storing results - An object containing the outcome measures is stored as .rds file 
     at every repetition for every condition in a temporary output folder.