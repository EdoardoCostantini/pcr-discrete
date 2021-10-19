### Title:    Subroutine doRep (windows parallelization framework)
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-10-19

## Make sure we have a clean environment:
rm(list = ls())

## Initialize the environment:
source("./init.R")

## Prepare storing results
source("./fs.R")

## Progress report file
dir.create(fs$outDir)
file.create(paste0(fs$outDir, fs$fileName_prog, ".txt"))

cat(paste0("SIMULATION PROGRESS REPORT",
           "\n",
           "Starts at: ", Sys.time(),
           "\n", "------", "\n" ),
    file = paste0(fs$outDir, fs$fileName_prog, ".txt"),
    sep = "\n",
    append = TRUE)

## Define repetitions and clusters
reps <- 1 : 500
clus <- makeCluster(10)

## Export to worker nodes
clusterExport(cl = clus, varlist = "settings", envir = .GlobalEnv) # export global env
clusterEvalQ(cl = clus, expr = source("./init.R")) # execute script

# mcApply parallel --------------------------------------------------------

sim_start <- Sys.time()

## Run the computations in parallel on the 'clus' object:
out <- parLapply(cl    = clus,
                 X     = reps,
                 fun   = doRep,
                 conds = conds,
                 parms = parms,
                 settings = settings)

## Kill the cluster:
stopCluster(clus)

sim_ends <- Sys.time()

cat(paste0("\n", "------", "\n",
           "Ends at: ", Sys.time(), "\n",
           "Run time: ",
           round(difftime(sim_ends, sim_start, units = "hours"), 3), " h",
           "\n", "------", "\n"),
    file = paste0(settings$outDir, settings$fileName_progress, ".txt"),
    sep = "\n",
    append = TRUE)

# Attach Extract Info Objects
out_support <- list()
out_support$parms <- parms
out_support$conds <- conds
out_support$session_info <- devtools::session_info()

# Save output -------------------------------------------------------------

saveRDS(out_support,
        paste0(settings$outDir, "sInfo.rds"))

# Zip output folder -------------------------------------------------------

writeTarGz(settings$fileName)