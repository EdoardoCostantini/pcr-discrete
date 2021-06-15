### Title:    Defining Settings for a run
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-15
### Modified: 2021-06-15

# Empty List
settings    <- list()

# File System
settings$start_time <- format(Sys.time(), "%Y%d%m_%H%M%S")
settings$outDir <- paste0("../output/",
                          settings$start_time,
                          "/")
dir.create(settings$outDir)
settings$fileName <- settings$start_time
settings$fileName_progress <- "progress"
