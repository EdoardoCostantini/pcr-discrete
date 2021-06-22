### Title:    Pooling results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-21

  ## Make sure we have a clean environment:
  rm(list = ls(all = TRUE))

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
<<<<<<< HEAD
  runName <- "20212206_093452"
=======
  runName <- "20212106_181226"
>>>>>>> 7c9b6c1afe71f46bc94b40448496daa03243a412
  subDir <- paste0(inDir, runName, "/")
  sInfo <- readRDS(paste0(inDir, runName, "/sInfo.rds"))

  # Define filenames
  allrds <- grep(".rds",
                 list.files(subDir),
                 value = TRUE)
  fileNames <- allrds[!grepl("sInfo", allrds)]

  # Read all of them in
  out <- lapply(paste0(subDir, fileNames), readRDS)

  # Extract last element as session info object
  sInfo <- readRDS(paste0(inDir, runName, "/sInfo.rds"))

  # Support Functions
  source("./helper/functions.R")

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

  # Give unique name to all objects
  names(out) <- fileNames

  # Define the Unique repetitions
  reps <- sInfo$parms$dt_rep # you need to input this manually right now
  cond_tag <- sInfo$conds$tag

  # Create an index based on the repetition membership
  index <- lapply(cond_tag,
                  function(x) {
                    grep(x, names(out), value = TRUE)
                  }
  )
  names(index) <- cond_tag

  # Re-aggregate Results
  out_list <- lapply(index, function(x) {
    temp_out <- out[x]
    names(temp_out) <- gsub("_cond.*",
                            "",
                             names(temp_out))
    return(temp_out)
    }
  )

  # Append the parms object for this run
  out <- list()
  out$results <- out_list
  out$parms <- sInfo$parms
  out$conds <- sInfo$conds
  out$session_info <- sInfo$session_info

  # Save
  saveRDS(out,
          file = paste0("../output/",
                        runName,
                        "_res",
                        ".rds")
  )


