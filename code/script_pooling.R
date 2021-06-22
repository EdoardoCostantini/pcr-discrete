### Title:    Pooling results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-22

  ## Make sure we have a clean environment:
  rm(list = ls(all = TRUE))

  ## Support Functions
  source("./helper/functions.R")

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  files <- list.files(inDir)
  target_tar <- files[length(files)]
  output <- read.tar.gz(target_tar)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

  # Give unique name to all objects
  names(output$out) <- output$file_names

  # Define the Unique repetitions
  reps <- output$sInfo$parms$dt_rep # you need to input this manually right now
  cond_tag <- output$sInfo$conds$tag

  # Create an index based on the repetition membership
  index <- lapply(cond_tag,
                  function(x) {
                    grep(x, names(output$out), value = TRUE)
                  }
  )
  names(index) <- cond_tag

  # Re-aggregate Results
  out_list <- lapply(index, function(x) {
    temp_out <- output$out[x]
    names(temp_out) <- gsub("_cond.*",
                            "",
                             names(temp_out))
    return(temp_out)
    }
  )

  # Append the parms object for this run
  out <- list()
  out$results <- out_list
  out$parms <- output$sInfo$parms
  out$conds <- output$sInfo$conds
  out$session_info <- output$sInfo$session_info

  # Save
  saveRDS(out,
          file = paste0("../output/",
                        output$name_run,
                        "_res",
                        ".rds")
  )


