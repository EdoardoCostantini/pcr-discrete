### Title:    Pooling results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-10-19

  ## Make sure we have a clean environment:
  rm(list = ls())

  ## Support Functions
  source("./helper/functions.R")

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  files <- grep("tar", list.files(inDir), value = TRUE)
  target_tar <- files[length(files)]
  output <- readTarGz(target_tar)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

  # Give unique name to all objects
  names(output$out) <- output$file_names

# Restructure for Box plot ------------------------------------------------
  out <- do.call(rbind, output$out)
  gg_shape <- reshape2::melt(out, id.var = colnames(out)[1:8])

  # Save
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_box",
                        ".rds")
  )

# Restructure for line plot -----------------------------------------------
  # Get rid of tag
  out_clean <- out[, -8]

  # Define condition columns
  col_conds <- 1:7

  # Melt r2
  targets <- c("mses.", "r2.", "cors.")

  # Melt r2
  list_melt <- lapply(targets, function (x){
    col_select <- grep(x, colnames(out_clean))
    dat_select <- reshape2::melt(out_clean[, c(col_conds, col_select)],
                                 id.var = colnames(out_clean)[col_conds],
                                 value.name = x)
    dat_select$variable <- gsub(x, "", dat_select$variable)
    dat_select
  })

  # Append
  base_df <- cbind(out_clean[, col_conds],
                   method = unlist(lapply(list_melt, function (i){i$variable})),
                   result = sapply(1:length(list_melt), function (i){
                     list_melt[[i]][, targets[i]]
                   }))
  colnames(base_df)[grep("result", colnames(base_df))] <- targets

  # Summarize
  comp_grouping <- c("K", "D", "rho", "blocks", "interval", "method")
  results <- data.frame(base_df %>%
                          group_by_at(comp_grouping) %>%
                          dplyr::summarize(mses_av = mean(mses.),
                                           mses_sd = sd(mses.),
                                           rmses_av = mean(sqrt(mses.)),
                                           rmses_sd = sd(sqrt(mses.)),
                                           cors_av = mean(cors.),
                                           cors_sd = sd(cors.),
                                           r2_av = mean(r2.),
                                           r2_sd = sd(r2.))
  )

  gg_line <- results
  head(gg_line,20)

  # Save
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_lin",
                        ".rds")
  )