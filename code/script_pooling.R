### Title:    Pooling results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-11-03

  ## Make sure we have a clean environment:
  rm(list = ls())

  ## Support Functions
  source("./init.R")

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  files <- grep("tar", list.files(inDir), value = TRUE)
  target_tar <- files[9]
  output <- readTarGz(target_tar)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

  # Give unique name to all objects
  names(output$out) <- output$file_names

  # Punt into a single data.frame
  out <- do.call(rbind, output$out)

  # Store
  saveRDS(out,
          file = paste0("../output/",
                        output$name_run,
                        "_out",
                        ".rds")
  )

  # Read
  file_name <- grep("out", list.files(inDir), value = TRUE)[2]
  run_name <- gsub("_out.rds", "", file_name)
  out <- readRDS(paste0("../output/", file_name))

  tag_column <- grep("tag", colnames(out))

# Restructure for Box plot ------------------------------------------------
  gg_shape <- reshape2::melt(out, id.var = colnames(out)[1:tag_column])

  # Save
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_box",
                        ".rds")
  )

# Restructure for line plot -----------------------------------------------
  # Get rid of tag
  out_clean <- out[, -tag_column]

  # Define condition columns
  col_conds <- 1:(tag_column-1)

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

  # Correct order of methods: dumm, disj, PCAmix, numm, poly, orig
  method_order <- unique(base_df$method)[c(5, 4, 6, 2, 3, 1)]
  base_df$method <- factor(base_df$method,
                           levels = method_order)

  # Summarize
  comp_grouping <- c(colnames(out)[1:(tag_column-1)], "method")
  results <- data.frame(base_df %>%
                          group_by_at(comp_grouping) %>%
                          dplyr::summarize(mses_av = mean(mses.),
                                           mses_md = median(mses.),
                                           mses_sd = sd(mses.),
                                           rmses_av = mean(sqrt(mses.)),
                                           rmses_md = median(sqrt(mses.)),
                                           rmses_sd = sd(sqrt(mses.)),
                                           cors_av = mean(cors.),
                                           cors_md = median(cors.),
                                           cors_sd = sd(cors.),
                                           r2_av = mean(r2.),
                                           r2_md = median(r2.),
                                           r2_sd = sd(r2.))
  )

  # Save
  saveRDS(results,
          file = paste0("../output/",
                        output$name_run,
                        "_lin",
                        ".rds")
  )