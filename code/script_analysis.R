# Title:    Analysing results
# Author:   Edoardo Costantini
# Project:  Ordinality
# Created:  2021-06-10
# Modified: 2022-01-05

  ## Make sure we have a clean environment:
  rm(list = ls())

# Packages ----------------------------------------------------------------

  pack_list <- c("ggplot2",
                 "dplyr",
                 "forcats",
                 "stringr")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  file_box <- grep("_box", list.files(inDir), value = TRUE)
  file_lin <- grep("_lin", list.files(inDir), value = TRUE)

  # Read output
  gg_shape <- readRDS(paste0(inDir, file_box[4]))
  gg_line <- readRDS(paste0(inDir, file_lin[3]))

  # Support Functions
  source("./init.R")

# Plots -------------------------------------------------------------------

  result <- c("mses.", "npcs.", "r2.", "cors.")[1]

  K_conditions <- rev(sort(unique(gg_shape$K)))
  D_conditions <- sort(unique(gg_shape$D))[3]
  int_conditions <- unique(gg_shape$interval)[2]

  methods <- paste(
    c("orig", "nume", "poly", "dumm", "disj", "PCAmix"),
    collapse = "|"
  )

  plot1 <- gg_shape %>%
    # Subset
    filter(grepl(result, variable)) %>%
    filter(grepl(methods, variable)) %>%
    filter(D %in% D_conditions) %>%
    filter(K %in% K_conditions) %>%
    filter(interval %in% int_conditions) %>%
    # Obtain Root MSE
    mutate(rmse = sqrt(value)) %>%
    # Change labels of X axis
    mutate(variable = fct_relabel(variable, str_replace, result, "")
    ) %>%
    # Main Plot
    ggplot(aes(x = variable, y = rmse)) +
    geom_boxplot() +
    # Grid
    facet_grid(rows = vars(factor(D,
                                  labels = paste0("D = ", D_conditions))),
               cols = vars(factor(npcs)),
               scales = "fixed") +
    # Format
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 15)) +
    labs(title = paste0("interval: ", int_conditions),
         x     = NULL,
         y     = result)

  plot1

# Save plots --------------------------------------------------------------

  file_format <- ".png"
  plot_name <- paste0("interval_sacle_", int_conditions)
  out_dir <- "~/Desktop/"
  file_name <- paste0(out_dir, plot_name, file_format)
  if(file_format == ".pdf"){
    pdf(file_name, width = 15, height = 15)
  }
  if(file_format == ".png"){
    png(file_name, width = 15, height = 15, units = "in", res = 384)
  }
  plot1
  dev.off()