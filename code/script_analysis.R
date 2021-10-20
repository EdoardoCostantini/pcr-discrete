### Title:    Analysing results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-10-20

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
  file_lin <- grep("_lin", list.files(inDir), value = TRUE)
  file_box <- grep("_box", list.files(inDir), value = TRUE)
  runName <- files[length(files)]

  # Read output
  gg_shape <- readRDS(paste0(inDir, file_lin))
  gg_line <- readRDS(paste0(inDir, file_box))

  # Support Functions
  source("./init.R")

# Plots -------------------------------------------------------------------

  result <- c("mses.", "r2.", "cors.")[1]

  K_conditions <- rev(sort(unique(gg_shape$K)))
  D_conditions <- sort(unique(gg_shape$D))
  int_conditions <- unique(gg_shape$interval)[1]
  rho_conditions <- unique(gg_shape$rho)[7]
  blocks_conditions <- unique(gg_shape$blocks)[2]

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
    filter(blocks %in% blocks_conditions) %>%
    filter(rho %in% rho_conditions) %>%
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
               cols = vars(factor(K,
                                  levels = K_conditions,
                                  labels = paste0("K = ", K_conditions))),
               scales = "fixed") +
    # Format
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 15)) +
    labs(title = paste0("interval: ", int_conditions, "; ",
                        "cor level: ", rho_conditions, "; ",
                        "cov blocks: ", blocks_conditions),
         x     = NULL,
         y     = result) +
    coord_cartesian(ylim = c(0, 25))

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

# Produce line plot -------------------------------------------------------

# Inputs
  dat = gg_line
  meth_sel = paste0(c("orig", "nume", "poly", "dumm", "disj", "PCAmix"),#[c(4:6)],
                    collapse = "|")
  plot_x_axis = "rho"
  plot_y_axis = "rmses_av"
  x_axis_name = "Predictors Correlation"
  y_axis_name = "rmse"
  moderator = "method"
  grid_x_axis = "K"
  grid_y_axis = "D"
  scales = NULL # or "free"
  error_bar = FALSE
  scale_x_cont = FALSE
  filters = list(blocks = c(5),
                 D = unique(gg_line$D)[c(2, 4, 5)],
                 K = c(2, 3, 5, 7))

  # Subset data
  dat_sub <- dat %>%
    # filter(grepl(par_est, variable)) %>%
    filter(grepl(meth_sel, method))

  # Apply extra filters
  for (f in seq_along(filters)){
    filter_factor <- names(filters)[f]
    filter_lvels <- filters[[f]]
    dat_sub <- dat_sub %>%
      filter(!!as.symbol(filter_factor) %in% filter_lvels)
  }

  # Main Plot
  plot_main <- dat_sub %>%
    ggplot(aes_string(x = plot_x_axis,
                      y = plot_y_axis,
                      group = moderator,
                      color = moderator)) +
    geom_line(aes_string(linetype = moderator)) +
    geom_point(size = 1)

  # Add error bars if wanted
  if(error_bar == TRUE){
    plot_main <- plot_main +
      geom_errorbar(aes(ymin = lwr_avg,
                        ymax = upr_avg,
                        group = method),
                    width = .1)
  }

  # Grid
  plot_grid <- plot_main +
    facet_grid(reformulate(grid_x_axis, grid_y_axis),
               labeller = label_both,
               scales = scales)

  # Format
  plot_themed <- plot_grid +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 10)) +
    labs(title = NULL,
         x     = x_axis_name,
         y     = paste(y_axis_name))
  plot_themed