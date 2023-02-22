# Project:   pcr_discrete
# Objective: pre-process input data for actual use in shiny app
# Author:    Edoardo Costantini
# Created:   2023-02-21
# Modified:  2023-02-22
# Notes:     This script prepares the input data for the shiny app plotpcrnotnormal.

# Read output
inDir <- "../output/"
grep("_box", list.files(inDir), value = TRUE)
resPCRdis <- readRDS(paste0(inDir, "20220421_154258_box.rds"))

# Split variable into two useful columns
resPCRdis[c("outcome", "method")] <- stringr::str_split_fixed(resPCRdis$variable, "\\.", 2)

# Define a factor with desired order of the methods
resPCRdis$method <- factor(
    resPCRdis$method,
    levels = unique(resPCRdis$method)
)

# Express proportion of discretized variables as a ratio
resPCRdis$D <- factor(
    resPCRdis$D,
    levels = unique(resPCRdis$D)[c(2, 3, 1)],
    labels = c("1/3", "2/3", "1")
)

# Save the two objects as .rda ready for shiny app
save(resPCRdis, file = "../output/resPCRdis.rda")
