# Project:   pcr_discrete
# Objective: pre-process input data for actual use in shiny app
# Author:    Edoardo Costantini
# Created:   2023-02-21
# Modified:  2023-02-21
# Notes:     This script prepares the input data for the shiny app plotpcrnotnormal.

# Read output
inDir <- "../output/"
grep("_box", list.files(inDir), value = TRUE)
resPCRDIS <- readRDS(paste0(inDir, "20220421_154258_box.rds"))

# Split variable into two useful columns
resPCRDIS[c("outcome", "method")] <- str_split_fixed(gg_shape$variable, "\\.", 2)

# Save the two objects as .rda ready for shiny app
save(resPCRDIS, file = "../output/resPCRDIS.rda")
