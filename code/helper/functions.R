### Title:    helper functions
### Project:  Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-05-19

# Average correlation -----------------------------------------------------

extract_avg_cor <- function(cmat_list = list()){
  # Given a list of correlation matrices return the average correlation
  # this function discretize the numeric vector j
  # cmat_list = list(cont = cor(dat_cont),
  #                  disc = cor(dat_disc),
  #                  atte = cor(dat_disc_cont))
  # Body
  imat <- lower.tri(cmat_list[[1]]) # index matrix
  cmat_low_tri <- lapply(cmat_list, "[", imat)
  mean_cor <- sapply(cmat_low_tri, mean)

  return(mean_cor)
}

# LM coefficients and CIs -------------------------------------------------

extract_lm <- function(dts = list()){
  # Given a list of correlation matrices return the average correlation
  # this function discretize the numeric vector j
  # Example input
  # dts = list(cont = dat_cont,
  #            disc = dat_disc,
  #            atte = dat_disc_cont)

  # Transform to data.frames
  dts_df <- lapply(dts, as.data.frame)

  # Fit lm model
  lm_fits <- lapply(dts_df, lm, formula = "z1 ~ z2 + z3")

  # Extract coefficients
  lm_coefs <- sapply(lm_fits, coef)

  # Extract confidence intervals
  lm_confint <- lapply(lm_fits, confint)

  # Output
  return(list(est = lm_coefs,
              cis = lm_confint))
}

# Average reg coefficient estiamtes from sim ------------------------------

average_coefs <- function(cond, d_place = 3){
  collection <- lapply(cond, function(i){
    i$coefs$est
  })
  out <- Reduce('+', collection)/length(collection)
  return(round(out, d_place))
}

# Average correlation estiamtes from sim ----------------------------------

average_corr <- function(cond, d_place = 3){
  collection <- lapply(cond, function(i){
    i$cors
  })
  out <- Reduce('+', collection)/length(collection)
  return(round(out, d_place))
}
