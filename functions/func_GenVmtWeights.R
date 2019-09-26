# Author: Schatz Energy Research Center
# Original Version: Andy Harris
# Edits: Jerome Carman and/or Daug Saucedo and/or Andy Harris and/or Michah Wright
# Version: 2.0
# Date: March 30, 2019
# Description: A function to generate a list of vmt weights to build a fleet with a given distribution of vmts.
#
# Required Variables
# mean_vmt: number (type double) specifying fleet-wide mean daily vehicle miles traveled
# max_vmt: number (type double) specifying the max vmt value put to which to create a distribution
# bin_width: The bin size (in miles) on which we want to build a distribution
# loc_class: "urban" or "rural"
# dow: number (type integrer) specifying the day of week. 1 for weekday and 2 for weekend
#
# Optional Variables
# sd_vmt: the standard deviation of daily vehicle miles travelled. Currently set to a default
#
# Version History
# 1: Initial development
# 2.0: Complete re-design using distributions of urban and rural derived from NHTS data
#
vmt_WeightDistGen <- function(mean_vmt, max_vmt, bin_width, loc_class, dow) {
  # Based on Barter et al 2015, Lin et al 2012, and Tamor et al 2015, we believe the gamma distribution to be a reasonable
  # approximation of the distribution of daily vmt. 
  
  # The gamma distribution is defined by 2 variables: shape (k) and scale (theta). The distribution mean is equal to 
  # shape * scale, and the variance is equal to shape * (scale)^2. Step one is to get these parameter values from the 
  # vmt distribution of the fleet
  
  # load the estimated gamma distribution parameters from est_VMT_parameters.Rmd 
  param <- fread("data/NHTS/gamma_est.csv")
 
  # extract rate estimate and use it to get the shape for the given mean
  rate.var <- param[urban == loc_class & day_of_week == dow, rate]
  shape.var <- rate.var * mean_vmt

  # Create a data table loaded with the vmt bins we want
  # This is right bin edge
  vmt_weights <- data.table(name = seq(bin_width, max_vmt, bin_width))
  
  # To assign weights, calculate the value of the CDF at each VMT bin. We will then subtract the CDF value from the 
  # previous bin to get the weight percentage for that bin.
  vmt_weights[, cdf := pgamma(name, shape = shape.var, rate = rate.var)]
  vmt_weights[, prev.cdf := c(0, cdf[.I - 1])]
  vmt_weights[, weight := cdf - prev.cdf]
  
  # Now trim the extraneous columns
  vmt_weights[,':=' (cdf = NULL, prev.cdf = NULL)]
  
  return(vmt_weights)
}
