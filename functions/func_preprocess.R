# Author: Schatz Energy Research Center
# Original Version: Micah Wright
# Date: March 01, 2019
# Description: Processes and saves the evi sessions and time series load profile 
#   for all ambient temoperatures. This is not designed to be an interactive 
#   function, timestep is assumed to be 15 minutes in all cases.

#######################
# Edits: by Max Blasdel
# Date: 9/12/19
# Purpose: Re-run this function on additional SUV data from NREL
# Changes: changing data sources and file save locations
# changes mark with `# suv` where appropriate
#######################

#######################
# NOTE: Since this was written the RProj file had been accidentaly erased
# This was a product of issues with the git projects (we think)
# The working directory is now required to be set for all function calls

# set wd
setwd("/media/spin/sein-evi/source/NREL-Demo_Project")
#######################

# load necessary packages
library(future.apply)
library(data.table)

# source functions
# loads EVIPro data and stores in a single data table
source("functions/func_LoadEVIPro.R") 

# pre-calculates load profile for all unique_vid in evi_raw
source("functions/func_calcBaseEVILoad.R") 

# set up par
plan(multiprocess, workers = 6)

temp_list <- seq(-20, 40, 5)

temp_list <- paste0(temp_list, "C")

future_lapply(seq(1:length(temp_list)), function(i) {
  # load the raw data
  evi_raw <- load_EVIPro("../../data/sessions_all_ambient_temps_suv/", # suv
                         temp_list[i], 
                         "../../data/chts_dvmt.csv", # path change
                         10)
  saveRDS(evi_raw, paste0("../../data/preprocessed/evi_raw_suv/", # suv
                          temp_list[i], 
                          ".rds"))
  
  evi_load_profiles <- calcBaseEVILoad(evi_raw, 0.25)
  
  saveRDS(evi_load_profiles, paste0("../../data/preprocessed/load_profile_suv/", # suv
                          temp_list[i], 
                          ".rds"))
})
