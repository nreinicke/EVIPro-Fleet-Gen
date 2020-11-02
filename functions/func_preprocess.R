# Author: Schatz Energy Research Center
# Original Version: Micah Wright
# Date: March 01, 2019
# Description: Processes and saves the evi sessions and time series load profile for all ambient temperatures.
# Version History
#			2019-09-12 Max: minor edits, run on SUV data
#			2020-05-04 Jerome: turn into formal function for use in .Rmd in order to formalize and document all data generation steps

# load necessary packages
library(future.apply)
library(data.table)

# source functions
source("functions/func_loadEVIPro.R") # loads EVIPro data and stores in a single data table
source("functions/func_calcBaseEVILoad.R") # pre-calculates load profile for all unique_vid in evi_raw

#Define function
preprocess_NREL_data <- function(temp_list,              # vector of character strings of ambient temperature values (i.e. c("20C","30C","35C"))
                                 inputdir_evipro,        # character string of directory containing files provided by NREL
                                 inputdir_chts,          # character string of file path to chts_dvmt.csv file provided by NREL
                                 outputdir_eviraw,       # character string of directory to save raw evi .rds files
                                 outputdir_loadprofile,  # character string of directory to save raw load profile .rds files
                                 vmt_bin_size,           # integer of vmt bin size for load_EVIPro() function
                                 loadprofile_timestep) { # float of time step in decimal hours for calcBaseEVILoad() function
  
  # split the temp vector into groups of the desired size
  temp_groups <- split(temp_list, ceiling(seq_along(temp_list)/4))
  
  for(group in 1:length(temp_groups)) {
    # Parallelize lapply across temperatures
    future_lapply(seq(1:length(temp_groups[[group]])), function(i) {
      # load charging session data into data table
      evi_raw <- load_EVIPro(inputdir_evipro,
                             temp_groups[[c(group, i)]],
                             inputdir_chts,
                             vmt_bin_size)
      
      # Save charging session data table
      if(!dir.exists(outputdir_eviraw)) {
        dir.create(outputdir_eviraw, recursive=T)
      }
      saveRDS(evi_raw, paste0(outputdir_eviraw,
                              temp_groups[[c(group, i)]], 
                              ".rds"))
      
      # Create load profiles
      evi_load_profiles <- calcBaseEVILoad(evi_raw, loadprofile_timestep)
      
      # Save load profiles data table
      if(!dir.exists(outputdir_loadprofile)) {
        dir.create(outputdir_loadprofile, recursive=T)
      }
      saveRDS(evi_load_profiles, paste0(outputdir_loadprofile,
                                        temp_groups[[c(group, i)]],
                                        ".rds"))
    })
  }

}
