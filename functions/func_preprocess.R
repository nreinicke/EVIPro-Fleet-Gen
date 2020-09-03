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
library(parallel)


#Define function
preprocess_NREL_data <- function(temp_list,              # vector of character strings of ambient temperature values (i.e. c("20C","30C","35C"))
                                 inputdir_evipro,        # character string of directory containing files provided by NREL
                                 inputdir_chts,          # character string of file path to chts_dvmt.csv file provided by NREL
                                 outputdir_eviraw,       # character string of directory to save raw evi .rds files
                                 outputdir_loadprofile,  # character string of directory to save raw load profile .rds files
                                 vmt_bin_size,           # integer of vmt bin size for load_EVIPro() function
                                 loadprofile_timestep,   # float of time step in decimal hours for calcBaseEVILoad() function
                                 public_load_shift,      # string of work/public load shift strategy 
                                 home_load_shift,        # string of home load shift strategy 
                                 temp_group_size) {      # integer the number of temperatures to calculate in parallel 
		
  
    # split the temp vector into groups of four
    temp_groups <- split(temp_list, ceiling(seq_along(temp_list)/temp_group_size))
  
		# Parallelize lapply across temperatures in groups according to group size
    cl <- makeCluster(13, outfile="")
    
    for(group in 1:length(temp_groups)) {
		  parLapply(cl, seq(1:length(temp_groups[[group]])), function(i) {
		    print(paste0("working on temperature ", temp_groups[[c(group, i)]]))
        # source functions
        library(data.table) 
        source("functions/func_loadEVIPro.R") # loads EVIPro data and stores in a single data table
        source("functions/func_calcBaseEVILoad.R") # pre-calculates load profile for all unique_vid in evi_raw
		  
		    # load charging session data into data table
			  evi_raw_file <- paste0(outputdir_eviraw, temp_groups[[c(group, i)]], ".rds")
			  if(!file.exists(evi_raw_file)) {
  			  print("could not find existing evi raw file, computing..")
  		    evi_raw <- load_EVIPro(inputdir_evipro,
  		                          temp_groups[[c(group, i)]],
  		                          inputdir_chts,
  		                          vmt_bin_size)
  		  
  		    # Save charging session data table
  		    if(!dir.exists(outputdir_eviraw)) {
  		      dir.create(outputdir_eviraw, recursive=T)
  		    }
  		    saveRDS(evi_raw, evi_raw_file) 
			  }
			  else {
  			  print("found existing evi raw file, loading from file..")
			    evi_raw <- readRDS(evi_raw_file)
			  }
		  
		    # Create load profiles
		    evi_load_profiles <- calcBaseEVILoad(evi_raw, loadprofile_timestep, public_load_shift, home_load_shift)
		  
		    # Save load profiles data table
		    if(!dir.exists(outputdir_loadprofile)) {
		      dir.create(outputdir_loadprofile, recursive=T)
		    }
		    saveRDS(evi_load_profiles, paste0(outputdir_loadprofile,
		                                      temp_groups[[c(group, i)]],
		                                      "_public-",
		                                      public_load_shift,
		                                      "_home-",
		                                      home_load_shift,
		                                      ".rds"))
		  })
    }
		stopCluster(cl)

}
