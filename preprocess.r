# load necessary packages
library(future.apply)
library(data.table)

# Setup futures parallel processing
options(future.fork.enable = T)
options(future.globals.maxSize = 25000 * 1024^2) # Set max RAM allowed for global variables. XMB * 1024^2

source("functions/func_preprocess.R") # Does some pre-processing of data sent by NREL and saves as .rds files
source("functions/func_loadRawData.R") # Loads pre-processed EVI-Pro model data generated by preprocess_NREL_data()
source("functions/func_loadEVIPro.R") #loads raw EVIPro model output data and stores in a single data table

#Generate list of temperatures that we have for EVI-Pro data that we want to process
temp_vec <- seq(-20,40, 10)
temp_vec <- paste0(temp_vec, "C")

work_shift_vec <- c("min_delay", "max_delay", "load_leveling")
home_shift_vec <- c("min_delay", "max_delay", "load_leveling", "timed_charging")

for(work_shift in 1:length(work_shift_vec)){
  for(home_shift in 1:length(home_shift_vec)) {
    
    print(paste0("Working on load shift combination: work-", work_shift_vec[work_shift], " home-", home_shift_vec[home_shift]))
    
    # # Sedans: nothing returned. Results saved in specified output directories
    # print("working on sedan data")
    # preprocess_NREL_data(temp_list = temp_vec,
    #                     inputdir_evipro = "input/EVIPro_sessions_all_ambient_temps_sedan/",
    #                     inputdir_chts = "input/chts_dvmt.csv",
    #                     outputdir_eviraw = "input/preprocessed/evi_raw_sedan/",
    #                     outputdir_loadprofile = "input/preprocessed/load_profile_sedan/",
    #                     vmt_bin_size = 10, # bin width in miles of fleet dvmt distribution. Must be an integer
    #                     loadprofile_timestep = 0.25, # time step in decimal hours of electricity demand profile
    #                     work_load_shift = work_shift_vec[work_shift], # string of load shift strategy
    #                     home_load_shift = home_shift_vec[home_shift], # string of load shift strategy
    #                     temp_group_size = 4, # The number of temperatures that the program will work on at a time
    #                     desired_time = 1.0) # desired time specifically for the timed charging load shift strategy
    
    # SUVs: nothing returned. Results saved in specified output directories
    print("working on SUV data")
    preprocess_NREL_data(temp_list = temp_vec,
                         inputdir_evipro = "input/EVIPro_sessions_all_ambient_temps_suv/",
                         inputdir_chts = "input/chts_dvmt.csv",
                         outputdir_eviraw = "input/preprocessed/evi_raw_suv/",
                         outputdir_loadprofile = "input/preprocessed/load_profile_suv/",
                         vmt_bin_size = 10, # bin width in miles of fleet dvmt distribution
                         loadprofile_timestep = 0.25, # time step in decimal hours of electricity demand profile
                         work_load_shift = work_shift_vec[work_shift], # string of load shift strategy
                         home_load_shift = home_shift_vec[home_shift], # string of load shift strategy
                         temp_group_size = 4, # The number of temperatures that the program will work on at a time
                         desired_time = 1.0) # desired time specifically for the timed charging load shift strategy
  }
  
}

