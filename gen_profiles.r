# Source R packages
library(future.apply)
library(data.table)

# Source functions
#		Note that many of these functions are not generic and have some hard-coded values in them. Double check functions before running.
source("functions/func_preprocess.R") # Does some pre-processing of data sent by NREL and saves as .rds files
source("functions/func_openEVI.R") # 
source("functions/func_randVector.R") # 
source("functions/func_loadProfiles.R") # 
source("functions/func_loadRawData.R") # Loads pre-processed EVI-Pro model data generated by preprocess_NREL_data()
source("functions/func_loadMat.R") # Used by load_EVIPro() for loading raw EVI-Pro model output data
source("functions/func_joinOn.R") #Fast join function
source("functions/func_strEval.R") #String evulation function
source("functions/func_pp.R") #Alias for paste
source("functions/func_loadEVIPro.R") #loads raw EVIPro model output data and stores in a single data table
source("functions/func_EVIFleetGen.R") #Generates a fleet of EVIPro vids
source("functions/func_calcBaseEVILoad.R") #Pre-calculates load profile for all unique_vid in evi_raw and writes results to disk
source("functions/func_genVmtWeights.R") #Generates vmt distribution for fleet generation
source("functions/func_createFleetWeights.R") #Creates fleet weights from values hard coded in this function.
source("functions/func_genFleetProfiles.R") #Creates 48-hour load profile for the resulting fleet
source("functions/func_estDVMTGammaParameters.R") # Creates gamma distribution parameters from NHTS data for generating DVMT distributions

# Setup futures parallel processing
options(future.fork.enable = T)
options(future.globals.maxSize = 25000 * 1024^2) # Set max RAM allowed for global variables. XMB * 1024^2

# Define function to retrieve names from list
getNames <- function(vec, option) {
  # subset list to match given option	 
  out <-	lapply(vec, function(x) {
    x[all(x == unlist(option))]
  })
  # filter out zero length lists
  return(names(Filter(length, out)))
}

# output folder
out_folder <- paste0("output/fleetprofiles_",format(Sys.time(), "%d-%b-%Y_%H-%M"),"/")
if(!dir.exists(out_folder)) {
  dir.create(out_folder, recursive=T)
}

# Initialize log file to catch debugging output
log_file <<- file(paste0(out_folder,"runlog_", format(Sys.time(), "%d-%b-%Y_%H-%M"), ".log"), open = 'a')

# Indicate whether to include verbose output for debugging
#   Note this is not well implemented yet.
debugFleetGen <- T

temp_vec <- c("-20C",
              "-10C",
              "0C",
              "10C",
              "20C",
              "30C",
              "40C")

# Fleet Size
#		Vector can have any number of fleet sizes. Must be an integer.
#		Note that fleet sizes greater than 30,000 are essentially scaled versions of a ~30,000 vehicle fleet. This
#				is because the CHTS data set represents around 30,000 vehicles, so larger fleets begin to simply
#				replicate EVI-Pro data.
fleet_size_vec <- list(1000,
                       10000,
                       50000) 

# Average Daily Vehicel Miles Traveled (DVMT) per Vehicle 
#		Vector can have any number of DVMT values within the range 10 through 60. Can be a decimal or integer. Units are miles.
#		Note that DVMT at the extremes are difficult to build fleets around because there are
#				relatively few CHTS responses with really high or low DVMT values.
mean_dvmt_vec <- c(25,
                   35,
                   45)

# PEV Type Distribution
#		Vector structure is c(% %PHEV 20, % PHEV 50, % BEV 100, % BEV 250 )
#		Any number of distribution vectors can be included in the list
pev_type_vec <- list("BEV" = c(.10, .15, .25, .50),   # BEV Dominant
                     "PHEV" = c(.25, .50, .10, .15),  # PHEV Dominant
                     "EQUAL" = c(.15, .35, .15, .35)) # PHEV/BEV Equal share

# Vehicle Class Distribution
#		Vector structure is c(% Sedans, % SUVs)
#		Any number of distribution vectors can be included in the list
veh_class_vec <- list("Sedan" = c(0.8, 0.2), # sedan dominant
                      "Equal" = c(0.5, 0.5), # equal distribution
                      "SUV" = c(0.2, 0.8))   # suv dominant

# Home Work Preference Distribution
#		Vector structure is c(% that prefer charging at home, % that prefer charging at work)
#		Any number of distribution vectors can be included in the list
pref_vec <- list("Home100" = c(1.0, 0.0), # 100% perfer charging at home
                 "Home80" = c(0.8, 0.2),  # 80% prefer charging at home
                 "Home60" = c(0.6, 0.4))  # 60% prefer charging at home

# Home Access and Power
# 	Vector structure is c(% with L1 plugs, % with L2 plugs, % without home access)
#		Any number of distribution vectors can be included in the list
home_power_vec <- list("HA100_MostL1" = c(0.80, 0.20, 0),    # 100% access to home power, 80% of those L1
                       "HA100_MostL2" = c(0.20, 0.80, 0),    # 100% access to home power, 80% of those L2
                       "HA100_Equal" = c(0.5, 0.5, 0),       # 100% access to home power, 50% of those L2
                       "HA75_MostL1" = c(0.6, 0.15, 0.25),   # 75% access to home power, 80% of those L1
                       "HA75_MostL2" = c(0.15, 0.6, 0.25),   # 75% access to home power, 80% of those L2
                       "HA75_Equal" = c(0.375, 0.375, 0.25), # 75% access to home power, 50% of those L2
                       "HA50_MostL1" = c(0.4, 0.1, 0.5),     # 50% access to home power, 80% of those L1
                       "HA50_MostL2" = c(0.1, 0.4, 0.5),     # 50% access to home power, 80% of those L2
                       "HA50_Equal" = c(0.25, 0.25, 0.5))    # 50% access to home power, 50% of those L2

# Work Power Distribution
#		Vector structure is c(% with L1 plugs, % with L2 plugs)
#		Any number of distribution vectors can be included in the list
work_power_vec <- list("MostL2" = c(0.2, 0.8), # Most workplace charging is L2
                       "Equal" = c(0.5, 0.5),  # Equal distribution of L1 and L2
                       "MostL1" = c(0.8, 0.2)) # Most workplace charging is L1

# Geography
# 	Options are "urban" or "rural".
#		Can only choose one.
loc_class_vec <- list("urban")

load_shift <- "min_delay"

plan(multicore, workers = 12) # 12 workers = ~160GB RAM

start_time = Sys.time()
# Create df of all fleet characteristics options, excluding temperature. A separate results file is generated for each temperature.
all_options <- data.table(expand.grid(numveh = fleet_size_vec,
                                      pev = pev_type_vec,
                                      dvmt = mean_dvmt_vec,
                                      home = home_power_vec,
                                      work = work_power_vec,
                                      loc = loc_class_vec,
                                      vclass = veh_class_vec,
                                      pref = pref_vec)
)

# split all options list so lapply can run on it
all_options[, ID := seq(1:nrow(all_options))] # add ID column to split on
all_options_list <- split(all_options, all_options$ID)

# Generate fleets for each ambient temperature for all permutations of all other fleet characteristics
#   Parallelize on permutations of fleet characteristics
print("starting lapply..")
lapply(temp_vec, function(temp) {
  
  # Load raw charging session and load profile .rds files
  raw_data <- loadRawData(temp, load_shift)
  
  # Create load profiles by looping over all permutations of options
  fleet_load <- future_lapply(all_options_list, function(options_list) {
    
    # Create fleet and load profile -----------------------------------------------------
    
    fleet_sub <-
      openEVI(
        evi_raw = raw_data[[1]],
        fleet = unlist(options_list$numveh),
        pev = unlist(options_list$pev),
        dvmt = options_list$dvmt,
        pref = unlist(options_list$pref),
        home = unlist(options_list$home),
        work = unlist(options_list$work),
        loc = options_list$loc,
        veh_class = unlist(options_list$vclass))
    
    # Create load profile of fleet
    load_to_bind <- get_fleet_profiles(fleet_sub,
                                       unlist(options_list$numveh),
                                       raw_data[[2]])
    
    # Create Names ----------------------------------------------------------------------- 
    
    load_to_bind[, ':=' (loc_class = options_list$loc[[1]],
                         temp_c = temp,
                         fleet_size = options_list$numveh[[1]],
                         mean_dvmt = options_list$dvmt,
                         pev_dist = getNames(pev_type_vec, options_list$pev),
                         pref_dist = getNames(pref_vec, options_list$pref),
                         home_access_dist = regmatches(getNames(home_power_vec, options_list$home), # split home access vector, select first element, home access
                                                       regexpr("_", getNames(home_power_vec, options_list$home)),
                                                       invert = T)[[1]][1],
                         home_power_dist = regmatches(getNames(home_power_vec, options_list$home), # split home access vector, select second element, home power distribution
                                                      regexpr("_", getNames(home_power_vec, options_list$home)),
                                                      invert = T)[[1]][2],
                         work_power_dist = getNames(work_power_vec, options_list$work),
                         class_dist = getNames(veh_class_vec, options_list$vclass)
    )
    ] # end add naming columns
    
    # Summarize and Return Data ----------------------------------------------------------
    
    # summarize data based on distinct options
    load_to_bind[time_of_day > 24,time_of_day := time_of_day - 24] # Wrap loads after hour 24 to morning
    load_to_bind <- load_to_bind[, .(kw = sum(avg_kw)),
                                 by = c("loc_class",
                                        "temp_c",
                                        "fleet_size",
                                        "mean_dvmt",
                                        "pev_dist",
                                        "pref_dist",
                                        "home_access_dist",
                                        "home_power_dist",
                                        "work_power_dist",
                                        "class_dist",
                                        "day_of_week",
                                        "pev_type", 
                                        "dest_type", 
                                        "dest_chg_level",
                                        "class_type",
                                        "time_of_day"
                                 )
    ] # End summarize results
    
    # Sort results
    setkey(load_to_bind,
           temp_c,
           fleet_size,
           mean_dvmt,
           pev_dist,
           pref_dist,
           home_access_dist,
           home_power_dist,
           work_power_dist,
           day_of_week,
           pev_type,
           dest_type,
           dest_chg_level,
           class_type,
           time_of_day)
    
    
    return(load_to_bind)
  }) # end of all_options_list future_lapply
  
  # bind results together
  fleet_load <- rbindlist(fleet_load)
  
  # Change data types --------------------------------------------------------------------------------- 
  #   change temp, fleet size, dvmt to integer
  fleet_load[, temp_c := as.integer(gsub("C", "", temp_c))]
  fleet_load[, fleet_size := as.integer(fleet_size)]
  fleet_load[, mean_dvmt := as.integer(mean_dvmt)]
  
  # Save output ----------------------------------------------------------------------------------------
  fwrite(fleet_load,
         file = paste0(out_folder,
                       gsub("-", "", Sys.Date()), "_", # date the run
                       temp,
                       "_",
                       load_shift,
                       ".csv"))
}) # end of temp vec lapply
end_time = Sys.time()
elapsed_time_minutes = (end_time - start_time) / 60
print(paste0('done! took ', elapsed_time_minutes, " minutes"))