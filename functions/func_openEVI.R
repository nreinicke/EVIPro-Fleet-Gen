# Author: Schatz Energy Research Center
# Original Version: Micah Wright
# Edits: Jerome Carman
# Version: 2.0
# Description: wrapper function to create a PEV fleet and produce load profiles
# Required Variables
#   fleet_size: integer specifying the size of the desired fleet
#   temperature: specify the temperature in order to grab the correct evi_raw and load_profile data sets
#   weights: list of data.tables with two columns ("name" and "weight") and rows containing names and associated decimals (with a sum of 1) that represent the fraction of fleet vehicles comprised of each variable in the name column.
#     - pref_w: length 2 vector indicating the proportion of drivers who prefer home or work
#     - home_w: length 3 vector indicating the proportion of fleet with access to level 1, 2, qnd 3 home charging stations
#     - work_w: length 2 vector indicating the proportion of fleet with access to level 1 and 2 work charging stations
#     - pub_w: length 3 vector indicating the proportion of fleet with access to level 1, 2, qnd 3 public charging stations
#   mean_vmt: number (type double) specifying the mean daily vmt for the fleet
#   loc_class: character, either "urban" or "rural". This is used in combination with mean_vmt to create a vmt weights distribution
# Version History
#   1.0: JKC added language to header and added comments.
#   2.0: JKC restructured to allow for embedded parallelization and looping through permutations of weights

# source function scripts
source("functions/func_joinOn.R") #Fast join function
source("functions/func_strEval.R") #String evulation function
source("functions/func_pp.R") #Alias for paste
source("functions/func_LoadEVIPro.R") #loads EVIPro data and stores in a single data table
source("functions/func_EVIFleetGen.R") #Generates a fleet of EVIPro vids
source("functions/func_calcBaseEVILoad.R") #Pre-calculates load profile for all unique_vid in evi_raw
source("functions/func_measureFleetWeights.R") #Creates statistics of generated fleet
source("functions/func_LoadFleetWeights.R") #Loads .csv file where fleet characteristic weights are stored
source("functions/func_GenVmtWeights.R") #Generates vmt distribution for fleet generation
source("functions/func_CreateFleetWeights.R") #Creates fleet weights from values hard coded in this function.
source("functions/func_GenFleetProfiles.R") #Creates 48-hour load profile for the resulting fleet

library(data.table)

openEVI <- function(evi_raw,
                    evi_load_profiles,
                    #temp = "-20C", # varied in for loop
                    fleet = c(1000),
                    pev = c(0.25,0.25,0.25,0.25),
                    dvmt = c(30),
                    pref = c(0.8,0.2),
                    home = c(0.20,0.70,0.1),
                    work = c(0,1),
                    loc_class = "urban",
																				veh_class = c(0.5, 0.5)) { # additional var here for suv/sedan
  
  #Create data table of fleet weights that will work with evi_fleetGen()
  fleet_weights <- create_fleet_weights(pev, # change for suvs
                                        pref,
                                        home,
                                        work,
  																																						veh_class)
  
  #Create fleet
  # step through below function
  # 
  evi_fleet <- evi_fleetGen(evi_raw,
                            fleet,
                            fleet_weights, # list of five weights
                            mean_vmt = dvmt, 
                            bin_width = 10, #Do not change this from 10 unless evi_load_profiles are re-run with a different bin width
                            loc_class = loc_class)
  
  #Create 24-hour load profile for the fleet
  evi_fleet_prof <- get_fleet_profiles(evi_fleet, fleet, evi_load_profiles)
  
  #Return just the fleet load profile. Ignore the fleet activity and fleet stats for the iterative generation of load profiles for NREL
  return(evi_fleet_prof)
  
}

