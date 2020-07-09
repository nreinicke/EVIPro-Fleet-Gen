# Author: Schatz Energy Research Center
# Original Version: Micah Wright
# Edits: Jerome Carman, Max Blasdel
# Version: 2.1
# Description: wrapper function to create a PEV fleet
# Required Variables
#			evi_raw: data table of EVI-Pro charge session data
#   fleet: integer specifying the size of the desired fleet
#   pev: length 4 vector, type double and sum(pev)=1, indicating the proportion of PHEV20, PHEV50, BEV100, and BEV250
#			dvmt: number (type double) specifying the mean daily vmt for the fleet
#   pref: length 2 vector, type double and sum(pref)=1, indicating the proportion of drivers who prefer home or work
#   home: length 3 vector, type double and sum(home)=1, indicating the proportion of fleet with access to level 1, 2, qnd 3 home charging stations
#   work: length 2 vector, type double and sum(work)=1, indicating the proportion of fleet with access to level 1 and 2 work charging stations
#			loc_class: string of value either "urban" or "rural". Impacts dvmt distribution.
#			veh_class: length 2 vector, type double and sum(veh_class)=1, indicating proportion Sedan and SUV vehicles
# Version History
#   1.0: JKC added language to header and added comments.
#   2.0: JKC restructured to allow for embedded parallelization and looping through permutations of weights
#			2.1: JKC and MB edited to allow for use with run-all.R wrapper script.

# library(data.table)

openEVI <- function(evi_raw,
                    fleet = c(1000),
                    pev = c(0.25,0.25,0.25,0.25),
                    dvmt = c(30),
                    pref = c(0.8,0.2),
                    home = c(0.20,0.70,0.1),
                    work = c(0,1),
                    loc_class = "urban",
                    veh_class = c(0.8,0.2)) {
	
  #Create data table of fleet weights that will work with evi_fleetGen()
  fleet_weights <- create_fleet_weights(pev, # change for suvs
                                        pref,
                                        home,
                                        work,
                                        veh_class)

  #Create fleet
  # step through below function
  evi_fleet <- evi_fleetGen(evi_raw,
                            fleet_size = fleet,
                            weights = fleet_weights, # list of five weights
                            mean_vmt = dvmt, 
                            bin_width = 10, #Do not change this from 10 unless evi_load_profiles are re-run with a different bin width
                            loc_class = loc_class)
  
  # Return fleet
  return(evi_fleet)
}

