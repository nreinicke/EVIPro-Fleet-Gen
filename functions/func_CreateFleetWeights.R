# Author: Schatz Energy Research Center
# Original Version: Jerome Carman, September 2018
# Edits: Jerome Carman and/or Doug Saucedo 
# Version: 2.1
# Date: March 30, 2019
# Description: Used to define weights of different fleet characteristics
# Variables
#   None
# Version Changes
#   v1.1: jkc: added ambient temperature variable temp_weights
#   v2.0: removed temp and pev weights
#   v2.1: added pev weights back in. Removed public power weights
#			v3: MB: added a sedan/suv vehicle class

create_fleet_weights <- function(pev_w = c(0.25,0.25,0.25,0.25),
                                 pref_w = c(0.8,0.2),
                                 home_w = c(0.2,0.8,0),
                                 work_w = c(0.0,1),
																																	veh_w = c(0.5, 0.5)) { # additional variable for suv/sedan # pay attention to naming conventions
                                #pub_w = c(0.8,0.2,0))) { 
 
  # error checking
  stopifnot(length(pev_w) == 4,
            length(pref_w) == 2,
            length(home_w) == 3,
            length(work_w) == 2,
  										length(veh_w) == 2)
            #length(pub_w) == 3)
  
  #PEV Type
  pev_weights <- data.table(
    name = c("PHEV20","PHEV50","BEV100","BEV250"),
    weight = pev_w
  )
  
  # Preferred Location
  pref_weights <- data.table(
    name = c('PrefHome','PrefWork'),
    weight = pref_w
  )
  
  # Home Charging
  home_weights <- data.table(
    name = c('HomeL1','HomeL2','HomeNone'),
    weight = home_w
  )
  
  # Work Charging
  work_weights <- data.table(
    name = c('WorkL1','WorkL2'),
    weight = work_w
  )
  
  # vehicle class
  vehicle_weights <- data.table(
  	name = c('Sedan', 'SUV'),
  	weight = veh_w
  )
  
  # # Public Charging
  # public_weights <- data.table(
  #   name = c('Public50kW','Public150kW','Public400kW'),
  #   weight = pub_w
  # )
  
  return(list(pev_weights = pev_weights,
              pref_weights = pref_weights,
              home_weights = home_weights,
              work_weights = work_weights,
  												vehicle_weights = vehicle_weights))
              #public_weights=public_weights))
}
