# Author: Schatz Energy Research Center
# Original Version: Micah Wright (pieced together from older code from Jerome and Doug)
# Date: February 22, 2019
# Description: generates fleet-specific load profiles. Returns 48-hour profile.
# Variables
#   fleet: fleet data table, generated in evi_fleetGen
#   fleet_size: fleet size, integer
#   load_profile: load profile for entire data set, generated in calcBaseEVILoad

get_fleet_profiles <- function(fleet, fleet_size, load_profile) {
  
  # calculate fleet size scale
  if(fleet_size > 50000) {
    fleet_size_scale <- fleet_size / 50000
    fleet_size <- 50000
  } else {
    fleet_size_scale <- 1
  }
  
  #Build list of unique_vid from evi_fleet that we'll use to construct the full load profile
  id_key <- fleet$data[,.(unique_vid = unique(unique_vid)), by = fleet_id]
  setkey(id_key, unique_vid)
  
  #Subset load profiles specific to those unique_vids in the evi_fleet. This is the full load profile of the fleet
  setkey(load_profile, unique_vid, session_id)
  fleet_load_profiles <- load_profile[unique_vid %in% fleet$data[, unique(unique_vid)]] #Subset to those unique_vids we are interested in
  fleet_load_profiles <- id_key[fleet_load_profiles, allow.cartesian = TRUE] #Capture duplicate unique_vids by using fleet_id
  
  #Add day_of_week information
  weekday_ref <- fleet$data[!duplicated(unique_vid), day_of_week, by = unique_vid]
  setkey(weekday_ref, unique_vid)
  setkey(fleet_load_profiles, unique_vid)
  fleet_load_profiles <- merge(weekday_ref, fleet_load_profiles, by = "unique_vid", all.y = TRUE)
  
  #Scale load profile by fleet size scale factor.
  # The approach is a straight multiplier on the kW of those vehicles in fleet_load_profiles. This does not increase the number of fleet
  #   vehicles. For example, if the desired fleet size is 100,000, then there is a fleet of 50,000 and the kW associated with each charge
  #   event is increased by 100,000 / 50,000 = 2.
  # The variables schedule_vmt and kwh are also scaled.
  fleet_load_profiles[, schedule_vmt := schedule_vmt * fleet_size_scale][,avg_kw := avg_kw * fleet_size_scale][,kwh := kwh * fleet_size_scale]
  
  return(fleet_load_profiles)
  
}

