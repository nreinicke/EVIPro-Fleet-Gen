	
loadProfiles <- function(temp) {
	
	evi_load_profiles <- readRDS(paste0("../../data/preprocessed/load_profile/", temp, ".rds"))
	
	# add vehicle class
	evi_load_profiles[, "vehicle_class" := "sedan"]
	
	# Repeat for SUVs
	evi_load_profiles_suv <- readRDS(paste0("../../data/preprocessed/load_profile/", temp, ".rds"))
	
	# add vehicle class
	evi_load_profiles_suv[, "vehicle_class" := "SUV"]
	
	# add one hundred million to unique vid to distinguish SUV class of vehicle
	evi_load_profiles_suv[, unique_vid := unique_vid + 100000000]
	
	# create empty data.table
	#evi_load_profiles_dt <- data.table()
	
	# bind together into a single data.table
	# evi_load_profiles_dt <- rbind(evi_load_profiles_suv,
	# 																											evi_load_profiles)
	evi_load_profiles_dt <- rbindlist(list(evi_load_profiles, evi_load_profiles_suv))
	
	
	# gc(rm(evi_load_profiles, evi_load_profiles_suv))
	
	return(evi_load_profiles_dt)
	
}
