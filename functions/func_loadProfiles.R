	
loadProfiles <- function(temp) {
	
	evi_load_profiles_sedan <- readRDS(paste0("input/preprocessed/load_profile_sedan/", temp, ".rds"))
	
	# add vehicle class
	evi_load_profiles_sedan[, "vehicle_class" := "Sedan"]
	
	# Repeat for SUVs
	evi_load_profiles_suv <- readRDS(paste0("input/preprocessed/load_profile_suv/", temp, ".rds"))
	
	# add vehicle class
	evi_load_profiles_suv[, "vehicle_class" := "SUV"]
	
	# add one hundred million to unique vid to distinguish SUV class of vehicle
	evi_load_profiles_suv[, unique_vid := unique_vid + 100000000]
	
	# bind together into a single data.table
	evi_load_profiles_dt <- rbindlist(list(evi_load_profiles, evi_load_profiles_suv))
	
	return(evi_load_profiles_dt)
	
}
