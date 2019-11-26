loadRawData <- function(temp) {
					evi_raw <- readRDS(paste0("../../data/preprocessed/evi_raw/", temp, ".rds"))
					# add column for vehicle class
					evi_raw[, "vehicle_class" := "Sedan"]
					
					# Repeat for SUVs
					evi_raw_suv <- readRDS(paste0("../../data/preprocessed/evi_raw_suv/", temp, ".rds"))
					
					# add column for vehicle class
					evi_raw_suv[, "vehicle_class" := "SUV"]
					
					# add one hundred million to the unique vids to distinguish from sedan data
					evi_raw_suv[, unique_vid := unique_vid + 100000000]
					
					# create empty data.table
					# evi_raw_dt <- data.table()
					
					# bind together into a single data.table
				 #	evi_raw_dt <- rbind(evi_raw_suv,evi_raw)
				
					# list approach
					evi_raw_dt <- rbindlist(list(evi_raw, evi_raw_suv))
				
				#	return(evi_raw_dt)
				gc(rm(evi_raw_suv, evi_raw))
				# load the load profiles
				# loadprof_data_path_sedan <- paste0("../../data/preprocessed/load_profile/", temp_vec[i], ".rds")
				# evi_load_profiles <- readRDS(loadprof_data_path_sedan)
				
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

					return(list(evi_raw_dt, evi_load_profiles_dt))
}
