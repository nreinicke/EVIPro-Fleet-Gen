loadRawData <- function(temp, sedan_raw_dir, suv_raw_dir, sedan_lp_dir, suv_lp_dir) {
          
          print('Memory totals before loading data:')
          print(gc())
					
					# Get EVI-Pro charge session data -------------------------------------------------------
					evi_raw_sedan <- readRDS(paste0(sedan_raw_dir, temp, ".rds"))
					# add column for vehicle class
					evi_raw_sedan[, "class_type" := "Sedan"]
					
					print('Memory totals after loading Sedan Raw data:')
					print(gc())
					
					# Repeat for SUVs
					evi_raw_suv <- readRDS(paste0(suv_raw_dir, temp, ".rds"))
					
					# add column for vehicle class
					evi_raw_suv[, "class_type" := "SUV"]
					
					print('Memory totals after loading SUV Raw data:')
					print(gc())
					
					# add one hundred million to the unique vids to distinguish from sedan data
					evi_raw_suv[, unique_vid := unique_vid + 100000000]
				
					# Bind sedan and suv data into a list
					evi_raw_dt <- rbindlist(list(evi_raw_sedan, evi_raw_suv))
					
					print('Memory totals after binding Sedan data and SUV data:')
					print(gc())
					
					#Factor class_type
					evi_raw_dt$class_type <- factor(evi_raw_dt$class_type,levels=c("Sedan","SUV"))
				
					#	Garbage collection
					gc(rm(evi_raw_suv, evi_raw_sedan))
					print('Memory totals after garbage collection:')
					print(gc())
					
					# Get pre-calculated load profiles --------------------------------------------------------
					evi_load_profiles <- readRDS(paste0(sedan_lp_dir, temp, ".rds"))

					# add vehicle class
					evi_load_profiles[, "class_type" := "Sedan"]
					
					print('Memory totals after loading Sedan load profiles:')
					print(gc())

					# Repeat for SUVs
					evi_load_profiles_suv <- readRDS(paste0(suv_lp_dir, temp, ".rds"))

					# add vehicle class
					evi_load_profiles_suv[, "class_type" := "SUV"]
					
					print('Memory totals after loading SUV load profiles:')
					print(gc())

					# add one hundred million to unique vid to distinguish SUV class of vehicle
					evi_load_profiles_suv[, unique_vid := unique_vid + 100000000]

					# Bind sedan and suv data into a list
					evi_load_profiles_dt <- rbindlist(list(evi_load_profiles, evi_load_profiles_suv))
					
					print('Memory totals after binding Sedan and SUV load profiles:')
					print(gc())
					
					#Factor class_type
					evi_load_profiles_dt$class_type <- factor(evi_load_profiles_dt$class_type,levels=c("Sedan","SUV"))
					
					#Garbage collection
				  gc(rm(evi_load_profiles, evi_load_profiles_suv))
				  
				  print('Memory totals after garbage collection:')
				  print(gc())

				  # Return loaded data ----------------------------------------------------------------------
					return(list(evi_raw_dt, evi_load_profiles_dt))
}
