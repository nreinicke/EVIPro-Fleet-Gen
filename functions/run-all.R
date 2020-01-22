


# Libraries ---------------------------------------------------------------

library(future.apply)
library(ggplot2)
library(data.table)
library(magrittr)
library(foreach)
library(doFuture)
library(doParallel)

# helper
# unregister <- function() {
# 	env <- foreach:::.foreachGlobals
# 	rm(list=ls(name=env), pos=env)
# }

# Set this option to unsure correct parallelization behaviour
# This is a known issue and will be investigated at a later date
options(future.fork.enable = T)

# retireive names from list
getNames <- function(vec, option) {
	# subset list to match given option	 
	out <-	lapply(vec, function(x) {
		x[all(x == unlist(option))]
	})
	# filter out zero length lists
	return(names(Filter(length, out)))
}

# other functions
source("functions/func_openEVI.R")
source("functions/func_randVector.R")

# load data functions
source("functions/loadProfiles.R")
source("functions/loadRawData.R")

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

# Load Vectors ------------------------------------------------------------

# count of total number of PEVs
fleet_size_vec <- list(
																							1000,
																							10000,
																							50000) 

# average daily miles traveled per vehicle
mean_dvmt_vec <- c(
																			25,
																			35,
																			45) 

# Temperature
temp_vec <- c("-20C", # celsius
														"-10C",
														"0C",
														"10C",
														"20C",
														"30C",
														"40C"
) 

# PEV Type (confirm the meaning of the labels) (def named wrong)
pev_type_vec <- list(# "BEV100" = c(0, 0, .30, .70), # PHEV20, PHEV50,BEV100, BEV250; all BEV
																					"BEV" = c(.10, .15, .25, .50), # BEV Dominant
																					"PHEV" = c(.25, .50, .10, .15), # PHEV Dominant
																					"EQUAL" = c(.15, .35, .15, .35)) # PHEV/BEV Equal share

# adding the vehicle class variable
veh_class_vec <- list("Sedan" = c(0.8, 0.2), # sedan dominant
																						"Equal" = c(0.5, 0.5), # equal distribution
																						"SUV" = c(0.2, 0.8)) # suv dominant

# Geography
loc_class_vec <- list("urban")

# Day of Week (included in code)
# c(Weekday, Weekend)

# Home Access and Power 																# L1, L2, % without access to home power 
home_power_vec <- list("HA100_MostL1" = c(0.80, 0.20, 0), #100% access to home power, 80% of those L1
																							"HA100_MostL2" = c(0.20, 0.80, 0), #100% access to home power, 80% of those L2
																							"HA100_Equal" = c(0.5, 0.5, 0), #100% access to home power, 50% of those L2
																							"HA75_MostL1" = c(0.6, 0.15, 0.25), #75% access to home power, 80% of those L1
																							"HA75_MostL2" = c(0.15, 0.6, 0.25), #75% access to home power, 80% of those L2
																							"HA75_Equal" = c(0.375, 0.375, 0.25), #75% access to home power, 50% of those L2
																							"HA50_MostL1" = c(0.4, 0.1, 0.5), #50% access to home power, 80% of those L1
																							"HA50_MostL2" = c(0.1, 0.4, 0.5), #50% access to home power, 80% of those L2
																							"HA50_Equal" = c(0.25, 0.25, 0.5)) #50% access to home power, 50% of those L2

# drop all L2 cases, replace with 50/50 split
# make these an 80/20 split

# Work power
work_power_vec <- list(
	"MostL2" = c(0.2, 0.8),
	"Equal" = c(0.5, 0.5),
	"MostL1" = c(0.8, 0.2)
)

# Home work preference
pref_vec <- list(
	"Home100" = c(1.0, 0.0),
	"Home80" = c(0.8, 0.2),
	"Home60" = c(0.6, 0.4)
)

# Set Data ----------------------------------------------------------------

# create df of all vector options
all_options <- data.table(expand.grid(numveh = fleet_size_vec,
																																						pev = pev_type_vec,
																																						dvmt = mean_dvmt_vec, 
																																						home = home_power_vec, 
																																						work = work_power_vec, 
																																						loc = loc_class_vec, 
																																						vclass = veh_class_vec, 
																																						pref = pref_vec))

# add ID column to split on
all_options[, ID := seq(1:nrow(all_options))]

# Number of iterations; full run is 81648
# nrow(all_options) * 2

#Set max RAM allowed for global variables to 10GB
options(future.globals.maxSize = 25000 * 1024^2)

# Set number of workers (cores) globally
works <- 3

## testing -----------------------
temp_vec <- temp_vec[3:4]
 

# Run loop ----------------------------------------------------------------

# run loop for each temp vec
lapply(temp_vec, function(temp) {
	
	# time testing
	start <- Sys.time()
	
	# split all options list
	all_options_list <- split(all_options, all_options$ID)
	
	# load big data
	raw_data <- loadRawData(temp)
	
	# set workers
	plan(multicore, workers = works)
	
	fleet_load <- future_lapply(all_options_list, function(options_list) {
		
		# load_to_bind <- 
		fleet_sub <-
			openEVI(
				evi_raw = raw_data[[1]],
				#evi_load_profiles = evi_load_profiles,
				fleet = unlist(options_list$numveh),
				pev = unlist(options_list$pev),
				dvmt = options_list$dvmt,
				pref = unlist(options_list$pref),
				home = unlist(options_list$home),
				work = unlist(options_list$work),
				loc = options_list$loc,
				veh_class = unlist(options_list$vclass))
		
		# test out change function location	
		load_to_bind <- get_fleet_profiles(fleet_sub,
																																					unlist(options_list$numveh),
																																					raw_data[[2]])
		
		# Create Names ----------------------------------------------------------------------- 
		
		load_to_bind[, ':=' (# loc_class = options_list$loc[[1]],
			temp_c = temp,
			fleet_size = options_list$numveh[[1]],
			mean_dvmt = options_list$dvmt,
			pev_dist = getNames(pev_type_vec, options_list$pev),
			pref_dist = getNames(pref_vec, options_list$pref),
			# split home access vector, select first element, home access
			home_access_dist = regmatches(getNames(home_power_vec, options_list$home),
																												regexpr("_", getNames(home_power_vec, options_list$home)),
																												invert = T)[[1]][1],
			# split home access vector, select second element, home power distribution 
			home_power_dist = regmatches(getNames(home_power_vec, options_list$home),
																																regexpr("_", getNames(home_power_vec, options_list$home)),
																																invert = T)[[1]][2],
			work_power_dist = getNames(work_power_vec, options_list$work),
			class_dist = getNames(veh_class_vec, options_list$vclass)
		)] # end add naming columns
		

		# Summarize Data ----------------------------------------------------------

		
		# summarize data based on distinct options
		load_to_bind[time_of_day > 24,time_of_day := time_of_day - 24]
		load_to_bind <- load_to_bind[, .(kw = sum(avg_kw)),
																															by = c(# "loc_class",
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
																																)]
		setkey(load_to_bind,
									#loc_class,
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
	}) # end of future apply
	
	# bind results together
	fleet_load <- rbindlist(fleet_load)
	
	# remove large objects
	#gc(rm(raw_data))
	
	# Change data types --------------------------------------------------------------------------------- 
	#change temp, fleet size, dvmt to integer
	fleet_load[, temp_c := as.integer(gsub("C", "", temp_c))]
	
	# convert other columns to integer
	fleet_load[, fleet_size := as.integer(fleet_size)]
	fleet_load[, mean_dvmt := as.integer(mean_dvmt)]
	
	
	# Save output ----------------------------------------------------------------------------------------
	#Write fleet_load out to disk
	fwrite(fleet_load,
								file = paste0("outputs/2020_jan_results/",
																						gsub("-", "", Sys.Date()), "_", # date the run
																						temp,
																						".csv"))
	
	print(Sys.time() - start)
	# gc(rm(raw_data) full = T)
	
}) # end of temp vec lapply

# This function restarts the R session and can be used to clear memory
# Cannot be used within any type of looping function
.rs.restartR()
