# Author: Schatz Energy Research Center
# Original Version: Jerome Carman
# Edits: Jerome Carman and/or Daug Saucedo and/or Andy Harris and/or Micah Wright
# Version: 2.1
# Date: March 7, 2019
# Description: Loads a data table containing EVIPro model data from NREL, applies user defined weights of fleet characteristics, and generates a fleet of vids
# Required Variables
#   evi_raw: data table created using the loadEVIPro() function. Must have the following columns
#            "schedule_vmt_bin","power_public","power_work","power_home","preferred_loc","pev_type","day_of_week","vid"
#            Values in these columns MUST equal the values in the "name" column in the weights data tables listed below
#   fleet_size: integer specifying the size of the desired fleet
#   weights: list of data.tables with two columns ("name" and "weight") and rows containing names and associated decimals (with a sum of 1) that represent the fraction of fleet vehicles comprised of each variable in the name column.
#   bin_width: width in miles of dvmt distribution. Must be the same bin width used in preprocess_NREL_data() function
#   loc_class: character, either "urban" or "rural"
# Version History
#   1.1: JKC added the "rbind" approach to creating the weighted fleet, as opposed to the binary search appraoch. This creates a much closer
#         match to the desired fleet characteristics weights at the expense of not matching the fleet size exactly.
#   1.2: JKC added vmt_weights. Also added if statement to choose between "rbind" and binary search fleet creation approach depending on the
#         error between the resulting fleet size and the target fleet size.
#   1.3: JKC addressed issue with returning NA when looking for a vid in evi[] that matched all vehicle characteristics
#   1.4: JKC added comments, confirmed that use of vmt_weights applies to total daily vmt, not electric vmt.
#   1.5: JKC added use of temp_weights to accommodate new data set that varies by ambient temperature
#   2.0: MW - see GIT history for changes. One big change is we no longer allow specifying both pev_weights and vmt_weights. This is because
#         we were getting too many NA values, particularly in edge cases. We decided this was pushing the boundary of how flexible this dataset is.
#        NOTE: no longer has legacy support for older scripts
#   2.1: JKC added pev_type back in. Dealing with NAs. Removed public_weights.

####################### Begin ##################################

evi_fleetGen <- function(evi_raw,
                         fleet_size,
                         weights,
                         mean_vmt = 40,
                         bin_width = 10,
                         loc_class = "urban") {
  
  ###############################################################################################################################_#
  #Create a data table of all potential permutations of fleet characteristics whose weights are user defined####
  ###############################################################################################################################_#

  # estimate vmt weights for the day of week
  # paste day of week onto name column, which aslo casts name to character 
  vmt_list <- sapply(c("weekday", "weekend"), simplify = FALSE, USE.NAMES = TRUE, function(day) {
    vmt_wt <- vmt_WeightDistGen(mean_vmt, 
                                max_vmt = max(evi_raw$schedule_vmt, na.rm = TRUE),
                                bin_width, 
                                loc_class, 
                                day) 
    
    vmt_wt[, name := paste(day, name, sep = "_")]
  })
  
  # Bind weekday and weekend vmt weights
  vmt_wt <- rbindlist(vmt_list)
  
  # add vmt weights to other weights
  t_weights <- c(weights, list("vmt_weights" = vmt_wt))
  
  # data table of all weights and groups combined
  all_weights <- rbindlist(t_weights)

  # Use expand.grid to create a data table of all permutations of groups to consider
  all_perms <- as.data.table(expand.grid(lapply(t_weights[c("pev_weights",
                                                            "pref_weights",
                                                            "home_weights",
                                                            "work_weights",
                                                            "vmt_weights",
                                                            "vehicle_weights")],
                                                function(x) x[, name])))
  
  # specify column names
  colnames(all_perms) <- c("pev_type",
                           "preferred_loc",
                           "power_home",
                           "power_work",
                           "schedule_vmt_bin",
                           "class_type")  
  
  #Calculate total weight for each permutation of groups
  # Iteratively join all_weights to all_perms, calculate the total weight for each permutation, then remove the joined weight column.
  setkey(all_weights, name) # sorting function (DT, key) for efficiently sorting data for other functions
  
  all_perms_names <- colnames(all_perms)
  all_perms[, stat_weight := 1] # mutate column / give column value 1
 
  for(i in 1:length(all_perms_names)) {
    setkeyv(all_perms, all_perms_names[[i]])
    all_perms <- all_weights[all_perms]
    all_perms[, stat_weight := stat_weight * weight][, weight := NULL]
    setnames(all_perms, "name", all_perms_names[[j]])
  }
  
  #Factor values in each column. The corresponding factor numeric values correspond to the integer values used by NREL's
  #     file naming convention.
  # We don't factor schedule_vmt_bin (if created) as this will be re-cast back to integer
  all_perms[, power_work := factor(power_work, levels=c("WorkL1","WorkL2"))]
  all_perms[, power_home := factor(power_home, levels=c("HomeL1","HomeL2","HomeNone"))]
  all_perms[, preferred_loc := factor(preferred_loc, levels=c("PrefHome","PrefWork"))]
  all_perms[, pev_type := factor(pev_type, levels=c("PHEV20","PHEV50","BEV100","BEV250"))]

  # factor the vehicle class
	all_perms[, class_type := factor(class_type, levels = c("Sedan","SUV"))]
		
  ################################################################################################################################
  #Create a fleet data table where each row is the combined characteristics for each vehicle in the fleet.
  ################################################################################################################################
  
  #Create a fleet where each row is associated with a vehicle. Aggregated distribution matches weights - the number of
  # vehicles matching a particular description = fleet size * stat_weight; so if the combined weight for a vmt/public power/
  # work power/home power/preference/pev type combinatuion is 0.02 and the fleet size is 100, there will be 2 vehciles
  # in the fleet with that specific combination.
  # Fleet size may be slightly off for large fleets, and substantially off for small fleets when
  #     coupled with small stat_weight values because round(stat_weight*fleet_size,0) will return a lot of zeros.
  fleet <- all_perms[stat_weight!=0,
              do.call("rbind", replicate(round(stat_weight * fleet_size,0),.SD,simplify=FALSE)),
              by = c("power_work",
                     "power_home",
                     "preferred_loc",
                     "pev_type",
                     "schedule_vmt_bin",
                     "class_type")]
  
  # partition schedule_vmt_bin to actual mileage bins and day of week indicators
  # recast schedule_vmt_bin to integer
  fleet[, ':=' (schedule_vmt_bin = as.integer(sub(".*\\_", "",schedule_vmt_bin)),
                day_of_week = sub("\\_.*", "",  schedule_vmt_bin))]
  fleet[,day_of_week:=factor(day_of_week, levels = c("weekday", "weekend"))]
  
  #If the difference between the resulting fleet size and the target fleet size is larger than 0.1%,
  #   apply correction to obtain target fleet size at the expense of attaining exact distribution of fleet characteristics
  fleet_size_error <- sapply(c("weekday", "weekend"), simplify = TRUE, USE.NAMES = TRUE, function(i) {
    abs((nrow(fleet[day_of_week == i])-fleet_size)/fleet_size)
  })

  # check for fleet size errors and correct
  if( max(fleet_size_error) > 0.001 ) {
  
    # Print warning if fleet size error is egregious. This means distribution of fleet characteristics may be
    #   significantly different than what was specified.
  	if(any(fleet_size_error > .1)) {
  		warning(paste0("Warning: fleet size error of ",as.character(fleet_size_error),". Updating..."))
  	}
    
    updated_fleet <- lapply(c("weekday", "weekend"), function(day){
      
      #If too small, add vehicles by randomly duplicating existing vehicles in fleet[]
      if(nrow(fleet[day_of_week == day]) < fleet_size ) {
        index <- fleet[day_of_week == day, sample(.I, fleet_size - .N)]
        fleet <- rbind(fleet[day_of_week == day], fleet[day_of_week == day][index])
        
      #If too large, randomly delete vehicles from fleet[]
      } else if(nrow(fleet[day_of_week == i]) > fleet_size ) {
        index <- fleet[day_of_week == i, sample(.I, .N - fleet_size)]
        fleet <- fleet[day_of_week == i][-index]
      }
    })
   
    # recombine weekend and weekday fleets
    fleet <- rbindlist(updated_fleet) 
  }
  
  ###############################################################################################################################_#
  #Identify vid that matches each row of characteristics in the fleet data table. Merge in charge session data ####
  ###############################################################################################################################_#
  
  # Randomly pull and append a vid that has the characteristics specified in the fleet data table.
  # Identify the subset of vids in evi that are associated with each row in your fleet. Randomly pull one of these vids
  #     and associate it with each entry in your fleet. You now have a list of vids equal in size to your fleet.
  # Note that the number of unique vids may be less than the fleet size. This is because one vid can apply to more than
  #     one group characteristic permutation
  setkeyv(evi_raw, c("day_of_week",
                     "power_work",
                     "power_home",
                     "preferred_loc",
                     "pev_type",
                     "schedule_vmt_bin",
                     "class_type"))
  
  setkeyv(fleet, c("day_of_week",
                   "power_work",
                   "power_home",
                   "preferred_loc",
                   "pev_type",
                   "schedule_vmt_bin",
                   "class_type"))
  
  # Add VIDs to fleet[]
  # stat weight gets dropped here
  fleet <- evi_raw[fleet,sample(unique_vid,1,replace=TRUE),by=.EACHI] #with replacement
  setnames(fleet,"V1","unique_vid")
  
  # Create a specific fleet ID number for each vehicle in the fleet 
  # unique_vid value can be chosen more than once 
  # so cannot be relied upon to be a truly unique identifier for each vehicle.
  fleet[,fleet_id:=1:.N]
  
  # Check for and remove NA rows
  # NAs may occur if there is no vehicle which matches the specified weights
  if(nrow(fleet[is.na(unique_vid)]) > 0 ) {
    warning(paste0("NAs found. Removing ",as.character(nrow(fleet[is.na(unique_vid)]))," vehicles."))
    fleet <- fleet[!is.na(unique_vid),.SD]
  }
  
  # Create the final charging activity itinerary for the full fleet. This pulls all charging events for each unique_vid.
  # Note: when using vmt_weights, this only works if there are matching labels for evi_raw[,schedule_vmt_bin] and fleet[,schedule_vmt_bin]. This
  #     only makes sense if the bin widths used for the two data tables are equal.
  setkeyv(evi_raw,c("day_of_week","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin", "class_type", "unique_vid"))
  setkeyv(fleet,c("day_of_week","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin", "class_type", "unique_vid"))
  
  # Merge charge events with fleet
  # Getting duplicate fleet_ids on this, this is due to multiple charging events in a given day by the same vehicle
  fleet_activity <- evi_raw[fleet]
  
  #Return results
  return(fleet_activity)
  
}


