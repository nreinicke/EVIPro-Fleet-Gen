
maxDelay <- function(activity_data, index) {
  
  # adjust the starting charge time to the late charge time
  activity_data$start_time[index] <- activity_data$start_time_late[index]
  
  # adjust the end charge time to the end of the park time
  activity_data$end_time_chg[index] <- activity_data$end_time_prk[index] 
  
  return(activity_data)
}

loadLeveling <- function(activity_data, index) {
  
  # adjust the end charge time to equal the end of parking for all values in the given index
  activity_data$end_time_chg[index] <- activity_data$end_time_prk[index]
  
  # calculate the average kw needed to charge the vehicle over the dwell time
  activity_data$avg_kw[index] <- activity_data$kwh[index]/((activity_data$end_time_chg[index] - activity_data$start_time[index]) * 24)
  
  return(activity_data)
}

timedCharging <- function(activity_data, index, desired_time) {
  
  # if the desired time allows the vehicle to achieve a full charge in the given
  # dwell time, adjust the starting time and ending time
  # first, calculate the time needed to charge for all values in the index
  activity_data$new_end_time_chg[index] <- desired_time + (activity_data$end_time_chg[index] - activity_data$start_time[index])
  
  # constrain the index further by specifying that the new charge end must come before the end of parking 
  # and the desired time is greater than the starting charge time
  index <- index & (activity_data$new_end_time_chg <= activity_data$end_time_prk) & (desired_time > activity_data$start_time)
  
  # with the updated index, valid events will be updated to the new desired charge start time and new end charge time
  activity_data$start_time[index] <- desired_time
  activity_data$end_time_chg[index] <- activity_data$new_end_time_chg[index]
  
  # remove new end time charge column
  # activity_data <- subset(activity_data, select = -c(new_end_time_chg))
  return(activity_data)
}

loadShift <- function(activity_data, work_strategy, home_strategy, desired_time) {
  # default value for desired time with respect to the time shift strategy is 1.0
  if(missing(desired_time)) {
    desired_time <- 1.0
  }
  # create logical vectors to identify which events to alter depending on destination type
  work_index <- activity_data$dest_type=='Work'
  home_index <- activity_data$dest_type=='Home'
    
  # mutate data according to the desired work strategy
  if(work_strategy == 'min_delay') {
    print("work locations shifted using MIN DELAY")
  }
  else if(work_strategy == 'max_delay') {
    activity_data <- maxDelay(activity_data, work_index)
    print("work locations shifted using MAX DELAY")
  }
  else if(work_strategy == 'load_leveling') {
    activity_data <- loadLeveling(activity_data, work_index)
    print("work locations shifted using LOAD LEVELING")
  }
  else if(work_strategy == 'timed_charging') {
    activity_data <- timedCharging(activity_data, work_index, desired_time)
    print(paste0("work locations shifted using TIMED CHARGING with desired time: ", desired_time))
  }
  else {
    print("public load shift parameter not understood. using min delay as default")
  }
  
  # mutate the data according to the desired home strategy
  if(home_strategy == 'min_delay') {
    print("home locations shifted using MIN DELAY")
  }
  else if(home_strategy == 'max_delay') {
    activity_data <- maxDelay(activity_data, home_index)
    print("home locations shifted using MAX DELAY")
  }
  else if(home_strategy == 'load_leveling') {
    activity_data <- loadLeveling(activity_data, home_index)
    print("home locations shifted using LOAD LEVELING")
  }
  else if(home_strategy == 'timed_charging') {
    activity_data <- timedCharging(activity_data, home_index, desired_time)
    print(paste0("home locations shifted using TIMED CHARGING with desired time: ", desired_time))
  }
  else {
    print("home load shift parameter not understood. using min delay as default")
  }
  
  return(activity_data)
}