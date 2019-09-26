# Pre-requisite functions
source(paste0("/media/spin/projects/sein-evi/",'source/common/func_LoadShapes_avg.R'))
source(paste0("/media/spin/projects/sein-evi/",'source/common/func_generateLoadProfiles.R'))
#**************************************************************************************************
# FUNCTION calc.BESS1
#
# VERSION:
# The following function will accept EVI-Pro or BEAM formatted session data
# and a data.table containing the target power shape over a 24 hour 
# period. The target profile shape is scaled to the fleet charge energy
# demand
#**************************************************************************************************
calc.BESS1 <- function(sessions,target_shape){
  result <- list(
    description = 'Unmanaged Fleet + BESS Control Trajectories for Forecasted Charging and Target demands'
  )
  # Define the fleet 24 hour early load profile
  result$fleet <- generateLoadProfiles(sessions,0,48,time_step,late_profile = FALSE)
  
  # Scale the target shape to the fleet charge energy demanded
  result$target <- list(
    data = data.table(time_of_day = target_shape$raw[,time_of_day]),
    energy = result$fleet$daily$early$nrg_cons,
    scale = NULL
  )
  result$target$scale <- result$target$energy/target_shape$energy
  result$target$data[,power := result$target$scale * (target_shape$raw[,power])]
  result$target$data[,cumu.nrg := cumsum(power)*time_step]
  
  # calculate the BESS1 power dynamics to meet the target profile
  result$control <- list(data = data.table(time_of_day = result$target$data[,time_of_day]))
  # BESS1 power seeks to be the difference between the target profile and the fleet charge demand so that the site's net demand looks like
  # the scaled CAISO profile.
  result$control$data[
    ,power := result$target$data[,power] - result$fleet$daily$early$data[,power]
    ]
  result$control$data[,cumu_nrg := cumsum(power)*time_step]
  # Collect Data on the BESS control strategy
  result$power$charge     <- result$control$data[power > 0,max(power)]
  result$power$discharge  <- result$control$data[power < 0,min(power)]
  result$energy$charge    <- result$control$data[power > 0,sum(power)*time_step]
  result$energy$discharge <- result$control$data[power < 0,sum(power)*time_step]
  return(result)
}