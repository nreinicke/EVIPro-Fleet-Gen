###################################################################################################
# DPP Functions using Session Methods
# Version 2019-01-04 
## Holy fuck, here we go.
#============================
require(svMisc) # for progress bars and reporting
###################################################################################################
# FUNCTION DPP Driver
##
###################################################################################################
dpp.driver <- function(fleet,target,time_step){
  # The fleet is now based on the data level.
  ## Need to determine the last time step.
  # Normalize data so that all times start at 0 and runs up to hour 48.
  fleet[,start_time := (start_time - target.day)*24]
  fleet[,start_time_late := (start_time_late - target.day)*24]
  fleet[,end_time_chg := (end_time_chg - target.day)*24]
  fleet[,end_time_prk := (end_time_prk - target.day)*24]
  fleet[,session_index := 1:.N]
  
  dpp <- list()
  # Build a global DPP table
  # Global Manually Initiate the Terminal Stage
  istage <- 48/time_step+1
  dpp$global$stages[[istage]] <- stage.init()
  dpp$global$stages[[istage]]$stage.return = 0
  dpp$global$stages[[istage]]$return.function = 0
  
  # Build the session DPP table
  message('dpp.driver: Evaluating the DPP for all sessions ...\n')
  nsession <- fleet[,.N]
  for(isession in 1:nsession){
    # process a session
    progress(value = irow, max.value = nfleet)
    # Create a list to contain session meta-data and controlled session dynamics
    dpp$sessions[[isession]] <- session.init(
      session = fleet[isession,.SD],
      target = target,
      time_step = time_step
      )
    # -----------------------------------------------------------------------------------
    # Manually Initiate the Terminal Stage
    istage <- 48/time_step+1
    dpp$sessions[[isession]]$stages[[istage]] <- stage.init()
    dpp$sessions[[isession]]$stages[[istage]]$stage.table[2,decision := 1]
    dpp$sessions[[isession]]$stages[[istage]]$decision <- dpp$sessions[[isession]]$stages[[istage]]$stage.table[decision == 1,states]
    dpp$sessions[[isession]]$stages[[istage]]$stage.return = 0.
    dpp$sessions[[isession]]$stages[[istage]]$return.function = 0.
    
    # -----------------------------------------------------------------------------------
    # Run through the rest of the stages
    for(istage in seq(from = 48/time_step, to = 1, by = -1)){
      # Build the stage table for the current stage and determine the optimal decision
      dpp$sessions[[isession]]$stages[[istage]] <- stage.return(
        session = dpp$sessions[[isession]],
        next_stage = dpp$sessions[[isession]]$stages[[istage+1]],
        istage,
        time_step
      )
      # Map the results of the stage into the session dynamics table.
      dpp$sessions[[isession]] <- session.update(istage,dpp$sessions[[isession]])
    }
  }
return(dpp)
}
###################################################################################################
# FUNCTION Update Session States
## Once a preferred state is found from the DPP, update the session state table to reflect the
## decision
###################################################################################################
session.update <- function(istage,session,time_step){
  session$data[
    istage, 
    power := session$stages[[istage]]$session.power 
    ]
  session$data[
    istage, 
    energy := session$data[istage+1,energy] - 
      session$stages[[istage]]$session.power*session$data[istage,time_active]
    ]
  return(session)
}

###################################################################################################
## FUNCTION Session Init
# Create a function that initializes the state table for the session
# For a given session and target profile shape, define the early and late charge power and energy
# limits for the session over a 48 period along with the 48 hour target series that is only active
# during the park time.
###################################################################################################
session.init <- function(session,target,time_step){
  result <- list(
    session.index = session[,session_index],
    row.ID = session[,row_id],
    session.ID = session[,sessionId],
    vid = session[,vid],  # There may be a conflict here on EVI (unique_vid) versus BEAM (vid)
    start_time = session[,start_time],
    start_time_late = session[,start_time_late],
    end_time_chg = session[,end_time_chg],
    end_time_prk = session[,end_time_prk],
    # while the session is split between the two 24 hour period, force the late power profile
    #  in the 2nd 24 hour period
    forced.late = NULL, #session[,start_time < 24. & end_time_prk>= 24.],
    avg.kW = session[,avg_kw],
    kWh = session[,kwh],
    data = data.table(
      time_of_day = seq(from = 0, to = 48-time_step,by = time_step),
      power = 0,
      energy = 0
    )
  )
  # ===============================================================================================
  # Early Power Profile
  # Case 1: Whole Time-Steps
  result$data[, early.power := 0]
  result$data[result$start_time <= time_of_day & time_of_day <= result$end_time_chg, early.power := result$avg.kW]
  # Case 2: Start of Time-Step
  ## (a) start time is less than or equal to the the time-step
  ## (b) end time is not greater than the next time step but is greater than the current time step
  result$data[
      (result$start_time <= time_of_day)&
      !(time_of_day+time_step < result$end_time_chg)&
        (time_of_day < result$end_time_chg),
    early.power := result$avg.kW*(result$end_time_chg-time_of_day)/time_step
    ]
  result$data[result$start_time > time_of_day | time_of_day > result$end_time_prk, power := 0]
  # Case 3: Within Time-Step 
  ## (a) start time is greater than the time but less than the next time
  ## (b) end time is not greater than the next time step but is greater than the current time step
  result$data[
    !(result$start_time <= time_of_day)&
    (result$start_time < time_of_day+time_step)&
      !(time_of_day+time_step < result$end_time_chg)&
      (time_of_day < result$end_time_chg),
    early.power := result$avg.kW*(result$end_time_chg-result$start_time)/time_step
    ]
  # Case 4: End of Time-Step
  ## (a) start time is not less than or equal to the time-step but is less than or equal to the next time-step
  ## (b) end time is greater than or equal to the next time step
  result$data[
    !(result$start_time <= time_of_day)&
      (result$start_time < time_of_day+time_step)&
      (time_of_day+time_step < result$end_time_chg),
    early.power := result$avg.kW*(time_of_day+time_step-result$start_time)/time_step
    ]
  result$data[, early.energy := cumsum(early.power)*time_step]
  # ----------------------------------------------------------------------------------------------
  # late power limit
  result$data[,late.power := 0]
  # Case 1: Whole Time-Steps
  result$data[result$start_time_late <= time_of_day & time_of_day <= result$end_time_prk, late.power := result$avg.kW]
  # Case 2: Start of Time-Step
  ## (a) start time is less than or equal to the the time-step
  ## (b) end time is not greater than the next time step but is greater than the current time step
  result$data[
    (result$start_time_late <= time_of_day)&
      !(time_of_day+time_step < result$end_time_prk)&
      (time_of_day < result$end_time_prk),
    late.power := result$avg.kW*(result$end_time_prk-time_of_day)/time_step
    ]
  # Case 3: Within Time-Step 
  ## (a) start time is greater than the time but less than the next time
  ## (b) end time is not greater than the next time step but is greater than the current time step
  result$data[
    !(result$start_time_late <= time_of_day)&
      (result$start_time_late < time_of_day+time_step)&
      !(time_of_day+time_step < result$end_time_prk)&
      (time_of_day < result$end_time_prk),
    late.power := result$avg.kW*(result$end_time_prk-result$start_time_late)/time_step
    ]
  # Case 4: End of Time-Step
  ## (a) start time is not less than or equal to the time-step but is less than or equal to the next time-step
  ## (b) end time is greater than or equal to the next time step
  result$data[
    !(result$start_time_late <= time_of_day)&
      (result$start_time_late < time_of_day+time_step)&
      (time_of_day+time_step < result$end_time_prk),
    late.power := result$avg.kW*(time_of_day+time_step-result$start_time_late)/time_step
    ]
  result$data[,late.energy := cumsum(late.power)*time_step]
  
  # ===============================================================================================
  # Design the target load shape for the session
  # duplicate the target load shape over the 48 hour time-period
  result$data[,target := rbind(target[,.(power)],target[,.(power)]) ]
  # mark all time_of_day stages outside the sessions parking window as zero targets
  result$data[
    !(floor(result$start_time) < time_of_day & time_of_day < ceil(result$end_time_prk)), 
    target := 0
    ]
  # scale the remaining power profile by charge energy demanded in the time-period
  load_nrg <- result$data[target > 0,sum(target)*time_step]
  result$data[,target := target*(result$kWh/load_nrg)]
  
  # ===============================================================================================
  # Define the terminal state
  result$data <- rbind(
    result$data[,.SD],
    data.table(
      time_of_day = 48.,
      power = 0.,
      energy = result$kWh,
      early.power = 0.,
      early.energy = result$data[.N-1,early.energy],
      late.power = 0.,
      late.energy = result$data[.N-1,late.energy],
      target = 0
    )
  )
  
  # ==============================================================================================
  # Add a time-step fraction metric.
  # Since the session is active, calculate fraction of the time-step the session is active.
  result$data[,time_active := 0]
  # All time-step
  result$data[
    (result$start_time <= time_of_day) & (time_of_day+time_step <= result$end_time_prk),
    time_active := time_step
    ]
  # Beginning of time-Step
  result$data[
    (result$start_time <= time_of_day) & 
      (time_of_day < result$end_time_prk)&
      (time_of_day+time_step > result$end_time_prk),
    time_active:= (result$end_time_prk - time_of_day)
    ]
  # End of the time-step
  result$data[
    result$start_time > time_of_day & 
      result$start_time < time_of_day + time_step & 
      time_of_day+time_step < result$end_time_prk,
    time_active:= (time_of_day+time_step - result$start_time)
    ]
  # Within the time-step
  result$data[
    result$start_time > time_of_day & 
      time_of_day+time_step > result$end_time_prk,
    time_active:= (result$end_time_prk - result$start_time)
    ]
  
  # ==============================================================================================
  return(result)
}
###################################################################################################
# FUNCTION Stage Init
## For a fleet and a given stage index, 
### Build a table representing the available state space for the stage. 
#### The table will contain all possible, and feasible, charger state combinations.
#### Session Indices will be used along the rows and scenarios being along the columns. The stage.return
#### for a given scenario would then be a weighted sum for a given scenario's column.
####
###
###################################################################################################
stage.init <- function(){
  return( 
    list(
      stage.table = data.table(
        states = factor(c('Charge','Idle')),
        decision = c(0,0),
        stage.return = c(0,0),
        return.function = c(0,0)
      ),
      flexible = FALSE,
      decision = NULL,
      bess.power = NULL,
      session.power = NULL,
      target.power = NULL,
      target.nrg = NULL,
      early.power = NULL,
      early.nrg = NULL,
      late.power = NULL,
      late.nrg = NULL,
      env.lower.nrg  = NULL,
      env.upper.nrg  = NULL,
      env.lower.power = NULL,
      env.upper.power = NULL,
      stage.return = NULL,     # change in BESS2 energy
      return.function = NULL   # net change in BESS2 energy
    )
  )
}

###################################################################################################
## Stage Return: v 0.0.1
# Create a function that evaluates the stage return for states
# fleet is a list of load profile lists that contain the data.table for early and late time-series
#  The list can be found from the beam list object beam$fleet$samples[[ifleet]]$load_profiles
# target is the target list found in the CAISO list object
###################################################################################################
stage.return <- function(session,next_stage,istage,time_step){
  result <- stage.init()
  result$time_of_day <- session$data[istage,time_of_day]
  # Calculate the time the session is schedule within the stage
  result$time_active <- session$data[istage,time_active]
  # Calculate the maximum time the session can be active within the stage
  result$time_charge <- min(
    session$data[istage+1,energy]/session$avg.kW,
    result$time_active
    )  # hours
  
  # Target Power for the Stage
  result$target.power <- session$data[istage,target]
  # Energy Limits ofr the Stage
  result$early.energy <- session$data[istage,early.energy]
  result$late.energy <- session$data[istage,late.energy]
    # =============================================================================================
  # Is the session active?
  active_test <- (result$time_charge > 0)
  # If the session is not active return early
  if(!active_test){
    result$flexible=FALSE
    # Fill out the dpp table
    ## (1) For each state, determine the stage.return and the return function
    ### Invoke the constraints on the charge and discharge power to maintain fleet charging and target power
    
    ## (2) Based on the return function values, select the optimal state for the stage and 
    ##   record the return.function
    decide <- 2
    result$stage.table[decide,decision:=1]
    result$session.power <- 0
    result$bess.power <- result$target.power*result$time_active/time_step
    result$stage.return <- result$bess.power*time_step
    result$return.function <- result$stage.return + next.stage$return.function
    return(result)
  }
  
  # =============================================================================================
  # The session is active, now what?
  # Is this stage flexible?
  print(paste0('istage = ',istage,' Avg. Power = ',session$data[istage+1,energy/session$data[1:istage,sum(time_active)]] ))
  flexible_test <- (
      # If the energy state in the current stage will be below the 48 hour early energy limit without charge
      # power during the stage, then the stage is flexible to turn off the power, otherwise the charger
      # needs to be active
      (session$data[istage+1,energy] < session$data[istage, result$early.energy]) &
      # The average power needed to complete the charge session is less than the  avg.kW
      (session$data[istage+1,energy/session$data[1:istage-1,sum(time_active)]] < session$avg.kW)  
  )
  print(paste0('istage = ',istage,' Avg. Power = ',session$data[istage+1,energy/session$data[1:istage,sum(time_active)]],' Flexible = ',flexible_test ))
  # If there is flexibility, we can choose whether or not to charge
  if(flexible_test){
    # Task 2a, if the state is flexible, evaluate the DPP
    #
    result$flexible=TRUE
    # Fill out the dpp table
    ## (1) For each state, determine the stage.return and the return function
    ### Invoke the constraints on the charge and discharge power to maintain fleet charging and target power
    
    ### Return how much energy the battery would consume.
    result$stage.table[,stage.return:= c(
      (result$target.power - session$avg.kW)*(result$time_charge),     # Session Charge
      0                                                                # Session Idle
    )]
    result$stage.table[,return.function :=stage.return + next_stage$return.function]
    
    ## (2) Based on the return function values, select the optimal state for the stage and 
    ##   record the return.function
    decide <- result$stage.table[,which.min(abs(return.function))]
    result$stage.table[decide,decision:=1]
    result$decision <- result$stage.table[decide,states]
    result$stage.return <- result$stage.table[decide,stage.return]
    result$return.function <- result$stage.table[decide,return.function]
    if(decide == 1){ #charge active
      result$bess.power <- (result$target.power - session$avg.kW)*result$time_charge/time_step
      result$session.power <- result$target.power*result$time_charge/time_step - result$bess.power
    } else{
      result$bess.power <- result$target.power*result$time_charge/time_step  
      result$session.power <- 0
    }
  } else{ # active but not flexible
    result$flexible=FALSE
    ## (2) Based on the return function values, select the optimal state for the stage and 
    ##   record the return.function
    decide <- 1 # when it's inflexible, we must charge the vehicle
    result$stage.table[decide,decision:=1]
    result$decision <- result$stage.table[decide,states]
    result$bess.power <- (result$target.power - session$avg.kW)*result$time_charge/time_step
    result$session.power <- session$avg.kW*result$time_charge/time_step
    result$stage.return <- result$bess.power * time_step
    result$return.function <- result$stage.return + next.stage$return.function
  }
  return(result)
}



