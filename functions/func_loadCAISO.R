#**************************************************************************************************
# FUNCTION agg.pwr
# VERSION: 20190117
#**************************************************************************************************
agg.pwr <- function(power_table,time,ldelta_t,delta_t){
  return(
    power_table[
      time_of_day >= time & time_of_day < time+delta_t,
      sum(power)*ldelta_t/delta_t]
  )
}
#**************************************************************************************************
# FUNCTION load.caiso
# VERSION: 20190117
#**************************************************************************************************
load.caiso <- function(caiso_path,time_step){
  # Load the raw data. The data was provided in column format rather than row format. Some fenagling
  # is needed to create a nice data.table
  result <- list(
    raw = fread(caiso_path,header = FALSE)[,col_id := 1:.N] %>% melt(id.vars = "col_id") %>% dcast(variable ~ col_id)
  )
  colnames(result$raw) <- c('row_id','time_of_day_str','power_fc_1hr','power_avg_5min','net_demand') # MW
  # The first and last rows were garbage.
  result$filtered <- result$raw[2:(.N-1),.(time_of_day_str,power_fc_1hr,power_avg_5min,net_demand)]
  # Convert time_of_day into a fraction of day format
  result$filtered[,time_of_day := as.numeric(substr(time_of_day_str,1,2)) + as.numeric(substr(time_of_day_str,4,5))/60]
  result$filtered[,power_fc_1hr := as.numeric(power_fc_1hr)*1e3]          # convert to kW
  result$filtered[,power_avg_5min := as.numeric(power_avg_5min)*1e3]
  result$filtered[,net_demand := as.numeric(net_demand)*1e3]
  
  ### Aggregate 5 minute CAISO data to 15 minute data
  # We now seek to aggregate the 5 minute time series to a 15 minute time series. 
  # Assign the next 15 minutes of activity to the time-stamp. 
  #   let delta_t = 5 minutes and t be the current time. 
  #   t+1 is a 5 minute time step from the current time.
  # Average Power in the Interval: 
  #   P(t) = sum(P(tau),tau = t,t+delta_t, ldelta_t)*(ldelta_t/60)/(delta_t/60) 
  #   ldelta_t is the delta_t local time to the time-series
  
  # Create the aggregated time-series
  result$aggregated <- data.table(time_of_day = seq(from = 0, to = 24 - time_step, by = time_step))
  # Forecast 1 hr power
  result$aggregated[,
                   power_fc_1hr := agg.pwr(
                     result$filtered[,power := power_fc_1hr], 
                     time_of_day,
                     ldelta_t = 5/60,
                     delta_t = time_step),
                   by=.(time_of_day)
                   ]
  # Power 15 minute Average
  result$aggregated[,
                   power := agg.pwr(
                     result$filtered[,power := power_avg_5min],
                     time_of_day,ldelta_t = 5/60,
                     delta_t = time_step),
                   by=.(time_of_day)
                   ]
  # Net Demand
  result$aggregated[,
                   net_demand:= agg.pwr(
                     result$filtered[,power := net_demand],
                     time_of_day,
                     ldelta_t = 5/60,
                     delta_t = time_step
                     ),
                   by= .(time_of_day)
                   ]
  
  ###  Design and Dissect the CAISO Target Profile
  # How to design the target load shapes. Let's get philosophical for a second.
  # We argue that we wish to support the CAISO net demand in such a way as to level 
  # out the forecasted demand toward so baseline load such that variations in 
  # renewable generation and loads can be supported by the fleet and the BESS.
  # From the charging aggregator's perspective, they will need to meet both the 
  # fleet mobility requirements, the desired target load, and BESS charging demand.
  # If only the fleet mobility and target loads are considered, and energy deficit will accumulate.
  
  # Target Load Shape
  # The raw target only considers the desired CAISO load support.
  result$target <- list(raw = data.table(time_of_day = result$aggregate[,time_of_day]))
  result$target$raw[,invres_power := -result$aggregate[,net_demand-mean(net_demand)]]
  #.................
  # 2019-01-01 DS: Zero Out the load shape so that all action is above the curve
  result$target$raw[,power := invres_power+abs(min(invres_power))]
  #.................
  result$target$raw[,cumu_nrg := cumsum(power)*time_step]
  result$target$energy = result$target$raw[,max(cumu_nrg)]
  
  # CAISO load support demanded from fleet aggregator
  result$target$load <- data.table(
    time_of_day = result$target$raw[,time_of_day],
    power = result$target$raw[,power]
  )[power <0, power := 0]
  result$target$load[,cumu_nrg := cumsum(power)*time_step]
  
  # If generator support is desired from the fleet operator, then the BESS will need to be net discharged
  # To balance the BESS energy, charging requirements will be synchronized to load demand from the grid.
  # CAISO Generator support demanded from fleet aggregator
  result$target$generator <- data.table(
    time_of_day = result$target$raw[,time_of_day],
    power = result$target$raw[,power]
  )[power > 0, power := 0]
  result$target$generator[,cumu_nrg := cumsum(power)*time_step]
  return(result)
}