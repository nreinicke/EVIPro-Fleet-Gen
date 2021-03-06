# Author: Schatz Energy Research Center
# Original Version: Jerome Carman
# Edits: Jerome Carman
# Version: 1.2
# Date: January 09, 2019
# Description: Pre-calculates the time series load profile of all individual sessions in the EVI-Pro data set. This function
#              takes about 5 minutes to run.
#   activity: data table created using the loadEVIPro() function. Must have the following columns
#       unique_vid,pev_type,schedule_vmt,dest_type,dest_chg_level,start_time,end_time_chg,avg_kw,kwh
#   time_step: decimal in hours specifying the time series bin size for the load profile.
# Version History
#   1.1: JKC edited to capture events that start and stop in the same time bin. Also modified the kW scaling of the session
#        start and stop time bins by adding an ifelse statement. This prevents double counting kW when sessions start or stop
#        at a time equal to the bin edge.
#   1.2: JKC applied edits suggested by Colin to substantially improve the run time. Removed cbind call and applied Colin's
#        join.on() custom function.

source("functions/func_joinOn.R") #Fast join function
source("functions/func_strEval.R") #String evulation function
source("functions/func_pp.R") #Alias for paste

calcBaseEVILoad <- function(activity_data,time_step) {
  #Initialize load profile data table using subset of columns of evi_raw[]
  activity_data <- activity_data[,.(unique_vid,pev_type,schedule_vmt,dest_type,dest_chg_level,start_time,end_time_chg,avg_kw,kwh)]
  
  setkey(activity_data,unique_vid,start_time)
  
  #Create a unique key to use for creating a time sequence for each charging event
  activity_data[,session_id:=1:.N]
  
  #Create time sequence for each charging event
  # Time series is right bin edge (0.25 hours captures all kW that occured from 0 to 0.25)
  evi_load_profiles <- activity_data[,.(time_of_day=seq(from=(ceiling(start_time*24/time_step)*time_step),
                                                        to=(ceiling(end_time_chg*24/time_step)*time_step),
                                                        by=time_step)
                                        ),
                                     by=session_id]
  
  #Merge activity_data into evi_load_profiles
  # avg_kw is repeated for all time series bins for each session_id
  evi_load_profiles <- join.on(evi_load_profiles,activity_data,'session_id','session_id')
  
  #Create data tables of index values associated with the start time and end time bin for all charging sessions
  start_index <- evi_load_profiles[,.I[which.min(time_of_day)],by=session_id]
  end_index <- evi_load_profiles[,.I[which.max(time_of_day)],by=session_id]
  
  #Determine index values where the charging event starts and stops in the same time bin. These are "short events"
  setkey(start_index,V1)
  setkey(end_index,V1)
  short_event_index <- start_index[end_index,nomatch=0][,i.session_id:=NULL] #inner join to capture when the start and stop bin are the same index
  
  #Scale short event sessions by fraction that the event occurs within the time step
  #Remove those sessions from start_index and end_index so they aren't scaled again below
  evi_load_profiles[short_event_index$V1,
                    avg_kw:=avg_kw*(end_time_chg-start_time)*24/time_step,
                    by=session_id]
  start_index <- start_index[!(V1 %in% short_event_index$V1),.SD]
  end_index <- end_index[!(V1 %in% short_event_index$V1),.SD]
  
  #Scale the start time bin kW for each session to the fraction of the time_step that the event occurs
  evi_load_profiles[start_index$V1,                                            #index associated with start bin for each session_id, excluding short events
                    avg_kw:=avg_kw*ifelse((1-(start_time*24/time_step)%%1)==1, #ifelse for case where modulus is zero
                                          0,                                   #This case requires that zero kW be assigned to the time bin (because right bin edge)
                                          (1-(start_time*24/time_step)%%1)),   #Else scale avg_kw by 1 - modulus (because right bin edge)
                    by=session_id]
  
  #Scale the end time bin kW for each session to the fraction of the time_step that the event occurs
  evi_load_profiles[end_index$V1,                                              #index associated with end bin for each session_id, excluding short events
                    avg_kw:=avg_kw*ifelse(((end_time_chg*24/time_step)%%1)==0, #ifelse for case where modulus is zero.
                                          1,                                   #This case requires that the full kW be assigned to the time bin (because right bin edge)
                                          (end_time_chg*24/time_step)%%1),     #Else scale by modulus (because right bin edge)
                    by=session_id]
  
  #For each session, uniformly scale all kW values to align with kWh totals
  evi_load_profiles[, avg_kw := avg_kw * (unique(kwh)/(sum(avg_kw)*time_step)), by=session_id ]
  
  #There are some sessions which have non-zero kWh but zero time delta such that avg_kw calculates to NA. Catch and correct these.
  evi_load_profiles[is.na(avg_kw), avg_kw := kwh / time_step] ## EDIT MB 9/12/19: added uderscore to time step
  
  return(evi_load_profiles)
}
