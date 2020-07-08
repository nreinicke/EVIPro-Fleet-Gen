# Author: Schatz Energy Research Center
# Original Version: Micah Wright
# Edits: Jerome Carman
# Version: 1.1
# Date: June, 2020
# Description: Generates gamma distribution parameter for constructing DVMT distribution for EVI-Pro fleets
# Required Variables
#   input_nhts_path: raw trippub.csv file downloaded from https://nhts.ornl.gov/downloads
#   input_chts_path: .csv file with two columns: chts_vid, dvmt. This is a subset of the "Drive Cycle Data by Vehicle" dataset than can be downloaded from NREL at https://www.nrel.gov/transportation/secure-transportation-data/tsdc-california-travel-survey.html
#   output_dir: path to save output from this function.
# Version History
#   1.0: Created by Micah Wright as a .Rmd file
#   1.1: Converted to R function by Jerome Carman

library(bit64)
library(data.table)

estDVMTGammaParameters <- function(input_nhts_path = "",
                                   input_chts_path = "",
                                   output_dir = "") {
  # Load input data
  trips <- fread(input_nhts_path)
  chts_vmt <- fread(input_chts_path)
  
  #In preperation, what is the highest daily vmt from NREL? This will be used to truncate the VMT from NHTS later.
  vmt_max <- max(chts_vmt$dvmt)
  
  #Filter the trips to only include those that happend by car, truck, van, or SUV (TRPTRANS is one of 3:6),
  #   where a household vehicle was used (TRPHHVEH is 1), trip miles are greater than 0, and the respondent was the driver (DRVR_FLG is 1).
  #   The reason for the last condition is that if there are many individuals in a car, the trip is repeated across them, as can be shown in
  #   the following knitr expression: knitr::kable(trips[HOUSEID == 30023735 ,.(PERSONID, TDTRPNUM,STRTTIME, ENDTIME, TRPMILES,TDAYDATE, TRAVDAY, VEHID, DRVR_FLG, DRIVER)])
  trips <- trips[TRPTRANS %in% 3:6 & TRPHHVEH == 1 & TRPMILES > 0 & DRVR_FLG == 1]
  
  #Remove possibly erroneous observations. These are cases where travel day is not a weekend but is identified as a weekend trip, or vice-versa.
  trips <- trips[TDWKND == 1 & TRAVDAY %in% c(1, 7) | TDWKND == 2 & TRAVDAY %in% 2:6]
  
  #Calculate daily VMT. This assumes that trips are not repeated if the data are grouped by
  #   vehicle (VEHID), household (HOUSEID), date (TDAYDATE, in YYYYMM format), day of the week (TRAVDAY), and trip ID (TDCASEID).
  #   This also includes urban/rural indicator (URBRUR), which we will need later.
  vmt <- trips[, .(DAILYVMT= sum(TRPMILES)), 
               by = c("VEHID", "HOUSEID", "PERSONID", "TDAYDATE", "TRAVDAY", "URBRUR")]
  
  # There are some very large values for daily VMT. You can look at those whose trip VMT > `r vmt_max` with the following knitr expression:
  #   knitr::kable(trips[TRPMILES > vmt_max,.(HOUSEID, PERSONID, TDTRPNUM, STRTTIME, ENDTIME, TRVLCMIN, TRPMILES, TRPTRANS, TRPMILES)])
  #   It looks like most of these trips were longer than 12 hours (overlapped a day), or these are simply errors.
  #   How many vehicles actually went more than 1,000 miles in a day?
  ltrip <- vmt[, .(DAILYVMT, ge_vmt_max = "No")][DAILYVMT > vmt_max, ge_vmt_max := "Yes"][, .N, by = ge_vmt_max]
  
  # There are `r ltrip[ge_vmt_max == "Yes", N]` vehicles that went over `r vmt_max` miles in a day.
  #   Remove any vehicles with daily vmt greater than `r vmt_max` miles for now.
  vmt <- vmt[DAILYVMT < vmt_max]
  
  # Create a weekend/weekday column, and give urban/rural meaningful names.
  vmt[, ':=' (TDWKND = factor(ifelse(TRAVDAY %in% c(1,7), "weekend", "weekday")),
              URBRUR =  factor(URBRUR, labels = c("urban", "rural")))]
  
  # Create a list of subsets of the data by urban/rural and weekday/weekend and estimate the gamma distribution parameters.
  dt_list <- split(vmt, by = c("TDWKND", "URBRUR")) 
  est_list <- lapply(dt_list, function(x) MASS::fitdistr(x$DAILYVMT, "gamma", lower = c(0,0)))  
  
  # Convert the output to a data frame and return it.
  est_list <- lapply(est_list, function(x) as.data.table(broom::tidy(x)))
  est_dt <- rbindlist(est_list, idcol = TRUE)
  est_dt[, ':=' (day_of_week = sub("\\..*", "", .id),
                 urban = sub(".*\\.", "", .id))]
  est_dt <- dcast(est_dt, day_of_week + urban ~ term, value.var = "estimate")
  return(est_dt)
  
}