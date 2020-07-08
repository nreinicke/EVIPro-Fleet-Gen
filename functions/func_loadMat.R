################################################################################
# This function loads .mat files as data tables
#
# Author: Micah Wright, Humboldt State University
################################################################################

# load necessary packages
library(tools)
library(data.table)
library(R.matlab)

load_MatFiles <- function(path, temperature, ftype) {
  
  tc <- c("-20C", "-15C", "-10C", "-5C", "0C", "5C", "10C", "15C", "20C", "25C", "30C", "35C", "40C")
  
  if(!(temperature %in% tc)) { 
    
    stop("Temperature is not one of specified options")
    
  }
  
  if(!dir.exists(paste0(path, temperature))) {
    
    stop("Specified directory does not exist")
    
  }
  
  if(ftype == "matlab") {
    # get file paths, ignore csv files and batch files
    mat_file_paths <- list.files(paste0(path,
                                        temperature),     
                                 full.names = TRUE,
                                 recursive = TRUE,
                                 pattern = "[1-4].mat")
    
    # load the .mat files, specify col names, and convert to dt
    mat_files <- lapply(mat_file_paths, function(x) { 
      
      # read the .mat file
      mat <- readMat(x, fixNames = FALSE) 
      
      # extract column names
      cnm <- rownames(mat[["sessions"]])
      
      # create a matrix of the values from mat
      values_matrix <- do.call("cbind", mat[["sessions"]])
      
      # specify column names
      colnames(values_matrix) <- cnm
      
      # convert to dt
      values_dt <- as.data.table(values_matrix)
      
      # remove rows with 0 
      values_dt <- values_dt[lat != 0 & lon != 0 & kwh != 0]
      
      # specify temp and filename columns
      values_dt[, ':='(filename = file_path_sans_ext(basename(x)),
                       temp_c = ifelse(grepl(temperature, "-") | temperature == "0C",
                                       temperature,
                                       paste0("+", temperature)))]
      
      values_dt[, ':=' (power_home = as.numeric(substr(filename, 1,1)),
                        power_work = as.numeric(substr(filename, 3,3)),
                        power_public = as.numeric(substr(filename, 5,5)),
                        preferred_loc = as.numeric(substr(filename, 7,7)),
                        pev_type = as.numeric(gsub('.*h([0-4]+).*','\\1',filename)))]
      
      # change day of week column name
      setnames(values_dt, "dow", "day_of_week")
      
      return(values_dt)
    })
  }
  
  if(ftype == "csv") {
    
    # get file paths batch files
    mat_file_paths <- list.files(paste0(path,
                                        temperature),     
                                 full.names = TRUE,
                                 recursive = TRUE,
                                 pattern = "[1-4].csv")
    
    # load the .mat files, specify col names, and convert to dt
    mat_files <- lapply(mat_file_paths, function(x) { 
      
      # read the .mat file
      values_dt <- fread(x, col.names = c("vid", "start_time", "end_time_chg", "end_time_prk",
                                          "dest_type", "dest_chg_level", "kwh", "avg_kw"))#,
      																			# fill = T) # fixes an error in missing data 
      # values_dt <- na.omit(values_dt) # remove NAs created by fill = T above
      # specify temp and filename columns
      values_dt[, ':='(filename = file_path_sans_ext(basename(x)),
                       temp_c = ifelse(temperature %in% c("-20C", "-15C", "-10C", "-5C", "0C"),
                                       temperature,
                                       paste0("+", temperature)))]
      
      # parse other variables from file name
      # first split the file name
      values_dt[, var_list := strsplit(filename, "_")]
      
      # extract the correct values from the split filename, all should be the 
      # same in each file so index 1 is appropriate
      values_dt[, ':='(power_home = as.numeric(var_list[[1]][1]),
                       power_work = as.numeric(var_list[[1]][2]),
                       power_public = as.numeric(var_list[[1]][3]),
                       preferred_loc = as.numeric(var_list[[1]][4]),
                       pev_type = as.numeric(gsub("[^0-9]", "", var_list[[1]][6])),
                       day_of_week = as.numeric(gsub("[^0-9]", "", var_list[[1]][7])))]
      
      # remove var_list column
      values_dt[, var_list := NULL]
      
      return(values_dt)
    })
  }
  
  
  # return a single dt
  return(rbindlist(mat_files))
}
