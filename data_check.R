

temp_list <- seq(-20, 40, 5)

temp_list <- paste0(temp_list, "C")


# for (i in 1:length(temp_list)) {
# 	
files <- dir(paste0("../../data/sessions_all_ambient_temps_suv/", temp_list[3]),
 				full.names = T)

rows <- lapply(files, function(x) {
	y <- fread(x, fill = F)
	
	c <- nrow(y)
	
	return(c)
})


rows_nofill <- lapply(files, function(x) {
	y <- fread(x, fill = T)
	
	c <- nrow(y)
	
	return(c)
})


for (i in 1:length(rows)) {
	binary = rows[[i]] == rows_nofill[[i]]
if (binary == F) break
}


files[133]
ter <- fread(files[133], fill = T)
library(data.table)
na.omit(ter)
ter
data.table::