
## post some results check for duplicates
library(dplyr)
library(feather)
library(data.table)

# test write out as feather format
write_feather(X20191127_20C_, "outputs/full_run_nov_2019/test_output.feather")

# set as data table for speed
setDT(X20191127_20C_)

# drop the time of day and kw
# This should result in unique combinations of variables
# X20191127_20C_ %>% select(-time_of_day, -kw)
X20191127_20C_[, c('kw' , 'time_of_day') := NULL ]


head(X20191127_20C_)

combinations <- distinct(X20191127_20C_)


setDT(X20191016_20C)
X20191016_20C[, c('kw' , 'time_of_day') := NULL ]

old_combs <- distinct(X20191016_20C)


old_combs <- sapply(old_combs, function(x) {
	unique(x)
})

combinations <- sapply(combinations, function(x) {
	unique(x)
})

t <- read_feather("outputs/full_run_nov_2019/test_output.feather")

t %>% filter(
	fleet_size == 50000 &
		mean_dvmt == 45 &
		pev_dist == 'EQUAL_DIST' &
		pref_dist == 'Home60' &
		home_access == 'HA50' &
		home_power_dist == 'L2_20' &
		work_power_dist == 'L2_20' &
		day_of_week == 'weekend' &
		pev_type == 'BEV250' &
		dest_type == 'Public' &
		dest_chg_level == 'L3' &
		vehicle_class_dist == 'Sed20_Suv80' &
		time_of_day == 24
)


