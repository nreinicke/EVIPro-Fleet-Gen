
# Overview 

The objective of the set of functions provided in this repository is to generate PEV electric load shapes that represent EV charging loads at the city / utility level by using output results from [NREL's EVI-Pro Model](https://www.nrel.gov/docs/fy18osti/70831.pdf). The resulting load profiles are intended to be used in the on-line [EVI-Pro Lite Tool](https://afdc.energy.gov/evi-pro-lite), and also by local government, state, and electric utility planners to prepare for the integration of plug-in battery electric and hybrid electric light duty vehicles.

# Requirements

The following are required to utilize these scripts:
1. Hardware
    + Minimum 40GB RAM, 160GB or more recommended to take advantage of parallelization
 
2. Software
    + R v3.6.2 or greater
    + data.table package, v1.12.8 or greater
    + future package, v1.16.0 or greater
    + future.apply package, v1.4.0 or greater
 
3. Data
    + Vehicle charging event data output from the EVI-Pro model.
        + Individual charging session results for each ambient temperature and each vehicle class. Session files are named as A_B_C_D_E_VehF_dowG.csv where...
            + A= Power level of home charger (1=Level 1, 2=Level 2, 3=No home charger)
            + B= Power level of work charger (1=Level 1, 2=Level 2)
            + C= Rated power of public DCFC (1=50kw, 2=150kw, 3=400kw)
            + D= Consumer preferred charging location (1=home, 2=work)
            + E= Ambient temperature (1=-20C, 2=-10C, 3=0C, 4=+10C, 5=+20C, 6=+25C, 7=+30C, 8=+40C)
            + F= PEV type (1=PHEV20, 2=PHEV50, 3=BEV100, 4=BEV250)
            + G= Day of week (1=weekday, 2=weekend)
        + Each charging session file should have the following columns of data:
            + vid - travel day id
            + start_time - start time of park event in days (assumed to coincide with start of charge event)
            + end_time_chg - end time of charge event in days
            + end_time_prk - end time of park event in days (same as departure time)
            + dest_type - categorical value for destination type (1=home, 2=work, 3=public)
            + dest_chg_level - categorical value for charger power (1=Level 1, 2=Level 2, 3=DCFC)
            + kwh - dispensed energy at charger measured in kwh
            + avg_kw - average power of charge event measured at charger in kw (excludes time spent at zero power)
