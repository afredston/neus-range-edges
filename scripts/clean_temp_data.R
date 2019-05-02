# this analysis uses data from SODA, the Simple Ocean Data Assimilation project: http://www.soda.umd.edu/
# various datasets for SODA are available on that website. some are also available through NOAA's ERDDAP API (the rerddap package), but they are not the newest versions of the models (J. Carton pers. comm.). 
# selecting a SODA model for ecological purposes is challenging because the different sub-models vary in certain assumptions that are difficult for us to judge. I used soda 3.4.2 which is commonly used by meteorologists (J. Carton pers. comm.), but of course the most thorough approach would be to try all of them and see how sensitive the ecological results are to the temperature model. 
# for replication, any time-series dataset of sea bottom temperature (or sea surface temperature, for that matter) from 1968-2017 could be substituted here. 

library(here)
library(ncdf4)
library(chron)

soda.raw <- nc_open(here("data","soda3.4.2_mn_ocean_reg_bottemp.nc"))
# what's in here? 
# print(soda.raw)
# variables: temp
# dimensions: lat, lon, time 
# how much time?
time <- ncvar_get(soda.raw, "time")
tunits <- ncatt_get(soda.raw,"time","units")
nt <- dim(time)


tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))
