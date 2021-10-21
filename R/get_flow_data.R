# get flow data

library(dplyr)
library(mapview)
mapviewOptions(fgb=FALSE)
library(CDECRetrieve)


# Get Metadata ------------------------------------------------------------

# get data options 
cdec_datasets("LIS")
cdec_datasets("FRE")

# look at map 
cdec_stations("LIS") %>% map_stations()
cdec_stations("FRE") %>% map_stations()

# Download Data -----------------------------------------------------------

tictoc::tic()
lis_flow <- cdec_query(station = "LIS", sensor_num = "20",
                       dur_code = "e", start_date = "2010-10-01")
tictoc::toc()

fre_flow <- cdec_query(station = "FRE", sensor_num = "20",
                       dur_code = "h", start_date = "2010-10-01")


# Download with Custom Data -----------------------------------------------

library(wateRshedTools)

tictoc::tic()
lis_flow2 <- get_cdec(station = "LIS", sensor = "20",duration = "E",
                     start = "2010-10-01", end = Sys.Date())
tictoc::toc()

# Iterate -----------------------------------------------------------------

# library(purrr)
# stations_of_interest <- c("FRE", "LIS", "RIO")
# 'map' through the stations of interest and apply them to the function
# tst <- map_df(stations_of_interest, ~cdec_query(station = .x, sensor_num = "20", dur_code = "h"))
