# add data to a geopackage

library(sf)
library(dplyr)
library(here)
library(purrr)
library(glue)
library(fs)
library(mapview)
mapviewOptions(fgb=FALSE)

# List Shapefiles ---------------------------------------------------------

# list all files that end in .shp
shps <- fs::dir_ls("data/spatial", glob = "*shp")

# map over each file and read it in:
shp_ls <- map(shps, ~st_read(.x))

# transform to all the same thing
shp_ls <- map(shp_ls, ~st_transform(.x, 4326))

# check it worked
map(shp_ls, ~st_crs(.x)$epsg)

# View --------------------------------------------------------------------

mapview(shp_ls[1]) + mapview(shp_ls[2]) + mapview(shp_ls[3])
mapview(shp_ls[4]) + mapview(shp_ls[5]) + mapview(shp_ls[6])
mapview(shp_ls[7]) + mapview(shp_ls[8]) + mapview(shp_ls[9])
mapview(shp_ls[10]) + mapview(shp_ls[11]) + mapview(shp_ls[12])
mapview(shp_ls[13]) # sac
# US water areas and lines are 15 and 16 but too large
mapview(shp_ls[17]) 
mapview(shp_ls[18]) 
mapview(shp_ls[19]) 

# American Forks: Major mainstems for American watershed
# Analysis_areas3.shp = Cosumnes polygon
# full bay delta: Bay_delta_selection
# Fea Forks: Major mainstems for Feather
# Flood_bypasses: Yolo bypass polygon
# Flood_bypasses2: Yolo bypass extended
# Legal_Delta
# M_forks: Merced
# Mk_forks: Mokelumne
# PrimarySecondaryDelta: southern loab of delta
# S_forks: Stanislaus
# SJ_forks: San Joaquin
# Sac_forks: Sacramento
# 17: hydro_poly_MWT (full waterbody poly)
# 18: hydro_poly_Sac_SJR (waterbody poly for sac/sjr)
# 19: t_forks: Tuolumne
