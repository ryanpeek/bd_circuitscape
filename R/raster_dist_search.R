# see great page here: https://www.seascapemodels.org/Advanced-spatial-analysis-2021/Advanced-spatial-analysis-2021.html#Introducting_raster_data

# and here: https://www.seascapemodels.org/rstats/2020/02/08/calculating-distances-in-R.html

# dist search via raster
library(tidyverse)
library(sf)
library(tmap)
library(mapview)
mapviewOptions(fgb=FALSE)
library(terra)
library(rnaturalearth)
# devtools::install_github("ropensci/rnaturalearthhires")

# Import Data -------------------------------------------------------------

aus <- rnaturalearth::ne_countries(type="countries", scale = "large", country = "australia", returnclass = "sf")
plot(aus$geometry)

# get data
load("~/Downloads/data-for-course/spatial-data/copepods_standardised.rda")
rsst <- rast("~/Downloads/data-for-course/spatial-data/MeanAVHRRSST.grd")
plot(rsst)
plot(aus$geometry, add=TRUE)

# map
tm_shape(rsst) +
  tm_raster(palette = "-RdBu", title = "SST")

# crop raster
tas_ext <- raster::extent(136, 151, -44, -35)
stas <- st_crop(aus, st_bbox(tas_ext))
plot(stas$geometry)
plot(rsst, add=TRUE)

sdat_crop <- st_crop(sdat_std, st_bbox(tas_ext))

# intersect points
sdat_shelf <- st_join(sdat_crop, stas, join = st_intersects)
tm_shape(stas, bbox = sdat_shelf) +
  tm_polygons(col = "grey10") +
  tm_shape(sdat_shelf) +
  tm_dots(col = "region", palette = "RdBu") +
  tm_graticules()

# pick a subset of points:
sdat_pt1 <- sdat_crop %>% 
  filter(silk_id == 245 & segment_no == 13)
  
sdat_pt2 <- sdat_crop %>% 
  filter(silk_id == 187 & segment_no == 29)

sdat_pt3 <- sdat_crop %>% 
  filter(silk_id == 75 & segment_no == 5)

sdat_pts <- bind_rows(sdat_pt1, sdat_pt2, sdat_pt3)
#mapview(sdat_pts)

# make a plot
tm_shape(stas) +
  tm_polygons() +
  tm_graticules(col = "grey60") +
  tm_shape(sdat_pts) +
  tm_symbols(col = "black") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(sdat_pts) +
  tm_text("segment_no", ymod = -1)


# Calc Distances ----------------------------------------------------------

# basic great circle distance
m <- st_distance(sdat_pts)
m/1000


# Euclidean Distances -----------------------------------------------------

# centered on tas 
tas_utm <- st_crs("+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs")
stas2 <- st_transform(stas, crs = tas_utm)
pts2 <- st_transform(sdat_pts, crs = tas_utm)

# map
tm_shape(stas2) +
  tm_polygons() +
  tm_graticules(col = "grey60") +
  tm_shape(pts2) +
  tm_symbols(col = "black") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(pts2) +
  tm_text("segment_no", ymod = -1)

m2 <- st_distance(pts2)
m2/1000 # not much diff!


# Fasterize ---------------------------------------------------------------

library(fasterize)
library(raster)

r <- raster(extent(stas2), nrows = 50, ncols = 50)
rtas <- fasterize(summarize(stas2), r)

rtas_pts <- rtas
xy <- st_coordinates(pts2)
icell <- cellFromXY(rtas, xy)
rtas_pts[icell[3]] <- 2

plot(rtas)
plot(rtas_pts)

# calc
d <- gridDistance(rtas_pts, origin = 2,
                  omit = 1)/1000

# plot
plot(d)
plot(stas2$geometry, add=TRUE)
plot(pts2$geometry, pch=21, col="black", bg="gray", add=TRUE)

# get dists
d[icell]
m[2,3]/1000
