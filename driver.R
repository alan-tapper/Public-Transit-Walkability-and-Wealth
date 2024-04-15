####################################################################################################
# Langston Ford and Alan Tapper
#
# Main file
#
# This R file, when given a list of cities and corresponding shapefiles, creates new shapefiles with
# the following data at the census block group level:
# 
# GISJOIN: The GISJOIN of the block group (from the original shapefile).
#
# INCOME: The median household income (from the original shapefile, renamed).
#
# VALUE: The median value of owner-occupied housing units (from the original shapefile, renamed).
#
# LAT, LNG: The latitude and longitude of the centroid of the block group (unless the centroid
#   falls outside the block group, in which case they represent the point in the block group that
#   is closest to the centroid) (from the original shapefile, renamed).
#
# geometry: The list of points bounding the block group (from the original shapefile).
#
# WALKSCO: The Walk Score of the centroid. Calculated using the Walk Score API
#   (https://www.walkscore.com/professional/api.php)
#
# CLOSEST: The name of the closest rail transit stop to the centroid. This includes
#   heavy rail/metro/subway and light rail.
#
# WALKTIM, WALKDIS: The time (in seconds) and distance (in meters) that it takes to walk from the
#   centroid to CLOSEST. Calculated using the Routes API
#   (https://developers.google.com/maps/documentation/routes)
#
#
# It also generates several plots and stores them in a directory called "plots" and in a
# subdirectory for each city. If those directories exist already, it will put plots there. If not,
# it will create them before creating plots. All plots are created at the census block group scale,
# and have the rail transit stops superimposed. The following plots are created for each city:
#
# Median Household Income (INCOME) - color gradient
#
# Median Value of Owner-Occupied Housing Units (VALUE) - color gradient
#
# Walk Score (WALKSCO) - color gradient
#
# Closest Rail Stop (CLOSEST) - one color for each stop
#
# Walking Time (WALKTIM) - color gradient
#
# Log Walking Time (log(WALKTIM)) - color gradient
#
#
# Note that we are plotting Log Walking Time because we are using that for our regression models.
####################################################################################################

library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(spdep)
library(writexl)
library(compare)
library(httr2)
library(jsonlite)
library(stringr)

setwd("~/Desktop/soci460_stuff/SOCI460Project")

#load source files
source("localhelpers.R")
source("apicalls.R")

####################################################################################################

#table of cities with their long and short names
city_info <- data.frame(c("dallas", "houston", "atlanta", "miami", "dc"),
                        c("dal", "hou", "atl", "mia", "dc"),
                        c("Dallas", "Houston", "Atlanta", "Miami", "DC"))
colnames(city_info) <- c("LONG", "SHORT", "DISPLAY")

####################################################################################################

#if directory doesn't exist for plots, create it
if (!dir.exists("plots")) {
  dir.create("plots")
}

#loop through cities
for (city_index in 1:nrow(city_info)) {
  
  #reset working directory
  setwd("~/Desktop/soci460_stuff/SOCI460Project")
  
  #get long and short names of the city
  city_long_name <- city_info[city_index, "LONG"]
  city_short_name <- city_info[city_index, "SHORT"]
  city_display_name <- city_info[city_index, "DISPLAY"]

  #get filenames
  block_data_shapefile_filename <- paste0(city_long_name, "_values")
  stop_shapefile_name <- paste0(city_short_name, ".stops.no_repeats")

  #extract block group data from shapefile
  values <- setupBlockGroups(block_data_shapefile_filename)
  print("block group data extracted")

  #extract stop data from shapefile
  stops <- setupStops(stop_shapefile_name)
  print("stop data extracted")
  
  #if we don't have a shapefile with the full values in the blockdata directory, build it
  if (!file.exists(paste0("blockdata/", city_long_name, ".block_group_full_data"))) {
    #add walk scores
    values_walkscore <- addWalkScoresToBlockGroups(values)
    print("walk scores added")

    #add walk times and walk distances
    values_full <- addWalktimes(values_walkscore, stops)
    print("walk times and distances added")

    #create filename to produce shapefile at
    city_fname <- paste0(city_long_name, ".", "block_group_full_data")

    #export shapefile
    st_write(values_full, dsn=city_fname, layer=paste0(city_fname, ".shp"), driver="ESRI Shapefile")
  }
  
  #get shapefile with full values
  values_full <- st_read(dsn=paste0("blockdata/", city_long_name, ".block_group_full_data"), layer=paste0(city_long_name, ".block_group_full_data"),
                         stringsAsFactors=FALSE, options="ENCODING=latin1")
  
  #if directory doesn't exist for this city's plots, create it
  if (!dir.exists(paste0("plots/", city_long_name))) {
    dir.create(paste0("plots/", city_long_name))
  }
  
  #set working directory to the plots subdirectory for this city
  setwd(paste0("~/Desktop/soci460_stuff/SOCI460Project/plots/", city_long_name))
  
  #plot income
  pdf(paste0(city_long_name, " income.pdf"))
  values_full %>%
    ggplot()+
    geom_sf(data = values_full, aes(fill = INCOME), lwd = 0)+
    geom_sf(data = stops, aes (color = "Transit Stop"), size = 0.5)+
    labs (title = paste0(city_display_name, " Block Group By Median Household Income"),
          fill = "Median Household Income\nin the Past 12 months")+
    theme_void()
  dev.off()
  
  #plot value
  pdf(paste0(city_long_name, " value.pdf"))
  values_full %>%
    ggplot()+
    geom_sf(data = values_full, aes(fill = VALUE), lwd = 0)+
    geom_sf(data = stops, aes (color = "Transit Stop"), size = 0.5)+
    labs (title = paste0(city_display_name, " Block Group By Median Value of Owner-occupied Housing Unit"),
          fill = "Median Value of\nHousing Unit in Dollars")+
    theme_void() 
  dev.off()
  
  #plot walkscore
  pdf(paste0(city_long_name, " walkscore.pdf"))
  values_full %>%
    ggplot()+
    geom_sf(data = values_full, aes(fill = WALKSCO), lwd = 0)+
    geom_sf(data = stops, aes (color = "Transit Stop"), size = 0.5)+
    labs (title = paste0(city_display_name, " Block Group By Walk Score"),
          fill = "Walk Score")+
    theme_void() 
  dev.off()
  
  #plot closest stop
  pdf(paste0(city_long_name, " closeststop.pdf"))
  values_full %>%
    ggplot()+
    geom_sf(data = values_full, aes(fill = CLOSEST), lwd = 0)+
    geom_sf(data = stops, aes (color = "Transit Stop"), size = 0.5)+
    scale_color_manual( values = "black", name="")+
    labs (title = paste0(city_display_name, " Block Group By Closest Stop"),
          fill = "Closest Stop")+
    theme_void()+
    theme(legend.position = "none")
  dev.off()
  
  #plot walktime
  pdf(paste0(city_long_name, " walktime.pdf"))
  values_full %>%
    ggplot()+
    geom_sf(data = values_full, aes(fill = WALKTIM), lwd = 0)+
    geom_sf(data = stops, aes(color = "Transit Stop"), size = 0.5)+
    labs (title = paste0(city_display_name, " Block Group By Walk Time to Closest Transit Stop"),
          fill = "Walk Time to Closest Transit Stop")+
    theme_void()
  dev.off()
  
  #plot log(walktime)
  pdf(paste0(city_long_name, " logwalktime.pdf"))
  values_full %>%
    ggplot()+
    geom_sf(data = values_full, aes(fill = log(WALKTIM)), lwd = 0)+
    geom_sf(data = stops, aes (color = "Transit Stop"), size = 0.5)+
    labs (title = paste0(city_display_name, " Block Group By Log Walk Time to Closest Transit Stop"),
          fill = "Log Walk Time to Closest Transit Stop")+
    theme_void()
  dev.off()
  city_index <- city_index + 1
}
