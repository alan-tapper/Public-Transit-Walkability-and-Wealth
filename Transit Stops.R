####################################################################################################
# Langston Ford and Alan Tapper
#
# Who Gets to Live Near Rail Transit?
# Effects of Rail Transit Access and Walkability on the Housing Market in Large Southern Cities
#
# Transit Stops
#
# This file contains code for downloading and processing transit stop data. It creates and
# writes a shapefile for each of the five cities. Each file contains every rail transit stop that
# is within the city limits for that city.
####################################################################################################

setwd("/Users/langstonford/Library/CloudStorage/OneDrive-RiceUniversity/Junior 2023-2024/2024 Spring Semester/SOCI 460/Research Project/City Blocks")
library (tidytransit)
library(dplyr)
library(spdep)
library(ggplot2)


######################################################################################################
#Atlanta Transit Mapping


# Import shapefile that has been subsetted 
atl_blocks <- st_read(dsn="atl_blocks", layer="atl_blocks",
                      stringsAsFactors=FALSE, options="ENCODING=latin1")

# Import the shapefile    !this may take a minute!
atl_transit <- read_gtfs(path="https://transitfeeds.com/p/marta/65/latest/download")

# Convert to sf
atl_transit <- set_servicepattern(atl_transit)
atl_transit <- gtfs_as_sf(atl_transit)
atl_transit$shapes$length <- st_length(atl_transit$shapes)


# Filter stops by Rail 
atl.stops <- atl_transit$stops %>%
  filter( as.numeric(stop_id) <= 1200)

atl.stops <- atl.stops%>%
  filter( as.numeric(stop_code) != 906311)


# Confirm just Rail Stops
#plot(atl.stops[, "geometry"], lwd=0.5)
  


# Create 1000 m buffer around transit stops and transform geometry 
atl_buffer <- st_buffer(atl.stops,1000) 

atl_buffer <- st_transform(atl_buffer, crs= st_crs(atl_blocks))

# Create 2000 m buffer around transit stops and transform geometry 
atl_buffer.2 <- st_buffer(atl.stops,2000) 

atl_buffer.2 <- st_transform(atl_buffer.2, crs= st_crs(atl_blocks))


# Plot city of Atlanta by blocks and over lay transit buffer map
#ggplot() + geom_sf(data = atl_blocks) + geom_sf(data = pt_buffer,color = 'red')


# Intersection of buffer points 
atl.transit.area<- st_intersection( atl_buffer, atl_blocks)
atl.transit.area.2<- st_intersection( atl_buffer.2, atl_blocks)


# Remove repeats from buffer points  
atl.no_repeats <- atl.transit.area %>% 
  filter(!duplicated(GEOID20))
test <- atl.transit.area.2 %>% 
  group_by(stop_name)%>%
  filter(!duplicated(GEOID20))


plot(atl.transit.area.2[, "geometry"], lwd=0.5)
plot(test[, "geometry"], lwd=0.5)


# Get data on transit stops in city limits
atl.stops <- st_transform(atl.stops, crs= st_crs(atl_blocks))
atl.stops.sub<- st_intersection( atl_blocks, atl.stops)
atl.stops.no_repeats <- atl.stops.sub %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(atl.stops.no_repeats[, "geometry"], lwd=0.5)


######################################################################################################
#Miami Transit Mapping


mia_blocks <- st_read(dsn="miami_blocks", layer="miami_blocks",
                        stringsAsFactors=FALSE, options="ENCODING=latin1")


# Import the shapefile    !this may take a minute!
miami_transit <- read_gtfs(path="http://www.miamidade.gov/transit/googletransit/current/google_transit.zip")


# Convert to sf
miami_transit <- set_servicepattern(miami_transit)
miami_transit <- gtfs_as_sf(miami_transit)
miami_transit$shapes$length <- st_length(miami_transit$shapes)

# Filter by Rail
miami.stops <- miami_transit$stops %>%
  filter( grepl('RAIL ', stop_name))


miami.stops <- miami_transit$stops %>%
  filter( grepl('STATION', stop_name))

miami.stops <- miami.stops %>%
  filter( stop_id != '1384', stop_id != '11302', stop_id != '11307' )


#plot(miami.stops[, "geometry"], lwd=0.5)



# Create 1000 m buffer around transit stops and transform geometry 
mia_buffer <- st_buffer(miami.stops,1000) 

mia_buffer <- st_transform(mia_buffer, crs= st_crs(mia_blocks))

# Create 2000 m buffer around transit stops and transform geometry 
mia_buffer.2 <- st_buffer(miami.stops,2000) 

mia_buffer.2 <- st_transform(mia_buffer.2, crs= st_crs(mia_blocks))


# Plot city of Atlanta by blocks and over lay transit buffer map
#ggplot() + geom_sf(data = miami_blocks) + geom_sf(data = mia_buffer,color = 'red')


# Intersection of buffer points 
mia.transit.area<- st_intersection( mia_buffer, mia_blocks)
mia.transit.area.2<- st_intersection( mia_buffer.2, mia_blocks)

#Remove Repeats
mia.no_repeats <- mia.transit.area %>% 
  filter(!duplicated(GEOID20))
mia.no_repeats.2 <- mia.transit.area.2 %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(mia.no_repeats.2[, "geometry"], lwd=0.5)
plot(mia.no_repeats[, "geometry"], lwd=0.5)


# Get data on transit stops in city limits
miami.stops <- st_transform(miami.stops, crs= st_crs(mia_blocks))
mia.stops.sub<- st_intersection( mia_blocks, miami.stops)
mia.stops.no_repeats <- mia.stops.sub %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(mia.stops.no_repeats[, "geometry"], lwd=0.5)




######################################################################################################
#Houston

hou_blocks <- st_read(dsn="houston_blocks", layer="houston_blocks",
                          stringsAsFactors=FALSE, options="ENCODING=latin1")


# Import the shapefile    !this may take a minute!
hou_transit <- read_gtfs(path="https://transitfeeds.com/p/metro/25/latest/download")


# Convert to sf
hou_transit <- set_servicepattern(hou_transit)
hou_transit <- gtfs_as_sf(hou_transit)
hou_transit$shapes$length <- st_length(hou_transit$shapes)

# Filter by Rail
hou.stops <- hou_transit$stops %>%
  filter( as.numeric(stop_id) > 25000 & as.numeric(stop_id) <  26000)


#plot(hou.stops[, "geometry"], lwd = .5)


# Create 1000 m buffer around transit stops and transform geometry 
hou_buffer <- st_buffer(hou.stops,1000) 

hou_buffer <- st_transform(hou_buffer, crs= st_crs(hou_blocks))

# Create 2000 m buffer around transit stops and transform geometry 
hou_buffer.2 <- st_buffer(hou.stops,2000) 

hou_buffer.2 <- st_transform(hou_buffer.2, crs= st_crs(hou_blocks))


# Plot city of Atlanta by blocks and over lay transit buffer map
#ggplot() + geom_sf(data = miami_blocks) + geom_sf(data = mia_buffer,color = 'red')


# Intersection of buffer points 
hou.transit.area<- st_intersection( hou_buffer, hou_blocks)
hou.transit.area.2<- st_intersection( hou_buffer.2, hou_blocks)

#Remove Repeats
hou.no_repeats <- hou.transit.area %>% 
  filter(!duplicated(GEOID20))
hou.no_repeats.2 <- hou.transit.area.2 %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(hou.no_repeats.2[, "geometry"], lwd=0.5)

# Get data on transit stops in city limits
hou.stops <- st_transform(hou.stops, crs= st_crs(hou_blocks))
hou.stops.sub<- st_intersection( hou_blocks, hou.stops)
hou.stops.no_repeats <- hou.stops.sub %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(hou.stops.no_repeats[, "geometry"], lwd=0.5)



######################################################################################################
# Dallas
dal_blocks <- st_read(dsn="dallas_blocks", layer="dallas_blocks",
                     stringsAsFactors=FALSE, options="ENCODING=latin1")

# Import the shapefile    !this may take a minute!
dallas_transit <- read_gtfs(path="http://www.dart.org/transitdata/latest/google_transit.zip")


# Convert to sf
dallas_transit <- set_servicepattern(dallas_transit)
dallas_transit <- gtfs_as_sf(dallas_transit)
dallas_transit$shapes$length <- st_length(dallas_transit$shapes)

# Filter by Rail
dallas.stops <- dallas_transit$stops %>%
  filter( grepl('STATION', stop_name))

#plot(dallas.stops)


# Create 1000 m buffer around transit stops and transform geometry 
dal_buffer <- st_buffer(dallas.stops,1000) 

dal_buffer <- st_transform(dal_buffer, crs= st_crs(dal_blocks))

# Create 2000 m buffer around transit stops and transform geometry 
dal_buffer.2 <- st_buffer(dallas.stops,2000) 

dal_buffer.2 <- st_transform(dal_buffer.2, crs= st_crs(dal_blocks))


# Plot city of Atlanta by blocks and over lay transit buffer map
#ggplot() + geom_sf(data = miami_blocks) + geom_sf(data = mia_buffer,color = 'red')


# Intersection of buffer points 
dal.transit.area<- st_intersection( dal_buffer, dal_blocks)
dal.transit.area.2<- st_intersection( dal_buffer.2, dal_blocks)

#Remove Repeats
dal.no_repeats <- dal.transit.area %>% 
  filter(!duplicated(GEOID20))
dal.no_repeats.2 <- dal.transit.area.2 %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(dal.no_repeats[, "geometry"], lwd=0.5)


# Get data on transit stops in city limits

dallas.stops <- st_transform(dallas.stops, crs= st_crs(dal_blocks))
dallas.stops.sub<- st_intersection( dal_blocks, dallas.stops)

dal.stops.no_repeats <- dallas.stops.sub %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(dal.stops.no_repeats[, "geometry"], lwd=0.5)


######################################################################################################


dc_blocks <- st_read(dsn="dc_blocks", layer="DC_block_2020",
                     stringsAsFactors=FALSE, options="ENCODING=latin1")


#library (httr)
# Import the shapefile    !this may take a minute!

#headers <- c('Api-Token' = '8320b14f24e946999547c598e5207867')

#VERB ("GET", url = "https://api.wmata.com/gtfs/rail-gtfs-static.zip?apikey=8320b14f24e946999547c598e5207867",
    #add_headers(headers))

dc_transit <- read_gtfs(path= "https://api.wmata.com/gtfs/rail-gtfs-static.zip?api_key=8320b14f24e946999547c598e5207867")

#plot (dc_transit$stop)
# Convert to sf
#dc_transit <- set_servicepattern(dc_transit)
dc_transit <- gtfs_as_sf(dc_transit)
dc_transit$shapes$length <- st_length(dc_transit$shapes)

# Filter by Rail
dc.stops <- dc_transit$stops

#plot(dc.stops)

#plot(dc.stops[, "geometry"], lwd=0.5)


# Create 1000 m buffer around transit stops and transform geometry 
dc_buffer <- st_buffer(dc.stops,1000) 

dc_buffer <- st_transform(dc_buffer, crs= st_crs(dc_blocks))


# Create 2000 m buffer around transit stops and transform geometry 
dc_buffer.2 <- st_buffer(dc.stops, 2000) 

dc_buffer.2 <- st_transform(dc_buffer.2, crs= st_crs(dc_blocks))


# Plot city of Atlanta by blocks and over lay transit buffer map
#ggplot() + geom_sf(data = miami_blocks) + geom_sf(data = mia_buffer,color = 'red')


# Intersection of buffer points 
dc.transit.area<- st_intersection( dc_buffer, dc_blocks)
dc.transit.area.2<- st_intersection( dc_buffer.2, dc_blocks)

#Remove Repeats
dc.no_repeats <- dc.transit.area %>% 
  filter(!duplicated(GEOID20))
dc.no_repeats.2 <- dc.transit.area.2 %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(dc.no_repeats.2[, "geometry"], lwd=0.5)


# Get data on transit stops in city limits
dc.stops <- st_transform(dc.stops, crs= st_crs(dc_blocks))
dc.stops.sub<- st_intersection( dc_blocks, dc.stops)
dc.stops.no_repeats <- dc.stops.sub %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(dc.stops.no_repeats[, "geometry"], lwd=0.5) 

## Plot
#plot(dc.transit.area[, "geometry"], lwd=0.5)


######################################################################################################
#Make this like ten times more efficient 


st_intersection( dc_buffer, dc_blocks)


#For 1 KM Buffer Group 
citynames <- c("atl", "mia", "dal", "hou", "dc")
for(city in citynames) {
    city_buffer <- paste0(city, "_", "buffer")
    city_blocks <- paste0(city, "_", "blocks")
    assign(paste0(city, "_", "buffer1KM"), st_intersection( get(city_buffer), get(city_blocks)))
  }  # close for loop

#For 2 KM Buffer Group 
for(city in citynames) {
  city_buffer <- paste0(city, "_", "buffer.2")
  city_blocks <- paste0(city, "_", "blocks")
  assign(paste0(city, "_", "buffer2KM"), st_intersection( get(city_buffer), get(city_blocks)))
}  # close for loop


#Saving Files
objtypes <- c("centroid", "buffer1KM", "buffer2KM")
for(type in objtypes) {
  for(city in citynames) {
    city_fname <- paste0(city, "_", type)
    st_write(get(city_fname), dsn=city_fname, layer=paste0(city_fname, ".shp"), driver="ESRI Shapefile")
  } } # close for loop



city_buffer <- paste0("mia", "_", "buffer")



# Get Block Centroids for Both Buffer Zones
bufferzone <- c("buffer1KM", "buffer2KM")
for(type in bufferzone) {
  for(city in citynames) {
    city_buffer <- paste0(city, "_", type)
    assign(paste0(city, "_", "centroid",type), st_centroid(get(city_buffer)))
} } # close for loop


plot(atl_centroid[,"geometry"], lwd = 0.5)


#Save Block Centroids, Buffer 1KM and Buffer 2KM
citynames <- c("atl", "mia", "dal", "hou", "dc")
objtypes <- c("centroidbuffer1KM","centroidbuffer2KM", "buffer1KM", "buffer2KM")
for(type in objtypes) {
  for(city in citynames) {
  city_fname <- paste0(city, "_", type)
  st_write(get(city_fname), dsn=city_fname, layer=paste0(city_fname, ".shp"), driver="ESRI Shapefile")
} } # close for loop


#Transit Stop Geoms
for(city in citynames) {
  city_fname <- paste0(city, ".", "stops.no_repeats")
  st_write(get(city_fname), dsn=city_fname, layer=paste0(city_fname, ".shp"), driver="ESRI Shapefile")
}


#Save Blocks around Transit Area No Repeats 1 KM
st_write(atl.no_repeats, "u_atl_buffer.shp")
st_write(mia.no_repeats, "u_mia_buffer.shp")
st_write(dal.no_repeats, "u_dal_buffer.shp")
st_write(hou.no_repeats, "u_hou_buffer.shp")
st_write(dc.no_repeats, "u_dc_buffer.shp")

#Save Blocks around Transit Area No Repeats 2 KM
st_write(atl.no_repeats.2, "u_atl_buffer2KM.shp")
st_write(mia.no_repeats.2, "u_mia_buffer2KM.shp")
st_write(dal.no_repeats.2, "u_dal_buffer2KM.shp")
st_write(hou.no_repeats.2, "u_hou_buffer2KM.shp")
st_write(dc.no_repeats.2, "u_dc_buffer2KM.shp")


#Save Stops Geom Point Values
st_write(atl.stops.no_repeats, "atl_stops.shp")
st_write(mia.stops.no_repeats, "mia_stops.shp")
st_write(dal.stops.no_repeats, "dal_stops.shp")
st_write(hou.stops.no_repeats, "hou_stops.shp")
st_write(dc.stops.no_repeats, "dc_stops.shp")


