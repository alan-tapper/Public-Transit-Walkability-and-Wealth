
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


# Confirm just Rail Stops
#plot(atl.stops[, "geometry"], lwd=0.5)
  


# Create 1000 m buffer around transit stops and transform geometry 
pt_buffer <- st_buffer(atl.stops,1000) 

pt_buffer <- st_transform(pt_buffer, crs= st_crs(atl_blocks))


# Plot city of Atlanta by blocks and over lay transit buffer map
#ggplot() + geom_sf(data = atl_blocks) + geom_sf(data = pt_buffer,color = 'red')


# Intersection of buffer points 
atl.transit.area<- st_intersection( pt_buffer, atl_blocks)


# Get data on transit stops in city limits
atl.stops <- st_transform(atl.stops, crs= st_crs(atl_blocks))
atl.stops.sub<- st_intersection( atl_blocks, atl.stops)
atl.stops.no_repeats <- atl.stops.sub %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(atl.stops.no_repeats[, "geometry"], lwd=0.5)

## Plot
#plot(atl.transit.area[, "geometry"], lwd=0.5)


######################################################################################################
#Miami Transit Mapping


miami_blocks <- st_read(dsn="miami_blocks", layer="miami_blocks",
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

#plot(miami.stops[, "geometry"], lwd=0.5)



# Create 1000 m buffer around transit stops and transform geometry 
mia_buffer <- st_buffer(miami.stops,1000) 

mia_buffer <- st_transform(mia_buffer, crs= st_crs(miami_blocks))


# Plot city of Atlanta by blocks and over lay transit buffer map
#ggplot() + geom_sf(data = miami_blocks) + geom_sf(data = mia_buffer,color = 'red')


# Intersection of buffer points 
miami.transit.area<- st_intersection( mia_buffer, miami_blocks)


# Get data on transit stops in city limits
miami.stops <- st_transform(miami.stops, crs= st_crs(miami_blocks))
mia.stops.sub<- st_intersection( miami_blocks, miami.stops)
mia.stops.no_repeats <- mia.stops.sub %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(mia.stops.no_repeats[, "geometry"], lwd=0.5)

## Plot
#plot(miami.transit.area[, "geometry"], lwd=0.5)


######################################################################################################
#Houston

houston_blocks <- st_read(dsn="houston_blocks", layer="houston_blocks",
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

hou_buffer <- st_transform(hou_buffer, crs= st_crs(houston_blocks))


# Plot city of Atlanta by blocks and over lay transit buffer map
#ggplot() + geom_sf(data = houston_blocks) + geom_sf(data = hou_buffer,color = 'red')


# Intersection of buffer points 
hou.transit.area<- st_intersection( hou_buffer, houston_blocks)

# Get data on transit stops in city limits
hou.stops <- st_transform(hou.stops, crs= st_crs(houston_blocks))
hou.stops.sub<- st_intersection( houston_blocks, hou.stops)
hou.stops.no_repeats <- hou.stops.sub %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(hou.stops.no_repeats[, "geometry"], lwd=0.5)

## Plot
#plot(hou.transit.area[, "geometry"], lwd=0.5)

######################################################################################################
# Dallas
dallas_blocks <- st_read(dsn="dallas_blocks", layer="dallas_blocks",
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
dallas_buffer <- st_buffer(dallas.stops,1000) 

dallas_buffer <- st_transform(dallas_buffer, crs= st_crs(dallas_blocks))



# Plot city of Atlanta by blocks and over lay transit buffer map
#ggplot() + geom_sf(data = dallas_blocks) + geom_sf(data = dallas_buffer,color = 'red')


# Intersection of buffer points 
dallas.transit.area<- st_intersection( dallas_buffer, dallas_blocks)


# Get data on transit stops in city limits

dallas.stops <- st_transform(dallas.stops, crs= st_crs(dallas_blocks))
dallas.stops.sub<- st_intersection( dallas_blocks, dallas.stops)

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


# Plot city of Atlanta by blocks and over lay transit buffer map
#ggplot() + geom_sf(data = dc_blocks) + geom_sf(data = dc_buffer,color = 'red')


# Intersection of buffer points 
dc.transit.area<- st_intersection( dc_buffer, dc_blocks)


# Get data on transit stops in city limits
dc.stops <- st_transform(dc.stops, crs= st_crs(dc_blocks))
dc.stops.sub<- st_intersection( dc_blocks, dc.stops)
dc.stops.no_repeats <- dc.stops.sub %>% 
  filter(!duplicated(GEOID20))

## Plot
plot(dc.stops.no_repeats[, "geometry"], lwd=0.5) 

## Plot
#plot(dc.transit.area[, "geometry"], lwd=0.5)




# For Removing Duplicates 

atl.no_repeats <- atl.transit.area %>% 
  filter(!duplicated(GEOID20))

mia.no_repeats <- miami.transit.area %>% 
  filter(!duplicated(GEOID20))

dal.no_repeats <- dallas.transit.area %>% 
  filter(!duplicated(GEOID20))

hou.no_repeats <- hou.transit.area %>% 
  filter(!duplicated(GEOID20))

dc.no_repeats <- dc.transit.area %>% 
  filter(!duplicated(GEOID20))

# Get Block Centroids 

atl_centroid <- st_centroid(atl.no_repeats)
mia_centroid <- st_centroid(mia.no_repeats)
dal_centroid <- st_centroid(dal.no_repeats)
hou_centroid <- st_centroid(hou.no_repeats)
dc_centroid <- st_centroid(dc.no_repeats)


plot(atl_centroid[,"geometry"], lwd = 0.5)


st_write(atl_centroid,  dsn=paste0("atl_centroid", "/", "atl_centroid.shp"))
st_write(mia_centroid, 
         dsn=paste0(".", "/", "mia_centroid.shp"),
         layer = "mia_centroid")
st_write(dal_centroid, "dal_centroid.shp")
st_write(hou_centroid, "hou_centroid.shp")
st_write(dc_centroid, "dc_centroid.shp")


st_write(atl.transit.area, "atl_buffer.shp")
st_write(miami.transit.area, "mia_buffer.shp")
st_write(dallas.transit.area, "dal_buffer.shp")
st_write(hou.transit.area, "hou_buffer.shp")
st_write(dc.transit.area, "dc_buffer.shp")



st_write(atl.no_repeats, "u_atl_buffer.shp")
st_write(mia.no_repeats, "u_mia_buffer.shp")
st_write(dal.no_repeats, "u_dal_buffer.shp")
st_write(hou.no_repeats, "u_hou_buffer.shp")
st_write(dc.no_repeats, "u_dc_buffer.shp")



st_write(atl.stops.no_repeats, "atl_stops.shp")
st_write(mia.stops.no_repeats, "mia_stops.shp")
st_write(dal.stops.no_repeats, "dal_stops.shp")
st_write(hou.stops.no_repeats, "hou_stops.shp")
st_write(dc.stops.no_repeats, "dc_stops.shp")

