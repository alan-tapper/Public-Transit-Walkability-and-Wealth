####################################################################################################
# Helper Functions
#
# These functions handle tasks related to building our our datasets.
####################################################################################################

# Loads a shapefile with block groups and data on income and value. Returns the data with only the
# columns we care about.
setupBlockGroups <- function(filename) {
  # Load in block group shape file. This file already includes information on 
  # Median Income in the last 12 months and Median Value of Owner-occupied Housing Unit 
  block_group_values <- st_read(dsn=paste0("blockdata/", filename), layer=filename,
                                stringsAsFactors=FALSE, options="ENCODING=latin1")
  
  # Restrict to the data we use
  block_group_values <- block_group_values[c("GISJOIN", "AQP6E001", "AQU4E001", "INTPTLAT", "INTPTLON")] %>%
    
    #rename to human-readable names
    rename_at('AQP6E001', ~'INCOME') %>%
    rename_at('AQU4E001', ~'VALUE') %>%
    rename_at('INTPTLAT', ~'LAT') %>%
    rename_at('INTPTLON', ~'LNG') %>%
    mutate(WALKSCO = 0)
  
  block_group_values
}

# Loads a shapefile with rail transit stops. Returns the data with only the columns we care about.
setupStops <- function(filename) {
  stops <- st_read(dsn=paste0("blockdata/", filename), layer=filename,
                   stringsAsFactors=FALSE, options="ENCODING=latin1")
  
  stops <- stops[c("GISJOIN", "INTPTLA", "INTPTLO", "stop_nm")] %>%
    rename_at('INTPTLA', ~'LAT') %>%
    rename_at('INTPTLO', ~'LNG')
}

# Takes in a dataframe with block group data, and adds a new column with the walk score of the
# centroid of that dataframe.
addWalkScoresToBlockGroups <- function(block_group_values) {
  #loop through rows
  for (i in 1:nrow(block_group_values)) {
    #extract block group coordinates
    block_lat <- toString(block_group_values[i, 'LAT']$LAT)
    block_lng <- toString(block_group_values[i, 'LNG']$LNG)
    
    #performs reverse geocoding (getting address from coordinates)
    address <- reverseGeocode(block_lat, block_lng)
    
    
    if (!is.null(address)) {
      #calculates the walk score at the centroid
      ws <- getWalkScore(block_lat, block_lng, address)
      
      if (!is.null(ws)){
        #stores the walk score in the data frame
        block_group_values[i, "WALKSCO"] = ws
      }
    }
  }
  
  block_group_values
}

# Takes in coordinates and calculates the nearest transit stop (using euclidean distance). Returns
# the identifier (GISJOIN), name, and coordinates of that stop, as well as the distance to that stop.
getClosestStop <- function(block_lat, block_lng, stops) {
  
  closest_stop_elements <- NULL
  closest_stop_distance <- Inf
  
  for (i in 1:nrow(stops)) {
    stop_lat <- as.numeric(stops[i, "LAT"]$LAT)
    stop_lng <- as.numeric(stops[i, "LNG"]$LNG)
    stop_distance <- sqrt((block_lat - stop_lat) ** 2 + (block_lng - stop_lng) ** 2)
    if (stop_distance < closest_stop_distance) {
      closest_stop_distance <- stop_distance
      closest_stop_elements <- c(stops[i, "GISJOIN"]$GISJOIN, 
                                 stops[i, "stop_nm"]$stop_nm, 
                                 stop_lat, 
                                 stop_lng, 
                                 stop_distance)
    }
  }
  
  closest_stop_elements
}

# Takes in dataframes of block group data and stop data. Creates three new columns in the block group
# dataframe. CLOSEST is the name of the closest stop (calculated using getClosestStop above). WALKTIM is the
# time (in seconds) it takes to walk from the centroid of the block to that stop. WALKDIS is the 
# distance (in meters).
addWalktimes <- function(block_group_values, stops) {
  #initialize columns
  block_group_values_walktime <- block_group_values %>%
    mutate(CLOSEST = "") %>%
    mutate(WALKTIM = 0) %>%
    mutate(WALKDIS = 0)
  
  #loop through block groups
  for (i in 1:nrow(block_group_values_walktime)) {
    #extract coordinates of the centroid of the block group
    block_lat <- as.numeric(block_group_values_walktime[i, 'LAT']$LAT)
    block_lng <- as.numeric(block_group_values_walktime[i, 'LNG']$LNG)
    
    #get closest stop and related info
    closest_stop_elements <- getClosestStop(block_lat, block_lng, stops)
    
    #unpack that info
    stop_gisjoin <- closest_stop_elements[1]
    stop_nm <- closest_stop_elements[2]
    stop_lat <- as.numeric(closest_stop_elements[3])
    stop_lng <- as.numeric(closest_stop_elements[4])
    stop_distance <- as.numeric(closest_stop_elements[5])
    
    #get walk time and distance
    walk_time_and_distance <- getWalkTime(block_lat, block_lng, stop_lat, stop_lng)
    
    #unpack
    walk_time <- walk_time_and_distance[1]
    walk_distance <- walk_time_and_distance[2]

    #input closest stop name
    block_group_values_walktime[i, "CLOSEST"] <- stop_nm
    
    #input walk time
    if (is.null(walk_time)) {
      block_group_values_walktime[i, "WALKTIM"] <- NA
    } else {
      block_group_values_walktime[i, "WALKTIM"] <- walk_time
    }
    
    #input walk distance
    if (is.null(walk_distance)) {
      block_group_values_walktime[i, "WALKDIS"] <- NA
    } else {
      block_group_values_walktime[i, "WALKDIS"] <- walk_distance
    }
  }
  
  block_group_values_walktime
}
