
setwd ("/Users/langstonford/Library/CloudStorage/OneDrive-RiceUniversity/Junior 2023-2024/2024 Spring Semester/SOCI 460/Research Project/nhgis0007_shape")

library(sf)



######################################################################################################
#Florida Blocks

filename <- "nhgis0007_shapefile_tl2020_120_block_2020"

# Unzip the shapefile folder 
unzip(zipfile=paste0(filename, ".zip"), exdir=filename)


# Find the layer name by opening the unzipped folder 
# and looking at the file names
# OR
# Look at info about the shapefile before importing
st_layers(dsn=filename)

# Record the name of the layer to import
layername <- "FL_block_2020"


# Import the shapefile    !this may take a minute!
blocks.fl <- st_read(dsn=filename, layer=layername,
                  stringsAsFactors=FALSE, options="ENCODING=latin1")







blocks_miami <- blocks.fl[blocks.fl$COUNTYFP20 %in% "011" | blocks.fl$COUNTYFP20 %in% "086" | blocks.fl$COUNTYFP20 %in% "099", ]


######################################################################################################
#Georgia Blocks

filename <- "nhgis0007_shapefile_tl2020_130_block_2020"

# Unzip the shapefile folder 
unzip(zipfile=paste0(filename, ".zip"), exdir=filename)


# Find the layer name by opening the unzipped folder 
# and looking at the file names
# OR
# Look at info about the shapefile before importing
st_layers(dsn=filename)

# Record the name of the layer to import
layername <- "GA_block_2020"


# Import the shapefile    !this may take a minute!
blocks.ga <- st_read(dsn=filename, layer=layername,
                     stringsAsFactors=FALSE, options="ENCODING=latin1")







blocks_atl <- blocks.ga[blocks.ga$COUNTYFP20 %in% "121" | blocks.ga$COUNTYFP20 %in% "089" | blocks.ga$COUNTYFP20 %in% "063", ]

######################################################################################################
#Texas Blocks

filename <- "nhgis0007_shapefile_tl2020_480_block_2020"

# Unzip the shapefile folder 
unzip(zipfile=paste0(filename, ".zip"), exdir=filename)


# Find the layer name by opening the unzipped folder 
# and looking at the file names
# OR
# Look at info about the shapefile before importing
st_layers(dsn=filename)

# Record the name of the layer to import
layername <- "TX_block_2020"


# Import the shapefile    !this may take a minute!
blocks.tx <- st_read(dsn=filename, layer=layername,
                     stringsAsFactors=FALSE, options="ENCODING=latin1")







blocks_atl <- blocks.ga[blocks.ga$COUNTYFP20 %in% "121" | blocks.ga$COUNTYFP20 %in% "089" | blocks.ga$COUNTYFP20 %in% "063", ]

######################################################################################################
#DC Blocks

filename <- "nhgis0007_shapefile_tl2020_110_block_2020"

# Unzip the shapefile folder 
unzip(zipfile=paste0(filename, ".zip"), exdir=filename)


# Find the layer name by opening the unzipped folder 
# and looking at the file names
# OR
# Look at info about the shapefile before importing
st_layers(dsn=filename)

# Record the name of the layer to import
layername <- "DC_block_2020"


# Import the shapefile    !this may take a minute!
blocks.dc <- st_read(dsn=filename, layer=layername,
                     stringsAsFactors=FALSE, options="ENCODING=latin1")







######################################################################################################
#US cities

filename <- "nhgis0008_shapefile_tl2020_us_place_2020"

# Unzip the shapefile folder 
unzip(zipfile=paste0(filename, ".zip"), exdir=filename)


# Find the layer name by opening the unzipped folder 
# and looking at the file names
# OR
# Look at info about the shapefile before importing
st_layers(dsn=filename)

# Record the name of the layer to import
layername <- "US_place_2020"


# Import the shapefile    !this may take a minute!
place <- st_read(dsn=filename, layer=layername,
                     stringsAsFactors=FALSE, options="ENCODING=latin1")


#Atlanta Subset
atl.city <- place[place$NAMELSAD %in% "Atlanta city" & place$STATEFP %in% "13",]
atl.blocks<- st_intersection( atl.city, blocks.ga)
atl.blocks.subset<- blocks.ga[blocks.ga$GEOID20 == atl.blocks$GEOID20]



#Miami Subset
miami.city <- place[place$NAMELSAD %in% "Miami city" & place$STATEFP %in% "12",]
miami.blocks<- st_intersection( miami.city, blocks.fl)

#Dallas Subset
dallas.city <- place[place$NAMELSAD %in% "Dallas city" & place$STATEFP %in% "48",]
dallas.blocks<- st_intersection( dallas.city, blocks.tx)

#Houston Subset
houston.city <- place[place$NAMELSAD %in% "Houston city" & place$STATEFP %in% "48",]
houston.blocks<- st_intersection( houston.city, blocks.tx)


#Place holder for making the file saveable. For some reason intersected file does not save. I assume because of the repitition columns
atl.blocks.subset <- filter(blocks.ga, GEOID20 %in% atl.blocks$GEOID20)
miami.blocks.subset <- filter(blocks.fl, GEOID20 %in% miami.blocks$GEOID20)
dallas.blocks.subset <- filter(blocks.tx, GEOID20 %in% dallas.blocks$GEOID20)
houston.blocks.subset <- filter(blocks.tx, GEOID20 %in% houston.blocks$GEOID20)


par(mar=c(0, 0, 0, 0))


plot(houston.city[, "geometry"], lwd=0.5)
plot(blocks.dc[, "geometry"], lwd=0.5)

#Save the Files
st_write(atl.blocks.subset, "atl_blocks.shp")
st_write(miami.blocks.subset, "miami_blocks.shp")
st_write(dallas.blocks.subset, "dallas_blocks.shp")
st_write(houston.blocks.subset, "houston_blocks.shp")



