####################################################################################################
# Langston Ford and Alan Tapper
#
# Who Gets to Live Near Rail Transit?
# Effects of Rail Transit Access and Walkability on the Housing Market in Large Southern Cities
#
# Block Group Incomes and Values
#
# This file contains code for downloading and processing block group shapefile data. It creates and
# writes a shapefile for each of the five cities. Each file contains every block group that has
# an intersection with the city limits of that city.
####################################################################################################

setwd("/Users/langstonford/Library/CloudStorage/OneDrive-RiceUniversity/Junior 2023-2024/2024 Spring Semester/SOCI 460/Research Project/City Block Group")
library(sf)
library(dplyr)

# UNZIP Block Group Files


abr <- c("DC", "FL", "GA", "TX")
for (abreviation in abr){
  assign(x = paste0(abreviation), 
         st_read(dsn=paste0(abreviation, "_blck_grp_2022"), 
                 layer=paste0(abreviation, "_blck_grp_2022"),
                 stringsAsFactors=FALSE, options="ENCODING=latin1"))
}



# Grab US Cities
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
atlanta.blocks<- st_intersection( atl.city, GA)
atlanta.blocks.subset<- blocks.ga[blocks.ga$GEOID20 == atl.blocks$GEOID20]

#Miami Subset
miami.city <- place[place$NAMELSAD %in% "Miami city" & place$STATEFP %in% "12",]
miami.blocks<- st_intersection( miami.city, FL)

#Dallas Subset
dallas.city <- place[place$NAMELSAD %in% "Dallas city" & place$STATEFP %in% "48",]
dallas.blocks<- st_intersection( dallas.city, TX)

#Houston Subset
houston.city <- place[place$NAMELSAD %in% "Houston city" & place$STATEFP %in% "48",]
houston.blocks<- st_intersection( houston.city, TX)

plot(atl.city [,"geometry"], lwd =.5)


for (statenames in states) {
    st_intersection( atl.city, blocks.ga)
    
}


landvalue <- read.csv("nhgis0003_ds262_20225_blck_grp.csv")
income <- read.csv ("nhgis0001_ds262_20225_blck_grp_E.csv")


citynames <- c("atlanta", "miami", "dallas", "houston")
for (city in citynames){
  assign(paste0(city, ".blocks"),
         select(get(paste0(city, ".blocks")), !GISJOIN))
  assign(paste0(city, ".blocks"),
         rename(get(paste0(city, ".blocks")), "GISJOIN" = "GISJOIN.1"))
}


landvalue.sub <- landvalue %>%
  select( AQU4E001, GISJOIN )
income.sub <- income %>%
  select( AQP6E001, GISJOIN )

value <- full_join(landvalue.sub, income.sub, by = "GISJOIN")


atlanta.blocks.subset <- filter(GA, GISJOIN %in% atlanta.blocks$GISJOIN)
miami.blocks.subset <- filter(FL, GISJOIN %in% miami.blocks$GISJOIN)
dallas.blocks.subset <- filter(TX, GISJOIN %in% dallas.blocks$GISJOIN)
houston.blocks.subset <- filter(TX, GISJOIN %in% houston.blocks$GISJOIN)




for (city in citynames){
  assign( paste0(city, "_values"), 
          left_join(get(paste0(city, ".blocks.subset")), 
                        value, by = "GISJOIN"))
}

left_join(dallas.blocks, value, by = "GISJOIN")
          

library(ggplot2)

dallas_values %>%
  ggplot( aes(geometry = geometry, fill = AQP6E001),
          lwd = 0)+
  geom_sf()

for(city in citynames) {
  city_fname <- paste0(city, "_", "values")
  select(!city_fname, )
} 


  for(city in citynames) {
    city_fname <- paste0(city, "_", "values")
    st_write(get(city_fname), dsn=city_fname, layer=paste0(city_fname, ".shp"), driver="ESRI Shapefile")
  } 
  
dc_values<- left_join(DC, value)


st_write(dc_values, dsn="dc_values", layer=paste0("dc_values", ".shp"), driver="ESRI Shapefile")


