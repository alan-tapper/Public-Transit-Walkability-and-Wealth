####################################################################################################
# External API Call Wrapper Functions
#
# These functions are wrapper functions for the api calls we perform.
####################################################################################################

# Takes in coordinates and performs reverse geocoding using Google's Geocoding API.
#
# For more information visit https://www.walkscore.com/professional/api.php.
reverseGeocode <- function(lat, lng) {
  
  resp <- request(paste0("https://maps.googleapis.com/maps/api/geocode/json?", 
                        "latlng=", lat, "," , lng,
                        "&key=", googleMapsApiKey)) |>
    req_perform()
  
  body <- resp_body_json(resp)
  topResultAddress <- body[["results"]][[1]][["formatted_address"]]
  
  topResultAddress
}


# Takes in coordinates and an address and calculates the Walk Score using Walk Score's Walk Score API.
#
# For more information visit https://www.walkscore.com/professional/api.php.
getWalkScore <- function(lat, lng, address) {
  
  #take off the ", USA" at the end
  address <- substr(address, 0, nchar(address) - 5)
  
  #remove commas
  address <- str_remove_all(address, ",")
  
  #remove #
  address <- str_remove_all(address, "#")
  
  #escape spaces
  addressWithSpacesEscaped <- str_replace_all(address, " ", "%20")
  
  resp <- request(paste0("https://api.walkscore.com/score?format=json",
                        "&address=", addressWithSpacesEscaped,
                        "&lat=", lat,
                        "&lon=", lng,
                        "&wsapikey=", walkScoreApiKey)) |>
    req_perform()
    
  body <- resp_body_json(resp)
  
  #if we exceeded our daily quota
  if (body[["status"]] == 41) {
    NULL
  }
  body[["walkscore"]]
}


# Takes in two sets of coordinates, one for the block group centroid and the other for the stop.
# Calculates the time (in seconds) and the distance (in meters) to walk from the centroid to the
# stop using Google's Routes API.
#
# For more information visit https://developers.google.com/maps/documentation/routes.
getWalkTime <- function(block_lat, block_lng, stop_lat, stop_lng) {
  
  obj <- jsonlite::fromJSON(paste0('{"origin":{"location":{"latLng":{',
                                   '"latitude": ', block_lat,
                                   ',"longitude": ', block_lng,
                                   '}}},"destination":{"location":{"latLng":{',
                                   '"latitude": ', stop_lat,
                                   ',"longitude": ', stop_lng,
                                   '}}},"travelMode": "WALK"}'))
  
  resp <- request("https://routes.googleapis.com/directions/v2:computeRoutes") |>
    req_headers(
      `Content-Type` = "application/json",
      `X-Goog-Api-Key` = googleMapsApiKey,
      `X-Goog-FieldMask` = "routes.duration,routes.distanceMeters") |>
    req_body_json(obj) |>
    req_perform()
  
  body <- resp_body_json(resp)
  if (length(body) == 0) {
    NULL
  } else {
    route <- body[["routes"]][[1]]
    
    durationString <- route[["duration"]]
    distanceMetersString <- route[["distanceMeters"]]
    
    #remove the s from the duration string
    durationString <- substr(durationString, 0, nchar(durationString) - 1)
    
    #return as an int
    c(strtoi(durationString), strtoi(distanceMetersString))
  }
}
