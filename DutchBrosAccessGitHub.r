# Dutch Brother Access R Script
# Henry McKay (hhmckay@protonmail.com)
# 12/04/2021

# Uncomment and run below line to remove data if needed
# rm(list = ls())

# Java must be downloaded and installed to run this script
# https://www.oracle.com/java/technologies/downloads/#java11
# Set java parameters
options(java.parameters = "-Xmx10G")

# Set numbers to not show in scientific notation
options(scipen = 100)

# Load necessary libraries
library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(akima)
library(dplyr)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(tigris)

# Input your Census API Key (if you want to bring in Census data)
census_api_key("Your Census API Key", 
               overwrite = FALSE, install = FALSE)

# Set data path to folder with input data
data_path = "/Users/henrymckay/Desktop/DutchBrosAccess"

# Build r5 network
# An .osm.pbf mile must be placed in the data folder
# Zipped GTFS files can be included as well
r5r_core <- setup_r5(data_path, verbose = FALSE)

# Load csv file with destination points
points <- fread(file.path(data_path, "DutchBrosAccessibility.csv"))

# Retrieve census geometry shapes from the Census API
shapes <- block_groups(
  state = "CA",
  county = "Sacramento",
  year = 2019,
  cb = FALSE
)

# Convert census geometry centroid lat/lon from chr to num
shapes$INTPTLAT <- as.numeric(shapes$INTPTLAT)
shapes$INTPTLON <- as.numeric(shapes$INTPTLON)

# Change census geometry col names for use in r5
names(shapes)[names(shapes) == 'INTPTLAT'] <- 'lat'
names(shapes)[names(shapes) == 'INTPTLON'] <- 'lon'
names(shapes)[names(shapes) == 'GEOID'] <- 'id'

# Convert Census geometry to data frame
shapes <- as.data.frame(shapes)

# Retrieve road shapes from TIGER (to show on map)
roads <- roads("CA", "Sacramento")
roadsI <- roads[ which(roads$RTTYP=='I'),] 
roadsS <- roads[ which(roads$RTTYP=='S'),] 
roadsU <- roads[ which(roads$RTTYP=='U'),] 

# Set parameters for r5 travel time matrix
mode <- c("WALK")
max_walk_dist <- 4828.03
max_trip_duration <- 360
departure_datetime <- as.POSIXct("11-10-2021 8:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
  
# calculate a travel time matrix
ttm <- travel_time_matrix(r5r_core = r5r_core,
                          origins = shapes,
                          destinations = points,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_dist = max_walk_dist,
                          max_trip_duration = max_trip_duration,
                          verbose = FALSE)
  
# Apply decay function to travel times
# Run 2 line block of code corresponding to mode of choice
# Decay functions calibrated using NHTS data

# All Modes (n = 480,000)
ttm_decay <- ttm %>%
  mutate(decay_value = 1.122 * exp(-0.0632 * travel_time))
  
# Walking
ttm_decay <- ttm %>%
  mutate(decay_value = 1 * exp(-0.0631 * travel_time))
  
# Transit
ttm_decay <- ttm %>%
  mutate(decay_value = 1.215 * exp(-0.0229 * travel_time))
  
# Sum weighted travel times by origin point
# Set a time cutoff for trips to include
ttm_sum <- ttm_decay[travel_time <= 60, .(Score = sum(decay_value)), by=fromId]

# Define function to normalize values between 0 and 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Apply function to travel time matrix
ttm_sum$Score <- normalize(ttm_sum$Score)

# Join travel time matrix with map geometry
final_shapes <- merge(shapes, ttm_sum, by.x = "id", by.y = "fromId", all = TRUE)

# Replace empty data with 0
final_shapes[is.na(final_shapes)] = 0

# Convert the data to a spatial object
final_shapes <- st_as_sf(final_shapes)
  
# Create ggplot map object
map <- ggplot(final_shapes) +
  geom_sf(aes(fill = Score), color = NA) +
  scale_fill_viridis_c(option="inferno") +
  geom_sf(data = roadsI, color = "grey67", size = .8) +
  geom_sf(data = roadsU, color = "grey67", size = .8) +
  geom_sf(data = roadsS, color = "grey67", size = .3) +
  coord_sf(xlim = c(-121.9, -121), ylim = c(38, 38.75)) +
  labs(title = "Dutch Brothers Coffee Accessibility Score",
       subtitle = "Walking",
       caption = "Caption") +
  theme_void() 

# Adjust font, color, and size of map text
map <- map + theme(
    plot.title = element_text(color = "grey23", size = 30, face = "bold"),
    plot.subtitle = element_text(color = "grey23", size = 25),
    plot.caption = element_text(color = "grey23", size = 15, face = "italic", hjust = 0),
    legend.title=element_text(color = "grey23", size=15, face = "bold")
)

# Set filepath to map output
filepath = paste0("/Users/henrymckay/Desktop/Map.png")

# Save map as png
ggsave(filepath, 
       plot = map,
       dpi = 600,
       height = 12,
       width = 12,
       bg = 'white')

# Stop r5
stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
