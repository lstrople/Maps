###########
#libraries
###########

require(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(tidyverse)
library(patchwork)
library(gridExtra)
library(cowplot)


###########
#functions
###########
LATDMtoDDcoorindate <- function(coordinate) {
  
  split_coordinate <- strsplit(coordinate, " ")[[1]]
  
  # Extract degrees, minutes, and seconds from the latitude
  latitude_parts <- strsplit(split_coordinate[1], "°")[[1]]
  latitude_deg <- as.numeric(latitude_parts[1])
  latitude_min_parts <- strsplit(latitude_parts[2],  " ")
  latitude_min <- as.numeric(latitude_min_parts[1])
  
  
  # Calculate the latitude in decimal degrees
  latitude <- latitude_deg + latitude_min / 60
  hemisphere <- split_coordinate[2]
  
  # Convert the coordinate to numeric if possible
  latitude <- as.numeric(latitude)
  
  #Return the latitude
  return(latitude)
}


LOTDMtoDDcoorindate <- function(coordinate) {
  split_coordinate <- strsplit(coordinate, " ")[[1]]
  
  # Extract degrees, minutes, and seconds from the latitude
  latitude_parts <- strsplit(split_coordinate[1], "°")[[1]]
  latitude_deg <- as.numeric(latitude_parts[1])
  latitude_min_parts <- strsplit(latitude_parts[2],  " ")
  latitude_min <- as.numeric(latitude_min_parts[1])
  
  
  # Calculate the latitude in decimal degrees
  longitude <- latitude_deg + latitude_min / 60
  hemisphere <- split_coordinate[2]
  
  # Determine the hemisphere and adjust the sign of the longitude
  if (tolower(split_coordinate[2]) == "w") {
    longitude <- -longitude
  }
  
  longitude_numeric <- as.numeric(longitude)
  
  # Return the longitude
  return(longitude_numeric)
}



############
#datasets
###########

setwd("C:/Users/lstrople/OneDrive - Norwegian University of Life Sciences/Winter_paper/KML files")

gaspe.df <- read.csv("C:/Users/lstrople/OneDrive - Norwegian University of Life Sciences/Winter_paper/equipment_locations.csv")

winter.df <- read.csv("C:/Users/lstrople/OneDrive - Norwegian University of Life Sciences/Winter_paper/Equipment_winterpaper.csv")

cascmini.df <- read.csv("C:/Users/lstrople/OneDrive - Norwegian University of Life Sciences/Winter_paper/Casc_winter.csv")

# Apply the function to the lat column using lapply
gaspe.df$latDD <- lapply(gaspe.df$lat_A, LATDMtoDDcoorindate)

gaspe.df$lonDD <- lapply(gaspe.df$long_A, LOTDMtoDDcoorindate)

winter.df$latDD <- lapply(winter.df$lat_A, LATDMtoDDcoorindate)

winter.df$lonDD <- lapply(winter.df$long_A, LOTDMtoDDcoorindate)

cascmini.df$latDD <- lapply(cascmini.df$lat_A, LATDMtoDDcoorindate)

cascmini.df$lonDD <- lapply(cascmini.df$long_A, LOTDMtoDDcoorindate)

