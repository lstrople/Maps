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

################################################
#set working directory for where kml is stored
###############################################

setwd("C:/Users/lstrople/OneDrive - Norwegian University of Life Sciences/Winter_paper/KML files")

#Change path to where recievers csv is stored 
recievers.df <- Reciever_location <- read.csv("C:/Users/lstrople/OneDrive - Norwegian University of Life Sciences/Telemetry_Thibaul/Reciever_location.csv")

###########
#KML file
###########

#run steps indvidually a warning will stop the code otherwise 

thibSpace <- st_read("thibault.KML") %>%
  st_transform(32620)

step1 <- thibSpace %>% dplyr::slice(1)
step2 <- step1 %>% st_difference(thibSpace %>% dplyr::slice(2))
step3 <- step2 %>% st_difference(thibSpace %>% dplyr::slice(3))
step4 <- step3 %>% st_difference(thibSpace %>% dplyr::slice(4))

# Check validity of geometries
valid_geometries <- st_is_valid(thibSpace$geometry)
invalid_geometries <- which(!valid_geometries)
print(paste("Invalid geometries at indices: ", paste(invalid_geometries, collapse = ", ")))


urm <- 32620

Thib_selected <- dplyr::select(thibSpace, geometry) %>% st_zm()



##################
#convert recievers
##################

recievers_tibble.df <- as_tibble(recievers.df)
recievers_tibble.df.df <- dplyr::filter(recievers_tibble.df, !is.na(Long) & !is.na(Lat))
recievers_space.df <- st_as_sf(recievers_tibble.df, coords = c("Long", "Lat"))
recievers_space.df <- st_set_crs(recievers_space.df, 4326)


######
#Plot
######


thibtel_plot <- ggplot() +
  geom_sf(data = Thib_selected, color="#343A40", fill="#ADB5BD") + 
  geom_sf(data = recievers_space.df, aes(color = "recievers_space.df", shape = "recievers_space.df"), show.legend = TRUE)+
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA, size = 2), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  scale_color_manual(name = "Legend", 
                     values = c ("black"),
                     labels = c("Recievers")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "black" ),
                    labels = c("Recievers")) +
  scale_shape_manual(name = "Legend", 
                     values = c(16),
                     labels = c("Recievers"))# Add scale and North arrow


thibtel_plot <- thibtel_plot+
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

print(thibtel_plot)
ggsave("ThibRec.png", plot = thibtel_plot, width = 7, height = 5, units = "in", dpi = 300)

