#########
#cascmini
##########
cascSpace <- st_read("Casc_top.KML") %>%
  st_transform(32620)

cascTSpace <- cascSpace %>%
  slice(1) %>%
  st_difference(cascSpace %>% slice(2)) %>%
  st_difference(cascSpace %>% slice(3)) %>%
  st_difference(cascSpace %>% slice(4))

urm <- 32620

cascT_selected <- dplyr::select(cascSpace, geometry) %>% st_zm()


##########
#cascplot
##########

cascTU.df <- subset(cascmini.df, type=="TU")
cascTrap.df <- subset(cascmini.df, type=="trap")
cascNet.df <- subset(cascmini.df, type=="NET")


###########
#convert TU
###########


cascTU_tibble.df <- as_tibble(cascTU.df)
cascTU_tibble.df <- dplyr::filter(cascTU_tibble.df, !is.na(lonDD) & !is.na(latDD))
cascTU_space.df <- st_as_sf(cascTU_tibble.df, coords = c("lonDD", "latDD"))
cascTU_space.df <- st_set_crs(cascTU_space.df, 4326)


##############
#convert nets
##############

cascNet_tibble.df <- as_tibble(cascNet.df)
cascNet_tibble.df <- dplyr::filter(cascNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
cascNet_space.df <- st_as_sf(cascNet_tibble.df, coords = c("lonDD", "latDD"))
cascNet_space.df <- st_set_crs(cascNet_space.df, 4326)

##############
#convert traps
##############

cascTrap_tibble.df <- as_tibble(cascTrap.df)
cascTrap_tibble.df <- dplyr::filter(cascTrap_tibble.df, !is.na(lonDD) & !is.na(latDD))
cascTrap_space.df <- st_as_sf(cascTrap_tibble.df, coords = c("lonDD", "latDD"))
cascTrap_space.df <- st_set_crs(cascTrap_space.df, 4326)

#######
#Lines

connections_df <- data.frame(
  from = c(3, 5, 2),  # Index of the starting points in sf_object
  to = c(5, 6, 6)     # Index of the ending points in sf_object
  
)


line <- st_sfc(st_linestring(st_coordinates(cascNet_space.df)),
               crs = st_crs(cascNet_space.df))

allCoords <- as.matrix(st_coordinates(cascNet_space.df))
lines <- lapply(1:nrow(connections_df),
                function(r){
                  rbind(allCoords[connections_df[r,1], ],
                        allCoords[connections_df[r,2], ])
                }) %>%
  st_multilinestring(.) %>%
  st_sfc(., crs = st_crs(cascNet_space.df))




# Create the zoomed-in plot
zoomed_cascW_plot <- ggplot() +
  geom_sf(data = cascT_selected, color = "#343A40", fill = "#DEE2E6") +
  geom_sf(data = cascTrap_space.df, aes(color = "cascTrap_space", fill = "cascTrap_space", shape = "cascTrap_space"), show.legend = FALSE) +
  geom_sf(data = cascTU_space.df, aes(color = "cascTU_space", fill = "cascTU_space", shape = "cascTU_space"), show.legend = FALSE) +
  geom_sf(data = cascNet_space.df, aes(color = "cascNet_space", fill = "cascNet_space", shape = "cascNet_space"), show.legend = FALSE) +
  geom_sf(data = lines, color = "black", linetype = "solid") +
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 2),
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  scale_color_manual(name = "Legend", 
                     values = c ("black","black","black"),
                     labels = c("Nets","Traps","Tip-Ups")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "black","#f8f9fa","#ADB5BD"),
                    labels = c("Nets","Traps","Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(21, 22, 24),
                     labels = c("Nets","Traps","Tip-Ups"))

# Set the zoomed-in extent

# Add scale and North arrow to the zoomed-in plot
zoomed_cascW_plot <- zoomed_cascW_plot +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) 


# Print and save the zoomed-in plot
print(zoomed_cascW_plot)

ggsave("zoomed_cascW.png", plot = zoomed_cascW_plot, width = 3, height = 4, units = "in", dpi = 300)


