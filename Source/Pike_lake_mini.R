######
#casc
######
pikeSpace <- st_read("pike_mini.KML") %>%
  st_transform(32620)

pikeSpace <- pikeMSpace %>%
  slice(1) %>%
  st_difference(pikeSpace %>% slice(2)) %>%
  st_difference(pikeSpace %>% slice(3)) %>%
  st_difference(pikeSpace %>% slice(4))

urm <- 32620

pikeM_selected <- dplyr::select(pikeSpace, geometry) %>% st_zm()


##########
#cascplot
##########

pike.df <- subset(pikemini.df, Lake=="Pike")
pikeHGN.df <- subset(pike.df, Gear=="HGN")
pikeMT.df <- subset(pike.df, Gear=="MT")
pikeANG.df <- subset(pike.df, Gear=="ANG")


###########
#convert HGN
###########


pikeHGN_tibble.df <- as_tibble(pikeHGN.df)
pikeHGN_tibble.df <- dplyr::filter(pikeHGN_tibble.df, !is.na(Latitude) & !is.na(Longitude))
pikeHGN_space.df <- st_as_sf(pikeHGN_tibble.df, coords = c("Longitude","Latitude"))
pikeHGN_space.df <- st_set_crs(pikeHGN_space.df, 4326)


##############
#convert nets
##############

pikeMT_tibble.df <- as_tibble(pikeMT.df)
pikeMT_tibble.df <- dplyr::filter(pikeMT_tibble.df,  !is.na(Latitude) & !is.na(Longitude))
pikeMT_space.df <- st_as_sf(pikeMT_tibble.df, coords = c("Longitude","Latitude"))
pikeMT_space.df <- st_set_crs(pikeMT_space.df, 4326)

##############
#convert traps
##############

pikeANG_tibble.df <- as_tibble(pikeANG.df)
pikeANG_tibble.df <- dplyr::filter(pikeANG_tibble.df, !is.na(Latitude) & !is.na(Longitude))
pikeANG_space.df <- st_as_sf(pikeANG_tibble.df, coords = c("Longitude","Latitude"))
pikeANG_space.df <- st_set_crs(pikeANG_space.df, 4326)



pike_plot <- ggplot() +
  geom_sf(data = pikeM_selected, color="#343A40", fill="#DEE2E6") + 
  geom_sf(data = pikeMT_space.df, aes(color = "pikeMT_space",fill = "pikeMT_space", shape = "pikeMT_space"), show.legend = TRUE) +
  geom_sf(data = pikeHGN_space.df, aes(color = "pikeHGN_space", fill = "pikeHGN_space",  shape = "pikeHGN_space"), show.legend = TRUE) +
  geom_sf(data = pikeANG_space.df, aes(color = "pikeANG_space", fill = "pikeANG_space", shape = "pikeANG_space"), show.legend = TRUE) +
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  scale_color_manual(name = "Legend", 
                     values = c ("black","black","black"),
                     labels = c("Angling","Tip-Ups", "Traps")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "black","#ADB5BD","#f8f9fa"),
                    labels = c("Angling","Tip-Ups", "Traps")) +
  scale_shape_manual(name = "Legend", 
                     values = c(21, 24, 22),
                     labels = c("Angling","Tip-Ups", "Traps"))# Add scale and North arrow
pike_plot <- pike_plot+
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  )


print(pike_plot)

ggsave("pike.png", plot =pike_plot, width = 7, height = 5, units = "in", dpi = 300)
