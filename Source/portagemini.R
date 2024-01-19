#############
#portagemini
#############
portageMSpace <- st_read("portagemini.KML") %>%
  st_transform(32620)

portageMSpace <- portageMSpace %>%
  slice(1) %>%
  st_difference(portageMSpace %>% slice(2)) %>%
  st_difference(portageMSpace %>% slice(3)) %>%
  st_difference(portageMSpace %>% slice(4))

urm <- 32620

portageM_selected <- dplyr::select(portageMSpace, geometry) %>% st_zm()


##########
#cascplot
##########

portage.df <- subset(portagemini.df, Lake=="Portage")
portageHGN.df <- subset(portage.df, Gear=="HGN")
portageMT.df <- subset(portage.df, Gear=="MT")
portageANG.df <- subset(portage.df, Gear=="ANG")


###########
#convert HGN
###########


portageHGN_tibble.df <- as_tibble(portageHGN.df)
portageHGN_tibble.df <- dplyr::filter(portageHGN_tibble.df, !is.na(Latitude) & !is.na(Longitude))
portageHGN_space.df <- st_as_sf(portageHGN_tibble.df, coords = c("Longitude","Latitude"))
portageHGN_space.df <- st_set_crs(portageHGN_space.df, 4326)


##############
#convert nets
##############

portageMT_tibble.df <- as_tibble(portageMT.df)
portageMT_tibble.df <- dplyr::filter(portageMT_tibble.df,  !is.na(Latitude) & !is.na(Longitude))
portageMT_space.df <- st_as_sf(portageMT_tibble.df, coords = c("Longitude","Latitude"))
portageMT_space.df <- st_set_crs(portageMT_space.df, 4326)

##############
#convert traps
##############

portageANG_tibble.df <- as_tibble(portageANG.df)
portageANG_tibble.df <- dplyr::filter(portageANG_tibble.df, !is.na(Latitude) & !is.na(Longitude))
portageANG_space.df <- st_as_sf(portageANG_tibble.df, coords = c("Longitude","Latitude"))
portageANG_space.df <- st_set_crs(portageANG_space.df, 4326)


portagemini_plot <- ggplot() +
  geom_sf(data = portageM_selected, color="#343A40", fill="#DEE2E6") + 
  geom_sf(data = portageMT_space.df, aes(color = "portageMT_space", fill = "portageMT_space", shape = "portageMT_space"), show.legend = FALSE) +
  geom_sf(data = portageANG_space.df, aes(color = "portageANG_space", fill = "portageANG_space", shape = "portageANG_space"), show.legend = FALSE) +
  geom_sf(data = portageHGN_space.df, aes(color = "portageHGN_space", fill = "portageHGN_space",  shape = "portageHGN_space"), show.legend = FALSE) +
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
                    values = c("#495057","black","#f8f9fa"),
                    labels = c("Nets","Traps","Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(23, 21, 22),
                     labels = c("Angling","Tip-Ups", "Traps"))# Add scale and North arrowportagemini_plot <- portage_plot+
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  )
  


print(portagemini_plot)

ggsave("portagemini.png", plot = portagemini_plot, width = 7, height = 5, units = "in", dpi = 300)
