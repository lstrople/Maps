######
#casc
######
portageSpace <- st_read("Portage.KML") %>%
  st_transform(32620)

portageSpace <- portageSpace %>%
  slice(1) %>%
  st_difference(portageSpace %>% slice(2)) %>%
  st_difference(portageSpace %>% slice(3)) %>%
  st_difference(portageSpace %>% slice(4))

urm <- 32620

portage_selected <- dplyr::select(portageSpace, geometry) %>% st_zm()


##########
#cascplot
##########

portage.df <- subset(canlakes.df, Lake=="Portage")
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


portage_plot <- ggplot() +
  geom_sf(data = portage_selected, color="#343A40", fill="#DEE2E6") + 
  geom_sf(data = portageMT_space.df, aes(color = "portageMT_space", fill = "portageMT_space", shape = "portageMT_space"), show.legend = TRUE) +
  geom_sf(data = portageANG_space.df, aes(color = "portageANG_space", fill = "portageANG_space", shape = "portageANG_space"), show.legend = TRUE) +
  geom_sf(data = portageHGN_space.df, aes(color = "portageHGN_space", fill = "portageHGN_space",  shape = "portageHGN_space"), show.legend = TRUE) +
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
                     labels = c("Tip-Ups", "Nets", "Traps")) +
  scale_fill_manual(name = "Legend", 
                    values = c("#ADB5BD","black","#f8f9fa"),
                    labels = c("Tip-Ups", "Nets", "Traps")) +
  scale_shape_manual(name = "Legend", 
                     values = c(24, 21, 22),
                     labels = c("Tip-Ups", "Nets", "Traps"))
  portage_plot <- portage_plot + ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )


print(portage_plot)

ggsave("portage.png", plot = portage_plot, width = 7, height = 5, units = "in", dpi = 300)
