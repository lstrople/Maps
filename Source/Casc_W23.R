######
#casc
######
cascSpace <- st_read("cascapedia.KML") %>%
  st_transform(32620)

cascSpace <- cascSpace %>%
  slice(1) %>%
  st_difference(cascSpace %>% slice(2)) %>%
  st_difference(cascSpace %>% slice(3)) %>%
  st_difference(cascSpace %>% slice(4))

urm <- 32620

casc_selected <- dplyr::select(cascSpace, geometry) %>% st_zm()

##########
#cascplot
##########

casc.df <- subset(gaspe.df, lake=="Cascapedia")
cascW23.df <- subset(casc.df, Season=="W23")
cascTU.df <- subset(cascW23.df, type=="TU")
cascTrap.df <- subset(cascW23.df, type=="trap")

###########
#convert TU
##########

cascTU_tibble.df <- as_tibble(cascTU.df)
cascTU_tibble.df <- dplyr::filter(cascTU_tibble.df, !is.na(lonDD) & !is.na(latDD))
cascTU_space.df <- st_as_sf(cascTU_tibble.df, coords = c("lonDD", "latDD"))
cascTU_space.df <- st_set_crs(cascTU_space.df, 4326)


##############
#convert traps
##############

cascTrap_tibble.df <- as_tibble(cascTrap.df)
cascTrap_tibble.df <- dplyr::filter(cascTrap_tibble.df, !is.na(lonDD) & !is.na(latDD))
cascTrap_space.df <- st_as_sf(cascTrap_tibble.df, coords = c("lonDD", "latDD"))
cascTrap_space.df <- st_set_crs(cascTrap_space.df, 4326)


# Select the 'geometry' column from 'th' and set Z and M values
casc_selected <- dplyr::select(cascSpace, geometry) %>% st_zm()

cascW23_plot <- ggplot() +
  geom_sf(data = casc_selected, color="#343A40", fill="#ADB5BD") + 
  geom_sf(data = cascTrap_space.df, aes(color = "cascTrap_space", shape = "cascTrap_space"), show.legend = FALSE) +
  geom_sf(data = cascTU_space.df, aes(color = "cascTU_space", shape = "cascTU_space"), show.legend = FALSE) +
  #geom_path(data = ThibaultNet_space.df, aes(x = your_x_column, y = your_y_column, group = group_column), color = "blue") +  # Replace your_x_column, your_y_column, and group_column with appropriate column names
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  #legend.text = element_text(size=8), 
  #legend.position = c(0.05, .95), 
  #legend.justification = c("right", "bottom"))
  scale_color_manual(name = "Legend", 
                     values = c ("#6C757D","#212529"),
                     labels = c("Traps","Tip-Ups")) +
  scale_fill_manual(name = "Legend", 
                    values = c("#6C757D","#212529" ),
                    labels = c("Traps","Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(15, 17),
                     labels = c("Traps","Tip-Ups"))

# Add scale and North arrow
cascW23_plot <- cascW23_plot+
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )


print(cascW23_plot)

ggsave("CascW23.png", plot = cascW23_plot, width = 7, height = 5, units = "in", dpi = 300)





