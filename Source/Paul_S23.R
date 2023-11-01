#########
#Paul map
#########
paulSpace <- st_read("Paul.KML") %>%
  st_transform(32620)

paulSpace <- paulSpace %>%
  slice(1) %>%
  st_difference(paulSpace %>% slice(2)) %>%
  st_difference(paulSpace %>% slice(3)) %>%
  st_difference(paulSpace %>% slice(4))

urm <- 32620

paul_selected <- dplyr::select(paulSpace, geometry) %>% st_zm()


Paul.df <- subset(gaspe.df, lake=="Paul")
PaulS23.df <- subset(Paul.df, Season=="S23")
PaulNet.df <- subset(PaulS23.df, type=="NET")


##############
#convert nets
##############

PaulNet_tibble.df <- as_tibble(PaulNet.df)
PaulNet_tibble.df <- dplyr::filter(PaulNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
PaulNet_space.df <- st_as_sf(PaulNet_tibble.df, coords = c("lonDD", "latDD"))
PaulNet_space.df <- st_set_crs(PaulNet_space.df, 4326)


##########
#Paulplot
###########

# Select the 'geometry' column from 'th' and set Z and M values
paul_selected <- dplyr::select(paulSpace, geometry) %>% st_zm()

paulS23_plot <- ggplot() +
  geom_sf(data = paul_selected, color="#343A40", fill="#ADB5BD") +
  geom_sf(data = PaulNet_space.df, aes(color = "PaulNet_space", shape = "PaulNet_space"), show.legend = FALSE) +
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
  #legend.justification = c("right", "bottom")
  scale_color_manual(name = "Legend", 
                     values = c ("black"),
                     labels = c("Nets")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "black" ),
                    labels = c("Nets")) +
  scale_shape_manual(name = "Legend", 
                     values = c(16),
                     labels = c("Nets"))


# Add scale and North arrow

paulS23_plot <- paulS23_plot+
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

print(paulS23_plot)

ggsave("pauls23_plot.png", plot =paulS23_plot, width = 6, height = 4, dpi = 300)
