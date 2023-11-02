######
#casc
######
HaySpace <- st_read("Haymard.KML") %>%
  st_transform(32620)

HaySpace <- HaySpace %>%
  slice(1) %>%
  st_difference(HaySpace %>% slice(2)) %>%
  st_difference(HaySpace %>% slice(3)) %>%
  st_difference(HaySpace %>% slice(4))

urm <- 32620

Hay_selected <- dplyr::select(HaySpace, geometry) %>% st_zm()



##########
#cascplot
##########

Hay.df <- subset(gaspe.df, lake=="Haymard")
HayS23.df <- subset(Hay.df, Season=="S23")
HayNetS23.df <- subset(HayS23.df, type=="NET")


##############
#convert nets
##############

#aes(x = lon, xend = lon2, y = lat, yend = lat2, colour = as.numeric(value)),

HayNetS23_tibble.df <- as_tibble(HayNetS23.df)
HayNetS23_tibble.df <- dplyr::filter(HayNetS23_tibble.df, !is.na(lonDD) & !is.na(latDD))
HayNetS23_space.df <- st_as_sf(HayNetS23_tibble.df, coords = c("lonDD", "latDD"))
HayNetS23_space.df <- st_set_crs(HayNetS23_space.df, 4326)


hayseg <- tibble(x=c(48.92858,48.92869), y=c(-66.27401,-66.27375))%>%
  st_as_sf(., coords=c("x", "y"))%>%
  st_set_crs(4326)


# Create a new data frame with start and end points
hayseg_segments <- hayseg %>%
  st_coordinates() %>%
  as.data.frame() 


# Select the 'geometry' column from 'th' and set Z and M values
Hay_selected <- dplyr::select(HaySpace, geometry) %>% st_zm()

HayS23_plot <- ggplot() +
  geom_sf(data = Hay_selected , color="#343A40", fill="#ADB5BD") + 
  geom_sf(data = HayNetS23_space.df, aes(color = "cascNet_space", shape = "cascNet_space"), show.legend = FALSE) +
  #geom_segment(data = hayseg_segments, aes(x = hayseg_segments[1, 2], xend = hayseg_segments[2, 2], y = hayseg_segments[1, 1], yend = hayseg_segments[2, 1]))
  geom_path(data = hayseg_segments, aes(x = X, y = Y ), color = "blue") +
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
                     values = c ("black"),
                     labels = c("Nets")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "black" ),
                    labels = c("Nets")) +
  scale_shape_manual(name = "Legend", 
                     values = c(16),
                     labels = c("Nets"))

# Add scale and North arrow
HayS23_plot <- HayS23_plot+
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


print(HayS23_plot)