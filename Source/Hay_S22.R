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
HayS22.df <- subset(Hay.df, Season=="S22")
HayNetS22.df <- subset(HayS22.df, type=="NET")
HaytrapS22.df <- subset(HayS22.df, type=="Trap")


##############
#convert nets
##############

#aes(x = lon, xend = lon2, y = lat, yend = lat2, colour = as.numeric(value)),

HayNetS22_tibble.df <- as_tibble(HayNetS22.df)
HayNetS22_tibble.df <- dplyr::filter(HayNetS22_tibble.df, !is.na(lonDD) & !is.na(latDD))
HayNetS22_space.df <- st_as_sf(HayNetS22_tibble.df, coords = c("lonDD", "latDD"))
HayNetS22_space.df <- st_set_crs(HayNetS22_space.df, 4326)

##############
#convert nets
##############

#aes(x = lon, xend = lon2, y = lat, yend = lat2, colour = as.numeric(value)),

HaytrapS22_tibble.df <- as_tibble(HaytrapS22.df)
HaytrapS22_tibble.df <- dplyr::filter(HaytrapS22_tibble.df, !is.na(lonDD) & !is.na(latDD))
HaytrapS22_space.df <- st_as_sf(HaytrapS22_tibble.df, coords = c("lonDD", "latDD"))
HaytrapS22_space.df <- st_set_crs(HaytrapS22_space.df, 4326)


# Select the 'geometry' column from 'th' and set Z and M values
Hay_selected <- dplyr::select(HaySpace, geometry) %>% st_zm()

HayS22_plot <- ggplot() +
  geom_sf(data = Hay_selected , color="#343A40", fill="#ADB5BD") + 
  geom_sf(data = HayNetS22_space.df, aes(color = "HayNetS22_space", shape = "HayNetS22_space"), show.legend = TRUE) +
  geom_sf(data = HaytrapS22_space.df, aes(color = "HayTrapS22_space", shape = "HayTrapS22_space"), show.legend = TRUE) +
  #geom_segment(data = hayseg aes(x = hayseg, xend = lon2, y = lat, yend = lat2))
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
                     values = c ("#212529","#6C757D"),
                     labels = c("Nets","Tip-Ups")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "#212529","#6C757D"),
                    labels = c("Nets","Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(16, 17),
                     labels = c("Nets","Tip-Ups"))


# Add scale and North arrow
HayS22_plot <- HayS22_plot+
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  )

print(HayS22_plot)
