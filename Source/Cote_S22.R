###################################
#must run main map file before use
###################################

coteSpace <- st_read("cote.KML") %>%
  st_transform(32620)

coteSpace <- coteSpace %>%
  slice(1) %>%
  st_difference(coteSpace %>% slice(2)) %>%
  st_difference(coteSpace %>% slice(3)) %>%
  st_difference(coteSpace %>% slice(4))

urm <- 32620

paul_selected <- dplyr::select(coteSpace, geometry) %>% st_zm()


##########
#coteplot
###########

cote.df <- subset(gaspe.df, lake=="Cote")
coteS22.df <- subset(cote.df, Season=="S22")
coteNet.df <- subset(coteS22.df, type=="NET")


##############
#convert nets
##############

coteNet_tibble.df <- as_tibble(coteNet.df)
coteNet_tibble.df <- dplyr::filter(coteNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
cotetNet_space.df <- st_as_sf(coteNet_tibble.df, coords = c("lonDD", "latDD"))
coteNet_space.df <- st_set_crs(cotetNet_space.df, 4326)


# Select the 'geometry' column from 'th' and set Z and M values
cote_selected <- dplyr::select(coteSpace, geometry) %>% st_zm()

cote_plot <- ggplot() +
  geom_sf(data = cote_selected, color="#343A40", fill="#ADB5BD") +
  geom_sf(data = coteNet_space.df, aes(color = "coteNet_space", shape = "coteNet_space"), show.legend = FALSE) +
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        #panel.border = element_rect(color = "black", 
        #fill = NA, 
        #linewidth = 2),
        axis.ticks.y = element_blank(), 
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
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

cote_plot <- cote_plot+
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

print(cote_plot)