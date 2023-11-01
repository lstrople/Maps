###################################
#must run main map file before use
###################################

goucheSpace <- st_read("gouche (1).KML") %>%
  st_transform(32620)

goucheSpace <- goucheSpace %>%
  slice(1) %>%
  st_difference(goucheSpace %>% slice(2)) %>%
  st_difference(goucheSpace %>% slice(3)) %>%
  st_difference(goucheSpace %>% slice(4))

urm <- 32620

gouche_selected <- dplyr::select(goucheSpace, geometry) %>% st_zm()


##########
#coteplot
###########

gouche.df <- subset(gaspe.df, lake=="Gouache")
goucheS22.df <- subset(gouche.df, Season=="S22")
goucheNet.df <- subset(goucheS22.df , type=="NET")


##############
#convert nets
##############

goucheNet_tibble.df <- as_tibble(goucheNet.df)
goucheNet_tibble.df <- dplyr::filter(goucheNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
goucheNet_space.df <- st_as_sf(goucheNet_tibble.df, coords = c("lonDD", "latDD"))
gouchNet_space.df <- st_set_crs(goucheNet_space.df, 4326)


gouache_plot <- ggplot() +
  geom_sf(data = gouche_selected, color="#343A40", fill="#ADB5BD") +
  geom_sf(data = gouchNet_space.df, aes(color = "gouchNet_space", shape = "gouchNet_space"), show.legend = FALSE) +
  coord_fixed(ratio = 1)+
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

gouache_plot <- gouache_plot+
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

print(gouache_plot)
