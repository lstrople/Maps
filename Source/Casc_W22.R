
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
cascW22.df <- subset(casc.df, Season=="W22")
cascNet.df <- subset(cascW22.df, type=="NET")
cascTU.df <- subset(cascW22.df, type=="TU")


##############
#convert nets
##############

cascNet_tibble.df <- as_tibble(cascNet.df)
cascNet_tibble.df <- dplyr::filter(cascNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
cascNet_space.df <- st_as_sf(cascNet_tibble.df, coords = c("lonDD", "latDD"))
cascNet_space.df <- st_set_crs(cascNet_space.df, 4326)

###########
#convert TU
##########

cascTU_tibble.df <- as_tibble(cascTU.df)
cascTU_tibble.df <- dplyr::filter(cascTU_tibble.df, !is.na(lonDD) & !is.na(latDD))
cascTU_space.df <- st_as_sf(cascTU_tibble.df, coords = c("lonDD", "latDD"))
cascTU_space.df <- st_set_crs(cascTU_space.df, 4326)

#######
#Lines
######

connections_df <- data.frame(
  from = c(1, 2, 3, 4),  # Index of the starting points in sf_object
  to = c(5, 6, 7, 8)     # Index of the ending points in sf_object
  
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


# Select the 'geometry' column from 'th' and set Z and M values
casc_selected <- dplyr::select(cascSpace, geometry) %>% st_zm()

cascW22_plot <- ggplot() +
  geom_sf(data = casc_selected, color="#343A40", fill="#ADB5BD") + 
  geom_sf(data = cascNet_space.df, aes(color = "cascNet_space", shape = "cascNet_space"), show.legend = TRUE) +
  geom_sf(data = cascTU_space.df, aes(color = "cascTU_space", shape = "cascTU_space"), show.legend = TRUE) +
  geom_sf(data = lines, color = "black", linetype="solid") +
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
                     values = c ("black", "#212529"),
                     labels = c("Nets", "Tip-Ups")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "black", "#212529" ),
                    labels = c("Nets", "Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(16, 17),
                     labels = c("Nets", "Tip-Ups"))

# Add scale and North arrow
cascW22_plot <- cascW22_plot+
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


print(cascW22_plot)

ggsave("CascW22.png", plot =cascW22_plot, width = 7, height = 5, units = "in", dpi = 300)

