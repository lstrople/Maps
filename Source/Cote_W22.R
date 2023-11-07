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
coteW22.df <- subset(cote.df, Season=="W22")
coteNet.df <- subset(coteW22.df, type=="NET")



##############
#convert nets
##############

coteNet_tibble.df <- as_tibble(coteNet.df)
coteNet_tibble.df <- dplyr::filter(coteNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
cotetNet_space.df <- st_as_sf(coteNet_tibble.df, coords = c("lonDD", "latDD"))
coteNet_space.df <- st_set_crs(cotetNet_space.df, 4326)



# Select the 'geometry' column from 'th' and set Z and M values
cote_selected <- dplyr::select(coteSpace, geometry) %>% st_zm()

connections_df <- data.frame(
  from = c(1, 3),  # Index of the starting points in sf_object
  to = c(2, 4)     # Index of the ending points in sf_object
  
)


line <- st_sfc(st_linestring(st_coordinates(coteNet_space.df)),
               crs = st_crs(coteNet_space.df))

allCoords <- as.matrix(st_coordinates(coteNet_space.df))
lines <- lapply(1:nrow(connections_df),
                function(r){
                  rbind(allCoords[connections_df[r,1], ],
                        allCoords[connections_df[r,2], ])
                }) %>%
  st_multilinestring(.) %>%
  st_sfc(., crs = st_crs(coteNet_space.df))

coteW22_plot <- ggplot() +
  geom_sf(data = cote_selected, color="#343A40", fill="#ADB5BD") +
  geom_sf(data = coteNet_space.df, aes(color = "coteNet_space", shape = "coteNet_space"), show.legend = FALSE) +
  geom_sf(data = lines, color = "black", linetype="dashed") +
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
  scale_color_manual(name = "Legend", 
                     values = c ("#212529","#6C757D"),
                     labels = c("Nets","Tip-Ups")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "#212529","#6C757D"),
                    labels = c("Nets","Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(16, 17),
                     labels = c("Nets","Tip-Ups"))

coteW22_plot <- coteW22_plot+
  ggspatial::annotation_scale(
    location = "bl",
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

print(coteW22_plot)


ggsave("CoteW22.png", plot = coteW22_plot, width = 7, height = 5, units = "in", dpi = 300)



