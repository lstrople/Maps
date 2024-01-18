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

cote_selected <- dplyr::select(coteSpace, geometry) %>% st_zm()


##########
#coteplot
###########

cote.df <- subset(winter.df, lake=="Cote")
coteTU.df <- subset(cote.df, type=="TU")
coteTrap.df <- subset(cote.df, type=="trap")
coteNet.df <- subset(cote.df, type=="NET")


###########
#convert TU
###########

coteTU_tibble.df <- as_tibble(coteTU.df)
coteTU_tibble.df <- dplyr::filter(coteTU_tibble.df, !is.na(lonDD) & !is.na(latDD))
coteTU_space.df <- st_as_sf(coteTU_tibble.df, coords = c("lonDD", "latDD"))
coteTU_space.df <- st_set_crs(coteTU_space.df, 4326)

##############
#convert Trap
##############

coteTrap_tibble.df <- as_tibble(coteTrap.df)
coteTrap_tibble.df <- dplyr::filter(coteTrap_tibble.df, !is.na(lonDD) & !is.na(latDD))
coteTrap_space.df <- st_as_sf(coteTrap_tibble.df, coords = c("lonDD", "latDD"))
coteTrap_space.df <- st_set_crs(coteTrap_space.df, 4326)


##############
#convert nets
##############

coteNet_tibble.df <- as_tibble(coteNet.df)
coteNet_tibble.df <- dplyr::filter(coteNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
cotetNet_space.df <- st_as_sf(coteNet_tibble.df, coords = c("lonDD", "latDD"))
coteNet_space.df <- st_set_crs(cotetNet_space.df, 4326)


connections_df <- data.frame(
  from = c(1, 3, 4),  # Index of the starting points in sf_object
  to = c(2, 7, 8)     # Index of the ending points in sf_object
  
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


# Select the 'geometry' column from 'th' and set Z and M values
cote_selected <- dplyr::select(coteSpace, geometry) %>% st_zm()

coteW_plot <- ggplot() +
  geom_sf(data = cote_selected, color="#343A40", fill="#DEE2E6") +
  geom_sf(data =  coteTU_space.df, aes(color = "coteTU_space", fill = "coteTU_space", shape = "coteTU_space"), show.legend = TRUE) +
  geom_sf(data = coteTrap_space.df, aes(color = "coteTrap_space", fill = "coteTrap_space", shape = "coteTrap_space"), show.legend =TRUE) +
  geom_sf(data = coteNet_space.df, aes(color = "coteNet_space", fill = "coteNet_space",  shape = "coteNet_space"), show.legend = TRUE) +
  geom_sf(data = lines, color = "black", linetype="solid") +
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
                     values = c ("black","black","black"),
                     labels = c("Nets","Traps","Tip-Ups")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "black","#f8f9fa","#ADB5BD"),
                    labels = c("Nets","Traps","Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(21, 22, 24),
                     labels = c("Nets","Traps","Tip-Ups"))

coteW_plot <- coteW_plot+
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

print(coteW_plot)

ggsave("CoteW.png", plot =coteW_plot, width = 7, height = 5, units = "in", dpi = 300)
