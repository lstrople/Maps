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


# Select the 'geometry' column from 'th' and set Z and M values
Hay_selected <- dplyr::select(HaySpace, geometry) %>% st_zm()

  
  connections_df <- data.frame(
    from = c(1, 2, 3),  # Index of the starting points in sf_object
    to = c(4, 5, 6)     # Index of the ending points in sf_object

  )


line <- st_sfc(st_linestring(st_coordinates(HayNetS23_space.df)),
               crs = st_crs(HayNetS23_space.df))

allCoords <- as.matrix(st_coordinates(HayNetS23_space.df))
lines <- lapply(1:nrow(connections_df),
       function(r){
         rbind(allCoords[connections_df[r,1], ],
               allCoords[connections_df[r,2], ])
       }) %>%
  st_multilinestring(.) %>%
  st_sfc(., crs = st_crs(HayNetS23_space.df))

HayS23_plot <- ggplot() +
  geom_sf(data = Hay_selected , color="#343A40", fill="#ADB5BD") + 
  geom_sf(data = HayNetS23_space.df, aes(color = "HayNet_space", shape = "HayNet_space"), show.legend = TRUE) +
  geom_sf(data = lines, color = "black", linetype="solid")+
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  scale_color_manual(name = "Legend", 
                     values = c ("black"),
                     labels = c("Nets")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "black"),
                    labels = c("Nets")) +
  scale_shape_manual(name = "Legend", 
                     values = c(16),
                     labels = c("Nets","Traps"))


# Add scale and North arrow
HayS23_plot <- HayS23_plot+
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) 


print(HayS23_plot)

ggsave("HayS23.png", plot = HayS23_plot, width = 7, height = 5, units = "in", dpi = 300)


