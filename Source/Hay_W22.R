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
#Hayplot
##########

Hay.df <- subset(gaspe.df, lake=="Haymard")
HayW22.df <- subset(Hay.df, Season=="W22")
HayNet.df <- subset(HayW22.df, type=="NET")


##############
#convert nets
##############

#aes(x = lon, xend = lon2, y = lat, yend = lat2, colour = as.numeric(value)),

HayNet_tibble.df <- as_tibble(HayNet.df)
HayNet_tibble.df <- dplyr::filter(HayNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
HayNet_space.df <- st_as_sf(HayNet_tibble.df, coords = c("lonDD", "latDD"))
HayNet_space.df <- st_set_crs(HayNet_space.df, 4326)


# Select the 'geometry' column from 'th' and set Z and M values
Hay_selected <- dplyr::select(HaySpace, geometry) %>% st_zm()


connections_df <- data.frame(
  from = c(1, 2),  # Index of the starting points in sf_object
  to = c(3, 4)     # Index of the ending points in sf_object
  
)


line <- st_sfc(st_linestring(st_coordinates(HayNet_space.df)),
               crs = st_crs(HayNet_space.df))

allCoords <- as.matrix(st_coordinates(HayNet_space.df))
lines <- lapply(1:nrow(connections_df),
                function(r){
                  rbind(allCoords[connections_df[r,1], ],
                        allCoords[connections_df[r,2], ])
                }) %>%
  st_multilinestring(.) %>%
  st_sfc(., crs = st_crs(HayNet_space.df))

HayW22_plot <- ggplot() +
  geom_sf(data = Hay_selected , color="#343A40", fill="#ADB5BD") + 
  geom_sf(data = HayNet_space.df, aes(color = "HayNet_space", shape = "HayNet_space"), show.legend = TRUE) +
  geom_sf(data = lines, color = "black", linetype="solid")+
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.text = element_text(size=8), 
        legend.position = c(0.05, .95), 
        legend.justification = c("right", "bottom"))+
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
HayW22_plot <- HayW22_plot+
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) 


print(HayW22_plot)

ggsave("HayW22new2.png", plot = HayW22_plot, width = 7, height = 5, units = "in", dpi = 300)


