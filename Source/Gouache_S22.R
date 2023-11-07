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
gouchetrap.df <- subset(goucheS22.df , type=="Trap")

##############
#convert nets
##############

goucheNet_tibble.df <- as_tibble(goucheNet.df)
goucheNet_tibble.df <- dplyr::filter(goucheNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
goucheNet_space.df <- st_as_sf(goucheNet_tibble.df, coords = c("lonDD", "latDD"))
gouchNet_space.df <- st_set_crs(goucheNet_space.df, 4326)

#########
#Trap
########
gouchetrapS22_tibble.df <- as_tibble(gouchetrap.df)
gouchetrapS22_tibble.df <- dplyr::filter(gouchetrapS22_tibble.df, !is.na(lonDD) & !is.na(latDD))
gouchetrapS22_space.df <- st_as_sf(gouchetrapS22_tibble.df, coords = c("lonDD", "latDD"))
gouchetrapS22_space.df <- st_set_crs(gouchetrapS22_space.df, 4326)

##########
#lines
###########
connections_df <- data.frame(
  from = c(1, 2, 3, 4, 5, 6, 7, 8),  # Index of the starting points in sf_object
  to = c(9, 10, 11, 12, 13, 14, 15, 16)     # Index of the ending points in sf_object
  
)


line <- st_sfc(st_linestring(st_coordinates(gouchNet_space.df)),
               crs = st_crs(gouchNet_space.df))

allCoords <- as.matrix(st_coordinates(gouchNet_space.df))
lines <- lapply(1:nrow(connections_df),
                function(r){
                  rbind(allCoords[connections_df[r,1], ],
                        allCoords[connections_df[r,2], ])
                }) %>%
  st_multilinestring(.) %>%
  st_sfc(., crs = st_crs(gouchNet_space.df))


gouacheS22_plot <- ggplot() +
  geom_sf(data = gouche_selected, color="#343A40", fill="#ADB5BD") +
  geom_sf(data = gouchNet_space.df, aes(color = "gouchNet_space", shape = "gouchNet_space"), show.legend = TRUE) +
  geom_sf(data = gouchetrapS22_space.df, aes(color = "gouchetrapS22_space", shape = "gouchetrapS22_space"), show.legend = TRUE) +
  geom_sf(data = lines, color = "black", linetype="solid") +
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(1,1,1,1), "cm"))+
  scale_color_manual(name = "Legend", 
                     values = c ("#6C757D","black"),
                     labels = c("Traps","Nets")) +
  scale_fill_manual(name = "Legend", 
                    values = c("#6C757D","black"),
                    labels = c("Traps","Nets")) +
  scale_shape_manual(name = "Legend", 
                     values = c(15,16),
                     labels = c("Traps","Nets"))

gouacheS22_plot <- gouacheS22_plot+
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) 

print(gouacheS22_plot)

ggsave("gouacheS22new.png", plot =gouacheS22_plot, width = 7, height = 5, units = "in", dpi = 300)


