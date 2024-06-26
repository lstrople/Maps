###################################
#must run main map file before use
###################################

thibSpace <- st_read("thibault.KML") %>%
  st_transform(32620)

step1 <- thibSpace %>% dplyr::slice(1)
step2 <- step1 %>% st_difference(thibSpace %>% dplyr::slice(2))
step3 <- step2 %>% st_difference(thibSpace %>% dplyr::slice(3))
step4 <- step3 %>% st_difference(thibSpace %>% dplyr::slice(4))

# Check validity of geometries
valid_geometries <- st_is_valid(thibSpace$geometry)
invalid_geometries <- which(!valid_geometries)
print(paste("Invalid geometries at indices: ", paste(invalid_geometries, collapse = ", ")))


urm <- 32620

Thib_selected <- dplyr::select(thibSpace, geometry) %>% st_zm()

#########
#Thibault
#########

Thibault.df <- subset(gaspe.df, lake=="Thibault")
ThibaultW23.df <- subset(Thibault.df, Season=="W23")
ThibaultTU.df <- subset(ThibaultW23.df, type=="TU")
ThibaultTrap.df <- subset(ThibaultW23.df, type=="trap")


###########
#convert TU
###########

ThibaultTU_tibble.df <- as_tibble(ThibaultTU.df)
ThibaultTU_tibble.df <- dplyr::filter(ThibaultTU_tibble.df, !is.na(lonDD) & !is.na(latDD))
ThibaultTU_space.df <- st_as_sf(ThibaultTU_tibble.df, coords = c("lonDD", "latDD"))
ThibaultTU_space.df <- st_set_crs(ThibaultTU_space.df, 4326)

##############
#convert Trap
##############

ThibaultTrap_tibble.df <- as_tibble(ThibaultTrap.df)
ThibaultTrap_tibble.df <- dplyr::filter(ThibaultTrap_tibble.df, !is.na(lonDD) & !is.na(latDD))
ThibaultTrap_space.df <- st_as_sf(ThibaultTrap_tibble.df, coords = c("lonDD", "latDD"))
ThibaultTrap_space.df <- st_set_crs(ThibaultTrap_space.df, 4326)



##############
#convert nets
##############

# Convert 'gaspe' to a tibble
ThibaultNet_tibble.df <- as_tibble(ThibaultNet.df)
ThibaultNet_tibble.df <- dplyr::filter(ThibaultNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
ThibaultNet_space.df <- st_as_sf(ThibaultNet_tibble.df, coords = c("lonDD", "latDD"))
ThibaultNet_space.df <- st_set_crs(ThibaultNet_space.df, 4326)


######
#Plot
######


thibW23_plot <- ggplot() +
  geom_sf(data = Thib_selected, color="#343A40", fill="#ADB5BD") + 
  geom_sf(data = ThibaultTU_space.df, aes(color = "ThibaultTU_space", shape = "ThibaultTU_space"), show.legend = TRUE) +
  geom_sf(data = ThibaultTrap_space.df, aes(color = "ThibaultTrap_space", shape = "ThibaultTrap_space"), show.legend = TRUE) +
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA, size = 2), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        #panel.border = element_rect(color = "black", 
        #fill = NA, 
        #linewidth = 2),
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  #legend.text = element_text(size=8), 
  #legend.position = c(0.05, .95), 
  #legend.justification = c("right", "bottom"))
  scale_color_manual(name = "Legend", 
                     values = c ("#6C757D","#212529"),
                     labels = c("Traps","Tip-Ups")) +
  scale_fill_manual(name = "Legend", 
                    values = c("#6C757D","#212529" ),
                    labels = c("Traps","Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(15, 17),
                     labels = c("Traps","Tip-Ups"))# Add scale and North arrow

thibW23_plot <- thibW23_plot+
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

print(thibW23_plot)

ggsave("thibW23.png", plot =thibW23_plot, width = 7, height = 5, units = "in", dpi = 300)


#+ xlim(c(min(lon1, lon2), max(lon1, lon2))
#+ ylim(c(min(lat1, lat2), max(lat1, lat2))

