#########
#Paul map
#########
paulSpace <- st_read("Paul.KML") %>%
  st_transform(32620)

paulSpace <- paulSpace %>%
  slice(1) %>%
  st_difference(paulSpace %>% slice(2)) %>%
  st_difference(paulSpace %>% slice(3)) %>%
  st_difference(paulSpace %>% slice(4))

urm <- 32620

paul_selected <- dplyr::select(paulSpace, geometry) %>% st_zm()


Paul.df <- subset(winter.df, lake=="Paul")
PaulTU.df <- subset(Paul.df, type=="TU")
PaulTrap.df <- subset(Paul.df, type=="trap")
PaulNet.df <- subset(Paul.df, type=="NET")


###########
#convert TU
###########


PaulTU_tibble.df <- as_tibble(PaulTU.df)
PaulTU_tibble.df <- dplyr::filter(PaulTU_tibble.df, !is.na(lonDD) & !is.na(latDD))
PaulTU_space.df <- st_as_sf(PaulTU_tibble.df, coords = c("lonDD", "latDD"))
PaulTU_space.df <- st_set_crs(PaulTU_space.df, 4326)


##############
#convert nets
##############

PaulNet_tibble.df <- as_tibble(PaulNet.df)
PaulNet_tibble.df <- dplyr::filter(PaulNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
PaulNet_space.df <- st_as_sf(PaulNet_tibble.df, coords = c("lonDD", "latDD"))
PaulNet_space.df <- st_set_crs(PaulNet_space.df, 4326)




##########
#Paulplot
###########

# Select the 'geometry' column from 'th' and set Z and M values
paul_selected <- dplyr::select(paulSpace, geometry) %>% st_zm()

paul_plot <- ggplot() +
  geom_sf(data = paul_selected, color="#343A40", fill="#DEE2E6") +
  geom_sf(data = PaulNet_space.df, aes(color = "PaulNet_space", fill = "PaulNet_space", shape = "PaulNet_space"), show.legend = FALSE) +
  geom_sf(data = PaulTU_space.df, aes(color = "PaulTU_space", fill = "PaulTU_space", shape = "PaulTU_space"), show.legend = FALSE) +
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  scale_color_manual(name = "Legend", 
                     values = c ("black","black"),
                     labels = c("Nets","Traps","Tip-Ups")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "black","#ADB5BD"),
                    labels = c("Nets","Traps","Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(21, 24),
                     labels = c("Nets","Traps","Tip-Ups")) 

# Add scale and North arrow

paul_plot <- paul_plot+
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

print(paul_plot)

ggsave("paul_plot.png", plot =paul_plot, width = 7, height = 5, units = "in", dpi = 300)


