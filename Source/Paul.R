#########
#Paul map
#########
paulSpace <- st_read("paul.KML") %>%
  st_transform(32620)

paulSpace <- (paulSpace %>% slice(1))
st_difference(paulSpace %>% slice(2)) %>%
  paulSpace <- st_difference(paulSpace %>% slice(3)) %>%
  paulSpace <- st_difference(paulSpace %>% slice(4))

urm <- 32620

paul_selected <- dplyr::select(paulSpace, geometry) %>% st_zm()




Paul.df <- subset(gaspe.df, lake=="Paul")
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



######
#paul
######
paulSpace <- st_read("Paul.KML") %>%
  st_transform(32620)

paulSpace <- paulSpace %>%
  slice(1) %>%
  st_difference(paulSpace %>% slice(2)) %>%
  st_difference(paulSpace %>% slice(3)) %>%
  st_difference(paulSpace %>% slice(4))

urm <- 32620

paul_selected <- dplyr::select(paulSpace, geometry) %>% st_zm()


##########
#Paulplot
###########

# Select the 'geometry' column from 'th' and set Z and M values
paul_selected <- dplyr::select(paulSpace, geometry) %>% st_zm()

paul_plot <- ggplot() +
  geom_sf(data = paul_selected, color="#343A40", fill="#ADB5BD") +
  geom_sf(data = PaulNet_space.df, aes(color = "PaulNet_space", shape = "PaulNet_space"), show.legend = TRUE) +
  geom_sf(data = PaulTU_space.df, aes(color = "PaulTU_space", shape = "PaulTU_space"), show.legend = TRUE) +
  theme(panel.border=element_blank(),
        panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(0,0,0,0), "cm")) +
  #legend.text = element_text(size=8), 
  #legend.position = c(0.05, .95), 
  #legend.justification = c("right", "bottom")
  scale_color_manual(name = "Legend", 
                     values = c ("#6C757D","#212529"),
                     labels = c("Traps", "Tip-Ups")) +
  scale_fill_manual(name = "Legend", 
                    values = c( "#6C757D","#212529"),
                    labels = c("Traps", "Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(15, 17),
                     labels = c("Traps", "Tip-Ups"))

print(paul_plot)
