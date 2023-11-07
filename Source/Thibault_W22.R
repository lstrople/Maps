###################################
#must run main map file before use
###################################

thibSpace <- st_read("thibault.KML") %>%
  st_transform(32620)

step1 <- thibSpace %>% dplyr::slice(1)
step2 <- step1 %>% st_difference(thibSpace %>% dplyr::slice(2))
step3 <- step2 %>% st_difference(thibSpace %>% dplyr::slice(3))
step4 <- step3 %>% st_difference(thibSpace %>% dplyr::slice(4))


urm <- 32620

Thib_selected <- dplyr::select(thibSpace, geometry) %>% st_zm()

#########
#Thibault
#########

Thibault.df <- subset(gaspe.df, lake=="Thibault")
ThibaultW22.df <- subset(Thibault.df, Season=="W22")
ThibaultNet.df <- subset(ThibaultW22.df, type=="NET")
ThibaultTU.df <- subset(ThibaultW22.df, type=="TU")


##############
#convert nets
##############

# Convert 'gaspe' to a tibble
ThibaultNet_tibble.df <- as_tibble(ThibaultNet.df)
ThibaultNet_tibble.df <- dplyr::filter(ThibaultNet_tibble.df, !is.na(lonDD) & !is.na(latDD))
ThibaultNet_space.df <- st_as_sf(ThibaultNet_tibble.df, coords = c("lonDD", "latDD"))
ThibaultNet_space.df <- st_set_crs(ThibaultNet_space.df, 4326)

###########
#convert TU
###########


ThibaultTU_tibble.df <- as_tibble(ThibaultTU.df)
ThibaultTU_tibble.df <- dplyr::filter(ThibaultTU_tibble.df, !is.na(lonDD) & !is.na(latDD))
ThibaultTU_space.df <- st_as_sf(ThibaultTU_tibble.df, coords = c("lonDD", "latDD"))
ThibaultTU_space.df <- st_set_crs(ThibaultTU_space.df, 4326)


connections_df <- data.frame(
  from = c(1, 2, 3, 4, 5, 6, 7),  # Index of the starting points in sf_object
  to = c(8, 9, 10, 11, 12, 13, 14)     # Index of the ending points in sf_object
  
)


line <- st_sfc(st_linestring(st_coordinates(ThibaultNet_space.df)),
               crs = st_crs(ThibaultNet_space.df))

allCoords <- as.matrix(st_coordinates(ThibaultNet_space.df))
lines <- lapply(1:nrow(connections_df),
                function(r){
                  rbind(allCoords[connections_df[r,1], ],
                        allCoords[connections_df[r,2], ])
                }) %>%
  st_multilinestring(.) %>%
  st_sfc(., crs = st_crs(ThibaultNet_space.df))



######
#Plot
######


thibW22_plot <- ggplot() +
  geom_sf(data = Thib_selected, color="#343A40", fill="#ADB5BD") + 
  geom_sf(data = ThibaultNet_space.df, aes(color = "ThibaultNet_space", shape = "ThibaultNet_space"), show.legend = TRUE) +
  geom_sf(data = ThibaultTU_space.df, aes(color = "ThibaultTU_space", shape = "ThibaultTU_space"), show.legend = TRUE) +
  geom_sf(data = lines, color = "black", linetype="solid") +
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA, size = 2), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  scale_color_manual(name = "Legend", 
                     values = c ("#212529","#6C757D"),
                     labels = c("Nets", "Tip-Ups")) +
  scale_fill_manual(name = "Legend", 
                    values = c("#212529","#6C757D"),
                    labels = c("Nets", "Tip-Ups")) +
  scale_shape_manual(name = "Legend", 
                     values = c(16, 17),
                     labels = c("Nets", "Tip-Ups"))

thibW22_plot <- thibW22_plot+
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

print(thibW22_plot)
ggsave("ThibW22.png", plot =thibW22_plot, width = 7, height = 5, units = "in", dpi = 300)
