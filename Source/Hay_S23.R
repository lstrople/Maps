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
HayNetS23_space.sf <- st_as_sf(HayNetS23_tibble.df, coords = c("lonDD", "latDD"))
HayNetS23_space.sf <- st_set_crs(HayNetS23_space.df, 4326)


hayseg <- tibble(x=c(48.92858,48.92869), y=c(-66.27401,-66.27375))%>%
  st_as_sf(., coords=c("x", "y"))%>%
  st_set_crs(4326)


#Create a new data frame with start and end points
hayseg_segments <- hayseg %>%
  st_coordinates() %>%
  as.data.frame() 


# Select the 'geometry' column from 'th' and set Z and M values
Hay_selected <- dplyr::select(HaySpace, geometry) %>% st_zm()

    
  start_point <- c(x = 48.92858, y = -66.27401)
  end_point <- c(x = 48.92869, y = -66.27376)
    
#test.df <-  geom_sf(data=HayNetS23.df %>%
           # unnest(lonDD, latDD) %>%
            #st_as_sf(., coords=c("lonDD", "latDD")) %>%
            #st_set_crs(4326))

  HayNetS23_space.sf <- st_as_sf(HayNetS23_space.df, wkt = "geometry")
  
  # Extract latitude and longitude
  HayNetS23_space.sf$latitude <- st_coordinates(HayNetS23_space.sf)[, 2]
  HayNetS23_space.sf$longitude <- st_coordinates( HayNetS23_space.sf)[, 1]

  
  data <- data.frame(
    geom = c(HayNetS23_space.sf[1, 7],
             HayNetS23_space.sf[2, 7])
  )
  
  point1 <- HayNetS23_space.sf[1, 7]
  point2 <- HayNetS23_space.sf[2, 7]
  
  
  # Set the CRS to WGS84 (EPSG:4326)
  sf_points <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
  
  
  
  connections_df <- data.frame(
    from = c(1, 2, 3),  # Index of the starting points in sf_object
    to = c(4, 5, 6)     # Index of the ending points in sf_object

  )
  
  str(connections_df)
  
  # Verify that the indices in connections_df are valid
  valid_indices <- all(connections_df$from > 0 & connections_df$from <= nrow(HayNetS23_space.sf) &
                         connections_df$to > 0 & connections_df$to <= nrow(HayNetS23_space.sf))
  
  if (!valid_indices) {
    stop("Invalid indices in connections_df")
  }
  
  extracted_from_rows <- HayNetS23_space.sf[connections_df$from, ]
  extracted_to_rows <- HayNetS23_space.sf[connections_df$to, ]
  
  # Check the structure of extracted rows
  str(extracted_from_rows)
  str(extracted_to_rows)
  
  # Create an sf object for the lines
  lines_sf <- st_sf(
    geometry = st_cast(HayNetS23_space.sf[connections_df$from, ], "LINESTRING")
  )
  
  head(lines_sf)
  
  lines_to_sf <- st_sf(
    geometry = st_cast(HayNetS23_space.sf[connections_df$to, ], "LINESTRING")
  )9
  

line <- st_sfc(st_linestring(st_coordinates(HayNetS23_space.sf)),
               crs = st_crs(HayNetS23_space.sf))

allCoords <- as.matrix(st_coordinates(HayNetS23_space.sf))
test <- lapply(1:nrow(connections_df),
       function(r){
         rbind(allCoords[connections_df[r,1], ],
               allCoords[connections_df[r,2], ])
       }) %>%
  st_multilinestring(.) %>%
  st_sfc(., crs = st_crs(HayNetS23_space.sf))

plot(test)



plot(line)
  

HayS23_plot <- ggplot() +
  geom_sf(data = Hay_selected , color="#343A40", fill="#ADB5BD") + 
  geom_sf(data = HayNetS23_space.df, aes(color = "cascNet_space", shape = "cascNet_space"), show.legend = FALSE) +
  geom_sf(data = test, color = "red", aes(linetype = "1"))+
  #geom_segment(aes(x = point1, y = point1, xend = point2, yend = point2),
               #linetype = "dashed", color = "blue") +
  theme(panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key = element_rect(fill = "transparent"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 
  #legend.text = element_text(size=8), 
  #legend.position = c(0.05, .95), 
  #legend.justification = c("right", "bottom"))
  #scale_color_manual(name = "Legend", 
                     #values = c ("black"),
                     #labels = c("Nets")) +
  #scale_fill_manual(name = "Legend", 
                    #values = c( "black" ),
                    #labels = c("Nets")) +
  #scale_shape_manual(name = "Legend", 
                     #values = c(16),
                     #labels = c("Nets"))

# Add scale and North arrow
HayS23_plot <- HayS23_plot+
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) 


print(HayS23_plot)

head(HayNetS23_space.df)

