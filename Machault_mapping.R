## Load required packages 
library("here")
library("ggplot2")         
library("sf")             
library("rnaturalearth")   
library("rnaturalearthdata") 
library("ggspatial") 
library("openxlsx")
library("cowplot")         

## Load spatial datasets from Natural Earth
world <- ne_countries(scale = "large", returnclass = "sf")  # Load world map
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada"))  # Load US & Canadian states and provinces
water <- ne_download(scale = 10, type = 'lakes', category = 'physical', returnclass = "sf")  # Load lakes data

## Set theme
theme_set(theme_bw())

## Load spreadsheet containing coordinates for Machault wreck and cities
df <- read.xlsx((here('Machault_site_city_coordinates.xlsx')))

## Create main map 
main_gg <- ggplot(data = world) +  
  geom_sf(fill="lightgrey") +  # Plot world map with grey fill
  geom_sf(data = state_prov, fill = "lightgrey") +  # Add state and province boundaries
  geom_sf(data = water, fill = "white") +  # Plot lakes 
  geom_point(data = df, aes(x = Long, y = Lat, shape = Group), size = 6) +  # Add points for Machault and cities
  scale_shape_manual(values = c("City" = 16, "Site" = 18)) +  # Define shape types for points
  
  # Add labels for Machault and cities in bold black text 
  geom_text(data= df, aes(x=Long, y=Lat, label=Location), color = "black", 
            fontface = "bold", size = 4, check_overlap = FALSE, nudge_y=-0.32) +
  
  # Set coordinate reference system and map extents 
  coord_sf(crs = st_crs(3857)) +  
  coord_sf(xlim = c(-76, -56), ylim = c(44.7, 55), expand = FALSE) +  
  
  # Add labels for major geographic features
  annotate("text", x = -61.7, y = 48.3513889, label = "Gulf of \n St. Lawrence", 
           size = 4, color = "black", fontface = "bold") +
  annotate("text", x = -65, y = 48.05, label = "Chaleur Bay", 
           size = 4, color = "black", fontface = "bold", angle=30) +
  annotate("text", x = -68.8, y = 48.6, label = "St. Lawrence River", 
           size = 4, color = "black", fontface = "bold", angle=42) +
  
  # Styling
  theme(axis.text.x = element_text(face="bold", size = 12),
        axis.text.y = element_text(face="bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  
  # Add scale bar and compass rose 
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.755, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

  # Display map
  print(main_gg)

## Create inset map 
inset_gg <- ggplot(data = world) +  
  geom_sf(fill="lightgrey") +  # Plot world map with grey fill
  geom_sf(data = state_prov, fill = "lightgrey") +  # Add state and province boundaries
  geom_sf(data = water, fill = "white") +  # Plot lakes
  
  # Set coordinate reference system and map extents 
  coord_sf(crs = st_crs(3857)) +  
  coord_sf(xlim = c(-52, -97), ylim = c(22, 66), expand = FALSE) +  
  
  # Add red rectangle showing main map's extent
  geom_rect(aes(xmin = -76, xmax = -56, ymin = 44.7, ymax = 55), 
            color = "red", fill = NA, linewidth = 1.2) +  
  
  # Add labels for major geographic features in bold black text 
  annotate("text", x = -63, y = 37, label = "Atlantic \nOcean", 
           size = 4, color = "black", fontface = "bold") +  
  
  # Styling 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

  # Display map
    print(inset_gg)

## Combine main and inset map 
map_combined<-ggdraw() +
  draw_plot(main_gg) +
  draw_plot(inset_gg, height = 0.4, x = -0.3, y = 0.56)

  # Display map
    print(map_combined)

## Save combined map as high-resolution TIFF 
    ggsave(
      filename = here("Machault_map_combined_2.tiff"),
      plot = map_combined,
      dpi = 300,
      width = 10,   
      height = 8,
      bg = "white"
    )
