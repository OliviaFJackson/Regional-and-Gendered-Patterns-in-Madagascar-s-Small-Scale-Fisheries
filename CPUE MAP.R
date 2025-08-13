install.packages("sf")
install.packages("tmap")
library(sf)
library(ggplot2)
library(tmap)
library(ggspatial)
combined_data<-read.csv(file=file.choose(), header = T)



# Define the bounding box coordinates for each region
bounding_boxes <- data.frame(
  region = c("NE", "NW", "SW"),
  xmin = c(49, 47, 42),    # Approximate longitude boundaries
  xmax = c(50.5, 48.5, 45),    # Adjust as needed
  ymin = c(-13, -14, -25),  # Approximate latitude boundaries
  ymax = c(-11.5, -12.5, -22)   # Adjust as needed
)

# Convert bounding boxes to spatial objects
bounding_boxes_sf <- st_as_sf(bounding_boxes, coords = c("xmin", "ymin", "xmax", "ymax"), crs = 4326)

# Create rectangles for each region
rectangles <- lapply(1:nrow(bounding_boxes), function(i) {
  box <- bounding_boxes[i,]
  st_polygon(list(rbind(
    c(box$xmin, box$ymin),
    c(box$xmin, box$ymax),
    c(box$xmax, box$ymax),
    c(box$xmax, box$ymin),
    c(box$xmin, box$ymin)
  )))
})

# Combine rectangles into a spatial object
regions_sf <- st_sfc(rectangles, crs = 4326)
regions_sf <- st_sf(region = bounding_boxes$region, geometry = regions_sf)

# Load Madagascar map
madagascar_map <- st_read("/Users/oliviajackson/Documents/ONJA PROJECT 2024/CPUE/Data/Madagascar.shp", layer = "gadm41_MDG_1")


region_labels <- data.frame(
  region = c("NW", "NE", "SW"),
  x = c(47.6, 49.8, 43.0),   # adjust these as needed
  y = c(-12.2, -11.2, -21.4) # adjust these as needed
)


# Create a data frame with your village information
village_data <- data.frame(
  Village = c("Tsandamba", "Salary", "Ambola", "Itampolo", "Tsifota", "Sarodrano", "Anakao", "Ankilimiova", "Maromena", "Beheloke",
              "Nosy Sakatia", "Ambatoloaka", "Ambavarano", "Ambodivahibe", "Ramena", "Antaravy", "Ivovona"),
  Reserve = c("Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "No", "Yes", "Yes",
              "Yes", "No", "Yes", "No", "No", "No", "Yes")
)

# Merge reserve information with village coordinates
village_coords$Reserve <- village_data$Reserve[match(village_coords$Village, village_data$Village)]



ggplot() +
  geom_sf(data = madagascar_map, fill = "lightgray", color = "black") +
  geom_rect(data = bounding_boxes,  
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),  
            fill = NA, color = "black", linewidth = 0.7) +  
  geom_point(data = village_coords, 
             aes(x = Longitude, y = Latitude, color = Reserve, shape = Reserve), 
             size = 4, position = position_jitter(width = 0.02, height = 0.02)) + 
  scale_color_manual(values = c("Yes" = "blue", "No" = "red")) +
  scale_shape_manual(values = c("Yes" = 17, "No" = 16)) +  
  geom_text(data = region_labels, 
            aes(x = x, y = y, label = region),  
            fontface = "bold", size = 5) +
  labs(title = "Study Regions and Villages in Madagascar", 
       color = "Paper Parks", shape = "Paper Parks") +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  theme_minimal() +
  theme(legend.position = "bottom")













library(ggplot2)
library(sf)
library(ggspatial)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)

# Load Africa map
africa <- ne_countries(continent = "Africa", returnclass = "sf")

# Africa inset map
africa_map <- ggplot() +
  geom_sf(data = africa, fill = "gray80", color = "black") +
  geom_rect(aes(xmin = 43, xmax = 51, ymin = -26, ymax = -12), color = "black", fill = NA, linewidth = 1) +
  coord_sf(xlim = c(-20, 60), ylim = c(-40, 40), expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA))

# Main map with study regions and villages
main_map <- ggplot() +
  geom_sf(data = madagascar_map, fill = "lightgray", color = "black") +
  geom_rect(data = bounding_boxes,  
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),  
            fill = NA, color = "black", linewidth = 1) +
  geom_point(data = village_coords, 
             aes(x = Longitude, y = Latitude, color = Reserve, shape = Reserve), 
             size = 3) +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red")) +
  scale_shape_manual(values = c("Yes" = 17, "No" = 16)) +  
  geom_text(data = region_labels, 
            aes(x = x, y = y, label = region),  
            fontface = "bold", size = 5) +
  labs(title = "Study Regions and Fishing Villages in Madagascar", 
       color = "Reserve Status", shape = "Reserve Status") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_minimal) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Combine main map and inset map
final_map <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(africa_map, x = 0.01, y = 0.7, width = 0.2, height = 0.2)

# Display the final map
print(final_map)











# Create a data frame with village names and reserve status
village_data <- data.frame(
  Village = c("Tsandamba", "Salary", "Ambola", "Itampolo", "Tsifota", "Sarodrano", "Anakao", "Ankilimiova", 
              "Maromena", "Beheloke", "Nosy Sakatia", "Ambatoloaka", "Ambavarano", "Ambodivahibe", 
              "Ramena", "Antaravy", "Ivovona"),
  Reserve = c("Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "No", "Yes", "Yes", 
              "Yes", "No", "Yes", "No", "No", "No", "Yes")
)



# Create a data frame with village names and coordinates
village_coords <- data.frame(
  Village = c("Nosy Sakatia", "Ambatoloaka", "Ambavarano", "Ambodivahibe", "Ramena", "Antaravy", "Ivovona",
              "Ankilimiova", "Tsandamba", "Tsifota", "Salary", "Ambola", "Anakao", "Sarodrano", "Itampolo", 
              "Beheloke", "Maromena"),
  Region = c("NW", "NW", "NE", "NE", "NE", "NE", "NE",
             "SW", "SW", "SW", "SW", "SW", "SW", "SW", "SW", 
             "SW", "SW"),
  Latitude = c(-13.3000, -13.3931, -12.3644, -12.3093, -12.1684, -12.1500, -12.3208, 
               -23.7103, -22.6739, -22.8304, -22.5811, -24.0893, -23.6643, -23.0110, -24.6840, 
               -23.9000, -23.8060),
  Longitude = c(48.1633, 48.2077, 49.5240, 49.5085, 49.3794, 49.3330, 49.3939, 
                43.6237, 43.4855, 43.3664, 43.2937, 43.6807, 43.6483, 44.3834, 43.9460, 
                43.6667, 43.6609)
)

# Merge reserve information with village coordinates
village_coords$Reserve <- village_data$Reserve[match(village_coords$Village, village_data$Village)]

# View the combined data frame
print(village_coords)


region_labels <- data.frame(
  region = c("NW", "NE", "SW"),
  x = c(47.7, 49.8, 43.0),   # adjust these as needed
  y = c(-12.2, -11.2, -21.6) # adjust these as needed
)

# Load Africa and Madagascar shapefiles
africa_shapefile <- st_read("/Users/oliviajackson/Documents/ONJA PROJECT 2024/CPUE/Data/africa_map", layer = "ne_110m_admin_0_countries")
madagascar_map <- st_read("/Users/oliviajackson/Documents/ONJA PROJECT 2024/CPUE/Data/Madagascar.shp", layer = "gadm41_MDG_1")

# Filter Africa outline
africa_outline <- africa_shapefile %>% filter(CONTINENT == "Africa")



# Main Madagascar map
# Plot with Village Names
madagascar_plot <- ggplot() +
  geom_sf(data = madagascar_map, fill = "lightgray", color = "black") +
  geom_rect(data = bounding_boxes,  
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),  
            fill = NA, color = "black", linewidth = 0.7) +
  geom_point(data = village_coords, 
             aes(x = Longitude, y = Latitude, color = Reserve, shape = Reserve), 
             size = 4) +
  # Add village names as text
  geom_text(data = village_coords, 
            aes(x = Longitude, y = Latitude, label = Village), 
            nudge_y = 0.1, hjust = 0, size = 4, fontface = "bold", color = "black") +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red")) +
  scale_shape_manual(values = c("Yes" = 17, "No" = 16)) +
  geom_text(data = region_labels, 
            aes(x = x, y = y, label = region),  
            fontface = "bold", size = 5) +
  labs(color = "Paper Park", shape = "Paper Park") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         style = north_arrow_orienteering()) +
  theme_minimal() +
  theme(legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, size = 1))

# Print the map
print(madagascar_plot)

# Inset map: Africa with Madagascar highlighted
africa_plot <- ggplot() +
  geom_sf(data = africa_outline, fill = "lightgray", color = "black") +
  geom_rect(aes(xmin = 42, xmax = 52, ymin = -27, ymax = -11), 
            color = "black", fill = NA, linewidth = 0.6) +  # Box around Madagascar
  annotate("text", x = 15, y = 35, label = NA, fontface = "bold", size = 5) +
  coord_sf(xlim = c(-20, 55), ylim = c(-40, 40)) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

# Combine maps: Madagascar as the main map and Africa inset inside the main map
final_map <- ggdraw(madagascar_plot) +
  draw_plot(africa_plot, x = 0.3215, y = 0.7935, width = 0.2, height = 0.2)  # Inset positioned within the main map

# Print the final map
print(final_map)



# Update your plot to have a white background
madagascar_plot <- madagascar_plot + 
  theme(panel.background = element_rect(fill = "white", color = NA))

# Save the final map as displayed
ggsave(filename = "~/Documents/ONJA PROJECT 2024/CPUE/Plots/Final_Map.png", 
       plot = final_map, 
       width = 8,       # Adjust width and height based on the plot window size you prefer
       height = 12,     # This should match the aspect ratio you see on screen
       dpi = 300, 
       bg = "white",
       device = "png")


# Save the final map with fixed layout
cowplot::save_plot(
  filename = "~/Documents/ONJA PROJECT 2024/CPUE/Plots/Final_Map.png",
  plot = final_map,
  base_width = 14,
  base_height = 8.7,
  dpi = 300,
  bg = "white"
)


# Copy the current plot device (Zoom window) and save it as a PNG file
dev.copy(png, filename = "~/Documents/ONJA PROJECT 2024/CPUE/Plots/Final_Map.png", 
         width = 1000, height = 1500, res = 300)
dev.off()










##### UPDATE MAP WITH DIANA REGION ###


library(tidyverse)
library(sf)
library(ggplot2)
library(cowplot)
library(ggspatial)

# ==== 1. Village + Reserve Status ====
village_data <- data.frame(
  Village = c("Tsandamba", "Salary", "Ambola", "Itampolo", "Tsifota", "Sarodrano", "Anakao", "Ankilimiova", 
              "Maromena", "Beheloke", "Nosy Sakatia", "Ambatoloaka", "Ambavarano", "Ambodivahibe", 
              "Ramena", "Antaravy", "Ivovona"),
  Reserve = c("Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "No", "Yes", "Yes", 
              "Yes", "No", "Yes", "No", "No", "No", "Yes")
)


# Village coordinates and reserve status
village_coords <- data.frame(
  Village = c("Nosy Sakatia", "Ambatoloaka", "Ambavarano", "Ambodivahibe", "Ramena", "Antaravy", "Ivovona",
              "Ankilimiova", "Tsandamba", "Tsifota", "Salary", "Ambola", "Anakao", "Sarodrano", "Itampolo", 
              "Beheloke", "Maromena"),
  Region = c("Diana Region", "Diana Region", "Diana Region", "Diana Region", "Diana Region", "Diana Region", "Diana Region",
             "SW", "SW", "SW", "SW", "SW", "SW", "SW", "SW", "SW", "SW"),
  Latitude = c(-13.3000, -13.3931, -12.3644, -12.3093, -12.1684, -12.1500, -12.3208, 
               -23.7103, -22.6739, -22.8304, -22.5811, -24.0893, -23.6643, -23.5143, -24.6840, 
               -23.9000, -23.8060),
  Longitude = c(48.1633, 48.2077, 49.5240, 49.5085, 49.3794, 49.3330, 49.3939, 
                43.6237, 43.4855, 43.3664, 43.2937, 43.6807, 43.6483, 43.7318, 43.9460, 
                43.6667, 43.6609),
  Reserve = c("Yes", "No", "Yes", "No", "No", "No", "Yes",
              "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
)

# Region label positions
region_labels <- data.frame(
  region = c("Diana Region", "SW"),
  x = c(48.9, 43.0),
  y = c(-11.2, -21.6)
)



# Original bounding boxes
bounding_boxes <- data.frame(
  xmin = c(47, 42),
  xmax = c(50.5, 45.0),
  ymin = c(-14, -25.0),
  ymax = c(-11.5, -22),
  region = c("Diana Region", "SW")
)



# Load shapefiles
africa_shapefile <- st_read("/Users/oliviajackson/Documents/ONJA PROJECT 2024/CPUE/Data/africa_map", layer = "ne_110m_admin_0_countries")
madagascar_map <- st_read("/Users/oliviajackson/Documents/ONJA PROJECT 2024/CPUE/Data/Madagascar.shp", layer = "gadm41_MDG_1")
africa_outline <- africa_shapefile %>% filter(CONTINENT == "Africa")

# Main map
madagascar_plot <- ggplot() +
  geom_sf(data = madagascar_map, fill = "lightgray", color = "black") +
  geom_rect(data = bounding_boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = NA, color = "black", linewidth = 0.7) +
  geom_point(data = village_coords, 
             aes(x = Longitude, y = Latitude, color = Reserve, shape = Reserve), size = 4) +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red")) +
  scale_shape_manual(values = c("Yes" = 17, "No" = 16)) +
  geom_text(data = region_labels, aes(x = x, y = y, label = region), 
            fontface = "bold", size = 5) +
  labs(color = "Paper Park", shape = "Paper Park",
       x = "Longitude", y = "Latitude") +   # Axis titles
  annotation_north_arrow(location = "br", which_north = "true", 
                         style = north_arrow_orienteering()) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),  # Increase legend title font
    legend.text = element_text(size = 13),                  # Increase legend label font
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(size = 14),                    # Tick labels
    axis.title = element_text(size = 16, face = "bold")     # Axis titles
  )

# Inset map of Africa
africa_plot <- ggplot() +
  geom_sf(data = africa_outline, fill = "lightgray", color = "black") +
  geom_rect(aes(xmin = 42, xmax = 52, ymin = -27, ymax = -11), 
            color = "black", fill = NA, linewidth = 0.6) +
  coord_sf(xlim = c(-20, 55), ylim = c(-40, 40)) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# Combine with inset
final_map <- ggdraw(madagascar_plot) +
  draw_plot(africa_plot, x = 0.29, y = 0.7935, width = 0.2, height = 0.2)


print(final_map)
cowplot::save_plot(
  filename = "~/Documents/ONJA PROJECT 2024/CPUE/Plots/Final_Map.png",
  plot = final_map,
  base_width = 12,
  base_height = 8,
  dpi = 300,
  bg = "white"
)
# Save
ggsave("~/Documents/ONJA PROJECT 2024/CPUE/Plots/Final_Map.png", 
       plot = final_map, width = 12, height = 8, dpi = 300, bg = "white")


# Open a new PNG device with your desired dimensions and resolution
png("~/Documents/ONJA PROJECT 2024/CPUE/Plots/Final_Map.png",
    width = 1000, height = 1500, res = 300)

# Draw the plot (will use last visible plot in Zoom window)
print(final_map)

# Close device
dev.off()

