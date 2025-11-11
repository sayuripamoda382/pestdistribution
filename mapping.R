library(skimr)
library(magrittr)
library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)

skim(df)

#make dataset for mapping
df_mapping <- df %>%
  select("Serial_No",
         "AGRO_ECO_R",
         "AGRO_ECO_Z",
         "GPS_location", 
         "X", "Y",
         "total_gall_count",
         "infected_plant_count",
         "Disease_Incidence",
         "Disease_Severity")

skim(df_mapping)


#

# Install if not yet installed
install.packages("leaflet")

# Load package
library(leaflet)


#### sample sites in sri lanka map
# Create leaflet map
leaflet(df_mapping) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~X,
    lat = ~Y,
    popup = ~paste("Serial:", Serial_No,
                   "<br>Total Galls:", total_gall_count,
                   "<br>Infected:", infected_plant_count,
                   "<br>Incidence:", round(Disease_Incidence, 1), "%",
                   "<br>Severity:", round(Disease_Severity, 2)),
    radius = 5,
    fillOpacity = 0.7,
    color = "red"
  ) %>%
  setView(lng = 80.7, lat = 7.9, zoom = 7)  # Centered on Sri Lanka


### agro eco regions with sampling sites
skim(df_mapping)

# Required libraries
library(sf)
library(ggplot2)
library(viridis)


# Load necessary libraries
library(sf)
library(ggplot2)
library(viridis)

# Read agro-ecological region polygons
url <- "https://gisapps.nsdi.gov.lk/server/rest/services/Srilanka/Agriculture/MapServer/34/query?where=1%3D1&outFields=zone,agro_eco_r&outSR=4326&f=geojson"
aer <- st_read(url)

# Convert your sampling dataset to sf object
sites_sf <- st_as_sf(df_mapping, coords = c("X", "Y"), crs = 4326)

# Make the plot
p1 <- ggplot() +
  geom_sf(data = aer, aes(fill = zone), color = "white", size = 0.3) +
  geom_sf(data = sites_sf, aes(size = Disease_Incidence, color = Disease_Incidence), alpha = 0.8) +
  scale_fill_viridis_d(name = "Agro-Eco Zone") +
  scale_color_viridis(name = "Incidence (%)", direction = -1) +
  scale_size(range = c(2, 7), name = "Incidence (%)") +
  labs(
    title = "Mg Disease Incidence by Agro-Ecological Region",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

print(p1)

# Convert your sampling dataset to sf object
sites_sf <- st_as_sf(df_mapping, coords = c("X", "Y"), crs = 4326)

unique(sites_sf$AGRO_ECO_R)


# Make the plot
p <- ggplot() +
  geom_sf(data = aer, aes(fill = zone), color = "white", size = 0.3) +
  geom_sf(data = sites_sf, aes(size = Disease_Incidence, color = Disease_Incidence), alpha = 0.8) +
  scale_fill_viridis_d(name = "Agro-Eco Zone") +
  scale_color_distiller(
    name = "Incidence (%)",
    palette = "YlOrRd",
    direction = 1
  ) +
  scale_size(
    range = c(2, 7),
    name = "Incidence (%)"
  ) +
  labs(
    title = "Mg Disease Incidence by Agro-Ecological Region",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(p)
-----
  
  # First, identify regions with data
  zones_with_data <- unique(sites_sf$AGRO_ECO_R)

# Create a filtered zone column: only keep zone names if they exist in the sampling data
aer$zone_filtered <- ifelse(aer$zone %in% zones_with_data, aer$zone, NA)

# Customized plot
p <- ggplot() +
  # Plot regions: fill only zones with data; others appear white
  geom_sf(data = aer, aes(fill = zone_filtered), color = "grey40", size = 0.4) +
  
  # Plot points with color and size indicating incidence
  geom_sf(data = sites_sf, aes(size = Disease_Incidence, color = Disease_Incidence), alpha = 0.9) +
  
  # Use a qualitative palette for regions (zones), white for NA
  scale_fill_brewer(
    palette = "Dark2",
    name = "Agro-Eco Zone",
    na.value = "white"
  ) +
  
  # Use sequential palette for incidence points
  scale_color_distiller(
    name = "Disease Incidence (%)",
    palette = "YlGnBl",
    direction = 1
  ) +
  
  scale_size(
    range = c(2.5, 7),
    name = "Incidence (%)"
  ) +
  
  labs(
    title = "Meloidogyne Disease Incidence by Agro-Ecological Region",
    subtitle = "Colored regions show where data were collected",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85")
  )

print(p)


# Plot
p_with_viridis <- ggplot() +
  # Regions with/without data
  geom_sf(data = aer, aes(fill = zone_filtered), color = "grey40", size = 0.4) +
  
  # Disease incidence points
  geom_sf(data = sites_sf, aes(size = Disease_Incidence, color = Disease_Incidence), alpha = 0.9) +
  
  # Viridis palette for regions (discrete)
  scale_fill_viridis_d(
    name = "Agro-Eco Zone",
    na.value = "white"
  ) +
  
  # YlOrRd sequential palette for incidence
  scale_color_distiller(
    name = "Disease Incidence (%)",
    palette = "Reds",
    direction = 1
  ) +
  
  scale_size(
    range = c(2.5, 7),
    name = "Incidence (%)"
  ) +
  
  labs(
    title = "Meloidogyne Disease Incidence by Agro-Ecological Region",
    subtitle = "Colored regions show where data were collected",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85")
  )

print(p_with_viridis)

p_with_viridis +
  theme(
    legend.box = "horizontal",              # Stack different legends vertically
    legend.position = "right",            # Place legend on the right
    legend.box.just = "center",           # Center them vertically
    legend.title.align = 0.03             # Center legend titles
  )


# Choose appropriate breaks for Disease Severity (adjust depending on your data)
severity_breaks <- pretty(range(sites_sf$Disease_Severity, na.rm = TRUE), n = 5)

p_with_viridis_severity <- ggplot() +
  # Regions with/without data
  geom_sf(data = aer, aes(fill = zone_filtered), color = "grey40", size = 0.4) +
  
  # Disease severity points
  geom_sf(data = sites_sf, aes(size = Disease_Severity, color = Disease_Severity), alpha = 0.9) +
  
  # Viridis palette for agro-ecological zones
  scale_fill_viridis_d(
    name = "Agro-Eco Zone",
    na.value = "white"
  ) +
  
  # Reds palette for disease severity
  scale_color_distiller(
    name = "Disease Severity",
    palette = "YlOrRd",
    direction = 1,
    breaks = severity_breaks
  ) +
  
  # Size for disease severity
  scale_size(
    range = c(2.5, 7),
    name = "Disease Severity",
    breaks = severity_breaks
  ) +
  
  labs(
    title = "Meloidogyne Disease Severity by Agro-Ecological Region",
    subtitle = "Colored regions show where data were collected",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85"),
    
    # Additional legend layout styling
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.title.align = 0.03
  )

# Display the plot
print(p_with_viridis_severity)


## legend update
library(ggplot2)







###----

p1 <-ggplot() +
  geom_sf(data = aer, aes(fill = zone), color = "white", size = 0.3) +
  geom_sf(data = sites_sf, aes(size = Disease_Incidence, color = Disease_Incidence), alpha = 0.8) +
  scale_fill_viridis_d(name = "Agro-Eco Zone") +
  scale_color_distiller(
    name = "Incidence (%)",
    palette = "YlOrRd",
    direction = 1
  ) +
  scale_size(
    range = c(2, 7),
    name = "Incidence (%)"
  ) +
  labs(
  
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )






### make the plot with out coloring the regions

# Load required libraries
install.packages(c("sf", "ggplot2"))
library(sf)
library(ggplot2)

# Read agro-ecological region polygons
aer_url <- "https://gisapps.nsdi.gov.lk/server/rest/services/Srilanka/Agriculture/MapServer/34/query?where=1%3D1&outFields=zone,agro_eco_r&outSR=4326&f=geojson"
aer <- st_read(aer_url)

# Convert your sampling dataset to sf object
sites_sf <- st_as_sf(df_mapping, coords = c("X", "Y"), crs = 4326)

# Plot: same color for all regions, boundaries emphasized
ggplot() +
  geom_sf(data = aer, fill = "white", color = "black", size = 0.5) +
  geom_sf(
    data = sites_sf, 
    aes(size = Disease_Incidence, color = Disease_Incidence), 
    alpha = 0.8
  ) +
  scale_color_viridis_c(name = "Incidence (%)", direction = -1) +
  scale_size(range = c(2, 7), name = "Incidence (%)") +
  labs(
    
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

#chnge clr paltte
ggplot() +
  geom_sf(data = aer, fill = "white", color = "black", size = 0.5) +
  geom_sf(
    data = sites_sf, 
    aes(size = Disease_Incidence, color = Disease_Incidence), 
    alpha = 0.8
  ) +
  scale_color_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Incidence (%)"
  ) +
  scale_size(
    range = c(2, 7),
    name = "Incidence (%)"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Pointwise Disease Incidence on Agro-Ecological Map"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



#### plot for disease severity

# Read agro-ecological region polygons
aer_url <- "https://gisapps.nsdi.gov.lk/server/rest/services/Srilanka/Agriculture/MapServer/34/query?where=1%3D1&outFields=zone,agro_eco_r&outSR=4326&f=geojson"
aer <- st_read(aer_url)

# Convert your sampling dataset to sf object
sites_sf <- st_as_sf(df_mapping, coords = c("X", "Y"), crs = 4326)

# Plot sampling sites over the regions
ggplot() +
  geom_sf(data = aer, fill = "lightgray", color = "black", size = 0.5) +
  geom_sf(
    data = sites_sf, 
    aes(size = Disease_Severity, color = Disease_Severity), 
    alpha = 0.8
  ) +
  scale_color_viridis_c(name = "Disease Severity", direction = -1) +
  scale_size(range = c(2, 7), name = "Disease Severity") +
  labs(
    title = "Mg Disease Severity by Location",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

# Plot sampling sites over the agro-ecological regions
ggplot() +
  geom_sf(data = aer, fill = "lightgray", color = "black", size = 0.5) +
  geom_sf(
    data = sites_sf, 
    aes(size = Disease_Severity, color = Disease_Severity), 
    alpha = 0.8
  ) +
  scale_color_distiller(
    palette = "", 
    direction = 1, 
    name = "Disease Severity"
  ) +
  scale_size(range = c(2, 7), name = "Disease Severity") +
  labs(
   
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()


# Install and load the viridis package if not already
install.packages("viridis")
library(viridis)

# Plot
ggplot() +
  geom_sf(data = aer, fill = "grey95", color = "grey60", size = 0.4) +
  geom_sf(
    data = sites_sf, 
    aes(size = Disease_Severity, color = Disease_Severity),
    alpha = 0.9
  ) +
  scale_color_viridis(
    option = "plasma",  # Options: "magma", "inferno", "plasma", "viridis", "cividis"
    direction = -1,
    name = "Disease Severity"
  ) +
  scale_size(range = c(2, 6), name = "Disease Severity") +
  labs(
    title = "Mg Disease Severity by Sampling Site",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )


#### Region wise distribution
skim(df)

df_without_serial <- df %>%
  select(-"Serial_No", -"Disease_Incidence", -"Disease_Severity")

skim(df_without_serial)

library(dplyr)

df_regions_infection <- df_without_serial %>%
  group_by(AGRO_ECO_R) %>%
  summarise(
    total_gall_count = sum(total_gall_count, na.rm = TRUE),
    total_infected_plant_count = sum(infected_plant_count, na.rm = TRUE),
    total_plants = sum(total_plants, na.rm = TRUE)

  )

df_regions_infection <- df_regions_infection %>%
  mutate(
    Disease_Incidence_R = (total_infected_plant_count / total_plants) * 100,
    Disease_Severity_R = total_gall_count / total_plants
  )

#plot incidence

# Load required libraries
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)  # for perceptually uniform color maps

# Step 1: Read agro-ecological region shapefile
aer_url <- "https://gisapps.nsdi.gov.lk/server/rest/services/Srilanka/Agriculture/MapServer/34/query?where=1%3D1&outFields=zone,agro_eco_r&outSR=4326&f=geojson"
aer <- st_read(aer_url)

# Step 2: Join with your summarized incidence data
aer_joined <- aer %>%
  left_join(df_regions_infection, by = c("agro_eco_r" = "AGRO_ECO_R"))

# Step 3: Plot the filled polygons
ggplot(aer_joined) +
  geom_sf(aes(fill = Disease_Incidence_R), color = "white", size = 0.3) +
  scale_fill_viridis_c(
    option = "magma",    # other options: "plasma", "viridis", "cividis", etc.
    direction = -1,
    name = "Disease Incidence (%)"
  ) +
  labs(
    title = "Disease Incidence by Agro-Ecological Region in Sri Lanka",
    subtitle = "Filled by average incidence per region",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )


#perplexity

library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


aer_url <- "https://gisapps.nsdi.gov.lk/server/rest/services/Srilanka/Agriculture/MapServer/34/query?where=1%3D1&outFields=zone,agro_eco_r&outSR=4326&f=geojson"
aer <- st_read(aer_url)

# If necessary, rename columns to match for joining
aer <- aer %>% rename(AGRO_ECO_R = agro_eco_r)

# Join your incidence data to the spatial polygons
aer_incidence <- left_join(aer, df_regions_infection, by = "AGRO_ECO_R")

ggplot(aer_incidence) +
  geom_sf(aes(fill = Disease_Incidence_R), color = "black", size = 0.5) +
  scale_fill_distiller(
    palette = "YlOrRd", 
    direction = 1, 
    name = "Disease Incidence (%)"
  ) +
  labs(
    title = "Agro-Ecological Regions Colored by Disease Incidence",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

### plot for severity
# Rename column for joining, if necessary
aer <- aer %>% rename(AGRO_ECO_R = agro_eco_r)

# Join severity data to spatial polygons
aer_severity <- left_join(aer, df_regions_infection, by = "AGRO_ECO_R")

ggplot(aer_severity) +
  geom_sf(aes(fill = Disease_Severity_R), color = "black", size = 0.5) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Disease Severity"
  ) +
  labs(
    title = "Agro-Ecological Regions Colored by Disease Severity",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

## try agin with white background
aer_url <- "https://gisapps.nsdi.gov.lk/server/rest/services/Srilanka/Agriculture/MapServer/34/query?where=1%3D1&outFields=zone,agro_eco_r&outSR=4326&f=geojson"
aer <- st_read(aer_url)

aer <- aer %>% rename(AGRO_ECO_R = agro_eco_r)
aer_map <- left_join(aer, df_regions_infection, by = "AGRO_ECO_R")


ggplot(aer_map) +
  geom_sf(aes(fill = Disease_Incidence_R), color = "black", size = 0.5) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Disease Incidence (%)",
    na.value = "grey"  # Make regions with NA incidence appear white
  ) +
  labs(
    title = "Agro-Ecological Regions Colored by Disease Incidence",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

ggplot(aer_map) +
  geom_sf(aes(fill = Disease_Incidence_R), color = "black", size = 0.5) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Disease Incidence (%)",
    na.value = "white"  # Make regions with NA incidence appear white
  ) +
  labs(
    title = "Agro-Ecological Regions Colored by Disease Incidence",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

ggplot(aer_map) +
  geom_sf(aes(fill = Disease_Severity_R), color = "black", size = 0.5) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Disease Severity",
    na.value = "white"  # Make regions with NA incidence appear white
  ) +
  labs(
    title = "Agro-Ecological Regions Colored by Disease Severity",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()


# Adding labels 
library(sf)
library(ggplot2)
library(dplyr)



# Read and prepare AER shapefile
aer_url <- "https://gisapps.nsdi.gov.lk/server/rest/services/Srilanka/Agriculture/MapServer/34/query?where=1%3D1&outFields=zone,agro_eco_r&outSR=4326&f=geojson"
aer <- st_read(aer_url, quiet = TRUE)

aer <- aer %>%
  rename(AGRO_ECO_R = agro_eco_r) %>%
  st_make_valid()

# Merge AER polygons with disease data
aer_map <- left_join(aer, df_regions_infection, by = "AGRO_ECO_R")

# Get a label point for each colored polygon (even if same label name)
label_points_incidence <- aer_map %>%
  filter(!is.na(Disease_Incidence_R)) %>%
  mutate(label_point = st_point_on_surface(geometry)) %>%
  st_set_geometry("label_point")  # assign label point as geometry

# Plot Incidence with all labels
ggplot(aer_map) +
  geom_sf(aes(fill = Disease_Incidence_R), color = "black", size = 0.5) +
  geom_sf_text(data = label_points_incidence, aes(label = AGRO_ECO_R), size = 3, color = "black") +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Disease Incidence (%)",
    na.value = "white"
  ) +
  labs(
    title = "Meloidogyne Disease Incidence (%) by Agro-Ecological Region",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()


#
# Label points for severity
label_points_severity <- aer_map %>%
  filter(!is.na(Disease_Severity_R)) %>%
  mutate(label_point = st_point_on_surface(geometry)) %>%
  st_set_geometry("label_point")

# Plot Severity
ggplot(aer_map) +
  geom_sf(aes(fill = Disease_Severity_R), color = "black", size = 0.5) +
  geom_sf_text(data = label_points_severity, aes(label = AGRO_ECO_R), size = 3, color = "black") +
  scale_fill_distiller(
    palette = "YlGnBu",
    direction = 1,
    name = "Disease Severity",
    na.value = "white"
  ) +
  labs(
    title = "Meloidogyne Disease Severity by Agro-Ecological Region",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()




#####--------------------------------------------
#### repeate labels

library(sf)
library(dplyr)
library(ggplot2)

# Download & prepare shapefile
aer_url <- "https://gisapps.nsdi.gov.lk/server/rest/services/Srilanka/Agriculture/MapServer/34/query?where=1%3D1&outFields=zone,agro_eco_r&outSR=4326&f=geojson"
aer <- st_read(aer_url, quiet = TRUE) %>%
  rename(AGRO_ECO_R = agro_eco_r) %>%
  st_make_valid()

# Merge infection data with shapefile
aer_map <- left_join(aer, df_regions_infection, by = "AGRO_ECO_R")

# Get a label point for each polygon, even if duplicated name
label_points_incidence <- aer_map %>%
  filter(!is.na(Disease_Incidence_R)) %>%
  st_cast("POLYGON") %>%
  st_point_on_surface()

# Final Plot
ggplot(aer_map) +
  geom_sf(aes(fill = Disease_Incidence_R), color = "black", size = 0.5) +
  geom_sf_text(data = label_points_incidence, aes(label = AGRO_ECO_R), 
               size = 3, color = "black", fontface = "bold") +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Disease Incidence (%)",
    na.value = "white"
  ) +
  labs(
    title = "Meloidogyne Disease Incidence (%) by Agro-Ecological Region",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


############ perplexity

aer_url <- "https://gisapps.nsdi.gov.lk/server/rest/services/Srilanka/Agriculture/MapServer/34/query?where=1%3D1&outFields=zone,agro_eco_r&outSR=4326&f=geojson"
aer <- st_read(aer_url, quiet = TRUE) %>%
  rename(AGRO_ECO_R = agro_eco_r) %>%
  st_make_valid()

aer_map <- left_join(aer, df_regions_infection, by = "AGRO_ECO_R")

label_points_incidence <- aer_map %>%
  filter(!is.na(Disease_Incidence_R)) %>%
  st_cast("POLYGON") %>%
  st_point_on_surface()

ggplot(aer_map) +
  geom_sf(aes(fill = Disease_Incidence_R), color = "black", size = 0.5) +
  geom_sf_text(data = label_points_incidence, aes(label = AGRO_ECO_R), 
               size = 3, color = "black", fontface = "bold") +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Disease Incidence (%)",
    na.value = "white"
  ) +
  labs(
    title = "Meloidogyne Disease Incidence (%) by Agro-Ecological Region",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

## addding labels by arrow

labels_df <- cbind(
  st_coordinates(label_points_incidence),
  AGRO_ECO_R = label_points_incidence$AGRO_ECO_R
)

ggplot(aer_map) +
  geom_sf(aes(fill = Disease_Incidence_R), color = "black", size = 0.5) +
  ggrepel::geom_text_repel(
    data = labels_df,
    aes(X, Y, label = AGRO_ECO_R),
    size = 3.5,
    fontface = "bold",
    color = "black",
    force = 10,             # Repel strongly to move labels outside
    max.overlaps = Inf,     # Allow all labels
    nudge_x = 1,            # Push labels outside (adjust as needed)
    box.padding = 1,        # More space for boxes
    point.padding = 0.5,    # Padding between arrow tip and label
    segment.color = "grey30",
    arrow = arrow(length = unit(0.015, "npc")),
    direction = "y"
  ) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Disease Incidence (%)",
    na.value = "white"
  ) +
  labs(
    title = "Meloidogyne Disease Incidence (%) by Agro-Ecological Region",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(lims_method = "geometry_bbox") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

