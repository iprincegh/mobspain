# Advanced Spatial Analysis Examples using mobspain
# This script demonstrates the new spatial analysis functions that use
# best practices for coordinate transformation and spatial workflows

library(mobspain)
library(sf)
library(ggplot2)
library(dplyr)

# =============================================================================
# 1. BUFFER-BASED ZONE SELECTION (Functional Urban Areas)
# =============================================================================

cat("=== BUFFER-BASED ZONE SELECTION ===\n")

# Get all zones for Spain (districts)
all_zones <- get_zones(level = "dist")
cat("Total zones in Spain:", nrow(all_zones), "\n")

# Define major urban centers
urban_centers <- data.frame(
  lon = c(-3.7038, 2.1734, -0.3763, -5.9845, -8.5416),
  lat = c(40.4168, 41.3851, 39.4699, 37.3891, 42.8805),
  id = c("Madrid", "Barcelona", "Valencia", "Sevilla", "A Coruña")
)

# Get zones within 25km of Madrid center (Madrid FUA)
madrid_fua <- get_zones_buffer(
  zones = all_zones,
  center_points = urban_centers[urban_centers$id == "Madrid", ],
  buffer_km = 25
)

cat("Madrid FUA zones (25km buffer):", nrow(madrid_fua), "\n")

# Get zones around multiple urban centers
major_urban_areas <- get_zones_buffer(
  zones = all_zones,
  center_points = urban_centers,
  buffer_km = 20
)

cat("Major urban areas (20km buffer):", nrow(major_urban_areas), "\n")

# Create a simple map of Madrid FUA
madrid_map <- ggplot() +
  geom_sf(data = all_zones, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = madrid_fua, fill = "red", alpha = 0.6, color = "darkred") +
  geom_point(data = urban_centers[urban_centers$id == "Madrid", ], 
             aes(x = lon, y = lat), color = "blue", size = 3) +
  coord_sf(xlim = c(-4.5, -3.0), ylim = c(39.8, 40.8)) +
  labs(title = "Madrid Functional Urban Area (25km buffer)",
       subtitle = "Red areas show zones within 25km of Madrid center") +
  theme_void()

print(madrid_map)

# =============================================================================
# 2. SPATIAL ACCESSIBILITY ANALYSIS
# =============================================================================

cat("\n=== SPATIAL ACCESSIBILITY ANALYSIS ===\n")

# Calculate accessibility matrix for Madrid FUA
madrid_accessibility <- calculate_accessibility_matrix(
  zones = madrid_fua,
  decay_function = "exponential",
  max_distance_km = 30,
  decay_parameter = 0.1
)

cat("Accessibility matrix dimensions:", dim(madrid_accessibility), "\n")

# Calculate accessibility index for each zone (sum of row values)
accessibility_index <- rowSums(madrid_accessibility)

# Add accessibility to zones data
madrid_fua$accessibility <- accessibility_index[madrid_fua$id]

# Create accessibility map
accessibility_map <- ggplot(madrid_fua) +
  geom_sf(aes(fill = accessibility), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Accessibility\nIndex", 
                       option = "plasma", trans = "sqrt") +
  labs(title = "Spatial Accessibility in Madrid FUA",
       subtitle = "Higher values indicate better accessibility to other zones") +
  theme_void()

print(accessibility_map)

# =============================================================================
# 3. SPATIAL LAG ANALYSIS
# =============================================================================

cat("\n=== SPATIAL LAG ANALYSIS ===\n")

# First, let's add some example data (population proxy using zone area)
madrid_fua$population_proxy <- madrid_fua$area_km2 * runif(nrow(madrid_fua), 50, 500)

# Calculate spatial lag of population using different methods
madrid_fua$pop_lag_contiguity <- calculate_spatial_lag(
  zones = madrid_fua,
  variable = "population_proxy",
  method = "contiguity"
)

madrid_fua$pop_lag_distance <- calculate_spatial_lag(
  zones = madrid_fua,
  variable = "population_proxy",
  method = "distance",
  max_distance_km = 15
)

madrid_fua$pop_lag_knn <- calculate_spatial_lag(
  zones = madrid_fua,
  variable = "population_proxy",
  method = "knn",
  k = 5
)

# Compare the spatial lag methods
cat("Population spatial lag statistics:\n")
cat("Original pop proxy - Mean:", round(mean(madrid_fua$population_proxy), 0), 
    "SD:", round(sd(madrid_fua$population_proxy), 0), "\n")
cat("Contiguity lag - Mean:", round(mean(madrid_fua$pop_lag_contiguity), 0), 
    "SD:", round(sd(madrid_fua$pop_lag_contiguity), 0), "\n")
cat("Distance lag - Mean:", round(mean(madrid_fua$pop_lag_distance), 0), 
    "SD:", round(sd(madrid_fua$pop_lag_distance), 0), "\n")
cat("KNN lag - Mean:", round(mean(madrid_fua$pop_lag_knn), 0), 
    "SD:", round(sd(madrid_fua$pop_lag_knn), 0), "\n")

# Create spatial lag comparison maps
library(gridExtra)

original_map <- ggplot(madrid_fua) +
  geom_sf(aes(fill = population_proxy), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Population\nProxy") +
  labs(title = "Original Population") +
  theme_void()

contiguity_map <- ggplot(madrid_fua) +
  geom_sf(aes(fill = pop_lag_contiguity), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Spatial Lag\n(Contiguity)") +
  labs(title = "Contiguity Spatial Lag") +
  theme_void()

distance_map <- ggplot(madrid_fua) +
  geom_sf(aes(fill = pop_lag_distance), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Spatial Lag\n(Distance)") +
  labs(title = "Distance Spatial Lag") +
  theme_void()

knn_map <- ggplot(madrid_fua) +
  geom_sf(aes(fill = pop_lag_knn), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Spatial Lag\n(KNN)") +
  labs(title = "K-Nearest Neighbors Lag") +
  theme_void()

# Combine maps
combined_map <- grid.arrange(original_map, contiguity_map, distance_map, knn_map, ncol = 2)

# =============================================================================
# 4. INTEGRATED SPATIAL ANALYSIS WITH MOBILITY DATA
# =============================================================================

cat("\n=== INTEGRATED SPATIAL ANALYSIS WITH MOBILITY DATA ===\n")

# Get mobility data for Madrid FUA
madrid_mobility <- get_mobility(
  dates = "2020-02-14",
  level = "dist",
  max_rows = 10000
)

# Filter mobility data to only include zones in our Madrid FUA
madrid_mobility_fua <- madrid_mobility %>%
  filter(origin %in% madrid_fua$id, dest %in% madrid_fua$id)

cat("Madrid FUA mobility records:", nrow(madrid_mobility_fua), "\n")

# Calculate mobility indicators with spatial context
madrid_fua$total_trips_origin <- madrid_mobility_fua %>%
  group_by(origin) %>%
  summarise(total_trips = sum(n_trips, na.rm = TRUE), .groups = "drop") %>%
  {setNames(.$total_trips, .$origin)}[madrid_fua$id]

madrid_fua$total_trips_origin[is.na(madrid_fua$total_trips_origin)] <- 0

# Calculate spatial lag of mobility
madrid_fua$mobility_spatial_lag <- calculate_spatial_lag(
  zones = madrid_fua,
  variable = "total_trips_origin",
  method = "distance",
  max_distance_km = 20
)

# Calculate spatial autocorrelation (Moran's I approximation)
mobility_centered <- madrid_fua$total_trips_origin - mean(madrid_fua$total_trips_origin, na.rm = TRUE)
mobility_lag_centered <- madrid_fua$mobility_spatial_lag - mean(madrid_fua$mobility_spatial_lag, na.rm = TRUE)
morans_i <- sum(mobility_centered * mobility_lag_centered, na.rm = TRUE) / 
           sum(mobility_centered^2, na.rm = TRUE)

cat("Spatial autocorrelation (Moran's I approx.):", round(morans_i, 3), "\n")

# Create final integrated map
final_map <- ggplot(madrid_fua) +
  geom_sf(aes(fill = total_trips_origin), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Total Trips\n(Origin)", 
                       option = "plasma", trans = "sqrt") +
  labs(title = "Mobility Patterns in Madrid FUA",
       subtitle = paste("Spatial autocorrelation (Moran's I):", round(morans_i, 3))) +
  theme_void()

print(final_map)

# =============================================================================
# 5. FUNCTIONAL URBAN AREA COMPARISON
# =============================================================================

cat("\n=== FUNCTIONAL URBAN AREA COMPARISON ===\n")

# Compare different FUA definitions
madrid_fua_15km <- get_zones_buffer(
  zones = all_zones,
  center_points = urban_centers[urban_centers$id == "Madrid", ],
  buffer_km = 15
)

madrid_fua_35km <- get_zones_buffer(
  zones = all_zones,
  center_points = urban_centers[urban_centers$id == "Madrid", ],
  buffer_km = 35
)

# Calculate area statistics for different FUA definitions
cat("Madrid FUA comparison:\n")
cat("15km buffer: ", nrow(madrid_fua_15km), " zones, ", 
    round(sum(madrid_fua_15km$area_km2, na.rm = TRUE), 0), " km²\n")
cat("25km buffer: ", nrow(madrid_fua), " zones, ", 
    round(sum(madrid_fua$area_km2, na.rm = TRUE), 0), " km²\n")
cat("35km buffer: ", nrow(madrid_fua_35km), " zones, ", 
    round(sum(madrid_fua_35km$area_km2, na.rm = TRUE), 0), " km²\n")

# Create comparison map
comparison_map <- ggplot() +
  geom_sf(data = all_zones, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = madrid_fua_35km, fill = "lightblue", alpha = 0.4, color = "blue") +
  geom_sf(data = madrid_fua, fill = "orange", alpha = 0.6, color = "darkorange") +
  geom_sf(data = madrid_fua_15km, fill = "red", alpha = 0.8, color = "darkred") +
  geom_point(data = urban_centers[urban_centers$id == "Madrid", ], 
             aes(x = lon, y = lat), color = "black", size = 3) +
  coord_sf(xlim = c(-4.8, -2.8), ylim = c(39.5, 41.0)) +
  labs(title = "Madrid FUA Buffer Comparison",
       subtitle = "Red: 15km, Orange: 25km, Blue: 35km") +
  theme_void()

print(comparison_map)

cat("\n=== ADVANCED SPATIAL ANALYSIS COMPLETE ===\n")
cat("This example demonstrated:\n")
cat("1. Buffer-based zone selection for FUA definition\n")
cat("2. Spatial accessibility analysis with distance decay\n")
cat("3. Spatial lag calculations using different methods\n")
cat("4. Integration with mobility data for spatial analysis\n")
cat("5. Comparison of different FUA definitions\n")
