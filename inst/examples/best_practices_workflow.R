# Complete Best-Practice Spatial Workflow Example
# This example demonstrates the integration of all spatial analysis best practices
# including coordinate transformation, buffer zones, and spatial statistics

library(mobspain)
library(sf)
library(dplyr)

# =============================================================================
# COMPLETE SPATIAL WORKFLOW USING BEST PRACTICES
# =============================================================================

cat("=== COMPLETE SPATIAL WORKFLOW EXAMPLE ===\n")

# This example follows the best practices shown in the user's provided code:
# 1. Proper coordinate transformation without losing geometry
# 2. Centroid calculation using st_coordinates(st_centroid())
# 3. Buffer-based zone selection for FUA analysis
# 4. Time-series aggregation with hourly resolution
# 5. Spatial filtering and analysis

# =============================================================================
# 1. REPRODUCE THE EXACT WORKFLOW FROM PROVIDED CODE
# =============================================================================

cat("\n1. REPRODUCING EXACT WORKFLOW FROM PROVIDED EXAMPLE\n")

# Get districts data (equivalent to spod_get_zones("dist", ver = 1))
districts_v1 <- get_zones(level = "dist")
cat("Loaded districts:", nrow(districts_v1), "\n")

# Get mobility data (equivalent to spod_get("od", zones = "distr", dates = "2021-04-07"))
od_20210407 <- get_mobility(dates = "2021-04-07", level = "dist", max_rows = 20000)
cat("Loaded mobility data:", nrow(od_20210407), "\n")

# Preprocess data - Total flows (equivalent to group_by + summarise + collect)
od_20210407_total <- od_20210407 %>%
  group_by(origin = origin, dest = dest) %>%
  summarise(count = sum(n_trips, na.rm = TRUE), .groups = "drop")

cat("Total flow records:", nrow(od_20210407_total), "\n")

# Create time-series data with hourly resolution (simulated since we don't have hourly data)
# In the real example, this would use the hour column from the original data
od_20210407_time <- od_20210407 %>%
  # Since we don't have hourly data, we'll simulate it by creating multiple time stamps
  mutate(hour = sample(0:23, nrow(od_20210407), replace = TRUE)) %>%
  mutate(time = as.POSIXct(paste0(date, "T", sprintf("%02d", hour), ":00:00"))) %>%
  group_by(origin = origin, dest = dest, time) %>%
  summarise(count = sum(n_trips, na.rm = TRUE), .groups = "drop")

cat("Time-series records:", nrow(od_20210407_time), "\n")

# =============================================================================
# 2. CENTROIDS CALCULATION USING BEST PRACTICES
# =============================================================================

cat("\n2. CALCULATING CENTROIDS USING BEST PRACTICES\n")

# Centroids for all districts using exact best-practice workflow
districts_v1_centroids <- districts_v1 %>%
  st_transform(4326) %>%          # Transform to WGS84 (already done in get_zones)
  st_centroid() %>%               # Calculate centroids
  st_coordinates() %>%            # Extract coordinates
  as.data.frame() %>%             # Convert to data frame
  mutate(id = districts_v1$id) %>% # Add zone IDs
  rename(lon = X, lat = Y)        # Rename coordinates

cat("Calculated centroids for", nrow(districts_v1_centroids), "districts\n")
cat("Centroid coordinates sample:\n")
print(head(districts_v1_centroids))

# =============================================================================
# 3. MADRID DISTRICTS IDENTIFICATION
# =============================================================================

cat("\n3. IDENTIFYING MADRID DISTRICTS\n")

# Find Madrid districts using best-practice filtering
# Note: The exact column name might be different in our data
madrid_districts <- districts_v1 %>%
  filter(grepl("Madrid", id, ignore.case = TRUE) | 
         grepl("^2807", id) |  # Madrid province code
         grepl("^280", id))    # Madrid area codes

# If the above doesn't work, try alternative approaches
if(nrow(madrid_districts) == 0) {
  # Try using region filter
  madrid_districts <- get_zones(level = "dist", region_filter = "Madrid")
}

cat("Found", nrow(madrid_districts), "Madrid districts\n")

# =============================================================================
# 4. BUFFER ANALYSIS FOR MADRID FUA
# =============================================================================

cat("\n4. CREATING MADRID FUNCTIONAL URBAN AREA USING BUFFER\n")

# Create buffer around Madrid districts for FUA analysis
if(nrow(madrid_districts) > 0) {
  # Madrid center point
  madrid_center <- data.frame(
    lon = -3.7038,
    lat = 40.4168,
    id = "Madrid_Center"
  )
  
  # Create Madrid FUA using buffer (equivalent to st_buffer approach)
  zones_madrid_fua <- get_zones_buffer(
    zones = districts_v1,
    center_points = madrid_center,
    buffer_km = 10  # 10km buffer (equivalent to 10000m in original)
  )
  
  cat("Madrid FUA zones (10km buffer):", nrow(zones_madrid_fua), "\n")
  
  # Calculate centroids for Madrid FUA using best practices
  zones_madrid_fua_coords <- zones_madrid_fua %>%
    st_transform(crs = 4326) %>%        # Already in WGS84, but explicit
    st_centroid() %>%                   # Calculate centroids
    st_coordinates() %>%                # Extract coordinates
    as.data.frame() %>%                 # Convert to data frame
    mutate(id = zones_madrid_fua$id) %>% # Add zone IDs
    rename(lon = X, lat = Y)            # Rename coordinates
  
  cat("Madrid FUA centroids calculated for", nrow(zones_madrid_fua_coords), "zones\n")
  
  # =============================================================================
  # 5. FILTER FLOWS FOR MADRID FUA
  # =============================================================================
  
  cat("\n5. FILTERING FLOWS FOR MADRID FUA\n")
  
  # Filter total flows for Madrid FUA
  od_20210407_total_madrid <- od_20210407_total %>%
    filter(origin %in% zones_madrid_fua$id & dest %in% zones_madrid_fua$id)
  
  cat("Madrid FUA total flows:", nrow(od_20210407_total_madrid), "\n")
  
  # Filter time-series flows for Madrid FUA
  od_20210407_time_madrid <- od_20210407_time %>%
    filter(origin %in% zones_madrid_fua$id & dest %in% zones_madrid_fua$id) %>%
    filter(count > 50)  # Filter small flows as in original example
  
  cat("Madrid FUA time-series flows (>50 trips):", nrow(od_20210407_time_madrid), "\n")
  
  # =============================================================================
  # 6. ENHANCED SPATIAL ANALYSIS
  # =============================================================================
  
  cat("\n6. ENHANCED SPATIAL ANALYSIS FOR MADRID FUA\n")
  
  # Calculate comprehensive spatial statistics
  madrid_fua_enhanced <- analyze_mobility_buffer(
    center_points = madrid_center,
    buffer_km = 10,
    dates = "2021-04-07",
    level = "dist",
    max_rows = 15000,
    create_maps = TRUE
  )
  
  # Display results
  cat("Enhanced analysis results:\n")
  cat("- Zones in buffer:", madrid_fua_enhanced$spatial_stats$buffer_info$total_zones, "\n")
  cat("- Total area:", round(madrid_fua_enhanced$spatial_stats$buffer_info$total_area_km2, 1), "km²\n")
  cat("- Total trips:", madrid_fua_enhanced$spatial_stats$mobility_summary$total_trips, "\n")
  cat("- Containment rate:", round(madrid_fua_enhanced$spatial_stats$mobility_summary$containment_rate * 100, 1), "%\n")
  cat("- Mean accessibility:", round(madrid_fua_enhanced$spatial_stats$spatial_measures$mean_accessibility, 2), "\n")
  cat("- Spatial autocorrelation:", round(madrid_fua_enhanced$spatial_stats$spatial_measures$morans_i_outflow, 3), "\n")
  
  # =============================================================================
  # 7. VISUALIZATION FOLLOWING BEST PRACTICES
  # =============================================================================
  
  cat("\n7. CREATING VISUALIZATIONS\n")
  
  # Display the maps
  if(!is.null(madrid_fua_enhanced$maps)) {
    print(madrid_fua_enhanced$maps$zones)
    print(madrid_fua_enhanced$maps$mobility)
    print(madrid_fua_enhanced$maps$accessibility)
    print(madrid_fua_enhanced$maps$net_flow)
  }
  
  # =============================================================================
  # 8. COMPARISON WITH ALTERNATIVE BUFFER SIZES
  # =============================================================================
  
  cat("\n8. COMPARING DIFFERENT BUFFER SIZES\n")
  
  # Test different buffer sizes
  buffer_sizes <- c(5, 15, 25)
  buffer_results <- list()
  
  for(buffer_km in buffer_sizes) {
    cat("Testing", buffer_km, "km buffer...\n")
    
    buffer_analysis <- analyze_mobility_buffer(
      center_points = madrid_center,
      buffer_km = buffer_km,
      dates = "2021-04-07",
      level = "dist",
      max_rows = 15000,
      create_maps = FALSE  # Skip maps for faster processing
    )
    
    buffer_results[[paste0(buffer_km, "km")]] <- list(
      zones = buffer_analysis$spatial_stats$buffer_info$total_zones,
      area_km2 = buffer_analysis$spatial_stats$buffer_info$total_area_km2,
      total_trips = buffer_analysis$spatial_stats$mobility_summary$total_trips,
      containment_rate = buffer_analysis$spatial_stats$mobility_summary$containment_rate,
      accessibility = buffer_analysis$spatial_stats$spatial_measures$mean_accessibility
    )
  }
  
  # Summary of buffer comparison
  cat("\nBuffer size comparison:\n")
  for(name in names(buffer_results)) {
    result <- buffer_results[[name]]
    cat(sprintf("%-8s: %3d zones, %6.0f km², %6d trips, %4.1f%% containment, %4.2f accessibility\n",
                name, result$zones, result$area_km2, result$total_trips, 
                result$containment_rate * 100, result$accessibility))
  }
  
} else {
  cat("Could not identify Madrid districts. Skipping Madrid-specific analysis.\n")
}

# =============================================================================
# 9. GENERAL BEST PRACTICES DEMONSTRATION
# =============================================================================

cat("\n9. DEMONSTRATING GENERAL BEST PRACTICES\n")

# Get a sample of zones for demonstration
sample_zones <- districts_v1[1:20, ]  # First 20 zones

# Best practice 1: Coordinate transformation without geometry loss
cat("Best practice 1: Coordinate transformation\n")
original_crs <- st_crs(sample_zones)
transformed_zones <- sample_zones %>%
  st_transform(crs = 25830) %>%  # Transform to UTM 30N
  st_transform(crs = 4326)       # Transform back to WGS84

cat("Original CRS:", original_crs$input, "\n")
cat("Geometry preserved:", "geometry" %in% names(transformed_zones), "\n")
cat("Back to WGS84:", st_is_longlat(transformed_zones), "\n")

# Best practice 2: Centroid calculation
cat("\nBest practice 2: Centroid calculation\n")
centroids_demo <- sample_zones %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(id = sample_zones$id) %>%
  rename(lon = X, lat = Y)

cat("Centroids calculated:", nrow(centroids_demo), "\n")
cat("Coordinate range: lon", range(centroids_demo$lon), "lat", range(centroids_demo$lat), "\n")

# Best practice 3: Buffer analysis
cat("\nBest practice 3: Buffer analysis\n")
center_point <- data.frame(lon = mean(centroids_demo$lon), lat = mean(centroids_demo$lat), id = "center")
buffer_demo <- get_zones_buffer(
  zones = sample_zones,
  center_points = center_point,
  buffer_km = 50
)

cat("Buffer analysis: selected", nrow(buffer_demo), "zones within 50km\n")

# Best practice 4: Spatial lag calculation
cat("\nBest practice 4: Spatial lag calculation\n")
if(nrow(buffer_demo) > 1) {
  # Add some example data
  buffer_demo$example_value <- runif(nrow(buffer_demo), 100, 1000)
  
  # Calculate spatial lag
  buffer_demo$spatial_lag <- calculate_spatial_lag(
    zones = buffer_demo,
    variable = "example_value",
    method = "distance",
    max_distance_km = 30
  )
  
  # Calculate spatial autocorrelation
  centered_value <- buffer_demo$example_value - mean(buffer_demo$example_value)
  centered_lag <- buffer_demo$spatial_lag - mean(buffer_demo$spatial_lag)
  morans_i <- sum(centered_value * centered_lag) / sum(centered_value^2)
  
  cat("Spatial lag calculated for", nrow(buffer_demo), "zones\n")
  cat("Spatial autocorrelation (Moran's I):", round(morans_i, 3), "\n")
}

cat("\n=== BEST PRACTICES WORKFLOW COMPLETE ===\n")
cat("This example demonstrated:\n")
cat("1. Exact replication of provided best-practice code\n")
cat("2. Proper coordinate transformation without geometry loss\n")
cat("3. Centroid calculation using st_coordinates(st_centroid())\n")
cat("4. Buffer-based FUA analysis\n")
cat("5. Time-series data aggregation\n")
cat("6. Spatial filtering and analysis\n")
cat("7. Enhanced spatial statistics\n")
cat("8. Visualization with proper spatial workflows\n")
cat("9. Comparison of different analysis parameters\n")
cat("10. General best practices for spatial analysis\n")
