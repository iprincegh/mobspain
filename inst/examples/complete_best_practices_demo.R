# Enhanced mobspain Package: Complete Best Practices Integration
# This example demonstrates the complete integration of spatial analysis best practices

library(mobspain)
library(sf)
library(dplyr)
library(ggplot2)

cat("=== ENHANCED MOBSPAIN PACKAGE DEMONSTRATION ===\n")
cat("This example shows the integration of best practices for coordinate transformation,\n")
cat("spatial analysis, and mobility data processing following the provided guidelines.\n\n")

# =============================================================================
# 1. ENHANCED ZONE LOADING WITH BEST PRACTICES
# =============================================================================

cat("1. ENHANCED ZONE LOADING WITH SPATIAL STATISTICS\n")
cat("Following best practices for coordinate transformation and spatial analysis...\n")

# Load zones with enhanced spatial statistics
zones <- get_zones(level = "dist", region_filter = "Madrid")

cat("✓ Loaded", nrow(zones), "zones with enhanced spatial statistics\n")
cat("✓ New spatial attributes:", paste(names(zones)[8:length(names(zones))], collapse = ", "), "\n")

# Display sample of enhanced statistics
cat("\nSample of enhanced spatial statistics:\n")
sample_stats <- zones[1:3, c("id", "area_km2", "centroid_lon", "centroid_lat", "bbox_width", "bbox_height")]
print(sf::st_drop_geometry(sample_stats))

# =============================================================================
# 2. BUFFER-BASED FUNCTIONAL URBAN AREA ANALYSIS
# =============================================================================

cat("\n2. BUFFER-BASED FUNCTIONAL URBAN AREA ANALYSIS\n")
cat("Implementing buffer-based zone selection for FUA analysis...\n")

# Define Madrid center point
madrid_center <- data.frame(
  lon = -3.7038,
  lat = 40.4168,
  id = "Madrid_Center"
)

# Create Madrid FUA using buffer analysis
madrid_fua <- get_zones_buffer(
  zones = zones,
  center_points = madrid_center,
  buffer_km = 15
)

cat("✓ Created Madrid FUA with", nrow(madrid_fua), "zones within 15km buffer\n")
cat("✓ Total FUA area:", round(sum(madrid_fua$area_km2, na.rm = TRUE), 1), "km²\n")

# =============================================================================
# 3. CENTROID CALCULATION USING EXACT BEST PRACTICES
# =============================================================================

cat("\n3. CENTROID CALCULATION USING EXACT BEST PRACTICES\n")
cat("Replicating the exact workflow: st_transform(4326) %>% st_centroid() %>% st_coordinates()\n")

# Use the exact workflow from the provided code
madrid_fua_centroids <- madrid_fua %>%
  st_transform(4326) %>%          # Transform to WGS84 (explicit)
  st_centroid() %>%               # Calculate centroids
  st_coordinates() %>%            # Extract coordinates
  as.data.frame() %>%             # Convert to data frame
  mutate(id = madrid_fua$id) %>%  # Add zone IDs
  rename(lon = X, lat = Y)        # Rename coordinates

cat("✓ Calculated centroids using exact best-practice workflow\n")
cat("✓ Centroid coordinate range: lon", range(madrid_fua_centroids$lon), "lat", range(madrid_fua_centroids$lat), "\n")

# =============================================================================
# 4. SPATIAL ACCESSIBILITY ANALYSIS
# =============================================================================

cat("\n4. SPATIAL ACCESSIBILITY ANALYSIS\n")
cat("Calculating spatial accessibility matrix with distance decay...\n")

# Calculate accessibility matrix
accessibility_matrix <- calculate_accessibility_matrix(
  zones = madrid_fua,
  decay_function = "exponential",
  max_distance_km = 30,
  decay_parameter = 0.1
)

# Add accessibility index to zones
madrid_fua$accessibility_index <- rowSums(accessibility_matrix)

cat("✓ Accessibility matrix calculated:", dim(accessibility_matrix), "\n")
cat("✓ Mean accessibility index:", round(mean(madrid_fua$accessibility_index, na.rm = TRUE), 2), "\n")

# =============================================================================
# 5. SPATIAL LAG ANALYSIS
# =============================================================================

cat("\n5. SPATIAL LAG ANALYSIS\n")
cat("Calculating spatial lag variables using different methods...\n")

# Calculate spatial lag for area (proxy for urban intensity)
madrid_fua$area_lag_distance <- calculate_spatial_lag(
  zones = madrid_fua,
  variable = "area_km2",
  method = "distance",
  max_distance_km = 20
)

madrid_fua$area_lag_contiguity <- calculate_spatial_lag(
  zones = madrid_fua,
  variable = "area_km2",
  method = "contiguity"
)

# Calculate spatial autocorrelation (Moran's I approximation)
area_centered <- madrid_fua$area_km2 - mean(madrid_fua$area_km2, na.rm = TRUE)
area_lag_centered <- madrid_fua$area_lag_distance - mean(madrid_fua$area_lag_distance, na.rm = TRUE)
morans_i <- sum(area_centered * area_lag_centered, na.rm = TRUE) / sum(area_centered^2, na.rm = TRUE)

cat("✓ Spatial lag variables calculated\n")
cat("✓ Spatial autocorrelation (Moran's I):", round(morans_i, 3), "\n")

# =============================================================================
# 6. ENHANCED MOBILITY ANALYSIS WITH SPATIAL CONTEXT
# =============================================================================

cat("\n6. ENHANCED MOBILITY ANALYSIS WITH SPATIAL CONTEXT\n")
cat("Integrating mobility data with spatial analysis...\n")

# Enhanced mobility analysis with all spatial features
enhanced_analysis <- analyze_mobility_buffer(
  center_points = madrid_center,
  buffer_km = 15,
  dates = "2020-02-14",
  level = "dist",
  max_rows = 10000,
  create_maps = FALSE  # Skip maps for this demo
)

cat("✓ Enhanced mobility analysis completed\n")
cat("✓ Analysis results:\n")
cat("  - Zones analyzed:", enhanced_analysis$spatial_stats$buffer_info$total_zones, "\n")
cat("  - Total area:", round(enhanced_analysis$spatial_stats$buffer_info$total_area_km2, 1), "km²\n")
cat("  - Mobility records:", nrow(enhanced_analysis$mobility), "\n")
cat("  - Total trips:", enhanced_analysis$spatial_stats$mobility_summary$total_trips, "\n")
cat("  - Containment rate:", round(enhanced_analysis$spatial_stats$mobility_summary$containment_rate * 100, 1), "%\n")
cat("  - Mean accessibility:", round(enhanced_analysis$spatial_stats$spatial_measures$mean_accessibility, 2), "\n")
cat("  - Spatial autocorrelation:", round(enhanced_analysis$spatial_stats$spatial_measures$morans_i_outflow, 3), "\n")

# =============================================================================
# 7. COORDINATE TRANSFORMATION ROBUSTNESS
# =============================================================================

cat("\n7. COORDINATE TRANSFORMATION ROBUSTNESS\n")
cat("Testing coordinate transformation without geometry loss...\n")

# Test multiple coordinate transformations
test_zones <- madrid_fua[1:5, ]

# Transform to UTM (projected)
utm_zones <- st_transform(test_zones, 25830)
cat("✓ Transform to UTM 30N successful\n")

# Transform back to WGS84
wgs84_zones <- st_transform(utm_zones, 4326)
cat("✓ Transform back to WGS84 successful\n")

# Verify geometry preservation
geometry_preserved <- "geometry" %in% names(wgs84_zones)
coords_valid <- st_is_longlat(wgs84_zones)

cat("✓ Geometry column preserved:", geometry_preserved, "\n")
cat("✓ Coordinates valid WGS84:", coords_valid, "\n")

# =============================================================================
# 8. COMPREHENSIVE SPATIAL STATISTICS SUMMARY
# =============================================================================

cat("\n8. COMPREHENSIVE SPATIAL STATISTICS SUMMARY\n")
cat("Summary of all enhanced spatial analysis capabilities...\n")

# Create comprehensive summary
spatial_summary <- data.frame(
  zone_count = nrow(madrid_fua),
  total_area_km2 = sum(madrid_fua$area_km2, na.rm = TRUE),
  mean_area_km2 = mean(madrid_fua$area_km2, na.rm = TRUE),
  mean_accessibility = mean(madrid_fua$accessibility_index, na.rm = TRUE),
  spatial_autocorr = morans_i,
  compactness_mean = mean(madrid_fua$compactness, na.rm = TRUE),
  bbox_width_mean = mean(madrid_fua$bbox_width, na.rm = TRUE),
  bbox_height_mean = mean(madrid_fua$bbox_height, na.rm = TRUE)
)

cat("✓ Comprehensive spatial statistics:\n")
print(round(spatial_summary, 3))

# =============================================================================
# 9. INTEGRATION WITH EXISTING FUNCTIONS
# =============================================================================

cat("\n9. INTEGRATION WITH EXISTING FUNCTIONS\n")
cat("Testing integration with existing mobspain functions...\n")

# Get mobility data
mobility_data <- get_mobility(
  dates = "2020-02-14",
  level = "dist",
  region_filter = "Madrid",
  max_rows = 5000
)

# Calculate containment with enhanced zones
containment_result <- calculate_containment(
  mobility_data = mobility_data,
  spatial_zones = madrid_fua,
  create_map = FALSE
)

cat("✓ Integration with existing functions successful\n")
cat("✓ Containment analysis results:\n")
cat("  - Zones analyzed:", nrow(containment_result$containment), "\n")
cat("  - Mean containment:", round(containment_result$summary$avg_containment, 3), "\n")
cat("  - Max containment:", round(containment_result$summary$max_containment, 3), "\n")

# =============================================================================
# 10. FEATURE COMPARISON: BEFORE AND AFTER ENHANCEMENTS
# =============================================================================

cat("\n10. FEATURE COMPARISON: BEFORE AND AFTER ENHANCEMENTS\n")
cat("Comparing capabilities before and after enhancements...\n")

cat("✓ BEFORE (Original package):\n")
cat("  - Basic zone loading\n")
cat("  - Simple coordinate transformation\n")
cat("  - Basic spatial statistics (area, centroid)\n")
cat("  - Region filtering\n")
cat("  - Mobility analysis\n")

cat("\n✓ AFTER (Enhanced package):\n")
cat("  - Robust zone loading with validation\n")
cat("  - Best-practice coordinate transformation with geometry preservation\n")
cat("  - Enhanced spatial statistics (centroids, bounding boxes, perimeter, compactness)\n")
cat("  - Buffer-based zone selection for FUA analysis\n")
cat("  - Spatial accessibility matrix calculation\n")
cat("  - Spatial lag variable calculation\n")
cat("  - Enhanced mobility analysis with spatial context\n")
cat("  - Comprehensive spatial autocorrelation analysis\n")
cat("  - Integrated spatial workflows\n")

# =============================================================================
# 11. BEST PRACTICES DEMONSTRATED
# =============================================================================

cat("\n11. BEST PRACTICES DEMONSTRATED\n")
cat("Summary of best practices implemented:\n")

cat("✓ Coordinate transformation:\n")
cat("  - Explicit CRS handling with st_transform()\n")
cat("  - Geometry validation and repair with st_is_valid() and st_make_valid()\n")
cat("  - Preservation of geometry column throughout transformations\n")

cat("✓ Centroid calculation:\n")
cat("  - Following exact workflow: st_transform() %>% st_centroid() %>% st_coordinates()\n")
cat("  - Proper coordinate extraction and data frame conversion\n")

cat("✓ Spatial analysis:\n")
cat("  - Buffer-based zone selection with proper projected CRS\n")
cat("  - Distance decay functions for accessibility analysis\n")
cat("  - Multiple spatial weights methods for lag calculations\n")

cat("✓ Data processing:\n")
cat("  - Memory-efficient processing with row limits\n")
cat("  - Comprehensive error handling and validation\n")
cat("  - Integration with existing workflows\n")

cat("\n=== ENHANCED MOBSPAIN PACKAGE DEMONSTRATION COMPLETE ===\n")
cat("The package now incorporates all best practices for:\n")
cat("- Coordinate transformation without geometry loss\n")
cat("- Spatial analysis with proper CRS handling\n")
cat("- Buffer-based functional urban area analysis\n")
cat("- Comprehensive spatial statistics calculation\n")
cat("- Integration with mobility data analysis\n")
cat("- Robust error handling and validation\n")
cat("\nAll functions maintain compatibility with existing workflows while\n")
cat("providing enhanced capabilities for advanced spatial analysis.\n")
