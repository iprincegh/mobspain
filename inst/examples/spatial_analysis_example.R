# Comprehensive mobspain Example for Spatial Data Analysis
# This script demonstrates key mobility analysis features with spatial filtering

# Install and load the package
# devtools::install_github("iprincegh/mobspain-r-package")
library(mobspain)

# =============================================================================
# Quick Start: Complete Spatial Analysis in One Function
# =============================================================================

# This function does everything for you - perfect for analysts and researchers
cat("=== QUICK SPATIAL ANALYSIS ===\n")

# Analyze mobility patterns for one day at district level
results <- quick_analysis(
  dates = "2020-02-14",  # Single day analysis
  level = "dist"         # District level (administrative boundaries)
)

# View the spatial maps
print(results$maps$indicators)   # Shows mobility intensity by zone
print(results$maps$containment)  # Shows how much people stay in their zone
print(results$maps$flows)        # Shows major movement patterns

# Summary statistics
print(results$summary)

# =============================================================================
# NEW: Efficient Data Filtering (Recommended for Analysts)
# =============================================================================

cat("\n=== REGIONAL FILTERING EXAMPLES ===\n")

# Option 1: Filter by region name (most intuitive)
cat("Loading Madrid region data...\n")
madrid_data <- get_region_mobility("Madrid", dates = "2020-02-14", max_rows = 3000)

# Option 2: Filter by province code (faster)
cat("Loading Valencia province data...\n")
valencia_data <- get_region_mobility("46", dates = "2020-02-14", max_rows = 2000)

# Option 3: Use region filtering instead of zone IDs (more reliable)
cat("Loading Madrid region data...\n")
madrid_region_data <- get_region_mobility("Madrid", dates = "2020-02-14", max_rows = 2000)
cat("Madrid region zones:", nrow(madrid_region_data$zones), "\n")

# Compare data sizes
cat("Data size comparison:\n")
cat("  Madrid zones:", nrow(madrid_data$zones), "\n")
cat("  Valencia zones:", nrow(valencia_data$zones), "\n") 
cat("  Madrid region zones:", nrow(madrid_region_data$zones), "\n")

# =============================================================================
# Step-by-Step Analysis (for detailed understanding)
# =============================================================================

cat("\n=== STEP-BY-STEP ANALYSIS ===\n")

# Step 1: Get mobility data and spatial zones (with filtering)
cat("Step 1: Loading Madrid data...\n")
madrid_mobility <- get_mobility(
  dates = "2020-02-14", 
  level = "dist",
  max_rows = 5000
)

madrid_zones <- get_zones(level = "dist")

# Step 2: Calculate basic mobility indicators
cat("Step 2: Calculating mobility indicators...\n")
indicators <- calc_indicators(madrid_mobility, madrid_zones)
print(indicators$map)  # Automatic spatial map

# Step 3: Analyze containment (how much people stay in their zone)
cat("Step 3: Analyzing containment...\n")
containment <- calculate_containment(madrid_mobility, madrid_zones)
print(containment$map)  # Containment map

# Step 4: Detect anomalies (unusual mobility patterns)
cat("Step 4: Detecting anomalies...\n")
anomalies <- detect_anomalies(madrid_mobility, madrid_zones, threshold = 2.5)
print(anomalies$map)    # Anomaly map

# Step 5: Create flow map (major movements between zones)
cat("Step 5: Creating flow map...\n")
flows <- create_flows(madrid_mobility, madrid_zones, top_flows = 15)
print(flows)

# =============================================================================
# Machine Learning Predictions with Spatial Mapping
# =============================================================================

cat("\n=== MACHINE LEARNING PREDICTIONS ===\n")

# Predict mobility patterns for next 2 days (using Madrid data)
predictions <- predict_patterns(
  mobility_data = madrid_mobility,
  prediction_dates = c("2020-02-15", "2020-02-16"),
  model_type = "linear_regression",
  spatial_zones = madrid_zones
)

# View predictions and spatial map
print(head(predictions$predictions))
print(predictions$spatial_map)

# =============================================================================
# Spatial Statistics
# =============================================================================

cat("\n=== SPATIAL STATISTICS ===\n")

# Calculate spatial statistics for each zone
spatial_stats <- calc_stats(madrid_mobility, madrid_zones)
print(head(spatial_stats))

# =============================================================================
# Working with Different Spatial Levels and Regions
# =============================================================================

cat("\n=== DIFFERENT SPATIAL LEVELS ===\n")

# Try municipality level for a specific region (smaller area)
valencia_muni_results <- quick_analysis(
  dates = "2020-02-14",
  level = "muni"  # Municipality level
)

print("Municipality level analysis complete!")
print(valencia_muni_results$summary)

# =============================================================================
# Analysis Exercises for Practice
# =============================================================================

cat("\n=== ANALYSIS EXERCISES ===\n")

# Exercise 1: Compare different regions
cat("Exercise 1: Compare different regions\n")
madrid_data <- get_region_mobility("Madrid", dates = "2020-02-14", max_rows = 3000)
valencia_data <- get_region_mobility("Valencia", dates = "2020-02-14", max_rows = 3000)

madrid_indicators <- calc_indicators(madrid_data$mobility, madrid_data$zones)
valencia_indicators <- calc_indicators(valencia_data$mobility, valencia_data$zones)

cat("Madrid avg trips per zone:", round(madrid_indicators$summary$avg_trips_per_zone, 0), "\n")
cat("Valencia avg trips per zone:", round(valencia_indicators$summary$avg_trips_per_zone, 0), "\n")

# Exercise 2: Compare containment levels
madrid_containment <- calculate_containment(madrid_data$mobility, madrid_data$zones)
valencia_containment <- calculate_containment(valencia_data$mobility, valencia_data$zones)

cat("Madrid avg containment:", round(madrid_containment$summary$avg_containment, 3), "\n")
cat("Valencia avg containment:", round(valencia_containment$summary$avg_containment, 3), "\n")

# =============================================================================
# Best Practices for Mobility Analysis
# =============================================================================

cat("\n=== BEST PRACTICES FOR MOBILITY ANALYSIS ===\n")

# 1. Always start with regional filtering to manage data size
cat("1. Start with regional filtering:\n")
cat("   region_data <- get_region_mobility('Madrid')\n")

# 2. Use appropriate spatial level for your analysis
cat("2. Choose appropriate spatial level:\n")
cat("   - 'dist' for overview analysis (faster)\n")
cat("   - 'muni' for detailed analysis (slower)\n")

# 3. Limit data size for memory-efficient processing
cat("3. Limit data for memory-efficient processing:\n")
cat("   mobility <- get_mobility(max_rows = 5000)\n")

# 4. Always visualize your results
cat("4. Always create spatial maps:\n")
cat("   All analysis functions automatically create maps!\n")

# 5. Compare different regions and parameters
cat("5. Compare different settings:\n")
cat("   Try different regions, thresholds, and time periods\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("You have successfully completed all spatial analysis examples!\n")
cat("Remember to:\n")
cat("  - Use filtering to manage data size\n")
cat("  - Always visualize results with spatial maps\n")
cat("  - Compare different regions and parameters\n")
cat("  - Explore the data systematically\n")

cat("Functions demonstrated:\n")
cat("  - get_region_mobility(): Get regional data in one call\n")
cat("  - get_mobility(): Get mobility data with filtering\n")
cat("  - get_zones(): Get spatial zones with filtering\n")
cat("  - calc_indicators(): Calculate mobility indicators\n")
cat("  - calculate_containment(): Analyze spatial containment\n")
cat("  - detect_anomalies(): Find unusual patterns\n")
cat("  - create_flows(): Visualize major movements\n")
cat("  - predict_patterns(): Machine learning predictions\n")
cat("  - quick_analysis(): Complete workflow in one function\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All spatial maps have been created and are ready for analysis!\n")
cat("Check the plots in your RStudio plots pane.\n")
