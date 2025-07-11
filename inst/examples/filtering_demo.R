# mobspain Package: Data Filtering Features Demo
# This example demonstrates how to use the new filtering capabilities
# to download only the data you need, reducing memory usage and download time.

library(mobspain)

# =============================================================================
# 1. DEFAULT BEHAVIOR: Download all zones (original behavior preserved)
# =============================================================================
cat("=== DEFAULT BEHAVIOR ===\n")

# Get all zones (default behavior)
all_zones <- get_zones(level = "dist")
cat("Total zones available:", nrow(all_zones), "\n")

# Get mobility data for all zones (with memory management)
all_mobility <- get_mobility(dates = "2020-02-14", level = "dist", max_rows = 5000)
cat("Total mobility records:", nrow(all_mobility), "\n\n")

# =============================================================================
# 2. ZONE-SPECIFIC FILTERING: Download specific zones by ID
# =============================================================================
cat("=== ZONE-SPECIFIC FILTERING ===\n")

# Note: Zone filtering by ID is available but complex due to data versioning
# For now, we'll focus on the more reliable region-based filtering
# If you need specific zone IDs, use region filtering instead

# Example of what zone IDs look like:
all_zones <- get_zones(level = "dist")
sample_madrid_zones <- all_zones$id[grepl("^2807", all_zones$id)][1:3]
sample_barcelona_zones <- all_zones$id[grepl("^0801", all_zones$id)][1:3]

cat("Sample Madrid zone IDs:", paste(sample_madrid_zones, collapse = ", "), "\n")
cat("Sample Barcelona zone IDs:", paste(sample_barcelona_zones, collapse = ", "), "\n")
cat("For specific zone filtering, use region filtering instead\n\n")

# =============================================================================
# 3. REGION-BASED FILTERING: Download by region name
# =============================================================================
cat("=== REGION-BASED FILTERING ===\n")

# Filter by region name (Madrid)
madrid_zones <- get_zones(level = "dist", region_filter = "Madrid")
cat("Madrid region zones:", nrow(madrid_zones), "\n")

# Get mobility data for Madrid region
madrid_mobility <- get_mobility(
  dates = "2020-02-14",
  level = "dist", 
  region_filter = "Madrid",
  max_rows = 3000
)
cat("Madrid mobility records:", nrow(madrid_mobility), "\n\n")

# =============================================================================
# 4. PROVINCE CODE FILTERING: Download by province code
# =============================================================================
cat("=== PROVINCE CODE FILTERING ===\n")

# Filter by province code (46 = Valencia)
valencia_zones <- get_zones(level = "dist", region_filter = "46")
cat("Valencia province zones:", nrow(valencia_zones), "\n")

# Get mobility data for Valencia province
valencia_mobility <- get_mobility(
  dates = "2020-02-14",
  level = "dist",
  region_filter = "46",
  max_rows = 2000
)
cat("Valencia mobility records:", nrow(valencia_mobility), "\n\n")

# =============================================================================
# 5. CONVENIENCE FUNCTION: Get both mobility and zones together
# =============================================================================
cat("=== CONVENIENCE FUNCTION ===\n")

# Get both mobility data and zones for Barcelona in one call
barcelona_data <- get_region_mobility(
  region = "Barcelona",
  dates = "2020-02-14",
  level = "dist",
  max_rows = 2000
)

cat("Barcelona analysis package:\n")
cat("  - Zones:", nrow(barcelona_data$zones), "\n")
cat("  - Mobility records:", nrow(barcelona_data$mobility), "\n")
cat("  - Region:", barcelona_data$region, "\n\n")

# =============================================================================
# 6. PRACTICAL EXAMPLE: Complete regional analysis
# =============================================================================
cat("=== COMPLETE REGIONAL ANALYSIS ===\n")

# Get Madrid data and run a complete spatial analysis
madrid_data <- get_region_mobility("Madrid", dates = "2020-02-14", max_rows = 3000)

# Calculate mobility indicators with spatial mapping
madrid_indicators <- calc_indicators(madrid_data$mobility, madrid_data$zones)
cat("Madrid mobility indicators calculated\n")

# Calculate containment analysis 
madrid_containment <- calculate_containment(madrid_data$mobility, madrid_data$zones)
cat("Madrid containment analysis completed\n")

# Create flow map
madrid_flows <- create_flows(madrid_data$mobility, madrid_data$zones, top_flows = 10)
cat("Madrid flow map created\n")

cat("\nMadrid Analysis Summary:\n")
cat("  - Total zones:", nrow(madrid_data$zones), "\n")
cat("  - Total mobility records:", nrow(madrid_data$mobility), "\n")
cat("  - Average containment:", round(madrid_containment$summary$avg_containment, 3), "\n")
cat("  - Average trips per zone:", round(madrid_indicators$summary$avg_trips_per_zone, 0), "\n")

# =============================================================================
# 7. MEMORY COMPARISON: Show the difference in data size
# =============================================================================
cat("\n=== MEMORY COMPARISON ===\n")

# Show how filtering reduces memory usage
cat("Memory usage comparison:\n")
cat("  - All zones object:", format(object.size(all_zones), "MB"), "\n")
cat("  - Madrid zones object:", format(object.size(madrid_zones), "MB"), "\n")
cat("  - Valencia zones object:", format(object.size(valencia_zones), "MB"), "\n")

cat("  - All mobility object:", format(object.size(all_mobility), "MB"), "\n")
cat("  - Madrid mobility object:", format(object.size(madrid_mobility), "MB"), "\n")
cat("  - Valencia mobility object:", format(object.size(valencia_mobility), "MB"), "\n")

# =============================================================================
# 8. BEST PRACTICES FOR USERS
# =============================================================================
cat("\n=== BEST PRACTICES FOR USERS ===\n")

cat("1. Start with regional filtering to reduce data size:\n")
cat("   madrid_data <- get_region_mobility('Madrid')\n\n")

cat("2. Use province codes for faster filtering:\n")
cat("   valencia_data <- get_region_mobility('46')  # Valencia province\n\n")

cat("3. Combine multiple regions if needed:\n")
cat("   madrid_zones <- get_zones(region_filter = 'Madrid')\n")
cat("   barcelona_zones <- get_zones(region_filter = 'Barcelona')\n")
cat("   combined_zones <- rbind(madrid_zones, barcelona_zones)\n\n")

cat("4. Use max_rows parameter to limit memory usage:\n")
cat("   mobility <- get_mobility(region_filter = 'Madrid', max_rows = 5000)\n\n")

cat("5. Always check data size before proceeding:\n")
cat("   cat('Downloaded', nrow(mobility), 'records for analysis')\n")

cat("\nDemo completed successfully!\n")
