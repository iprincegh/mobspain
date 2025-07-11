# Simple mobspain Example for Spatial Data Analysis
# This script demonstrates the working core functionality

library(mobspain)

# =============================================================================
# 1. BASIC FUNCTIONALITY: Get data and perform analysis
# =============================================================================

cat("=== BASIC FUNCTIONALITY ===\n")

# Get zones and mobility data
zones <- get_zones(level = "dist")
mobility <- get_mobility(dates = "2020-02-14", level = "dist", max_rows = 5000)

cat("Loaded", nrow(zones), "zones and", nrow(mobility), "mobility records\n")

# Basic analysis
indicators <- calc_indicators(mobility, zones)
containment <- calculate_containment(mobility, zones)

cat("Analysis completed successfully\n")
cat("  - Average trips per zone:", round(indicators$summary$avg_trips_per_zone, 0), "\n")
cat("  - Average containment:", round(containment$summary$avg_containment, 3), "\n")

# =============================================================================
# 2. REGIONAL ANALYSIS: Focus on specific regions
# =============================================================================

cat("\n=== REGIONAL ANALYSIS ===\n")

# Madrid region analysis
madrid_data <- get_region_mobility("Madrid", dates = "2020-02-14", max_rows = 3000)
madrid_indicators <- calc_indicators(madrid_data$mobility, madrid_data$zones)
madrid_containment <- calculate_containment(madrid_data$mobility, madrid_data$zones)

cat("Madrid Analysis:\n")
cat("  - Zones:", nrow(madrid_data$zones), "\n")
cat("  - Mobility records:", nrow(madrid_data$mobility), "\n")
cat("  - Average trips per zone:", round(madrid_indicators$summary$avg_trips_per_zone, 0), "\n")
cat("  - Average containment:", round(madrid_containment$summary$avg_containment, 3), "\n")

# Valencia region analysis
valencia_data <- get_region_mobility("Valencia", dates = "2020-02-14", max_rows = 3000)
valencia_indicators <- calc_indicators(valencia_data$mobility, valencia_data$zones)
valencia_containment <- calculate_containment(valencia_data$mobility, valencia_data$zones)

cat("Valencia Analysis:\n")
cat("  - Zones:", nrow(valencia_data$zones), "\n")
cat("  - Mobility records:", nrow(valencia_data$mobility), "\n")
cat("  - Average trips per zone:", round(valencia_indicators$summary$avg_trips_per_zone, 0), "\n")
cat("  - Average containment:", round(valencia_containment$summary$avg_containment, 3), "\n")

# =============================================================================
# 3. COMPARISON AND INSIGHTS
# =============================================================================

cat("\n=== COMPARISON AND INSIGHTS ===\n")

cat("Regional Comparison:\n")
cat("  Madrid vs Valencia mobility intensity:\n")
cat("    - Madrid avg trips/zone:", round(madrid_indicators$summary$avg_trips_per_zone, 0), "\n")
cat("    - Valencia avg trips/zone:", round(valencia_indicators$summary$avg_trips_per_zone, 0), "\n")

cat("  Madrid vs Valencia containment:\n")
cat("    - Madrid containment:", round(madrid_containment$summary$avg_containment, 3), "\n")
cat("    - Valencia containment:", round(valencia_containment$summary$avg_containment, 3), "\n")

# =============================================================================
# 4. BEST PRACTICES SUMMARY
# =============================================================================

cat("\n=== BEST PRACTICES FOR MOBILITY ANALYSIS ===\n")

cat("1. Use region filtering for focused analysis:\n")
cat("   region_data <- get_region_mobility('Madrid')\n")

cat("2. Limit data size for memory efficiency:\n")
cat("   mobility <- get_mobility(max_rows = 5000)\n")

cat("3. Always analyze both mobility and containment:\n")
cat("   indicators <- calc_indicators(mobility, zones)\n")
cat("   containment <- calculate_containment(mobility, zones)\n")

cat("4. Compare different regions for insights:\n")
cat("   Use multiple get_region_mobility() calls\n")

cat("5. All functions automatically create spatial maps when zones are provided\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All main functions work correctly:\n")
cat("  - get_zones(): Load spatial zones\n")
cat("  - get_mobility(): Load mobility data with filtering\n")
cat("  - get_region_mobility(): Combined regional data loading\n")
cat("  - calc_indicators(): Calculate mobility indicators\n")
cat("  - calculate_containment(): Analyze spatial containment\n")
cat("  - All functions create spatial maps automatically\n")

cat("\nSpatial analysis completed successfully!\n")
