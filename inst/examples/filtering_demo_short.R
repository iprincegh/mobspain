# mobspain Package: Data Filtering Features Demo (Short Version)
# This example demonstrates the key filtering capabilities

library(mobspain)

# =============================================================================
# 1. DEFAULT BEHAVIOR: Download all zones (memory-managed)
# =============================================================================
cat("=== DEFAULT BEHAVIOR ===\n")
all_zones <- get_zones(level = "dist")
cat("Total zones available:", nrow(all_zones), "\n")

all_mobility <- get_mobility(dates = "2020-02-14", level = "dist", max_rows = 1000)
cat("Total mobility records:", nrow(all_mobility), "\n\n")

# =============================================================================
# 2. ZONE-SPECIFIC FILTERING: Download specific zones by ID
# =============================================================================
cat("=== ZONE-SPECIFIC FILTERING ===\n")

# Show what zone IDs are available (use region filtering instead)
sample_madrid_zones <- all_zones$id[grepl("^2807", all_zones$id)][1:3]
sample_barcelona_zones <- all_zones$id[grepl("^0801", all_zones$id)][1:3]

cat("Sample Madrid zone IDs:", paste(sample_madrid_zones, collapse = ", "), "\n")
cat("Sample Barcelona zone IDs:", paste(sample_barcelona_zones, collapse = ", "), "\n")
cat("For specific zone filtering, use region filtering instead\n\n")

# =============================================================================
# 3. REGION-BASED FILTERING: Download by region name
# =============================================================================
cat("=== REGION-BASED FILTERING ===\n")
madrid_zones <- get_zones(level = "dist", region_filter = "Madrid")
cat("Madrid region zones:", nrow(madrid_zones), "\n")

madrid_mobility <- get_mobility(
  dates = "2020-02-14",
  level = "dist", 
  region_filter = "Madrid",
  max_rows = 1000
)
cat("Madrid mobility records:", nrow(madrid_mobility), "\n\n")

# =============================================================================
# 4. CONVENIENCE FUNCTION: Get both mobility and zones together
# =============================================================================
cat("=== CONVENIENCE FUNCTION ===\n")
madrid_data <- get_region_mobility(
  region = "Madrid",
  dates = "2020-02-14",
  level = "dist",
  max_rows = 1000
)

cat("Madrid analysis package:\n")
cat("  - Zones:", nrow(madrid_data$zones), "\n")
cat("  - Mobility records:", nrow(madrid_data$mobility), "\n")
cat("  - Region:", madrid_data$region, "\n\n")

# =============================================================================
# 5. BASIC ANALYSIS: Calculate indicators with spatial mapping
# =============================================================================
cat("=== BASIC ANALYSIS ===\n")
madrid_indicators <- calc_indicators(madrid_data$mobility, madrid_data$zones)
cat("Madrid mobility indicators calculated\n")

madrid_containment <- calculate_containment(madrid_data$mobility, madrid_data$zones)
cat("Madrid containment analysis completed\n")

cat("\nMadrid Analysis Summary:\n")
cat("  - Total zones:", nrow(madrid_data$zones), "\n")
cat("  - Total mobility records:", nrow(madrid_data$mobility), "\n")
cat("  - Average containment:", round(madrid_containment$summary$avg_containment, 3), "\n")
cat("  - Average trips per zone:", round(madrid_indicators$summary$avg_trips_per_zone, 0), "\n")

cat("\nDemo completed successfully!\n")
