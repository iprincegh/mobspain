# mobspain: Spanish Mobility Data Analysis

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

R package for analyzing Spanish mobility patterns using MITMA data with automatic spatial visualization.

## Installation

```r
devtools::install_github("iprincegh/mobspain-r-package")
library(mobspain)
```

## Quick Start

```r
# Complete analysis in one function
results <- quick_spatial_analysis(dates = "2023-01-01", level = "dist")
print(results$maps$indicators)   # View mobility map
```

## Core Functions

### Data Access

#### `get_mobility(dates, level, zones_filter = NULL, region_filter = NULL, max_rows = NULL)`
Get mobility data between zones
- `dates`: Date(s) as string or vector ("2023-01-01" or c("2023-01-01", "2023-01-03"))
- `level`: Spatial level ("dist", "muni", "prov")
- `zones_filter`: Specific zone IDs to filter by
- `region_filter`: Region name or province code to filter by
- `max_rows`: Maximum rows to return (for memory management)

#### `get_zones(level, zones_filter = NULL, region_filter = NULL)`
Get administrative boundaries with geometries
- `level`: Spatial level ("dist", "muni", "prov")
- `zones_filter`: Specific zone IDs to filter by
- `region_filter`: Region name or province code to filter by

#### `get_region_mobility(region, dates = NULL, level = "dist")`
Get both mobility data and zones for a region
- `region`: Region name or province code
- `dates`: Date(s) to get mobility data for
- `level`: Spatial level

### Spatial Analysis

#### `calculate_containment(mobility_data, spatial_zones = NULL, create_map = TRUE)`
Calculate how much mobility stays within zones
- `mobility_data`: Output from `get_mobility()`
- `spatial_zones`: sf object with zone geometries
- `create_map`: Whether to create spatial map

#### `detect_anomalies(mobility_data, spatial_zones = NULL, threshold = 2.5, create_map = TRUE)`
Find unusual mobility patterns
- `mobility_data`: Output from `get_mobility()`
- `spatial_zones`: sf object with zone geometries
- `threshold`: Number of standard deviations for anomaly detection
- `create_map`: Whether to create spatial map

#### `predict_patterns(mobility_data, prediction_dates, model_type = "linear_regression", spatial_zones = NULL, create_map = TRUE)`
Predict future mobility patterns
- `mobility_data`: Historical mobility data
- `prediction_dates`: Date(s) to predict
- `model_type`: "linear_regression" or "random_forest"
- `spatial_zones`: sf object with zone geometries
- `create_map`: Whether to create spatial map

#### `calc_indicators(mobility_data, spatial_zones = NULL, create_map = TRUE)`
Calculate basic mobility indicators
- `mobility_data`: Output from `get_mobility()`
- `spatial_zones`: sf object with zone geometries
- `create_map`: Whether to create spatial map

### Visualization

#### `create_flows(mobility_data, spatial_zones, top_flows = 20, min_trips = NULL)`
Create flow maps showing movement between zones
- `mobility_data`: Output from `get_mobility()`
- `spatial_zones`: sf object with zone geometries
- `top_flows`: Number of top flows to display
- `min_trips`: Minimum number of trips to display flow

#### `create_spatial_map(analysis_result, spatial_zones, map_type = "auto", title = NULL)`
Create spatial maps from analysis results
- `analysis_result`: Results from analysis functions
- `spatial_zones`: sf object with zone geometries
- `map_type`: "choropleth", "points", "flows", or "auto"
- `title`: Map title

### Quick Analysis

#### `quick_spatial_analysis(dates, level = "dist", region_filter = NULL)`
Complete spatial analysis workflow
- `dates`: Date(s) to analyze
- `level`: Spatial level
- `region_filter`: Region to focus on

## Examples

### Basic Usage
```r
# Get mobility data and zones
mobility <- get_mobility(dates = "2023-01-01", level = "dist")
zones <- get_zones(level = "dist")

# Quick analysis
indicators <- calc_indicators(mobility, zones)
print(indicators$map)
```

### Regional Analysis
```r
# Get regional data
madrid_data <- get_region_mobility("Madrid", dates = "2023-01-01")

# Analyze containment
containment <- calculate_containment(madrid_data$mobility, madrid_data$zones)
print(containment$map)
```

### Filtering Examples
```r
# Filter by region name
madrid_mobility <- get_mobility(dates = "2023-01-01", region_filter = "Madrid")

# Filter by province code
valencia_mobility <- get_mobility(dates = "2023-01-01", region_filter = "46")

# Filter by specific zones
specific_zones <- get_mobility(dates = "2023-01-01", zones_filter = c("28079", "08019"))
```

## Configuration

```r
# Optional configuration
configure_mobspain()
mobspain_status()  # Check status
```

## License

MIT License
