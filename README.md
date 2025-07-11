# mobspain: Advanced Spatial Mobility Analysis for Spanish Transportation Data

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D3.5-blue.svg)](https://www.r-project.org/)

A comprehensive R package for analyzing Spanish mobility patterns using MITMA (Ministry of Transport, Mobility and Urban Agenda) data with advanced spatial analysis capabilities, automatic map generation, and best-practice spatial workflows.

## 🚀 Key Features

- **🗺️ Automatic Spatial Visualization**: All analysis functions create beautiful maps when geometries are provided
- **📊 Advanced Spatial Analysis**: Buffer analysis, accessibility matrices, spatial lag, containment analysis
- **🤖 Machine Learning Integration**: Predict mobility patterns with built-in ML models
- **🔍 Smart Filtering**: Download only needed data by region, province, or specific zones
- **⚡ Memory Efficient**: Optimized for large datasets with intelligent memory management
- **🛠️ Best Practices**: Robust coordinate transformations, geometry validation, and error handling

## 📦 Installation

```r
devtools::install_github("iprincegh/mobspain-r-package")
library(mobspain)
```

## 🚀 Quick Start

```r
# Complete analysis in one function
results <- quick_analysis(dates = "2020-02-14", level = "dist")
print(results$maps$indicators)   # View mobility map

# Regional analysis with buffer zones
madrid_data <- get_region_mobility("Madrid", dates = "2020-02-14")
buffer_analysis <- analyze_mobility_buffer(
  center_points = c(-3.7038, 40.4168),  # Madrid coordinates
  buffer_km = 20,
  dates = "2020-02-14"
)
```

## 🔧 Core Functions

### 📊 Data Access

#### `get_mobility(dates, level, max_rows = 10000, zones_filter = NULL, region_filter = NULL)`
Retrieve mobility data between zones with advanced filtering
- `dates`: Date(s) as string or vector ("2020-02-14" or c("2020-02-14", "2020-02-16"))
- `level`: Spatial level ("dist", "muni", "prov")
- `max_rows`: Maximum rows to return (default: 10000 for memory management)
- `zones_filter`: Specific zone IDs to filter by
- `region_filter`: Region name or province code to filter by

#### `get_zones(level = "dist", year = 2023, zones_filter = NULL, region_filter = NULL)`
Get administrative boundaries with enhanced spatial statistics
- Returns zones with: centroid coordinates, area, perimeter, compactness, bbox dimensions
- Includes robust coordinate transformation and geometry validation
- `level`: Spatial level ("dist", "muni", "prov")
- `year`: Year for administrative boundaries (default: 2023)

#### `get_region_mobility(region, dates = "2020-02-14", level = "dist", max_rows = 10000)`
Get both mobility data and zones for a region in one call
- `region`: Region name or province code
- `dates`: Date(s) to get mobility data for
- `level`: Spatial level
- `max_rows`: Maximum rows to return

### 🗺️ Advanced Spatial Analysis

#### `calculate_containment(mobility_data, spatial_zones = NULL, create_map = TRUE)`
Calculate how much mobility stays within zones
- Returns containment ratios with automatic spatial mapping
- `mobility_data`: Output from `get_mobility()`
- `spatial_zones`: sf object with zone geometries
- `create_map`: Whether to create spatial map

#### `get_zones_buffer(zones, center_points, buffer_km = 10, crs_proj = 25830)`
**NEW**: Select zones within buffer distance of points
- Robust coordinate transformation to projected CRS
- Returns zones with buffer distance information
- `zones`: sf object with zone geometries
- `center_points`: Points as coordinates or sf object
- `buffer_km`: Buffer distance in kilometers
- `crs_proj`: Projected CRS for accurate distance calculations

#### `calculate_accessibility_matrix(zones, decay_function = "exponential", max_distance_km = 50)`
**NEW**: Calculate spatial accessibility between zones
- Supports exponential, linear, and power decay functions
- Returns normalized accessibility indices
- `zones`: sf object with zone geometries
- `decay_function`: "exponential", "linear", or "power"
- `max_distance_km`: Maximum distance for accessibility calculation

#### `calculate_spatial_lag(zones, variable, method = "contiguity", k = 5, max_distance_km = 20)`
**NEW**: Calculate spatial lag for spatial autocorrelation analysis
- Supports contiguity and distance-based spatial weights
- `zones`: sf object with zone geometries
- `variable`: Variable name for spatial lag calculation
- `method`: "contiguity" or "distance"
- `k`: Number of nearest neighbors (for distance method)

#### `analyze_mobility_buffer(center_points, buffer_km, dates, level = "dist")`
**NEW**: Complete buffer-based mobility analysis
- Analyzes mobility patterns within buffer zones
- Returns zones, mobility data, and analysis results
- `center_points`: Center points for buffer analysis
- `buffer_km`: Buffer distance in kilometers
- `dates`: Date(s) for analysis

### 🤖 Machine Learning & Anomaly Detection

#### `detect_anomalies(mobility_data, threshold = 2.5, spatial_zones = NULL, create_map = TRUE)`
Find unusual mobility patterns using statistical methods
- `mobility_data`: Output from `get_mobility()`
- `threshold`: Number of standard deviations for anomaly detection
- `spatial_zones`: sf object with zone geometries
- `create_map`: Whether to create spatial map

#### `predict_patterns(mobility_data, prediction_dates, model_type = "linear_regression", spatial_zones = NULL, create_map = TRUE)`
Predict future mobility patterns using machine learning
- `mobility_data`: Historical mobility data
- `prediction_dates`: Date(s) to predict
- `model_type`: "linear_regression" or "random_forest"
- `spatial_zones`: sf object with zone geometries
- `create_map`: Whether to create spatial map

#### `detect_outliers(mobility_data, threshold = 2.5, spatial_zones = NULL, create_map = TRUE)`
**NEW**: Advanced outlier detection with spatial context
- Uses multiple statistical methods for robust outlier detection
- Returns outlier zones with spatial visualization

### 📈 Mobility Indicators

#### `calc_indicators(mobility_data, spatial_zones = NULL)`
Calculate comprehensive mobility indicators
- Returns total trips, connectivity indices, trip density, and more
- `mobility_data`: Output from `get_mobility()`
- `spatial_zones`: sf object with zone geometries

#### `calc_stats(mobility_data, spatial_zones)`
**NEW**: Calculate enhanced spatial statistics
- Includes trip density, connectivity ratios, and spatial metrics
- Integrates with spatial zones for comprehensive analysis

### 🎨 Visualization

#### `create_flows(mobility_data, spatial_zones, top_flows = 20)`
Create flow maps showing movement between zones
- `mobility_data`: Output from `get_mobility()`
- `spatial_zones`: sf object with zone geometries
- `top_flows`: Number of top flows to display

#### `create_spatial_map(analysis_result, spatial_zones, map_type = "auto", title = NULL)`
Create spatial maps from analysis results
- `analysis_result`: Results from analysis functions
- `spatial_zones`: sf object with zone geometries
- `map_type`: "choropleth", "points", "flows", or "auto"
- `title`: Map title

#### `create_dashboard(mobility_data, spatial_zones, analyses = c("indicators", "containment", "anomalies"))`
**NEW**: Create comprehensive analysis dashboard
- Combines multiple analyses into a single visualization
- `analyses`: Vector of analyses to include in dashboard

### ⚡ Quick Analysis

#### `quick_analysis(dates = "2020-02-14", level = "dist")`
Complete spatial analysis workflow
- `dates`: Date(s) to analyze
- `level`: Spatial level

#### `analyze_complete(mobility_data, spatial_zones, analyses = c("containment", "anomalies", "indicators"))`
**NEW**: Comprehensive analysis with multiple metrics
- Runs multiple analyses and creates combined results
- `analyses`: Vector of analyses to perform

#### `analyze_spatial(mobility_data, spatial_zones, analyses = c("containment", "anomalies", "indicators"))`
**NEW**: Spatial-focused analysis workflow
- Emphasizes spatial relationships and patterns
## 💡 Examples

### Basic Usage
```r
# Get mobility data and zones
mobility <- get_mobility(dates = "2020-02-14", level = "dist")
zones <- get_zones(level = "dist")

# Quick analysis with automatic mapping
indicators <- calc_indicators(mobility, zones)
print(indicators$map)
```

### Regional Analysis
```r
# Get regional data
madrid_data <- get_region_mobility("Madrid", dates = "2020-02-14")

# Analyze containment
containment <- calculate_containment(madrid_data$mobility, madrid_data$zones)
print(containment$map)
```

### Advanced Spatial Analysis
```r
# Buffer analysis around Madrid
madrid_center <- c(-3.7038, 40.4168)
buffer_zones <- get_zones_buffer(zones, madrid_center, buffer_km = 20)

# Calculate accessibility matrix
accessibility <- calculate_accessibility_matrix(buffer_zones, max_distance_km = 30)

# Spatial lag analysis
spatial_lag <- calculate_spatial_lag(buffer_zones, "total_trips", method = "contiguity")
```

### Machine Learning Predictions
```r
# Get historical data
mobility_hist <- get_mobility(dates = c("2020-02-10", "2020-02-14"), level = "dist")

# Predict future patterns
predictions <- predict_patterns(
  mobility_hist, 
  prediction_dates = "2020-02-15",
  model_type = "random_forest",
  spatial_zones = zones
)
print(predictions$spatial_map)
```

### Filtering Examples
```r
# Filter by region name
madrid_mobility <- get_mobility(dates = "2020-02-14", region_filter = "Madrid")

# Filter by province code
valencia_mobility <- get_mobility(dates = "2020-02-14", region_filter = "46")

# Filter by specific zones
specific_zones <- get_mobility(dates = "2020-02-14", zones_filter = c("28079", "08019"))
```

### Complete Analysis Dashboard
```r
# Create comprehensive analysis dashboard
dashboard <- create_dashboard(
  mobility_data = mobility,
  spatial_zones = zones,
  analyses = c("indicators", "containment", "anomalies")
)
print(dashboard$combined_map)
```

## ⚙️ Configuration

```r
# Optional configuration for caching and performance
configure_mobspain(
  cache_dir = tempdir(),
  max_cache_size = 500,  # MB
  parallel = FALSE,
  cache_max_age_days = 7
)

# Check package status
mobspain_status()
```

## 🔧 Validation Functions

```r
# Validate MITMA data structure
validate_mitma_data(mobility_data)

# Check Spanish holidays
check_spanish_holidays(dates = "2020-02-14")

# Comprehensive Spanish mobility data validation
validate_spanish_mobility_data(mobility_data)
```

## 📊 Best Practices

The package implements spatial analysis best practices:

- **Coordinate Transformation**: Automatic CRS handling with proper projections
- **Geometry Validation**: Robust geometry checking and repair
- **Memory Management**: Efficient processing of large datasets
- **Error Handling**: Comprehensive error checking and user feedback
- **Spatial Statistics**: Enhanced spatial metrics and indicators

## 🎯 Advanced Features

### Buffer Analysis
```r
# Analyze mobility within buffer zones
buffer_analysis <- analyze_mobility_buffer(
  center_points = madrid_center,
  buffer_km = 25,
  dates = "2020-02-14",
  level = "dist"
)
```

### Accessibility Analysis
```r
# Calculate spatial accessibility
accessibility_matrix <- calculate_accessibility_matrix(
  zones = madrid_zones,
  decay_function = "exponential",
  max_distance_km = 50
)
```

### Spatial Autocorrelation
```r
# Calculate spatial lag for autocorrelation analysis
spatial_lag <- calculate_spatial_lag(
  zones = madrid_zones,
  variable = "total_trips",
  method = "distance",
  k = 8
)
```

## 📈 Performance

- **Memory Efficient**: Optimized for large datasets with intelligent sampling
- **Parallel Processing**: Optional parallel processing for large analyses
- **Caching**: Smart caching system to avoid redundant downloads
- **Filtering**: Download only needed data to reduce memory usage

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## 📄 License

MIT License

---

**Note**: This package uses MITMA (Ministry of Transport, Mobility and Urban Agenda) data. Please ensure compliance with data usage policies when using this package for research or commercial purposes.
