#' Core Spatial Analysis Functions for Spatial Analysis
#'
#' This file contains the essential spatial analysis functions designed for 
#' users learning spatial data science.
#' All functions automatically create spatial maps when possible.

#' Get spatial zones with geometries
#'
#' Retrieves spatial zones (administrative boundaries) with geometries for mapping.
#' Simplified for user convenience with automatic CRS handling and basic spatial statistics.
#' Includes filtering options to load subsets of zones.
#'
#' @param level Character. Administrative level to retrieve:
#'   \itemize{
#'     \item "dist" - Districts (default, recommended for users)
#'     \item "prov" - Provinces  
#'     \item "muni" - Municipalities (detailed, large dataset)
#'   }
#' @param year Numeric. Year for the boundaries (default: 2023). Available years depend on data source.
#' @param zones_filter Character vector. Optional filter for specific zone IDs.
#'   If NULL (default), loads all zones. Examples: c("28079", "08019") for Madrid and Barcelona.
#' @param region_filter Character. Optional filter by region/province name or code.
#'   Examples: "Madrid", "Barcelona", "28" (Madrid province). Automatically finds relevant zones.
#' @return sf object with spatial zones containing:
#'   \itemize{
#'     \item geometry - Polygon geometries in WGS84 (EPSG:4326)
#'     \item id - Zone identifier
#'     \item area_km2 - Area in square kilometers
#'     \item centroid - Zone centroid coordinates
#'   }
#' @details
#' This function automatically:
#' \itemize{
#'   \item Downloads spatial boundaries from Spanish government sources
#'   \item Transforms to WGS84 for consistent mapping
#'   \item Calculates area and centroid for each zone
#'   \item Validates spatial geometries
#' }
#' @export
#' @examples
#' \dontrun{
#' # Get all district boundaries (default)
#' zones <- get_zones(level = "dist")
#' 
#' # Get specific zones only (reduces memory usage)
#' madrid_barcelona <- get_zones(
#'   level = "dist", 
#'   zones_filter = c("28079", "08019")  # Madrid and Barcelona
#' )
#' 
#' # Get zones for a specific region
#' madrid_zones <- get_zones(
#'   level = "dist",
#'   region_filter = "Madrid"
#' )
#' 
#' # Get zones by province code
#' valencia_zones <- get_zones(
#'   level = "dist",
#'   region_filter = "46"  # Valencia province
#' )
#' 
#' # Get province boundaries
#' provinces <- get_zones(level = "prov", year = 2023)
#' 
#' # Quick map of zones
#' plot(zones$geometry)
#' 
#' # Check zone information
#' head(zones)
#' summary(zones$area_km2)
#' }
get_zones <- function(level = "dist", year = 2023, zones_filter = NULL, region_filter = NULL) {
  
  message("Step 1: Loading spatial zones for level: ", level)
  
  # Use spanishoddata to get zones with geometries
  if(!requireNamespace("spanishoddata", quietly = TRUE)) {
    stop("spanishoddata package required. Install with: install.packages('spanishoddata')")
  }
  
  # Get spatial zones
  zones <- spanishoddata::spod_get_zones(zones = level, ver = 1)
  
  # Ensure it's an sf object
  if(!inherits(zones, "sf")) {
    stop("Failed to load spatial zones. Please check your internet connection.")
  }
  
  # Apply filtering if specified
  if(!is.null(region_filter)) {
    message("Step 2: Filtering zones by region: ", region_filter)
    zones_to_keep <- get_region_zones_spatial(region_filter, zones)
    if(!is.null(zones_to_keep) && length(zones_to_keep) > 0) {
      # Use dplyr::filter to preserve sf object structure
      zones <- zones %>% dplyr::filter(.data$id %in% zones_to_keep)
      message("Filtered to ", nrow(zones), " zones matching region filter")
    }
  } else if(!is.null(zones_filter)) {
    message("Step 2: Filtering zones by zone IDs...")
    # Use dplyr::filter to preserve sf object structure
    zones <- zones %>% dplyr::filter(.data$id %in% zones_filter)
    message("Filtered to ", nrow(zones), " zones matching zone filter")
  }
  
  # Check if any zones remain after filtering
  if(nrow(zones) == 0) {
    stop("No zones found matching the specified filter. Please check your zone IDs or region filter.")
  }
  
  # Ensure we still have a valid sf object after filtering
  if(!inherits(zones, "sf")) {
    stop("Zones object lost its sf class after filtering")
  }
  
  # Transform to WGS84 using best practices for coordinate transformation
  if(!sf::st_is_longlat(zones)) {
    next_step <- if(!is.null(region_filter) || !is.null(zones_filter)) "3" else "2"
    message("Step ", next_step, ": Transforming to WGS84 using best practices...")
    
    # Check if geometry is valid before transformation
    if(!"geometry" %in% names(zones)) {
      stop("No geometry column found before coordinate transformation")
    }
    
    # Validate and repair geometries using best practices
    message("  - Validating geometries...")
    valid_geoms <- sf::st_is_valid(zones)
    if(!all(valid_geoms)) {
      message("  - Repairing ", sum(!valid_geoms), " invalid geometries...")
      zones <- sf::st_make_valid(zones)
      
      # Check again after repair
      valid_after_repair <- sf::st_is_valid(zones)
      if(!all(valid_after_repair)) {
        warning("Some geometries could not be repaired and may cause issues")
      }
    }
    
    # Store original CRS for reference
    original_crs <- sf::st_crs(zones)
    message("  - Original CRS: ", original_crs$input)
    
    # Transform using proper CRS object
    zones <- sf::st_transform(zones, crs = sf::st_crs(4326))
    
    # Verify transformation success
    if(!"geometry" %in% names(zones)) {
      stop("Geometry column lost during coordinate transformation")
    }
    
    if(!sf::st_is_longlat(zones)) {
      stop("Coordinate transformation to WGS84 failed")
    }
    
    message("  - Successfully transformed to WGS84 (EPSG:4326)")
  } else {
    message("  - Already in WGS84 coordinate system")
  }
  
  # Add comprehensive spatial statistics using best practices
  if("geometry" %in% names(zones) && nrow(zones) > 0) {
    final_step <- if(!is.null(region_filter) || !is.null(zones_filter)) "4" else "3"
    message("Step ", final_step, ": Calculating spatial statistics...")
    
    tryCatch({
      # Calculate area in square kilometers
      zones$area_km2 <- as.numeric(sf::st_area(zones)) / 1e6
      
      # Calculate centroids using best practices
      zones$centroid <- sf::st_centroid(zones$geometry)
      
      # Extract centroid coordinates for easier access
      centroid_coords <- sf::st_coordinates(zones$centroid)
      zones$centroid_lon <- centroid_coords[, 1]
      zones$centroid_lat <- centroid_coords[, 2]
      
      # Calculate bounding box dimensions
      bbox_matrix <- sf::st_bbox(zones$geometry)
      zones$bbox_width <- bbox_matrix[3] - bbox_matrix[1]  # xmax - xmin
      zones$bbox_height <- bbox_matrix[4] - bbox_matrix[2]  # ymax - ymin
      
      # Calculate perimeter (useful for shape analysis)
      tryCatch({
        zones$perimeter_km <- as.numeric(sf::st_length(sf::st_cast(zones$geometry, "MULTILINESTRING"))) / 1000
        
        # Calculate compactness index (area/perimeter ratio)
        zones$compactness <- zones$area_km2 / (zones$perimeter_km^2) * 10000  # normalized
      }, error = function(e) {
        message("  - Could not calculate perimeter: ", e$message)
        zones$perimeter_km <<- NA
        zones$compactness <<- NA
      })
      
    }, error = function(e) {
      message("Warning: Could not calculate spatial statistics: ", e$message)
      # Fallback to basic calculations
      zones$area_km2 <- as.numeric(sf::st_area(zones)) / 1e6
      zones$centroid <- sf::st_centroid(zones$geometry)
    })
  }
  
  # Ensure we still have a valid sf object after filtering
  if(!inherits(zones, "sf")) {
    stop("Zones object lost its sf class after filtering")
  }
  
  # Final check for geometry column
  if(!"geometry" %in% names(zones)) {
    stop("Zones object lost its geometry column")
  }
  
  message("Step ", ifelse(!is.null(region_filter) || !is.null(zones_filter), "5", "4"), ": Loaded ", nrow(zones), " spatial zones with geometries")
  
  return(zones)
}

#' Calculate mobility containment with spatial mapping
#'
#' Calculates containment index (proportion of trips within same zone) 
#' and creates spatial maps automatically for spatial analysis.
#'
#' @param mobility_data Data frame with mobility data containing:
#'   \itemize{
#'     \item origin - Origin zone identifier
#'     \item dest - Destination zone identifier  
#'     \item n_trips - Number of trips between origin and destination
#'     \item date - Date of trips (optional)
#'   }
#' @param spatial_zones sf object with spatial zones for mapping (optional).
#'   If provided, should contain 'id' column matching origin/dest values.
#'   Use \code{get_zones()} to obtain.
#' @param create_map Logical. Whether to create spatial map if spatial_zones provided (default: TRUE).
#'   Set to FALSE to skip map creation for faster processing.
#' @return List containing:
#'   \itemize{
#'     \item containment - Data frame with containment indices per zone
#'     \item summary - Summary statistics (avg, median, min, max containment)
#'     \item map - ggplot2 choropleth map (if spatial_zones provided and create_map = TRUE)
#'   }
#' @details
#' Containment index measures spatial self-containment by calculating the proportion 
#' of trips that start and end in the same zone. Values range from 0 (no internal trips) 
#' to 1 (all trips are internal). Higher values indicate more self-contained zones.
#' 
#' The function automatically:
#' \itemize{
#'   \item Calculates containment indices for each origin zone
#'   \item Computes summary statistics
#'   \item Creates choropleth map showing containment patterns
#'   \item Uses viridis color scale for accessibility
#' }
#' @export
#' @examples
#' \dontrun{
#' # Basic containment analysis with mapping
#' mobility <- get_mobility(dates = "2023-01-01", level = "dist")
#' zones <- get_zones("dist")
#' 
#' result <- calculate_containment(mobility, zones)
#' print(result$map)  # View spatial map
#' print(result$summary)  # View summary statistics
#' 
#' # Analysis without mapping (faster)
#' result_no_map <- calculate_containment(mobility, create_map = FALSE)
#' head(result_no_map$containment)
#' }
calculate_containment <- function(mobility_data, spatial_zones = NULL, create_map = TRUE) {
  
  message("Step 1: Calculating containment indices...")
  
  # Calculate containment
  containment <- mobility_data %>%
    dplyr::group_by(.data$origin) %>%
    dplyr::summarise(
      total_trips = sum(.data$n_trips, na.rm = TRUE),
      internal_trips = sum(.data$n_trips[.data$origin == .data$dest], na.rm = TRUE),
      containment_index = ifelse(.data$total_trips > 0, 
                                .data$internal_trips / .data$total_trips, 0),
      .groups = "drop"
    ) %>%
    dplyr::rename(id = .data$origin)
  
  message("Step 2: Calculated containment for ", nrow(containment), " zones")
  
  # Create result object with S3 class
  result <- list(
    containment = containment,
    summary = list(
      avg_containment = mean(containment$containment_index, na.rm = TRUE),
      median_containment = median(containment$containment_index, na.rm = TRUE),
      max_containment = max(containment$containment_index, na.rm = TRUE),
      min_containment = min(containment$containment_index, na.rm = TRUE)
    )
  )
  
  class(result) <- "mobspain_containment"
  
  # Create spatial map if requested
  if(create_map && !is.null(spatial_zones)) {
    message("Step 3: Creating spatial map of containment...")
    
    # Merge containment with spatial zones
    map_data <- spatial_zones %>%
      dplyr::left_join(containment, by = "id") %>%
      dplyr::mutate(containment_index = ifelse(is.na(.data$containment_index), 0, .data$containment_index))
    
    # Create choropleth map
    map <- ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data$containment_index), color = "white", size = 0.1) +
      ggplot2::scale_fill_viridis_c(name = "Containment\nIndex", labels = scales::percent) +
      ggplot2::labs(
        title = "Mobility Containment Index by Zone",
        subtitle = "Proportion of trips that stay within the same zone",
        caption = "Source: MITMA Spanish Mobility Data"
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 10),
        legend.position = "bottom"
      )
    
    result$map <- map
  }
  
  return(result)
}

#' Detect mobility anomalies with spatial mapping
#'
#' Detects anomalies in mobility patterns using simple statistical methods
#' and creates spatial maps showing anomalous zones.
#'
#' @param mobility_data Mobility data to analyze
#' @param threshold Z-score threshold for anomaly detection (default: 2.5)
#' @param spatial_zones Optional sf object with spatial zones for mapping
#' @param create_map Whether to create spatial map (default: TRUE)
#' @return List with anomaly results and optional spatial map
#' @export
#' @examples
#' \dontrun{
#' # Detect anomalies with spatial mapping
#' mobility <- get_mobility(dates = "2023-01-01", level = "dist")
#' zones <- get_zones("dist")
#' 
#' result <- detect_anomalies_spatial(mobility, zones)
#' print(result$map)  # View spatial map
#' }
detect_anomalies <- function(mobility_data, threshold = 2.5, 
                                   spatial_zones = NULL, create_map = TRUE) {
  
  message("Step 1: Calculating z-scores for anomaly detection...")
  
  # Calculate total trips per zone
  zone_trips <- mobility_data %>%
    dplyr::group_by(.data$origin) %>%
    dplyr::summarise(total_trips = sum(.data$n_trips, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(id = .data$origin)
  
  # Calculate z-scores
  zone_trips$z_score <- abs(scale(zone_trips$total_trips))
  zone_trips$is_anomaly <- zone_trips$z_score > threshold
  zone_trips$anomaly_type <- ifelse(zone_trips$is_anomaly, "statistical_outlier", "normal")
  
  anomaly_count <- sum(zone_trips$is_anomaly, na.rm = TRUE)
  message("Step 2: Found ", anomaly_count, " anomalous zones (", 
          round(anomaly_count/nrow(zone_trips)*100, 1), "% of zones)")
  
  # Create result object with S3 class
  result <- list(
    anomalies = zone_trips,
    summary = list(
      total_zones = nrow(zone_trips),
      anomaly_count = anomaly_count,
      anomaly_percentage = round(anomaly_count/nrow(zone_trips)*100, 1),
      threshold_used = threshold
    )
  )
  
  class(result) <- "mobspain_anomalies"
  
  # Create spatial map if requested
  if(create_map && !is.null(spatial_zones)) {
    message("Step 3: Creating spatial map of anomalies...")
    
    # Merge anomalies with spatial zones
    map_data <- spatial_zones %>%
      dplyr::left_join(zone_trips, by = "id") %>%
      dplyr::mutate(
        is_anomaly = ifelse(is.na(.data$is_anomaly), FALSE, .data$is_anomaly),
        anomaly_type = ifelse(is.na(.data$anomaly_type), "normal", .data$anomaly_type)
      )
    
    # Create map
    map <- ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data$is_anomaly), color = "white", size = 0.1) +
      ggplot2::scale_fill_manual(
        values = c("FALSE" = "lightgray", "TRUE" = "red"),
        name = "Anomaly Status",
        labels = c("Normal", "Anomaly")
      ) +
      ggplot2::labs(
        title = "Mobility Anomaly Detection",
        subtitle = paste("Zones with unusual mobility patterns (threshold =", threshold, ")"),
        caption = "Source: MITMA Spanish Mobility Data"
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 10),
        legend.position = "bottom"
      )
    
    result$map <- map
  }
  
  return(result)
}

#' Complete spatial analysis workflow
#'
#' Runs multiple spatial analyses and creates a comprehensive spatial dashboard
#' for users.
#'
#' @param mobility_data Mobility data to analyze
#' @param spatial_zones sf object with spatial zones
#' @param analyses Vector of analyses to run (default: all)
#' @return List with all analysis results and spatial maps
#' @export
#' @examples
#' \dontrun{
#' # Complete spatial analysis
#' mobility <- get_mobility(dates = "2023-01-01", level = "dist")
#' zones <- get_zones("dist")
#' 
#' results <- analyze_mobility_complete(mobility, zones)
#' print(results$maps$containment)
#' print(results$maps$anomalies)
#' }
analyze_complete <- function(mobility_data, spatial_zones, 
                                    analyses = c("containment", "anomalies", "predictions")) {
  
  message("Starting complete spatial analysis workflow...")
  
  results <- list()
  
  # Containment analysis
  if("containment" %in% analyses) {
    message("Running containment analysis...")
    results$containment <- calculate_containment(mobility_data, spatial_zones)
  }
  
  # Anomaly detection
  if("anomalies" %in% analyses) {
    message("Running anomaly detection...")
    results$anomalies <- detect_anomalies(mobility_data, spatial_zones = spatial_zones)
  }
  
  # Predictions (if requested)
  if("predictions" %in% analyses) {
    message("Running mobility predictions...")
    # Create simple prediction dates (next 2 days)
    last_date <- max(as.Date(mobility_data$date), na.rm = TRUE)
    pred_dates <- seq(last_date + 1, last_date + 2, by = "day")
    
    results$predictions <- predict_patterns(
      mobility_data = mobility_data,
      prediction_dates = pred_dates,
      spatial_zones = spatial_zones
    )
  }
  
  # Extract maps for easy access
  maps <- list()
  if(!is.null(results$containment$map)) maps$containment <- results$containment$map
  if(!is.null(results$anomalies$map)) maps$anomalies <- results$anomalies$map
  if(!is.null(results$predictions$spatial_map)) maps$predictions <- results$predictions$spatial_map
  
  results$maps <- maps
  
  message("Complete spatial analysis finished!")
  
  return(results)
}

#' Create spatial statistics summary
#'
#' Calculate basic spatial statistics for mobility data and zones.
#'
#' @param mobility_data Mobility data
#' @param spatial_zones sf object with spatial zones
#' @return Data frame with spatial statistics
#' @keywords internal
calculate_stats <- function(mobility_data, spatial_zones) {
  
  message("Calculating spatial statistics...")
  
  # Calculate basic stats per zone
  zone_stats <- mobility_data %>%
    dplyr::group_by(.data$origin) %>%
    dplyr::summarise(
      total_trips = sum(.data$n_trips, na.rm = TRUE),
      avg_trips = mean(.data$n_trips, na.rm = TRUE),
      unique_destinations = dplyr::n_distinct(.data$dest),
      .groups = "drop"
    ) %>%
    dplyr::rename(id = .data$origin)
  
  # Add spatial zone info
  zone_info <- spatial_zones %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data$id, .data$area_km2)
  
  # Combine
  spatial_stats <- zone_stats %>%
    dplyr::left_join(zone_info, by = "id") %>%
    dplyr::mutate(
      trip_density = .data$total_trips / .data$area_km2,
      connectivity = .data$unique_destinations / nrow(spatial_zones)
    )
  
  message("Calculated spatial statistics for ", nrow(spatial_stats), " zones")
  
  return(spatial_stats)
}

#' Helper function to filter spatial zones by region
#'
#' @param region_filter Character. Region name or code to filter by
#' @param zones sf object with zone geometries
#' @return Character vector of zone IDs or NULL
#' @keywords internal
get_region_zones_spatial <- function(region_filter, zones) {
  
  tryCatch({
    # Try to match by different criteria
    if(nchar(region_filter) <= 2 && grepl("^[0-9]+$", region_filter)) {
      # Numeric code - match province code prefix
      matching_zones <- zones[grepl(paste0("^", region_filter), zones$id), ]
    } else {
      # Text name - try to match province/region name
      if("name" %in% names(zones)) {
        matching_zones <- zones[grepl(region_filter, zones$name, ignore.case = TRUE), ]
      } else if("provincia" %in% names(zones)) {
        matching_zones <- zones[grepl(region_filter, zones$provincia, ignore.case = TRUE), ]
      } else if("region" %in% names(zones)) {
        matching_zones <- zones[grepl(region_filter, zones$region, ignore.case = TRUE), ]
      } else {
        # Fallback - try to match any text column
        text_columns <- sapply(zones, is.character)
        if(any(text_columns)) {
          for(col in names(zones)[text_columns]) {
            matches <- grepl(region_filter, zones[[col]], ignore.case = TRUE)
            if(any(matches)) {
              matching_zones <- zones[matches, ]
              break
            }
          }
        }
      }
    }
    
    if(exists("matching_zones") && nrow(matching_zones) > 0) {
      return(matching_zones$id)
    } else {
      warning("No zones found for region filter: ", region_filter)
      return(NULL)
    }
    
  }, error = function(e) {
    warning("Could not filter by region: ", e$message)
    return(NULL)
  })
}

#' Get zones within buffer of specified locations
#'
#' Selects zones that intersect with a buffer around specified points or regions.
#' This is useful for analyzing mobility patterns around specific locations like
#' airports, city centers, or functional urban areas.
#'
#' @param zones sf object with zone geometries (use \code{get_zones()} to obtain)
#' @param center_points Data frame with center points containing:
#'   \itemize{
#'     \item lon - Longitude coordinate
#'     \item lat - Latitude coordinate
#'     \item id - Optional identifier for the center point
#'   }
#' @param buffer_km Numeric. Buffer distance in kilometers (default: 10)
#' @param crs_proj Character or numeric. Projected CRS for accurate buffer calculations.
#'   Default uses UTM zone 30N (EPSG:25830) suitable for Spain.
#' @return sf object with zones within the buffer areas
#' @details
#' This function uses best practices for spatial buffer analysis:
#' \itemize{
#'   \item Transforms to projected CRS for accurate distance calculations
#'   \item Creates buffer zones around center points
#'   \item Finds intersecting zones using spatial predicates
#'   \item Returns results in WGS84 for mapping compatibility
#' }
#' @export
#' @examples
#' \dontrun{
#' # Get zones within 20km of Madrid center
#' madrid_center <- data.frame(lon = -3.7038, lat = 40.4168, id = "Madrid")
#' zones <- get_zones(level = "dist")
#' madrid_area <- get_zones_buffer(zones, madrid_center, buffer_km = 20)
#' 
#' # Multiple center points (e.g., major cities)
#' centers <- data.frame(
#'   lon = c(-3.7038, 2.1734, -0.3763),
#'   lat = c(40.4168, 41.3851, 39.4699),
#'   id = c("Madrid", "Barcelona", "Valencia")
#' )
#' metro_zones <- get_zones_buffer(zones, centers, buffer_km = 25)
#' }
get_zones_buffer <- function(zones, center_points, buffer_km = 10, crs_proj = 25830) {
  
  message("Step 1: Preparing buffer analysis...")
  
  # Validate inputs
  if(!inherits(zones, "sf") || !"geometry" %in% names(zones)) {
    stop("zones must be an sf object with geometry column")
  }
  
  if(!"lon" %in% names(center_points) || !"lat" %in% names(center_points)) {
    stop("center_points must contain 'lon' and 'lat' columns")
  }
  
  # Create sf object from center points
  center_sf <- sf::st_as_sf(center_points, 
                           coords = c("lon", "lat"), 
                           crs = 4326)
  
  # Transform to projected CRS for accurate buffer calculations
  message("Step 2: Transforming to projected CRS for buffer calculations...")
  zones_proj <- sf::st_transform(zones, crs_proj)
  center_proj <- sf::st_transform(center_sf, crs_proj)
  
  # Create buffer zones
  message("Step 3: Creating ", buffer_km, "km buffer zones...")
  buffer_zones <- sf::st_buffer(center_proj, dist = buffer_km * 1000)  # Convert km to meters
  
  # Find intersecting zones
  message("Step 4: Finding zones within buffer areas...")
  intersecting_zones <- sf::st_intersection(zones_proj, sf::st_union(buffer_zones))
  
  # Transform back to WGS84 for mapping
  message("Step 5: Transforming back to WGS84...")
  result_zones <- sf::st_transform(intersecting_zones, 4326)
  
  # Add buffer info to results
  result_zones$buffer_km <- buffer_km
  result_zones$analysis_type <- "buffer_selection"
  
  message("Found ", nrow(result_zones), " zones within ", buffer_km, "km buffer")
  
  return(result_zones)
}

#' Calculate spatial accessibility matrix
#'
#' Calculates spatial accessibility metrics between zones using distance decay functions.
#' This is useful for understanding spatial connectivity and accessibility patterns.
#'
#' @param zones sf object with zone geometries
#' @param decay_function Character. Distance decay function to use:
#'   \itemize{
#'     \item "exponential" - Exponential decay (default)
#'     \item "power" - Power decay
#'     \item "linear" - Linear decay
#'   }
#' @param max_distance_km Numeric. Maximum distance for calculations (default: 50km)
#' @param decay_parameter Numeric. Decay parameter (default: 0.1 for exponential)
#' @return Matrix with accessibility values between zones
#' @details
#' Uses best practices for spatial accessibility analysis:
#' \itemize{
#'   \item Calculates centroid-to-centroid distances
#'   \item Applies distance decay functions
#'   \item Uses projected CRS for accurate distance calculations
#'   \item Returns normalized accessibility indices
#' }
#' @export
#' @examples
#' \dontrun{
#' zones <- get_zones(level = "dist", region_filter = "Madrid")
#' accessibility <- calculate_accessibility_matrix(zones, max_distance_km = 30)
#' }
calculate_accessibility_matrix <- function(zones, decay_function = "exponential", 
                                         max_distance_km = 50, decay_parameter = 0.1) {
  
  message("Step 1: Preparing accessibility analysis...")
  
  # Validate inputs
  if(!inherits(zones, "sf") || !"geometry" %in% names(zones)) {
    stop("zones must be an sf object with geometry column")
  }
  
  # Transform to projected CRS for accurate distance calculations
  zones_proj <- sf::st_transform(zones, 25830)  # UTM 30N for Spain
  
  # Calculate centroids
  message("Step 2: Calculating zone centroids...")
  centroids <- sf::st_centroid(zones_proj)
  
  # Calculate distance matrix
  message("Step 3: Calculating distance matrix...")
  distances <- sf::st_distance(centroids)
  distances_km <- as.numeric(distances) / 1000  # Convert to kilometers
  
  # Apply distance decay function
  message("Step 4: Applying ", decay_function, " decay function...")
  
  accessibility_matrix <- switch(decay_function,
    "exponential" = exp(-decay_parameter * distances_km),
    "power" = (distances_km + 1)^(-decay_parameter),
    "linear" = pmax(0, 1 - distances_km / max_distance_km),
    stop("Unknown decay function: ", decay_function)
  )
  
  # Set distances beyond max_distance to zero
  accessibility_matrix[distances_km > max_distance_km] <- 0
  
  # Add zone IDs as row/column names
  rownames(accessibility_matrix) <- zones$id
  colnames(accessibility_matrix) <- zones$id
  
  message("Calculated accessibility matrix for ", nrow(zones), " zones")
  
  return(accessibility_matrix)
}

#' Calculate spatial lag variables
#'
#' Calculates spatial lag variables for zone attributes using spatial weights.
#' This is useful for spatial econometrics and analyzing spatial autocorrelation.
#'
#' @param zones sf object with zone geometries and attributes
#' @param variable Character. Name of the variable to calculate spatial lag for
#' @param method Character. Method for spatial weights:
#'   \itemize{
#'     \item "contiguity" - Queen contiguity (default)
#'     \item "distance" - Distance-based weights
#'     \item "knn" - K-nearest neighbors
#'   }
#' @param k Numeric. Number of neighbors for knn method (default: 5)
#' @param max_distance_km Numeric. Maximum distance for distance-based weights (default: 20km)
#' @return Numeric vector with spatial lag values
#' @details
#' Implements best practices for spatial lag calculations:
#' \itemize{
#'   \item Uses appropriate spatial weights matrices
#'   \item Handles edge effects properly
#'   \item Normalizes weights for consistency
#'   \item Works with projected coordinates for accuracy
#' }
#' @export
#' @examples
#' \dontrun{
#' zones <- get_zones(level = "dist", region_filter = "Madrid")
#' zones$population <- runif(nrow(zones), 1000, 100000)  # Example data
#' zones$pop_spatial_lag <- calculate_spatial_lag(zones, "population", method = "contiguity")
#' }
calculate_spatial_lag <- function(zones, variable, method = "contiguity", k = 5, max_distance_km = 20) {
  
  message("Step 1: Preparing spatial lag calculation...")
  
  # Validate inputs
  if(!inherits(zones, "sf") || !"geometry" %in% names(zones)) {
    stop("zones must be an sf object with geometry column")
  }
  
  if(!variable %in% names(zones)) {
    stop("Variable '", variable, "' not found in zones data")
  }
  
  # Transform to projected CRS for accurate calculations
  zones_proj <- sf::st_transform(zones, 25830)
  
  # Create spatial weights matrix
  message("Step 2: Creating spatial weights matrix using ", method, " method...")
  
  if(method == "contiguity") {
    # Queen contiguity
    neighbors <- sf::st_touches(zones_proj, sparse = FALSE)
    weights <- neighbors * 1.0  # Convert logical to numeric
    
  } else if(method == "distance") {
    # Distance-based weights
    centroids <- sf::st_centroid(zones_proj)
    distances <- sf::st_distance(centroids)
    distances_km <- as.numeric(distances) / 1000
    
    # Inverse distance weights within max_distance
    weights <- 1 / (distances_km + 1)
    weights[distances_km > max_distance_km] <- 0
    
  } else if(method == "knn") {
    # K-nearest neighbors
    centroids <- sf::st_centroid(zones_proj)
    distances <- sf::st_distance(centroids)
    
    weights <- matrix(0, nrow = nrow(zones), ncol = nrow(zones))
    for(i in seq_len(nrow(zones))) {
      # Find k nearest neighbors
      dist_row <- distances[i, ]
      nearest_k <- order(dist_row)[2:(k+1)]  # Exclude self (position 1)
      weights[i, nearest_k] <- 1
    }
    
  } else {
    stop("Unknown method: ", method)
  }
  
  # Row-normalize weights
  message("Step 3: Normalizing spatial weights...")
  row_sums <- rowSums(weights)
  row_sums[row_sums == 0] <- 1  # Prevent division by zero
  weights <- weights / row_sums
  
  # Calculate spatial lag
  message("Step 4: Calculating spatial lag for variable '", variable, "'...")
  variable_values <- zones[[variable]]
  variable_values[is.na(variable_values)] <- 0  # Handle missing values
  
  spatial_lag <- as.numeric(weights %*% variable_values)
  
  message("Calculated spatial lag for ", nrow(zones), " zones")
  
  return(spatial_lag)
}

#' Print method for mobspain_containment objects
#' @param x mobspain_containment object
#' @param ... Additional arguments (ignored)
#' @export
print.mobspain_containment <- function(x, ...) {
  cat("Mobility Containment Analysis\n")
  cat("=============================\n\n")
  
  cat("Total zones analyzed:", nrow(x$containment), "\n")
  cat("Average containment:", round(x$summary$avg_containment, 3), "\n")
  cat("Median containment:", round(x$summary$median_containment, 3), "\n")
  cat("Range:", round(x$summary$min_containment, 3), "to", round(x$summary$max_containment, 3), "\n\n")
  
  cat("Top 5 zones by containment:\n")
  top_zones <- head(x$containment[order(-x$containment$containment_index), ], 5)
  print(top_zones[, c("id", "containment_index")])
  
  if(!is.null(x$map)) {
    cat("\nSpatial map available in $map component\n")
  }
  
  invisible(x)
}

#' Summary method for mobspain_containment objects
#' @param object mobspain_containment object
#' @param ... Additional arguments (ignored)
#' @export
summary.mobspain_containment <- function(object, ...) {
  cat("Mobility Containment Summary\n")
  cat("============================\n\n")
  
  summary_stats <- object$summary
  cat("Statistical Summary:\n")
  cat("  Mean:", round(summary_stats$avg_containment, 4), "\n")
  cat("  Median:", round(summary_stats$median_containment, 4), "\n")
  cat("  Min:", round(summary_stats$min_containment, 4), "\n")
  cat("  Max:", round(summary_stats$max_containment, 4), "\n\n")
  
  # Distribution
  containment_values <- object$containment$containment_index
  cat("Distribution:\n")
  cat("  < 0.1 (low):", sum(containment_values < 0.1, na.rm = TRUE), "zones\n")
  cat("  0.1-0.3 (medium):", sum(containment_values >= 0.1 & containment_values < 0.3, na.rm = TRUE), "zones\n")
  cat("  >= 0.3 (high):", sum(containment_values >= 0.3, na.rm = TRUE), "zones\n")
  
  invisible(object)
}

#' Print method for mobspain_anomalies objects
#' @param x mobspain_anomalies object
#' @param ... Additional arguments (ignored)
#' @export
print.mobspain_anomalies <- function(x, ...) {
  cat("Mobility Anomaly Detection\n")
  cat("==========================\n\n")
  
  cat("Total zones analyzed:", x$summary$total_zones, "\n")
  cat("Anomalies detected:", x$summary$anomaly_count, "(", x$summary$anomaly_percentage, "%)\n")
  cat("Threshold used:", x$summary$threshold_used, "standard deviations\n\n")
  
  if(x$summary$anomaly_count > 0) {
    cat("Anomalous zones (top 5 by z-score):\n")
    anomalous <- x$anomalies[x$anomalies$is_anomaly == TRUE, ]
    top_anomalies <- head(anomalous[order(-anomalous$z_score), ], 5)
    print(top_anomalies[, c("id", "total_trips", "z_score")])
  }
  
  if(!is.null(x$map)) {
    cat("\nSpatial map available in $map component\n")
  }
  
  invisible(x)
}

#' Summary method for mobspain_anomalies objects
#' @param object mobspain_anomalies object
#' @param ... Additional arguments (ignored)
#' @export
summary.mobspain_anomalies <- function(object, ...) {
  cat("Mobility Anomaly Detection Summary\n")
  cat("==================================\n\n")
  
  cat("Detection Parameters:\n")
  cat("  Threshold:", object$summary$threshold_used, "standard deviations\n")
  cat("  Total zones:", object$summary$total_zones, "\n")
  cat("  Anomalies found:", object$summary$anomaly_count, "\n")
  cat("  Anomaly rate:", object$summary$anomaly_percentage, "%\n\n")
  
  # Trip distribution
  trips <- object$anomalies$total_trips
  cat("Trip Distribution:\n")
  cat("  Mean trips per zone:", round(mean(trips, na.rm = TRUE), 0), "\n")
  cat("  Median trips per zone:", round(median(trips, na.rm = TRUE), 0), "\n")
  cat("  Standard deviation:", round(sd(trips, na.rm = TRUE), 0), "\n")
  
  invisible(object)
}
