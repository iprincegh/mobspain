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
      zones <- zones[zones$id %in% zones_to_keep, ]
      message("Filtered to ", nrow(zones), " zones matching region filter")
    }
  } else if(!is.null(zones_filter)) {
    message("Step 2: Filtering zones by zone IDs...")
    zones <- zones[zones$id %in% zones_filter, ]
    message("Filtered to ", nrow(zones), " zones matching zone filter")
  }
  
  # Transform to a standard CRS for mapping
  if(!sf::st_is_longlat(zones)) {
    next_step <- if(!is.null(region_filter) || !is.null(zones_filter)) "3" else "2"
    message("Step ", next_step, ": Transforming to WGS84 for mapping...")
    zones <- sf::st_transform(zones, 4326)
  }
  
  # Add some basic spatial statistics
  zones$area_km2 <- as.numeric(sf::st_area(zones)) / 1e6
  zones$centroid <- sf::st_centroid(zones$geometry)
  
  message("Step 3: Loaded ", nrow(zones), " spatial zones with geometries")
  
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
  
  # Create result object
  result <- list(
    containment = containment,
    summary = list(
      avg_containment = mean(containment$containment_index, na.rm = TRUE),
      median_containment = median(containment$containment_index, na.rm = TRUE),
      max_containment = max(containment$containment_index, na.rm = TRUE),
      min_containment = min(containment$containment_index, na.rm = TRUE)
    )
  )
  
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
#' mobility <- get_mobility_matrix(dates = "2023-01-01", level = "dist")
#' zones <- get_spatial_zones("dist")
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
  
  # Create result object
  result <- list(
    anomalies = zone_trips,
    summary = list(
      total_zones = nrow(zone_trips),
      anomaly_count = anomaly_count,
      anomaly_percentage = round(anomaly_count/nrow(zone_trips)*100, 1),
      threshold_used = threshold
    )
  )
  
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
#' mobility <- get_mobility_matrix(dates = "2023-01-01", level = "dist")
#' zones <- get_spatial_zones("dist")
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
#' @export
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
