#' Spatial Visualization Functions for Analysis
#'
#' This file contains spatial visualization functions optimized for spatial analysis
#' and spatial data science education, using sf package for spatial operations.

#' Create spatial map of mobility analysis results
#'
#' A comprehensive function that creates spatial maps for any mobility analysis
#' result, automatically detecting the type of analysis and creating appropriate
#' spatial visualizations using sf geometries.
#'
#' @param analysis_result Result from any mobspain analysis function
#' @param spatial_zones Spatial zones sf object with geometries
#' @param map_type Type of map to create ("choropleth", "flow", "points", "auto")
#' @param color_palette Color palette for the map
#' @param title Map title
#' @return ggplot2 map object
#' @export
#' @examples
#' \dontrun{
#' # Create spatial map for containment analysis
#' containment <- calculate_containment_spatial(mobility_data)
#' map <- create_spatial_map(containment, zones, map_type = "choropleth")
#' 
#' # Create spatial map for anomaly detection
#' anomalies <- detect_simple_anomalies(mobility_data)
#' map <- create_spatial_map(anomalies, zones, map_type = "auto")
#' }
create_spatial_map <- function(analysis_result, spatial_zones, 
                                  map_type = c("auto", "choropleth", "flow", "points"),
                                  color_palette = "viridis",
                                  title = NULL) {
  
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required. Install with: install.packages('ggplot2')")
  }
  
  map_type <- match.arg(map_type)
  
  message("Step 1: Preparing spatial data for mapping...")
  
  # Auto-detect map type based on analysis result
  if(map_type == "auto") {
    map_type <- detect_map_type(analysis_result)
    message("Auto-detected map type: ", map_type)
  }
  
  # Ensure spatial zones are in appropriate CRS
  if(!sf::st_is_longlat(spatial_zones)) {
    message("Step 2: Transforming to WGS84 for mapping...")
    spatial_zones <- sf::st_transform(spatial_zones, 4326)
  }
  
  # Create the appropriate map
  message("Step 3: Creating ", map_type, " map...")
  
  map <- switch(map_type,
    "choropleth" = create_choropleth_map_sf(analysis_result, spatial_zones, color_palette, title),
    "flow" = create_flow_map_sf(analysis_result, spatial_zones, title),
    "points" = create_point_map_sf(analysis_result, spatial_zones, color_palette, title),
    "choropleth" # default fallback
  )
  
  message("Step 4: Spatial map created successfully!")
  
  return(map)
}

#' Create choropleth map using sf
#' @param analysis_result Analysis result data
#' @param spatial_zones Spatial zones sf object
#' @param color_palette Color palette
#' @param title Map title
#' @return ggplot2 choropleth map
#' @keywords internal
create_choropleth_map_sf <- function(analysis_result, spatial_zones, color_palette, title) {
  
  # Join analysis results with spatial zones
  if("id_origin" %in% names(analysis_result)) {
    # For origin-based analysis (like containment)
    map_data <- merge(spatial_zones, analysis_result, 
                     by.x = "id", by.y = "id_origin", all.x = TRUE)
    value_col <- detect_value_column(analysis_result)
  } else if("id" %in% names(analysis_result)) {
    # For zone-based analysis
    map_data <- merge(spatial_zones, analysis_result, 
                     by = "id", all.x = TRUE)
    value_col <- detect_value_column(analysis_result)
  } else {
    stop("Cannot determine how to join analysis results with spatial zones")
  }
  
  # Create the choropleth map
  map <- ggplot2::ggplot(map_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[value_col]]), 
                    color = "white", size = 0.1) +
    ggplot2::scale_fill_viridis_c(name = value_col, na.value = "grey90") +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      legend.title = ggplot2::element_text(size = 10)
    )
  
  if(!is.null(title)) {
    map <- map + ggplot2::ggtitle(title)
  }
  
  return(map)
}

#' Create flow map using sf
#' @param analysis_result Analysis result data
#' @param spatial_zones Spatial zones sf object
#' @param title Map title
#' @return ggplot2 flow map
#' @keywords internal
create_flow_map_sf <- function(analysis_result, spatial_zones, title) {
  
  # Calculate zone centroids for flow lines
  centroids <- spatial_zones %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.data.frame() %>%
    dplyr::mutate(id = spatial_zones$id) %>%
    dplyr::select(.data$id, lon = .data$X, lat = .data$Y)
  
  # Join with analysis results to get flow data
  if("id_origin" %in% names(analysis_result) && "id_destination" %in% names(analysis_result)) {
    
    # Add origin coordinates
    flow_data <- analysis_result %>%
      dplyr::left_join(centroids, by = c("id_origin" = "id")) %>%
      dplyr::rename(lon_origin = .data$lon, lat_origin = .data$lat) %>%
      # Add destination coordinates
      dplyr::left_join(centroids, by = c("id_destination" = "id")) %>%
      dplyr::rename(lon_dest = .data$lon, lat_dest = .data$lat) %>%
      dplyr::filter(!is.na(.data$lon_origin) & !is.na(.data$lat_origin) & 
                   !is.na(.data$lon_dest) & !is.na(.data$lat_dest))
    
    # Filter for significant flows (top 10% by default)
    value_col <- detect_value_column(analysis_result)
    threshold <- quantile(flow_data[[value_col]], 0.9, na.rm = TRUE)
    flow_data <- flow_data[flow_data[[value_col]] >= threshold, ]
    
    # Create flow map
    map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = spatial_zones, fill = "grey95", color = "white", size = 0.1) +
      ggplot2::geom_segment(data = flow_data,
                           ggplot2::aes(x = .data$lon_origin, y = .data$lat_origin,
                                       xend = .data$lon_dest, yend = .data$lat_dest,
                                       alpha = .data[[value_col]]),
                           color = "red", size = 0.5) +
      ggplot2::scale_alpha_continuous(name = value_col, range = c(0.3, 1)) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14)
      )
    
  } else {
    # Fallback to choropleth if no flow data
    return(create_choropleth_map_sf(analysis_result, spatial_zones, "viridis", title))
  }
  
  if(!is.null(title)) {
    map <- map + ggplot2::ggtitle(title)
  }
  
  return(map)
}

#' Create point map using sf
#' @param analysis_result Analysis result data
#' @param spatial_zones Spatial zones sf object
#' @param color_palette Color palette
#' @param title Map title
#' @return ggplot2 point map
#' @keywords internal
create_point_map_sf <- function(analysis_result, spatial_zones, color_palette, title) {
  
  # Calculate zone centroids for points
  centroids <- spatial_zones %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.data.frame() %>%
    dplyr::mutate(id = spatial_zones$id) %>%
    dplyr::select(.data$id, lon = .data$X, lat = .data$Y)
  
  # Join with analysis results
  if("id_origin" %in% names(analysis_result)) {
    point_data <- analysis_result %>%
      dplyr::left_join(centroids, by = c("id_origin" = "id"))
  } else if("id" %in% names(analysis_result)) {
    point_data <- analysis_result %>%
      dplyr::left_join(centroids, by = "id")
  } else {
    stop("Cannot determine how to join analysis results with spatial zones")
  }
  
  value_col <- detect_value_column(analysis_result)
  
  # Create point map
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = spatial_zones, fill = "grey95", color = "white", size = 0.1) +
    ggplot2::geom_point(data = point_data,
                       ggplot2::aes(x = .data$lon, y = .data$lat, 
                                   color = .data[[value_col]],
                                   size = .data[[value_col]]),
                       alpha = 0.7) +
    ggplot2::scale_color_viridis_c(name = value_col, na.value = "grey50") +
    ggplot2::scale_size_continuous(name = value_col, range = c(0.5, 3)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14)
    )
  
  if(!is.null(title)) {
    map <- map + ggplot2::ggtitle(title)
  }
  
  return(map)
}

#' Detect appropriate map type based on analysis result
#' @param analysis_result Analysis result data
#' @return Character string indicating map type
#' @keywords internal
detect_map_type <- function(analysis_result) {
  
  # Check if it's flow data (has origin and destination)
  if("id_origin" %in% names(analysis_result) && "id_destination" %in% names(analysis_result)) {
    return("flow")
  }
  
  # Check if it's point data (has location info but not aggregated)
  if("is_anomaly" %in% names(analysis_result) || "z_score" %in% names(analysis_result)) {
    return("points")
  }
  
  # Default to choropleth for aggregated zone data
  return("choropleth")
}

#' Detect the main value column for mapping
#' @param analysis_result Analysis result data
#' @return Character string indicating value column
#' @keywords internal
detect_value_column <- function(analysis_result) {
  
  # Priority order for value columns
  priority_cols <- c("containment", "n_trips", "predicted_trips", "total_trips", 
                    "z_score", "anomaly_score", "value", "count")
  
  available_cols <- names(analysis_result)
  
  # Find the first priority column that exists
  for(col in priority_cols) {
    if(col %in% available_cols) {
      return(col)
    }
  }
  
  # Fallback to first numeric column
  numeric_cols <- names(analysis_result)[sapply(analysis_result, is.numeric)]
  if(length(numeric_cols) > 0) {
    return(numeric_cols[1])
  }
  
  # Ultimate fallback
  return("value")
}

#' Create comprehensive spatial analysis dashboard
#'
#' Creates a multi-panel spatial dashboard showing different aspects of mobility
#' analysis results on maps using sf geometries.
#'
#' @param mobility_data Mobility data
#' @param spatial_zones Spatial zones sf object
#' @param analyses List of analysis types to include
#' @return List of ggplot2 maps
#' @export
#' @examples
#' \dontrun{
#' # Create comprehensive spatial dashboard
#' dashboard <- create_spatial_dashboard(
#'   mobility_data = mobility,
#'   spatial_zones = zones,
#'   analyses = c("containment", "anomalies", "predictions")
#' )
#' }
create_dashboard <- function(mobility_data, spatial_zones, 
                                   analyses = c("containment", "anomalies", "predictions")) {
  
  message("Creating comprehensive spatial dashboard...")
  
  dashboard <- list()
  
  if("containment" %in% analyses) {
    message("- Creating containment analysis map...")
    containment <- calculate_containment(mobility_data)
    dashboard$containment <- create_spatial_map(
      containment, spatial_zones, 
      map_type = "choropleth",
      title = "Mobility Containment by Zone"
    )
  }
  
  if("anomalies" %in% analyses) {
    message("- Creating anomaly detection map...")
    anomalies <- detect_outliers(mobility_data)
    # Aggregate anomalies by zone for mapping
    anomaly_summary <- anomalies %>%
      dplyr::group_by(id_origin = ifelse("id_origin" %in% names(.), id_origin, 
                                        rep("unknown", nrow(.)))) %>%
      dplyr::summarise(
        anomaly_count = sum(is_anomaly, na.rm = TRUE),
        anomaly_rate = mean(is_anomaly, na.rm = TRUE),
        .groups = "drop"
      )
    
    dashboard$anomalies <- create_spatial_map(
      anomaly_summary, spatial_zones, 
      map_type = "choropleth",
      title = "Anomaly Detection by Zone"
    )
  }
  
  if("predictions" %in% analyses) {
    message("- Creating prediction map...")
    predictions <- predict_patterns(
      mobility_data, 
      prediction_dates = Sys.Date() + 1,
      max_rows = 2000
    )
    
    # Aggregate predictions by zone
    pred_summary <- predictions %>%
      dplyr::group_by(.data$hour) %>%
      dplyr::summarise(
        avg_predicted_trips = mean(.data$predicted_trips, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(id = paste0("pred_", .data$hour))
    
    dashboard$predictions <- create_spatial_map(
      pred_summary, spatial_zones, 
      map_type = "choropleth",
      title = "Predicted Mobility Patterns"
    )
  }
  
  message("Spatial dashboard created successfully!")
  
  return(dashboard)
}

#' Simple spatial statistics for users
#'
#' Calculate basic spatial statistics that can be easily mapped and understood
#' by users.
#'
#' @param mobility_data Mobility data
#' @param spatial_zones Spatial zones sf object
#' @return Data frame with spatial statistics
#' @export
#' @examples
#' \dontrun{
#' # Calculate spatial statistics
#' spatial_stats <- calc_stats(mobility_data, zones)
#' 
#' # Create map of spatial statistics
#' map <- create_spatial_map(spatial_stats, zones, map_type = "choropleth")
#' }
calc_stats <- function(mobility_data, spatial_zones) {
  # Call the main spatial statistics function
  return(calculate_stats(mobility_data, spatial_zones))
}

#' Complete spatial mobility analysis for users
#'
#' A comprehensive function that performs multiple spatial analyses and creates
#' maps for each one. Perfect for users learning spatial
#' data science with mobility data.
#'
#' @param mobility_data Mobility data to analyze
#' @param spatial_zones Spatial zones sf object with geometries
#' @param analyses Vector of analyses to perform: "containment", "anomalies", "predictions", "spatial_stats"
#' @param save_maps Whether to save maps as individual files (default: FALSE)
#' @param output_dir Directory to save maps if save_maps = TRUE
#' @return List with analysis results and spatial maps
#' @export
#' @examples
#' \dontrun{
#' # Complete spatial analysis workflow
#' results <- analyze_mobility_spatial(
#'   mobility_data = mobility,
#'   spatial_zones = zones,
#'   analyses = c("containment", "anomalies", "predictions", "spatial_stats")
#' )
#' 
#' # View maps
#' print(results$maps$containment)
#' print(results$maps$anomalies)
#' print(results$maps$predictions)
#' print(results$maps$spatial_stats)
#' 
#' # Access analysis data
#' head(results$data$containment)
#' head(results$data$anomalies)
#' }
analyze_spatial <- function(mobility_data, spatial_zones, 
                                    analyses = c("containment", "anomalies", "predictions", "spatial_stats"),
                                    save_maps = FALSE,
                                    output_dir = "mobility_maps") {
  
  message("Starting comprehensive spatial mobility analysis...")
  message("Analyses requested: ", paste(analyses, collapse = ", "))
  
  results <- list(
    data = list(),
    maps = list(),
    summary = list()
  )
  
  # Create output directory if saving maps
  if(save_maps && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  # 1. Containment Analysis
  if("containment" %in% analyses) {
    message("\n1. Performing containment analysis...")
    containment <- calculate_containment(mobility_data)
    results$data$containment <- containment
    
    # Create map
    containment_map <- create_spatial_map(
      containment, spatial_zones, 
      map_type = "choropleth",
      title = "Mobility Containment by Zone"
    )
    results$maps$containment <- containment_map
    
    if(save_maps) {
      ggplot2::ggsave(file.path(output_dir, "containment_map.png"), 
                     containment_map, width = 10, height = 8)
    }
    
    message("   - Containment analysis complete")
  }
  
  # 2. Anomaly Detection
  if("anomalies" %in% analyses) {
    message("\n2. Performing anomaly detection...")
    anomalies <- detect_outliers(
      mobility_data, 
      threshold = 2.5
    )
    
    # Handle both list and data frame returns
    if(is.list(anomalies)) {
      results$data$anomalies <- anomalies$anomalies
    } else {
      results$data$anomalies <- anomalies
    }
    
    # Aggregate anomalies for mapping
    if("id_origin" %in% names(results$data$anomalies)) {
      anomaly_summary <- results$data$anomalies %>%
        dplyr::group_by(.data$id_origin) %>%
        dplyr::summarise(
          anomaly_count = sum(.data$is_anomaly, na.rm = TRUE),
          anomaly_rate = mean(.data$is_anomaly, na.rm = TRUE),
          avg_z_score = mean(.data$z_score, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::rename(id = .data$id_origin)
      
      anomaly_map <- create_spatial_map(
        anomaly_summary, spatial_zones, 
        map_type = "choropleth",
        title = "Anomaly Detection by Zone"
      )
      results$maps$anomalies <- anomaly_map
      
      if(save_maps) {
        ggplot2::ggsave(file.path(output_dir, "anomalies_map.png"), 
                       anomaly_map, width = 10, height = 8)
      }
    }
    
    message("   - Anomaly detection complete")
  }
  
  # 3. Predictions
  if("predictions" %in% analyses) {
    message("\n3. Performing mobility predictions...")
    predictions <- predict_patterns(
      mobility_data, 
      prediction_dates = Sys.Date() + 1,
      model_type = "linear_regression",
      max_rows = 2000
    )
    
    # Handle both list and data frame returns
    if(is.list(predictions)) {
      results$data$predictions <- predictions$predictions
    } else {
      results$data$predictions <- predictions
    }
    
    # Create simple prediction map (by hour)
    pred_summary <- results$data$predictions %>%
      dplyr::group_by(.data$hour) %>%
      dplyr::summarise(
        avg_predicted_trips = mean(.data$predicted_trips, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::slice_head(n = min(nrow(spatial_zones), nrow(.))) %>%
      dplyr::mutate(id = spatial_zones$id[seq_len(nrow(.))])
    
    if(nrow(pred_summary) > 0) {
      prediction_map <- create_spatial_map(
        pred_summary, spatial_zones, 
        map_type = "choropleth",
        title = "Predicted Mobility Patterns"
      )
      results$maps$predictions <- prediction_map
      
      if(save_maps) {
        ggplot2::ggsave(file.path(output_dir, "predictions_map.png"), 
                       prediction_map, width = 10, height = 8)
      }
    }
    
    message("   - Mobility predictions complete")
  }
  
  # 4. Spatial Statistics
  if("spatial_stats" %in% analyses) {
    message("\n4. Calculating spatial statistics...")
    spatial_stats <- calc_stats(mobility_data, spatial_zones)
    results$data$spatial_stats <- spatial_stats
    
    # Create map
    spatial_map <- create_spatial_map(
      spatial_stats, spatial_zones, 
      map_type = "choropleth",
      title = "Mobility Density by Zone (trips/km^2)"
    )
    results$maps$spatial_stats <- spatial_map
    
    if(save_maps) {
      ggplot2::ggsave(file.path(output_dir, "spatial_stats_map.png"), 
                     spatial_map, width = 10, height = 8)
    }
    
    message("   - Spatial statistics complete")
  }
  
  # Create summary
  results$summary$total_analyses <- length(analyses)
  results$summary$total_zones <- nrow(spatial_zones)
  results$summary$total_mobility_records <- nrow(mobility_data)
  results$summary$analysis_date <- Sys.time()
  
  if(save_maps) {
    message("\nMaps saved to directory: ", output_dir)
  }
  
  message("\nSpatial mobility analysis complete!")
  message("Generated ", length(results$maps), " spatial maps")
  
  return(results)
}
