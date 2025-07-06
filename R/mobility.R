#' Mobility Data Functions for Spatial Analysis
#'
#' Essential functions for accessing and analyzing Spanish mobility data
#' with automatic spatial mapping capabilities.

#' Get mobility matrix with filtering capabilities
#'
#' Downloads Spanish origin-destination mobility data from MITMA
#' with memory-efficient defaults and automatic data management.
#' Includes filtering options to download subsets of data for specific zones.
#'
#' @param dates Character vector of dates. Options:
#'   \itemize{
#'     \item Single date: "2023-01-01" 
#'     \item Date range: c("2023-01-01", "2023-01-07")
#'     \item Default: "2023-01-01"
#'   }
#' @param level Character. Spatial aggregation level:
#'   \itemize{
#'     \item "dist" - Districts (default, recommended for users)
#'     \item "muni" - Municipalities (detailed, larger dataset)
#'   }
#' @param max_rows Numeric. Maximum number of rows to return for memory management 
#'   (default: 10000 for memory-efficient processing). If dataset is larger, random sampling is applied.
#' @param zones_filter Character vector. Optional filter for specific zone IDs to download.
#'   If NULL (default), downloads all zones. Use this to reduce download size:
#'   \itemize{
#'     \item For districts: c("28079", "08019") for Madrid and Barcelona districts
#'     \item For municipalities: c("28079001", "08019001") for specific municipalities
#'   }
#' @param region_filter Character. Optional filter by region/province name or code.
#'   Examples: "Madrid", "Barcelona", "28" (Madrid province), "08" (Barcelona province).
#'   This automatically identifies relevant zone IDs for filtering.
#' @return Data frame with mobility data containing:
#'   \itemize{
#'     \item origin - Origin zone identifier
#'     \item dest - Destination zone identifier
#'     \item date - Date of trips
#'     \item n_trips - Number of trips between origin and destination
#'   }
#' @details
#' This function automatically:
#' \itemize{
#'   \item Downloads data from Spanish government MITMA database
#'   \item Filters out zero-trip records for efficiency
#'   \item Applies memory management by sampling large datasets
#'   \item Standardizes column names for consistency
#'   \item Provides analytical step-by-step messaging
#' }
#' 
#' Data represents aggregated mobility patterns based on mobile phone data,
#' providing insights into population movement between administrative zones.
#' @export
#' @examples
#' \dontrun{
#' # Basic usage - download all zones (default)
#' mobility <- get_mobility(dates = "2023-01-01", level = "dist")
#' 
#' # Filter by specific zones (reduces download size)
#' madrid_barcelona <- get_mobility(
#'   dates = "2023-01-01", 
#'   level = "dist",
#'   zones_filter = c("28079", "08019")  # Madrid and Barcelona districts
#' )
#' 
#' # Filter by region name (automatically finds relevant zones)
#' madrid_mobility <- get_mobility(
#'   dates = "2023-01-01",
#'   level = "dist", 
#'   region_filter = "Madrid"
#' )
#' 
#' # Filter by province code (smaller subset)
#' valencia_mobility <- get_mobility(
#'   dates = "2023-01-01",
#'   level = "dist",
#'   region_filter = "46"  # Valencia province code
#' )
#' 
#' # Date range with filtering
#' week_data <- get_mobility(
#'   dates = c("2023-01-01", "2023-01-07"), 
#'   level = "dist",
#'   region_filter = "Barcelona",
#'   max_rows = 5000
#' )
#' 
#' # Check the data structure
#' head(mobility)
#' summary(mobility)
#' }
get_mobility <- function(dates = "2023-01-01", level = "dist", max_rows = 10000, 
                        zones_filter = NULL, region_filter = NULL) {
  
  message("Step 1: Downloading mobility data for ", level, " level...")
  
  # Validate inputs
  if(!level %in% c("dist", "muni")) {
    stop("level must be 'dist' or 'muni'")
  }
  
  # Convert dates to proper format
  if(length(dates) == 1) {
    dates <- rep(dates, 2)
  }
  
  
  # Handle region filtering if specified
  zones_to_filter <- NULL
  if(!is.null(region_filter)) {
    message("Step 2: Identifying zones for region filter: ", region_filter)
    zones_to_filter <- get_region_zones(region_filter, level)
  } else if(!is.null(zones_filter)) {
    zones_to_filter <- zones_filter
  }
  
  # Get the data
  mobility_data <- tryCatch({
    spanishoddata::spod_get(
      dates = dates,
      zones = level
    )
  }, error = function(e) {
    stop("Failed to download mobility data: ", e$message)
  })
  
  # Apply zone filtering if specified
  if(!is.null(zones_to_filter)) {
    original_rows <- nrow(mobility_data)
    mobility_data <- mobility_data %>%
      dplyr::filter(.data$id_origin %in% zones_to_filter | .data$id_dest %in% zones_to_filter)
    message("Step 3: Filtered to ", nrow(mobility_data), " rows (from ", original_rows, ") based on zone filter")
  }
  
  # Memory management for users (after filtering)
  if(nrow(mobility_data) > max_rows) {
    message("Step 4: Large dataset detected (", nrow(mobility_data), " rows). Sampling ", max_rows, " rows.")
    mobility_data <- mobility_data[sample(nrow(mobility_data), max_rows), ]
  }
  
  # Clean and standardize column names
  mobility_data <- mobility_data %>%
    dplyr::select(
      origin = .data$id_origin,
      dest = .data$id_dest, 
      date = .data$date,
      n_trips = .data$n_trips
    ) %>%
    dplyr::filter(.data$n_trips > 0) # Remove zero trips
  
  final_step <- if(!is.null(zones_to_filter)) "5" else "3"
  message("Step ", final_step, ": Loaded ", nrow(mobility_data), " mobility records")
  
  return(mobility_data)
}

#' Calculate basic mobility indicators
#'
#' Calculates essential mobility indicators for spatial analysis
#' with automatic spatial mapping.
#'
#' @param mobility_data Mobility data from get_mobility_matrix()
#' @param spatial_zones Optional sf object with spatial zones for mapping
#' @return List with mobility indicators and optional spatial map
#' @export
#' @examples
#' \dontrun{
#' mobility <- get_mobility_matrix(dates = "2023-01-01")
#' zones <- get_spatial_zones("dist")
#' 
#' indicators <- calc_indicators(mobility, zones)
#' print(indicators$map)
#' }
calc_indicators <- function(mobility_data, spatial_zones = NULL) {
  
  message("Step 1: Calculating mobility indicators...")
  
  # Calculate indicators per zone
  indicators <- mobility_data %>%
    dplyr::group_by(.data$origin) %>%
    dplyr::summarise(
      total_trips = sum(.data$n_trips, na.rm = TRUE),
      avg_daily_trips = mean(.data$n_trips, na.rm = TRUE),
      unique_destinations = dplyr::n_distinct(.data$dest),
      # Calculate containment
      internal_trips = sum(.data$n_trips[.data$origin == .data$dest], na.rm = TRUE),
      containment_index = .data$internal_trips / .data$total_trips,
      .groups = "drop"
    ) %>%
    dplyr::rename(id = .data$origin)
  
  # Calculate connectivity (how connected each zone is)
  total_zones <- length(unique(c(mobility_data$origin, mobility_data$dest)))
  indicators$connectivity <- indicators$unique_destinations / total_zones
  
  # Create result
  result <- list(
    indicators = indicators,
    summary = list(
      total_zones = nrow(indicators),
      avg_trips_per_zone = mean(indicators$total_trips),
      avg_containment = mean(indicators$containment_index, na.rm = TRUE),
      avg_connectivity = mean(indicators$connectivity, na.rm = TRUE)
    )
  )
  
  # Create spatial map if zones provided
  if(!is.null(spatial_zones)) {
    message("Step 2: Creating spatial map of mobility indicators...")
    
    # Merge with spatial zones
    map_data <- spatial_zones %>%
      dplyr::left_join(indicators, by = "id") %>%
      dplyr::mutate(total_trips = ifelse(is.na(.data$total_trips), 0, .data$total_trips))
    
    # Create map
    map <- ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data$total_trips), color = "white", size = 0.1) +
      ggplot2::scale_fill_viridis_c(name = "Total Trips", trans = "log10") +
      ggplot2::labs(
        title = "Mobility Intensity by Zone",
        subtitle = "Total trips originating from each zone",
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

#' Create flow map of mobility patterns
#'
#' Creates a flow map showing the major mobility flows between zones
#' using sf geometries.
#'
#' @param mobility_data Mobility data 
#' @param spatial_zones sf object with spatial zones
#' @param top_flows Number of top flows to show (default: 20)
#' @return ggplot2 map with flow lines
#' @export
#' @examples
#' \dontrun{
#' mobility <- get_mobility_matrix(dates = "2023-01-01")
#' zones <- get_spatial_zones("dist")
#' 
#' flow_map <- create_flow_map(mobility, zones, top_flows = 20)
#' print(flow_map)
#' }
create_flows <- function(mobility_data, spatial_zones, top_flows = 20) {
  
  message("Step 1: Preparing flow data...")
  
  # Get top flows
  top_flows_data <- mobility_data %>%
    dplyr::filter(.data$origin != .data$dest) %>%  # Remove internal trips
    dplyr::group_by(.data$origin, .data$dest) %>%
    dplyr::summarise(total_trips = sum(.data$n_trips, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$total_trips)) %>%
    dplyr::slice_head(n = top_flows)
  
  # Get centroids of zones
  zone_centroids <- spatial_zones %>%
    dplyr::mutate(
      lon = sf::st_coordinates(sf::st_centroid(.data$geometry))[, 1],
      lat = sf::st_coordinates(sf::st_centroid(.data$geometry))[, 2]
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data$id, .data$lon, .data$lat)
  
  # Create flow lines
  flow_lines <- top_flows_data %>%
    dplyr::left_join(zone_centroids, by = c("origin" = "id")) %>%
    dplyr::left_join(zone_centroids, by = c("dest" = "id"), suffix = c("_origin", "_dest")) %>%
    dplyr::filter(!is.na(.data$lon_origin) & !is.na(.data$lon_dest))
  
  message("Step 2: Creating flow map...")
  
  # Create map
  flow_map <- ggplot2::ggplot() +
    # Base zones
    ggplot2::geom_sf(data = spatial_zones, fill = "lightgray", color = "white", size = 0.1) +
    # Flow lines
    ggplot2::geom_segment(
      data = flow_lines,
      ggplot2::aes(
        x = .data$lon_origin, y = .data$lat_origin,
        xend = .data$lon_dest, yend = .data$lat_dest,
        size = .data$total_trips,
        alpha = .data$total_trips
      ),
      color = "red"
    ) +
    ggplot2::scale_size_continuous(name = "Trips", range = c(0.1, 2)) +
    ggplot2::scale_alpha_continuous(name = "Trips", range = c(0.3, 0.8)) +
    ggplot2::labs(
      title = paste("Top", top_flows, "Mobility Flows"),
      subtitle = "Major origin-destination flows between zones",
      caption = "Source: MITMA Spanish Mobility Data"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )
  
  return(flow_map)
}

#' Quick spatial analysis for users
#'
#' Performs a complete spatial analysis workflow with all essential
#' functions and creates spatial maps.
#'
#' @param dates Date range for analysis
#' @param level Spatial level ("dist" or "muni")
#' @return List with all analysis results and maps
#' @export
#' @examples
#' \dontrun{
#' # Quick complete analysis
#' results <- quick_spatial_analysis(dates = "2023-01-01", level = "dist")
#' 
#' # View all maps
#' print(results$maps$indicators)
#' print(results$maps$containment)
#' print(results$maps$flows)
#' }
quick_analysis <- function(dates = "2023-01-01", level = "dist") {
  
  message("Starting quick spatial analysis workflow...")
  
  # Get data
  message("Loading mobility data and spatial zones...")
  mobility_data <- get_mobility(dates = dates, level = level)
  spatial_zones <- get_zones(level = level)
  
  # Run analyses
  results <- list()
  
  # Basic indicators
  results$indicators <- calc_indicators(mobility_data, spatial_zones)
  
  # Containment analysis
  results$containment <- calculate_containment(mobility_data, spatial_zones)
  
  # Flow map
  results$flows <- create_flows(mobility_data, spatial_zones, top_flows = 15)
  
  # Compile maps
  results$maps <- list(
    indicators = results$indicators$map,
    containment = results$containment$map,
    flows = results$flows
  )
  
  # Summary
  results$summary <- list(
    dates = dates,
    level = level,
    total_zones = nrow(spatial_zones),
    total_trips = sum(mobility_data$n_trips, na.rm = TRUE),
    avg_containment = results$containment$summary$avg_containment
  )
  
  message("Quick spatial analysis complete!")
  
  return(results)
}

#' Quick filtered mobility analysis for specific regions
#'
#' A convenience wrapper function that downloads both mobility data and spatial zones
#' for a specific region, making it easy for users to analyze specific areas.
#'
#' @param region Character. Region name or province code to analyze.
#'   Examples: "Madrid", "Barcelona", "46" (Valencia), "28" (Madrid province)
#' @param dates Character vector of dates (default: "2023-01-01")
#' @param level Character. Spatial level: "dist" or "muni" (default: "dist")
#' @param max_rows Numeric. Maximum mobility records to download (default: 10000)
#' @return List containing:
#'   \itemize{
#'     \item mobility - Filtered mobility data
#'     \item zones - Spatial zones for the region
#'     \item region - Region name used for filtering
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Quick analysis for Madrid
#' madrid_data <- get_region_mobility("Madrid")
#' head(madrid_data$mobility)
#' plot(madrid_data$zones$geometry)
#' 
#' # Barcelona province analysis
#' barcelona_data <- get_region_mobility("Barcelona", dates = c("2023-01-01", "2023-01-03"))
#' 
#' # Valencia province by code
#' valencia_data <- get_region_mobility("46", level = "dist")
#' }
get_region_mobility <- function(region, dates = "2023-01-01", level = "dist", max_rows = 10000) {
  
  message("Getting mobility data and zones for region: ", region)
  
  # Get spatial zones for the region
  zones <- get_zones(level = level, region_filter = region)
  
  if(nrow(zones) == 0) {
    stop("No zones found for region: ", region)
  }
  
  # Get mobility data filtered by the same region
  mobility <- get_mobility(
    dates = dates,
    level = level,
    region_filter = region,
    max_rows = max_rows
  )
  
  result <- list(
    mobility = mobility,
    zones = zones,
    region = region
  )
  
  message("Region analysis complete! ", nrow(zones), " zones, ", nrow(mobility), " mobility records")
  
  return(result)
}

#' Helper function to get zone IDs for region filtering
#'
#' @param region_filter Character. Region name or code to filter by
#' @param level Character. Spatial level ("dist" or "muni")
#' @return Character vector of zone IDs
#' @keywords internal
get_region_zones <- function(region_filter, level) {
  
  # Download zone boundaries to get zone IDs
  if(!requireNamespace("spanishoddata", quietly = TRUE)) {
    stop("spanishoddata package required for region filtering")
  }
  
  tryCatch({
    zones <- spanishoddata::spod_get_zones(level = level, year = 2023)
    
    # Try to match by different criteria
    if(nchar(region_filter) <= 2 && grepl("^[0-9]+$", region_filter)) {
      # Numeric code - match province code prefix
      if(level == "dist") {
        matching_zones <- zones[grepl(paste0("^", region_filter), zones$id), ]
      } else {
        matching_zones <- zones[grepl(paste0("^", region_filter), zones$id), ]
      }
    } else {
      # Text name - try to match province/region name
      if("name" %in% names(zones)) {
        matching_zones <- zones[grepl(region_filter, zones$name, ignore.case = TRUE), ]
      } else if("provincia" %in% names(zones)) {
        matching_zones <- zones[grepl(region_filter, zones$provincia, ignore.case = TRUE), ]
      } else {
        # Fallback - return all zones for now
        warning("Could not match region filter: ", region_filter, ". Using all zones.")
        return(zones$id)
      }
    }
    
    if(nrow(matching_zones) == 0) {
      warning("No zones found for region filter: ", region_filter, ". Using all zones.")
      return(zones$id)
    }
    
    message("Found ", nrow(matching_zones), " zones matching region filter: ", region_filter)
    return(matching_zones$id)
    
  }, error = function(e) {
    warning("Could not filter by region: ", e$message, ". Using all zones.")
    return(NULL)
  })
}
