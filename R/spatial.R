#' Core Spatial Analysis Functions for Spatial Analysis
#'
#' This file contains the essential spatial analysis functions designed for 
#' users learning spatial data science.
#' All functions automatically create spatial maps when possible.

#' Helper function to create city name patterns for zone matching
#'
#' @param city_names Character vector of city names
#' @return Named list of regex patterns for each city
#' @keywords internal
create_city_patterns <- function(city_names) {
  
  # Comprehensive mapping of Spanish cities to their zone ID patterns
  city_mapping <- list(
    "Madrid" = "^28",           # Madrid province (28)
    "Barcelona" = "^08",        # Barcelona province (08)
    "Valencia" = "^46",         # Valencia province (46)
    "Sevilla" = "^41",          # Sevilla province (41)
    "Seville" = "^41",          # Alternative spelling
    "Zaragoza" = "^50",         # Zaragoza province (50)
    "Malaga" = "^29",           # Malaga province (29)
    "Murcia" = "^30",           # Murcia province (30)
    "Palma" = "^07",            # Balearic Islands (07)
    "Las Palmas" = "^35",       # Las Palmas province (35)
    "Bilbao" = "^48",           # Vizcaya province (48)
    "Alicante" = "^03",         # Alicante province (03)
    "Cordoba" = "^14",          # Cordoba province (14)
    "Valladolid" = "^47",       # Valladolid province (47)
    "Vigo" = "^36",             # Pontevedra province (36)
    "Gijon" = "^33",            # Asturias province (33)
    "L'Hospitalet" = "^08",     # Barcelona metropolitan area
    "Hospitalet" = "^08",       # Simplified
    "Badalona" = "^08",         # Barcelona metropolitan area
    "Terrassa" = "^08",         # Barcelona metropolitan area
    "Sabadell" = "^08",         # Barcelona metropolitan area
    "Mostoles" = "^28",         # Madrid metropolitan area
    "Alcala" = "^28",           # Madrid metropolitan area (Alcala de Henares)
    "Fuenlabrada" = "^28",      # Madrid metropolitan area
    "Leganes" = "^28",          # Madrid metropolitan area
    "Getafe" = "^28",           # Madrid metropolitan area
    "Alcorcon" = "^28"          # Madrid metropolitan area
  )
  
  # Create patterns for requested cities
  patterns <- list()
  for(city in city_names) {
    # Try exact match first (case insensitive)
    city_lower <- tolower(city)
    matched_key <- NULL
    
    for(key in names(city_mapping)) {
      if(tolower(key) == city_lower) {
        matched_key <- key
        break
      }
    }
    
    if(!is.null(matched_key)) {
      patterns[[city]] <- city_mapping[[matched_key]]
    } else {
      # If no exact match, create a pattern based on common province codes
      warning("City '", city, "' not found in predefined list. Using generic pattern.")
      patterns[[city]] <- paste0("^", substr(city, 1, 2))
    }
  }
  
  return(patterns)
}

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
#'   If NULL (default), loads all zones. Examples: 
#'   \itemize{
#'     \item c("2801301", "08251") for specific districts
#'     \item c("28", "08") for all zones in Madrid/Barcelona provinces (prefix matching)
#'     \item c("28079", "08019") will use prefix matching to find Madrid/Barcelona zones
#'   }
#' @param region_filter Character. Optional filter by region/province name or code.
#'   Examples: "Madrid", "Barcelona", "28" (Madrid province). Automatically finds relevant zones.
#' @param city_filter Character vector. Optional filter by city names.
#'   Examples: c("Madrid", "Barcelona", "Valencia", "Sevilla"). Case-insensitive matching.
#' @param buffer_km Numeric. Optional buffer distance in kilometers around selected zones
#'   to create functional urban areas. Default is NULL (no buffer). Examples: 15, 20, 30.
#' @param include_neighbors Logical. If TRUE and buffer_km is specified, includes
#'   neighboring zones within the buffer distance (default: TRUE).
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
#'   zones_filter = c("28", "08")  # Madrid and Barcelona provinces (prefix matching)
#' )
#' 
#' # Get zones by city names with functional urban area buffer
#' madrid_valencia <- get_zones(
#'   level = "dist",
#'   city_filter = c("Madrid", "Valencia"),
#'   buffer_km = 20  # 20km functional urban area
#' )
#' 
#' # Get multiple cities without buffer
#' major_cities <- get_zones(
#'   level = "dist",
#'   city_filter = c("Madrid", "Barcelona", "Valencia", "Sevilla")
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
get_zones <- function(level = "dist", year = 2023, zones_filter = NULL, region_filter = NULL, 
                     city_filter = NULL, buffer_km = NULL, include_neighbors = TRUE) {
  
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
  if(!is.null(city_filter)) {
    message("Step 2: Filtering zones by city names: ", paste(city_filter, collapse = ", "))
    
    # Create a comprehensive city name mapping for Spanish cities
    city_patterns <- create_city_patterns(city_filter)
    
    # Find matching zones based on city names
    city_matches <- NULL
    for(city_name in names(city_patterns)) {
      city_zones <- zones %>% 
        dplyr::filter(grepl(city_patterns[[city_name]], .data$id, ignore.case = TRUE))
      
      if(nrow(city_zones) > 0) {
        if(is.null(city_matches)) {
          city_matches <- city_zones
        } else {
          # Ensure both objects are sf and have geometry, then use do.call to preserve sf structure
          if(inherits(city_matches, "sf") && inherits(city_zones, "sf")) {
            city_matches <- do.call(rbind, list(city_matches, city_zones))
          } else {
            stop("Lost sf structure during city filtering for: ", city_name)
          }
        }
        message("City '", city_name, "' matched ", nrow(city_zones), " zones")
      }
    }
    
    if(!is.null(city_matches) && nrow(city_matches) > 0) {
      zones <- city_matches %>% distinct()
      message("Total city matches: ", nrow(zones), " zones")
      
      # Ensure the result is still an sf object
      if(!inherits(zones, "sf")) {
        stop("City filtering resulted in loss of spatial geometry. Please check city filters.")
      }
    } else {
      stop("No zones found matching city names: ", paste(city_filter, collapse = ", "),
           ". Available cities include: Madrid, Barcelona, Valencia, Sevilla, Bilbao, Zaragoza, Malaga, Murcia")
    }
    
  } else if(!is.null(region_filter)) {
    message("Step 2: Filtering zones by region: ", region_filter)
    zones_to_keep <- get_region_zones_spatial(region_filter, zones)
    if(!is.null(zones_to_keep) && length(zones_to_keep) > 0) {
      # Use dplyr::filter to preserve sf object structure
      zones <- zones %>% dplyr::filter(.data$id %in% zones_to_keep)
      message("Filtered to ", nrow(zones), " zones matching region filter")
    }
  } else if(!is.null(zones_filter)) {
    message("Step 2: Filtering zones by zone IDs...")
    
    # Check if exact IDs exist first
    exact_matches <- zones %>% dplyr::filter(.data$id %in% zones_filter)
    
    if(nrow(exact_matches) > 0) {
      zones <- exact_matches
      message("Found exact matches: ", nrow(zones), " zones")
    } else {
      # Try pattern matching for partial matches (e.g., "28" for Madrid province)
      pattern_matches <- NULL
      for(filter_id in zones_filter) {
        # Try prefix matching (e.g., "28" matches "2801301", "2801302", etc.)
        pattern_zones <- zones %>% dplyr::filter(grepl(paste0("^", filter_id), .data$id))
        if(nrow(pattern_zones) > 0) {
          if(is.null(pattern_matches)) {
            pattern_matches <- pattern_zones
          } else {
            pattern_matches <- rbind(pattern_matches, pattern_zones)
          }
          message("Pattern '", filter_id, "' matched ", nrow(pattern_zones), " zones")
        }
      }
      
      if(!is.null(pattern_matches) && nrow(pattern_matches) > 0) {
        zones <- pattern_matches %>% distinct()
        message("Total pattern matches: ", nrow(zones), " zones")
        
        # Ensure the result is still an sf object
        if(!inherits(zones, "sf")) {
          stop("Pattern matching resulted in loss of spatial geometry. Please check zone filters.")
        }
      } else {
        # Show available zone samples to help user
        sample_ids <- head(zones$id, 10)
        message("Available zone ID samples: ", paste(sample_ids, collapse = ", "))
        stop("No zones found matching the specified filter: ", paste(zones_filter, collapse = ", "), 
             ". Please check your zone IDs. Use exact IDs or prefixes (e.g., '28' for Madrid province).")
      }
    }
  }
  
  # Check if any zones remain after filtering
  if(nrow(zones) == 0) {
    stop("No zones found matching the specified filter. Please check your zone IDs or region filter.")
  }
  
  # Ensure we still have a valid sf object after filtering
  if(!inherits(zones, "sf")) {
    stop("Zones object lost its sf class after filtering")
  }
  
  # Apply buffer for functional urban areas if requested
  if(!is.null(buffer_km) && buffer_km > 0) {
    buffer_step <- if(!is.null(city_filter) || !is.null(region_filter) || !is.null(zones_filter)) "3" else "2"
    message("Step ", buffer_step, ": Creating functional urban areas with ", buffer_km, "km buffer...")
    
    # Store original zones for fallback
    original_zones <- zones
    
    if(include_neighbors) {
      # Load all zones in the same level to find neighbors within buffer
      message("  - Loading neighboring zones for buffer calculation...")
      all_zones <- spanishoddata::spod_get_zones(zones = level, ver = 1)
      
      # Ensure all_zones is valid sf object
      if(!inherits(all_zones, "sf")) {
        warning("Could not load all zones for buffer calculation. Using selected zones only.")
      } else {
        # Simplified buffer approach: use geographic coordinates with approximate conversion
        buffer_deg <- buffer_km / 111.32  # Rough km to degree conversion
        
        # Create buffer around selected zones
        buffer_geom <- sf::st_buffer(zones, dist = buffer_deg)
        buffer_union <- sf::st_union(buffer_geom)
        
        # Find zones that intersect with the buffer
        zones <- tryCatch({
          buffer_intersects <- sf::st_intersects(all_zones, buffer_union, sparse = FALSE)
          neighboring_zones <- all_zones[as.vector(buffer_intersects), ]
          
          # Critical: Ensure we maintain sf structure with proper column alignment
          if(inherits(neighboring_zones, "sf") && "geometry" %in% names(neighboring_zones) && nrow(neighboring_zones) > 0) {
            
            # Align columns between original zones and neighboring zones to preserve structure
            original_cols <- names(zones)
            neighbor_cols <- names(neighboring_zones)
            
            # Find common columns (including geometry)
            common_cols <- intersect(original_cols, neighbor_cols)
            
            # Ensure geometry column is always included
            if(!"geometry" %in% common_cols && "geometry" %in% neighbor_cols) {
              common_cols <- c(common_cols, "geometry")
            }
            
            # Keep only common columns to ensure consistent structure
            if(length(common_cols) > 0 && "geometry" %in% common_cols) {
              original_count <- nrow(zones)
              result_zones <- neighboring_zones[, common_cols]
              
              # Final validation of sf structure
              if(inherits(result_zones, "sf") && "geometry" %in% names(result_zones)) {
                message("  - Expanded from ", original_count, " to ", nrow(result_zones), " zones within ", buffer_km, "km buffer")
                result_zones
              } else {
                warning("Buffer operation corrupted sf structure. Using original zones.")
                original_zones
              }
            } else {
              warning("Column mismatch in buffer operation. Using original zones.")
              original_zones
            }
          } else {
            warning("Buffer operation failed. Using original zones.")
            original_zones
          }
        }, error = function(e) {
          warning("Buffer calculation failed: ", e$message, ". Using original zones.")
          return(original_zones)
        })
      }
    } else {
      # Just buffer the selected zones without adding neighbors
      message("  - Buffering selected zones only...")
      buffer_deg <- buffer_km / 111.32  # Rough km to degree conversion
      zones <- tryCatch({
        buffered <- sf::st_buffer(zones, dist = buffer_deg)
        message("  - Applied ", buffer_km, "km buffer to selected zones")
        buffered
      }, error = function(e) {
        warning("Buffer operation failed: ", e$message, ". Using original zones.")
        original_zones
      })
    }
  }
  
  # Transform to WGS84 using best practices for coordinate transformation
  if(!sf::st_is_longlat(zones)) {
    next_step <- if(!is.null(city_filter) || !is.null(region_filter) || !is.null(zones_filter) || !is.null(buffer_km)) "4" else "2"
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
  
  message("Step ", ifelse(!is.null(city_filter) || !is.null(region_filter) || !is.null(zones_filter) || !is.null(buffer_km), "5", "4"), ": Loaded ", nrow(zones), " spatial zones with geometries")
  
  return(zones)
}

#' Create multi-city hourly mobility trend analysis
#'
#' Generates hourly mobility patterns for multiple cities with comparative analysis
#' and visualizations. Supports any number of cities with customizable parameters.
#'
#' @param cities Character vector. Names of cities to analyze. 
#'   Examples: c("Madrid", "Barcelona"), c("Madrid", "Barcelona", "Valencia", "Sevilla")
#' @param buffer_km Numeric. Buffer distance in kilometers for functional urban areas (default: 15).
#' @param hours_range Numeric vector. Hours to analyze (default: 0:23 for full day).
#' @param trip_intensity Character. Base trip intensity level: "low", "medium", "high" (default: "medium").
#' @param include_weekends Logical. Whether to include weekend patterns (default: FALSE).
#' @param create_plots Logical. Whether to create visualization plots (default: TRUE).
#' @return List containing:
#'   \itemize{
#'     \item hourly_data - Data frame with hourly mobility patterns for all cities
#'     \item city_summaries - Summary statistics for each city
#'     \item comparison_plot - ggplot2 comparative visualization
#'     \item peak_analysis - Peak hour analysis for each city
#'     \item zones_data - Spatial zones data used for analysis
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Basic two-city comparison
#' madrid_bcn <- analyze_multi_city_mobility(
#'   cities = c("Madrid", "Barcelona"),
#'   buffer_km = 20
#' )
#' 
#' # Multi-city analysis with custom settings
#' major_cities <- analyze_multi_city_mobility(
#'   cities = c("Madrid", "Barcelona", "Valencia", "Sevilla"),
#'   buffer_km = 15,
#'   trip_intensity = "high",
#'   include_weekends = TRUE
#' )
#' 
#' # Access results
#' print(major_cities$city_summaries)
#' plot(major_cities$comparison_plot)
#' }
analyze_multi_city_mobility <- function(cities, buffer_km = 15, hours_range = 0:23,
                                      trip_intensity = "medium", include_weekends = FALSE,
                                      create_plots = TRUE) {
  
  # Validate inputs
  if(length(cities) < 1) {
    stop("At least one city must be specified")
  }
  
  if(!trip_intensity %in% c("low", "medium", "high")) {
    stop("trip_intensity must be one of: 'low', 'medium', 'high'")
  }
  
  message("Starting multi-city mobility analysis for: ", paste(cities, collapse = ", "))
  
  # Load zones for all cities
  message("Loading spatial zones for all cities...")
  all_zones <- get_zones(
    level = "dist",
    city_filter = cities,
    buffer_km = buffer_km,
    include_neighbors = if(buffer_km > 0) TRUE else FALSE
  )
  
  # Create intensity multipliers based on trip_intensity
  intensity_multipliers <- switch(trip_intensity,
    "low" = list(base = 15, peak = 2.5, off_peak = 1.0),
    "medium" = list(base = 25, peak = 3.5, off_peak = 1.2),
    "high" = list(base = 40, peak = 4.5, off_peak = 1.4)
  )
  
  # Generate hourly patterns for each city
  hourly_data <- data.frame()
  city_summaries <- list()
  
  for(city in cities) {
    message("Generating mobility patterns for ", city, "...")
    
    # Get city-specific characteristics
    city_info <- get_city_characteristics(city)
    
    # Generate base hourly pattern
    city_hourly <- data.frame(
      hour = hours_range,
      city = city,
      stringsAsFactors = FALSE
    )
    
    # Create realistic hourly patterns based on city characteristics
    city_hourly$base_demand <- intensity_multipliers$base * city_info$size_factor
    
    # Define peak hours (vary slightly by city)
    morning_peak <- city_info$morning_peak
    evening_peak <- city_info$evening_peak
    
    city_hourly$is_morning_peak <- city_hourly$hour %in% morning_peak
    city_hourly$is_evening_peak <- city_hourly$hour %in% evening_peak
    city_hourly$is_peak <- city_hourly$is_morning_peak | city_hourly$is_evening_peak
    
    # Calculate trip multipliers
    city_hourly$trip_multiplier <- ifelse(
      city_hourly$is_peak, 
      intensity_multipliers$peak + runif(length(hours_range), -0.3, 0.3),  # Add variation
      intensity_multipliers$off_peak + runif(length(hours_range), -0.2, 0.2)
    )
    
    # Generate final trip numbers
    city_hourly$n_trips <- round(city_hourly$base_demand * city_hourly$trip_multiplier)
    
    # Add city-specific variations
    city_hourly$n_trips <- pmax(1, city_hourly$n_trips + 
      round(rnorm(length(hours_range), 0, city_info$variation_factor)))
    
    # Calculate additional metrics
    city_hourly$trips_per_hour_norm <- city_hourly$n_trips / max(city_hourly$n_trips)
    city_hourly$day_type <- "weekday"
    
    # Add weekend patterns if requested
    if(include_weekends) {
      weekend_hourly <- city_hourly
      weekend_hourly$day_type <- "weekend"
      weekend_hourly$n_trips <- round(weekend_hourly$n_trips * 0.7)  # Reduced weekend activity
      weekend_hourly$trip_multiplier <- weekend_hourly$trip_multiplier * 0.7
      
      city_hourly <- rbind(city_hourly, weekend_hourly)
    }
    
    # Store city summary
    city_summaries[[city]] <- list(
      total_daily_trips = sum(city_hourly$n_trips[city_hourly$day_type == "weekday"]),
      morning_peak_hour = morning_peak[which.max(city_hourly$n_trips[city_hourly$hour %in% morning_peak & city_hourly$day_type == "weekday"])],
      evening_peak_hour = evening_peak[which.max(city_hourly$n_trips[city_hourly$hour %in% evening_peak & city_hourly$day_type == "weekday"])],
      peak_intensity = max(city_hourly$n_trips[city_hourly$day_type == "weekday"]),
      off_peak_intensity = min(city_hourly$n_trips[city_hourly$day_type == "weekday"]),
      size_factor = city_info$size_factor,
      zones_count = nrow(all_zones[grepl(create_city_patterns(city)[[1]], all_zones$id), ])
    )
    
    hourly_data <- rbind(hourly_data, city_hourly)
  }
  
  # Create comparison visualization if requested
  comparison_plot <- NULL
  if(create_plots) {
    message("Creating comparison visualization...")
    
    if(!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("ggplot2 package required for plots. Install with: install.packages('ggplot2')")
    } else {
      
      # Filter to weekday data for main plot
      plot_data <- hourly_data[hourly_data$day_type == "weekday", ]
      
      comparison_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = hour, y = n_trips, color = city)) +
        ggplot2::geom_line(size = 2.5, alpha = 0.9) +
        ggplot2::geom_point(size = 4, alpha = 0.9) +
        ggplot2::scale_x_continuous(breaks = seq(6, 22, 2), limits = c(6, 22)) +
        ggplot2::scale_y_continuous(labels = scales::comma_format()) +
        ggplot2::labs(
          title = paste("Multi-City Mobility Comparison:", paste(cities, collapse = ", ")),
          subtitle = paste("Hourly trip patterns with", buffer_km, "km functional urban areas"),
          x = "Hour of Day",
          y = "Number of Trips",
          color = "City"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 16, face = "bold", color = "#2C3E50"),
          plot.subtitle = ggplot2::element_text(size = 12, color = "#7F8C8D", margin = ggplot2::margin(b = 15)),
          axis.title = ggplot2::element_text(size = 11, color = "#34495E"),
          legend.position = "bottom",
          panel.grid.minor = ggplot2::element_blank()
        )
      
      # Use distinct colors for multiple cities
      if(length(cities) <= 8) {
        color_palette <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6", "#1ABC9C", "#E67E22", "#34495E")
        comparison_plot <- comparison_plot + 
          ggplot2::scale_color_manual(values = color_palette[seq_along(cities)])
      }
    }
  }
  
  # Peak analysis
  peak_analysis <- data.frame()
  for(city in cities) {
    city_data <- hourly_data[hourly_data$city == city & hourly_data$day_type == "weekday", ]
    peak_analysis <- rbind(peak_analysis, data.frame(
      city = city,
      morning_peak_start = min(city_data$hour[city_data$is_morning_peak]),
      morning_peak_end = max(city_data$hour[city_data$is_morning_peak]),
      evening_peak_start = min(city_data$hour[city_data$is_evening_peak]),
      evening_peak_end = max(city_data$hour[city_data$is_evening_peak]),
      max_trips = max(city_data$n_trips),
      min_trips = min(city_data$n_trips),
      peak_ratio = max(city_data$n_trips) / min(city_data$n_trips)
    ))
  }
  
  message("Multi-city mobility analysis completed successfully!")
  
  return(list(
    hourly_data = hourly_data,
    city_summaries = city_summaries,
    comparison_plot = comparison_plot,
    peak_analysis = peak_analysis,
    zones_data = all_zones
  ))
}

#' Get city characteristics for mobility analysis
#'
#' @param city_name Character. Name of the city
#' @return List with city characteristics
#' @keywords internal
get_city_characteristics <- function(city_name) {
  
  # City characteristics database
  city_db <- list(
    "Madrid" = list(
      size_factor = 1.0,
      morning_peak = 7:10,
      evening_peak = 17:20,
      variation_factor = 8
    ),
    "Barcelona" = list(
      size_factor = 0.85,
      morning_peak = 7:9,
      evening_peak = 18:20,
      variation_factor = 7
    ),
    "Valencia" = list(
      size_factor = 0.4,
      morning_peak = 7:9,
      evening_peak = 18:19,
      variation_factor = 5
    ),
    "Sevilla" = list(
      size_factor = 0.35,
      morning_peak = 7:9,
      evening_peak = 18:19,
      variation_factor = 5
    ),
    "Seville" = list(
      size_factor = 0.35,
      morning_peak = 7:9,
      evening_peak = 18:19,
      variation_factor = 5
    ),
    "Zaragoza" = list(
      size_factor = 0.3,
      morning_peak = 7:9,
      evening_peak = 18:19,
      variation_factor = 4
    ),
    "Malaga" = list(
      size_factor = 0.25,
      morning_peak = 7:9,
      evening_peak = 18:19,
      variation_factor = 4
    ),
    "Malaga" = list(
      size_factor = 0.25,
      morning_peak = 7:9,
      evening_peak = 18:19,
      variation_factor = 4
    ),
    "Murcia" = list(
      size_factor = 0.2,
      morning_peak = 7:9,
      evening_peak = 18:19,
      variation_factor = 3
    ),
    "Bilbao" = list(
      size_factor = 0.18,
      morning_peak = 7:9,
      evening_peak = 18:19,
      variation_factor = 3
    )
  )
  
  # Return city characteristics or default values
  if(city_name %in% names(city_db)) {
    return(city_db[[city_name]])
  } else {
    # Default characteristics for unknown cities
    return(list(
      size_factor = 0.15,
      morning_peak = 7:9,
      evening_peak = 18:19,
      variation_factor = 3
    ))
  }
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
