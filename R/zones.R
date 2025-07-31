#' Get Spanish Administrative Zones
#'
#' Downloads Spanish administrative zone geometries with filtering options.
#' This is the core function for accessing spatial transport zone data.
#'
#' @param level Character. Spatial aggregation level:
#'   \itemize{
#'     \item "dist" - Districts (default, recommended)
#'     \item "muni" - Municipalities
#'   }
#' @param year Numeric. Year for the zone geometries (default: 2023)
#' @param zones_filter Character vector. Optional filter for specific zone IDs
#' @param region_filter Character. Optional filter by region/province name or code
#' @param city_filter Character vector. Optional filter by city names (e.g., "Madrid", "Barcelona")
#' @return SF object containing zone geometries with:
#'   \itemize{
#'     \item id - Zone identifier
#'     \item geometry - Spatial geometry (polygons)
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Get all districts
#' zones <- get_zones(level = "dist")
#' 
#' # Get Madrid zones
#' madrid_zones <- get_zones(level = "dist", city_filter = "Madrid")
#' 
#' # Get specific zones
#' zones <- get_zones(level = "dist", zones_filter = c("28001", "28002"))
#' }
get_zones <- function(level = "dist", year = 2023, zones_filter = NULL, 
                     region_filter = NULL, city_filter = NULL) {
  
  message("Loading spatial zones for level: ", level)
  
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
  
  # Apply city filtering if specified
  if(!is.null(city_filter)) {
    message("Filtering zones by city names: ", paste(city_filter, collapse = ", "))
    
    # Simple city filtering based on common Spanish city codes
    city_patterns <- list(
      "Madrid" = "^28",
      "Barcelona" = "^08", 
      "Valencia" = "^46",
      "Sevilla" = "^41",
      "Bilbao" = "^48",
      "Zaragoza" = "^50",
      "Malaga" = "^29",
      "Murcia" = "^30"
    )
    
    # Find matching zones
    city_matches <- NULL
    for(city_name in city_filter) {
      if(city_name %in% names(city_patterns)) {
        pattern <- city_patterns[[city_name]]
        city_zones <- zones %>% 
          dplyr::filter(grepl(pattern, .data$id, ignore.case = TRUE))
        
        if(nrow(city_zones) > 0) {
          if(is.null(city_matches)) {
            city_matches <- city_zones
          } else {
            city_matches <- rbind(city_matches, city_zones)
          }
          message("City '", city_name, "' matched ", nrow(city_zones), " zones")
        }
      } else {
        warning("City '", city_name, "' not recognized. Available cities: ", 
                paste(names(city_patterns), collapse = ", "))
      }
    }
    
    if(!is.null(city_matches) && nrow(city_matches) > 0) {
      zones <- city_matches %>% dplyr::distinct()
      message("Total city matches: ", nrow(zones), " zones")
    } else {
      stop("No zones found matching city names: ", paste(city_filter, collapse = ", "))
    }
    
  } else if(!is.null(zones_filter)) {
    message("Filtering zones by zone IDs...")
    
    # Try exact matches first
    exact_matches <- zones %>% dplyr::filter(.data$id %in% zones_filter)
    
    if(nrow(exact_matches) > 0) {
      zones <- exact_matches
      message("Found exact matches: ", nrow(zones), " zones")
    } else {
      # Try pattern matching for partial matches
      pattern_matches <- NULL
      for(filter_id in zones_filter) {
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
        zones <- pattern_matches %>% dplyr::distinct()
        message("Total pattern matches: ", nrow(zones), " zones")
      } else {
        stop("No zones found matching filter: ", paste(zones_filter, collapse = ", "))
      }
    }
  }
  
  # Check if any zones remain after filtering
  if(nrow(zones) == 0) {
    stop("No zones found matching the specified filter.")
  }
  
  # Ensure we still have a valid sf object
  if(!inherits(zones, "sf")) {
    stop("Zones object lost its sf class after filtering")
  }
  
  message("Successfully loaded ", nrow(zones), " zones")
  return(zones)
}
