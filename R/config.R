#' Configure mobspain package settings
#'
#' @param cache_dir Directory for caching downloaded data (default: tempdir())
#' @param max_cache_size Maximum cache size in MB (default: 500)
#' @param parallel Enable parallel processing (default: FALSE)
#' @param n_cores Number of cores for parallel processing (default: 2)
#' @export
#' @examples
#' \dontrun{
#' # Configure with default settings
#' configure_mobspain()
#' 
#' # Configure with custom cache directory
#' configure_mobspain(cache_dir = "~/mobspain_cache")
#' 
#' # Configure with parallel processing
#' configure_mobspain(parallel = TRUE, n_cores = 4)
#' 
#' # Configure with larger cache size
#' configure_mobspain(max_cache_size = 1000)  # 1GB cache
#' 
#' # Configure with all custom settings
#' configure_mobspain(
#'   cache_dir = "~/mobspain_cache",
#'   max_cache_size = 1000,
#'   parallel = TRUE,
#'   n_cores = 4
#' )
#' }
configure_mobspain <- function(cache_dir = tempdir(), max_cache_size = 500, 
                              parallel = FALSE, n_cores = 2) {
  options(
    mobspain.cache_dir = cache_dir,
    mobspain.max_cache_size = max_cache_size,
    mobspain.parallel = parallel,
    mobspain.n_cores = n_cores,
    mobspain.use_csv = TRUE  # Default to CSV access for reliability
  )
  
  if(parallel && !requireNamespace("parallel", quietly = TRUE)) {
    warning("parallel package not available, disabling parallel processing")
    options(mobspain.parallel = FALSE)
  }
  
  message("mobspain configured:")
  message("  Cache directory: ", cache_dir)
  message("  Max cache size: ", max_cache_size, " MB")
  message("  Parallel processing: ", ifelse(parallel, "enabled", "disabled"))
  message("  Data access method: CSV (recommended for reliability)")
  if(parallel) message("  Number of cores: ", n_cores)
}

#' Get package status and diagnostics
#'
#' @return List with package status information
#' @export
#' @examples
#' \dontrun{
#' # Get package status
#' status <- mobspain_status()
#' print(status)
#' 
#' # Check specific components
#' if (status$data_dir_exists) {
#'   cat("Data directory is set up correctly\n")
#' } else {
#'   cat("Run init_data_dir() to set up data directory\n")
#' }
#' 
#' # Check dependencies
#' if (status$sf_available) {
#'   cat("Spatial analysis available\n")
#' }
#' 
#' if (status$database_exists) {
#'   cat("Database size:", status$database_size_mb, "MB\n")
#' }
#' }
mobspain_status <- function() {
  data_dir <- getOption("mobspain.data_dir")
  cache_dir <- getOption("mobspain.cache_dir", tempdir())
  
  status <- list(
    package_version = utils::packageVersion("mobspain"),
    data_directory = data_dir,
    data_dir_exists = !is.null(data_dir) && dir.exists(data_dir),
    cache_directory = cache_dir,
    cache_dir_exists = dir.exists(cache_dir),
    spanishoddata_version = tryCatch(
      as.character(utils::packageVersion("spanishoddata")),
      error = function(e) "Not installed"
    ),
    duckdb_available = requireNamespace("duckdb", quietly = TRUE),
    sf_available = requireNamespace("sf", quietly = TRUE),
    parallel_enabled = getOption("mobspain.parallel", FALSE)
  )
  
  # Check database status
  if(status$data_dir_exists) {
    db_path <- file.path(data_dir, "spanishoddata.duckdb")
    status$database_exists <- file.exists(db_path)
    if(status$database_exists) {
      status$database_size_mb <- round(file.size(db_path) / 1024^2, 2)
    }
  }
  
  class(status) <- "mobspain_status"
  return(status)
}

#' Print method for mobspain_status
#' @param x mobspain_status object
#' @param ... Additional arguments (ignored)
#' @export
print.mobspain_status <- function(x, ...) {
  cat("mobspain Package Status\n")
  cat("=======================\n\n")
  
  cat("Package version:", as.character(x$package_version), "\n")
  cat("Data directory:", ifelse(is.null(x$data_directory), "Not set", x$data_directory), "\n")
  cat("Data directory exists:", ifelse(x$data_dir_exists, "YES", "NO"), "\n")
  
  if(!is.null(x$database_exists)) {
    cat("Database exists:", ifelse(x$database_exists, "YES", "NO"), "\n")
    if(x$database_exists) {
      cat("Database size:", x$database_size_mb, "MB\n")
    }
  }
  
  cat("\nDependencies:\n")
  cat("  spanishoddata:", x$spanishoddata_version, "\n")
  cat("  duckdb:", ifelse(x$duckdb_available, "YES", "NO"), "\n")
  cat("  sf:", ifelse(x$sf_available, "YES", "NO"), "\n")
  
  cat("\nConfiguration:\n")
  cat("  Parallel processing:", ifelse(x$parallel_enabled, "enabled", "disabled"), "\n")
}

#' Get optimal analysis parameters based on MITMA data characteristics
#' @param analysis_type Type of analysis: "exploratory", "detailed", "regional", "temporal"
#' @param data_size Expected data size: "small", "medium", "large"
#' @return List with recommended parameters
#' @export
#' @examples
#' \dontrun{
#' # Get parameters for exploratory analysis
#' params_explore <- get_optimal_parameters("exploratory", "medium")
#' print(params_explore)
#' 
#' # Get parameters for detailed analysis
#' params_detailed <- get_optimal_parameters("detailed", "large")
#' print(params_detailed)
#' 
#' # Get parameters for regional analysis
#' params_regional <- get_optimal_parameters("regional", "small")
#' print(params_regional)
#' 
#' # Use parameters in analysis
#' mobility_data <- get_mobility_matrix(
#'   dates = c("2023-01-01", "2023-01-07"),
#'   level = params_explore$spatial_level
#' )
#' }
get_optimal_parameters <- function(analysis_type = "exploratory", data_size = "medium") {
  
  params <- list()
  
  if(analysis_type == "exploratory") {
    params$spatial_level <- "lua"  # Large urban areas for faster processing
    params$date_range_days <- 7
    params$time_window <- NULL
    params$min_flow_threshold <- 100
    
  } else if(analysis_type == "detailed") {
    params$spatial_level <- "dist"  # Districts for detailed analysis
    params$date_range_days <- 30
    params$time_window <- c(7, 9)  # Focus on commuting
    params$min_flow_threshold <- 50
    
  } else if(analysis_type == "regional") {
    params$spatial_level <- "muni"  # Municipalities for regional studies
    params$date_range_days <- 90
    params$time_window <- NULL
    params$min_flow_threshold <- 200
    
  } else if(analysis_type == "temporal") {
    params$spatial_level <- "lua"  # Fewer zones for temporal focus
    params$date_range_days <- 365
    params$time_window <- NULL
    params$min_flow_threshold <- 500
  }
  
  # Adjust for data size
  if(data_size == "small") {
    params$date_range_days <- min(params$date_range_days, 14)
    params$min_flow_threshold <- params$min_flow_threshold * 2
  } else if(data_size == "large") {
    params$spatial_level <- ifelse(params$spatial_level == "dist", "muni", params$spatial_level)
    params$min_flow_threshold <- params$min_flow_threshold / 2
  }
  
  # Add memory and performance recommendations
  zone_counts <- list("dist" = 3909, "muni" = 8131, "lua" = 85)
  estimated_combinations <- zone_counts[[params$spatial_level]]^2 * params$date_range_days
  
  params$estimated_data_points <- estimated_combinations
  params$memory_recommendation <- ifelse(estimated_combinations > 1e6, "Use data filtering", "Standard processing")
  params$processing_time <- ifelse(estimated_combinations > 5e5, "Long (>5 min)", "Fast (<2 min)")
  
  return(params)
}
