#' Utility Functions for mobspain Package
#'
#' Helper functions to improve user experience and performance.

#' Setup data directory for Spanish mobility data
#'
#' Configures a permanent directory for caching Spanish mobility data to improve
#' performance and avoid repeated downloads.
#'
#' @param data_dir Character. Path to directory for data storage. 
#'   If NULL, creates 'mobspain_data' in user's home directory.
#' @param create_dir Logical. Whether to create the directory if it doesn't exist (default: TRUE).
#' @return Character. Path to the configured data directory.
#' @details
#' This function sets the SPANISH_OD_DATA_DIR environment variable to avoid
#' repeated downloads and improve performance. The directory will be used by
#' the spanishoddata package for caching.
#' @export
#' @examples
#' \dontrun{
#' # Setup data directory in home folder
#' setup_data_dir()
#' 
#' # Use custom directory
#' setup_data_dir("~/my_spanish_data")
#' 
#' # Check current setting
#' Sys.getenv("SPANISH_OD_DATA_DIR")
#' }
setup_data_dir <- function(data_dir = NULL, create_dir = TRUE) {
  
  # Use default location if not specified
  if(is.null(data_dir)) {
    data_dir <- file.path(path.expand("~"), "mobspain_data")
  } else {
    data_dir <- path.expand(data_dir)
  }
  
  # Create directory if requested and it doesn't exist
  if(create_dir && !dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message("Created data directory: ", data_dir)
  }
  
  # Set environment variable
  Sys.setenv(SPANISH_OD_DATA_DIR = data_dir)
  
  message("Spanish mobility data directory set to: ", data_dir)
  message("This will reduce download times for future sessions.")
  message("To make this permanent, add the following to your .Renviron file:")
  message("SPANISH_OD_DATA_DIR=", data_dir)
  
  return(data_dir)
}

#' Check mobspain package status and configuration
#'
#' Displays information about the mobspain package setup, data directories,
#' and available functionality.
#'
#' @return Invisible list with status information
#' @export
#' @examples
#' \dontrun{
#' # Check package status
#' mobspain_status()
#' }
mobspain_status <- function() {
  
  cat("=== mobspain Package Status ===\n")
  cat("Package Version: 1.0.0\n")
  cat("R Version:", R.version.string, "\n\n")
  
  # Check data directory
  data_dir <- Sys.getenv("SPANISH_OD_DATA_DIR")
  if(data_dir == "") {
    cat("Data Directory: Not configured (using temporary directory)\n")
    cat("Recommendation: Run setup_data_dir() to improve performance\n\n")
  } else {
    cat("Data Directory:", data_dir, "\n")
    cat("Directory exists:", dir.exists(data_dir), "\n\n")
  }
  
  # Check required packages
  cat("=== Required Packages ===\n")
  required_packages <- c("sf", "dplyr", "ggplot2", "spanishoddata")
  
  for(pkg in required_packages) {
    installed <- requireNamespace(pkg, quietly = TRUE)
    cat(sprintf("%-15s: %s\n", pkg, ifelse(installed, "OK Installed", "X Missing")))
  }
  
  cat("\n=== Available Functions ===\n")
  cat("Core Functions:\n")
  cat("  get_zones()           - Get spatial administrative boundaries\n")
  cat("  get_mobility()        - Get mobility flow data\n")
  cat("  analyze_spatial()     - Spatial analysis and mapping\n")
  cat("  create_flows()        - Create flow visualizations\n")
  cat("\nUtility Functions:\n")  
  cat("  setup_data_dir()      - Configure data caching\n")
  cat("  mobspain_status()     - Check package status\n")
  
  invisible(list(
    data_dir = data_dir,
    data_dir_exists = dir.exists(data_dir),
    required_packages = sapply(required_packages, requireNamespace, quietly = TRUE)
  ))
}
