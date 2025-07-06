#' Cache management utilities
#' @keywords internal

#' Get cache key for data request
get_cache_key <- function(dates, level, time_window = NULL) {
  key_parts <- c(
    paste(dates, collapse = "_"),
    level,
    if(!is.null(time_window)) paste(time_window, collapse = "_") else "all_hours"
  )
  digest::digest(paste(key_parts, collapse = "|"))
}

#' Check if cached data exists and is valid
#' @param cache_key Character string identifying the cache file
#' @return Cached data if valid, NULL otherwise
check_cache <- function(cache_key) {
  cache_dir <- getOption("mobspain.cache_dir", tempdir())
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
  
  if(!file.exists(cache_file)) {
    return(NULL)
  }
  
  # Check if cache is too old (default: 7 days)
  cache_age_days <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "days"))
  max_age <- getOption("mobspain.cache_max_age_days", 7)
  
  if(cache_age_days > max_age) {
    unlink(cache_file)
    return(NULL)
  }
  
  tryCatch({
    readRDS(cache_file)
  }, error = function(e) {
    unlink(cache_file)
    NULL
  })
}

#' Save data to cache
#' @param data Data to cache
#' @param cache_key Character string identifying the cache file
#' @return TRUE if successful, FALSE otherwise
save_to_cache <- function(data, cache_key) {
  cache_dir <- getOption("mobspain.cache_dir", tempdir())
  if(!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
  
  tryCatch({
    saveRDS(data, cache_file, compress = TRUE)
    
    # Clean up old cache files if needed
    cleanup_cache()
  }, error = function(e) {
    warning("Failed to save to cache: ", e$message)
  })
}

#' Clean up cache directory
cleanup_cache <- function() {
  cache_dir <- getOption("mobspain.cache_dir", tempdir())
  max_size_mb <- getOption("mobspain.max_cache_size", 500)
  
  if(!dir.exists(cache_dir)) return()
  
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  if(length(cache_files) == 0) return()
  
  # Get file info
  file_info <- file.info(cache_files)
  file_info$path <- rownames(file_info)
  
  # Calculate total size
  total_size_mb <- sum(file_info$size, na.rm = TRUE) / 1024^2
  
  if(total_size_mb > max_size_mb) {
    # Remove oldest files until under limit
    file_info <- file_info[order(file_info$mtime), ]
    
    cumulative_size <- 0
    for(i in seq_len(nrow(file_info))) {
      cumulative_size <- cumulative_size + file_info$size[i] / 1024^2
      if(cumulative_size > max_size_mb * 0.8) {  # Keep 80% of max size
        unlink(file_info$path[1:i])
        break
      }
    }
  }
}
