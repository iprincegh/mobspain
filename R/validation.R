#' Enhanced validation utilities
#' @name validation
#' @keywords internal
NULL

#' Validate date inputs
#' @param dates Date vector to validate
#' @return Validated date vector
#' @keywords internal
validate_dates <- function(dates) {
  if(is.null(dates) || length(dates) == 0) {
    stop("Dates cannot be NULL or empty", call. = FALSE)
  }
  
  if(length(dates) == 1) {
    dates <- rep(dates, 2)
  } else if(length(dates) > 2) {
    warning("Only first two dates will be used")
    dates <- dates[1:2]
  }
  
  # Try to convert to Date
  tryCatch({
    date_objects <- as.Date(dates)
    if(any(is.na(date_objects))) {
      stop("Invalid date format. Use 'YYYY-MM-DD' format", call. = FALSE)
    }
    if(date_objects[2] < date_objects[1]) {
      stop("End date must be after start date", call. = FALSE)
    }
    
    # MITMA data availability check
    mitma_start <- as.Date("2020-02-14")
    if(any(date_objects < mitma_start)) {
      warning("MITMA data is only available from 2020-02-14 onwards. Earlier dates may return no data.", 
              call. = FALSE)
    }
    
    # Future date check
    if(any(date_objects > Sys.Date())) {
      warning("Future dates requested. Data may not be available.", call. = FALSE)
    }
    
    return(as.character(date_objects))
  }, error = function(e) {
    stop("Date parsing failed: ", e$message, call. = FALSE)
  })
}

#' Validate spatial level
#' @param level Spatial level to validate
#' @return Standardized level name
#' @keywords internal
validate_level <- function(level) {
  valid_levels <- c("dist", "districts", "muni", "municipalities", "lua", "large_urban_areas")
  if(!level %in% valid_levels) {
    stop("Level must be one of: ", paste(valid_levels, collapse = ", "), call. = FALSE)
  }
  
  # Standardize level names
  level_map <- c(
    "districts" = "dist",
    "municipalities" = "muni", 
    "large_urban_areas" = "lua"
  )
  
  return(ifelse(level %in% names(level_map), level_map[level], level))
}

#' Validate time window
#' @param time_window Time window to validate
#' @return Validated time window
#' @keywords internal
validate_time_window <- function(time_window) {
  if(is.null(time_window)) return(NULL)
  
  if(length(time_window) != 2) {
    stop("time_window must be a vector of length 2", call. = FALSE)
  }
  
  if(!is.numeric(time_window) || any(time_window < 0) || any(time_window > 23)) {
    stop("time_window must contain hours between 0 and 23", call. = FALSE)
  }
  
  if(time_window[2] < time_window[1]) {
    stop("End hour must be after start hour", call. = FALSE)
  }
  
  return(as.integer(time_window))
}

#' Validate MITMA data quality
#' @param od_data Origin-destination mobility data
#' @return List with quality indicators and warnings
#' @export
#' @examples
#' \dontrun{
#' # Load mobility data
#' mobility_data <- get_mobility_matrix(dates = c("2023-01-01", "2023-01-07"))
#' 
#' # Validate data quality
#' quality_report <- validate_mitma_data(mobility_data)
#' print(quality_report)
#' 
#' # Check for issues
#' if (quality_report$has_issues) {
#'   cat("Data quality issues found:\n")
#'   print(quality_report$issues)
#' } else {
#'   cat("Data quality is good\n")
#' }
#' 
#' # View quality metrics
#' cat("Missing values:", quality_report$missing_values, "\n")
#' cat("Zero flows:", quality_report$zero_flows, "\n")
#' cat("Date range:", quality_report$date_range, "\n")
#' }
validate_mitma_data <- function(od_data) {
  quality_report <- list()
  
  # Check required columns
  required_cols <- c("id_origin", "id_destination", "n_trips", "date")
  missing_cols <- setdiff(required_cols, names(od_data))
  if(length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  # Check for negative trip counts
  negative_trips <- sum(od_data$n_trips < 0, na.rm = TRUE)
  quality_report$negative_trips <- negative_trips
  if(negative_trips > 0) {
    warning("Found ", negative_trips, " negative trip counts. This may indicate data quality issues.", call. = FALSE)
  }
  
  # Check for missing data patterns
  total_na <- sum(is.na(od_data$n_trips))
  quality_report$missing_trips <- total_na
  quality_report$missing_percentage <- (total_na / nrow(od_data)) * 100
  
  # Check date continuity
  dates <- unique(od_data$date)
  date_gaps <- length(seq(min(dates), max(dates), by = "day")) - length(dates)
  quality_report$date_gaps <- date_gaps
  
  # Check for extremely high values (potential outliers)
  trip_threshold <- quantile(od_data$n_trips, 0.99, na.rm = TRUE) * 10
  extreme_values <- sum(od_data$n_trips > trip_threshold, na.rm = TRUE)
  quality_report$extreme_values <- extreme_values
  
  # Weekend vs weekday data availability
  od_data$is_weekend <- lubridate::wday(od_data$date) %in% c(1, 7)
  weekend_coverage <- mean(od_data$is_weekend, na.rm = TRUE)
  quality_report$weekend_coverage <- weekend_coverage
  
  if(weekend_coverage < 0.1 || weekend_coverage > 0.5) {
    warning("Unusual weekend/weekday data distribution. Expected ~28% weekend data.", call. = FALSE)
  }
  
  return(quality_report)
}

#' Check for Spanish holidays that may affect mobility patterns
#' @param dates Vector of dates to check
#' @return Data frame with holiday information
#' @export
#' @examples
#' \dontrun{
#' # Check holidays for specific dates
#' dates <- c("2023-01-01", "2023-12-25", "2023-08-15", "2023-05-01")
#' holiday_info <- check_spanish_holidays(dates)
#' print(holiday_info)
#' 
#' # Check holidays for a date range
#' date_range <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
#' all_holidays <- check_spanish_holidays(date_range)
#' holidays_only <- all_holidays[all_holidays$is_likely_holiday, ]
#' print(holidays_only)
#' 
#' # Use in mobility analysis to flag potentially unusual dates
#' mobility_data <- get_mobility_matrix(dates = c("2023-01-01", "2023-01-07"))
#' unique_dates <- unique(mobility_data$date)
#' holiday_check <- check_spanish_holidays(unique_dates)
#' cat("Holidays in the data:", 
#'     sum(holiday_check$is_likely_holiday), "\n")
#' }
check_spanish_holidays <- function(dates) {
  # Common Spanish national holidays (approximate - varies by year)
  holiday_patterns <- data.frame(
    date_pattern = c("01-01", "01-06", "05-01", "08-15", "10-12", "11-01", "12-06", "12-08", "12-25"),
    holiday_name = c("New Year", "Epiphany", "Labour Day", "Assumption", "National Day", 
                     "All Saints", "Constitution", "Immaculate Conception", "Christmas"),
    stringsAsFactors = FALSE
  )
  
  date_strings <- format(as.Date(dates), "%m-%d")
  
  holiday_check <- data.frame(
    date = as.Date(dates),
    is_likely_holiday = date_strings %in% holiday_patterns$date_pattern,
    stringsAsFactors = FALSE
  )
  
  # Add holiday names
  holiday_check$holiday_name <- NA
  if(nrow(holiday_patterns) > 0) {
    for(i in seq_len(nrow(holiday_patterns))) {
      matches <- date_strings == holiday_patterns$date_pattern[i]
      holiday_check$holiday_name[matches] <- holiday_patterns$holiday_name[i]
    }
  }
  
  # Check for Easter (varies by year - simplified check)
  # Note: This is a simplified implementation
  easter_months <- format(as.Date(dates), "%m") %in% c("03", "04")
  holiday_check$is_easter_period <- easter_months
  
  return(holiday_check)
}

# Enhanced Spanish Mobility Data Validation and Quality Assessment

#' Validate Spanish mobility data comprehensively
#'
#' @param mobility_data Data frame with Spanish mobility data
#' @param version Data version (1 or 2) to validate against
#' @param check_completeness Check for data completeness
#' @param check_consistency Check for internal consistency
#' @param check_anomalies Check for statistical anomalies
#' @return List with validation results and recommendations
#' @export
#' @examples
#' \dontrun{
#' # Validate mobility data
#' mobility_data <- get_mobility_matrix(dates = c("2020-04-01", "2020-04-07"))
#' validation <- validate_spanish_mobility_data(mobility_data, version = 1)
#' print(validation$summary)
#' print(validation$recommendations)
#' }
validate_spanish_mobility_data <- function(mobility_data, version = NULL, 
                                         check_completeness = TRUE,
                                         check_consistency = TRUE,
                                         check_anomalies = TRUE) {
  
  if(is.null(version)) {
    version <- getOption("mobspain.data_version", 2)
  }
  
  validation_results <- list()
  
  # Basic structure validation
  validation_results$structure <- validate_data_structure(mobility_data, version)
  
  # Completeness validation
  if(check_completeness) {
    validation_results$completeness <- validate_data_completeness(mobility_data, version)
  }
  
  # Consistency validation
  if(check_consistency) {
    validation_results$consistency <- validate_data_consistency(mobility_data, version)
  }
  
  # Anomaly detection
  if(check_anomalies) {
    validation_results$anomalies <- validate_data_anomalies(mobility_data, version)
  }
  
  # Generate summary and recommendations
  validation_results$summary <- generate_validation_summary(validation_results)
  validation_results$recommendations <- generate_validation_recommendations(validation_results, version)
  
  class(validation_results) <- "spanish_mobility_validation"
  return(validation_results)
}

#' Validate data structure against Spanish mobility data specifications
#' @param mobility_data Data frame with mobility data
#' @param version Data version (1 or 2)
#' @return List with structure validation results
#' @keywords internal
validate_data_structure <- function(mobility_data, version) {
  
  # Expected columns for each version
  v1_required_cols <- c("date", "hour", "id_origin", "id_destination", "n_trips")
  v1_optional_cols <- c("distance", "activity_origin", "activity_destination", 
                       "residence_province_ine_code", "residence_province_name",
                       "trips_total_length_km")
  
  v2_required_cols <- c("date", "hour", "id_origin", "id_destination", "n_trips")
  v2_optional_cols <- c("distance", "age", "sex", "income", "activity_origin", 
                       "activity_destination", "residence_province_ine_code",
                       "trips_total_length_km")
  
  required_cols <- if(version == 1) v1_required_cols else v2_required_cols
  optional_cols <- if(version == 1) v1_optional_cols else v2_optional_cols
  
  actual_cols <- names(mobility_data)
  
  # Check required columns
  missing_required <- setdiff(required_cols, actual_cols)
  present_required <- intersect(required_cols, actual_cols)
  
  # Check optional columns
  missing_optional <- setdiff(optional_cols, actual_cols)
  present_optional <- intersect(optional_cols, actual_cols)
  
  # Check unexpected columns
  expected_all <- c(required_cols, optional_cols)
  unexpected_cols <- setdiff(actual_cols, expected_all)
  
  return(list(
    version = version,
    required_columns = list(
      expected = required_cols,
      present = present_required,
      missing = missing_required,
      complete = length(missing_required) == 0
    ),
    optional_columns = list(
      expected = optional_cols,
      present = present_optional,
      missing = missing_optional
    ),
    unexpected_columns = unexpected_cols,
    total_columns = length(actual_cols),
    structure_valid = length(missing_required) == 0
  ))
}

#' Validate data completeness
#' @param mobility_data Data frame with mobility data
#' @param version Data version
#' @return List with completeness validation results
#' @keywords internal
validate_data_completeness <- function(mobility_data, version) {
  
  total_rows <- nrow(mobility_data)
  
  completeness <- list()
  
  for(col in names(mobility_data)) {
    missing_count <- sum(is.na(mobility_data[[col]]))
    completeness[[col]] <- list(
      missing_count = missing_count,
      missing_percentage = round(missing_count / total_rows * 100, 2),
      complete_percentage = round((total_rows - missing_count) / total_rows * 100, 2)
    )
  }
  
  # Overall completeness score
  overall_missing <- sum(sapply(completeness, function(x) x$missing_count))
  total_cells <- total_rows * length(names(mobility_data))
  overall_completeness <- round((total_cells - overall_missing) / total_cells * 100, 2)
  
  return(list(
    by_column = completeness,
    total_rows = total_rows,
    total_columns = length(names(mobility_data)),
    overall_completeness_percentage = overall_completeness,
    completeness_threshold_met = overall_completeness >= 95  # 95% threshold
  ))
}

#' Validate data consistency
#' @param mobility_data Data frame with mobility data
#' @param version Data version
#' @return List with consistency validation results
#' @keywords internal
validate_data_consistency <- function(mobility_data, version) {
  
  consistency_checks <- list()
  
  # Date consistency
  if("date" %in% names(mobility_data)) {
    dates <- as.Date(mobility_data$date)
    consistency_checks$dates <- list(
      valid_dates = sum(!is.na(dates)),
      invalid_dates = sum(is.na(dates)),
      date_range = range(dates, na.rm = TRUE),
      unique_dates = length(unique(dates))
    )
  }
  
  # Hour consistency (should be 0-23)
  if("hour" %in% names(mobility_data)) {
    hours <- mobility_data$hour
    consistency_checks$hours <- list(
      valid_hours = sum(hours >= 0 & hours <= 23, na.rm = TRUE),
      invalid_hours = sum(hours < 0 | hours > 23, na.rm = TRUE),
      unique_hours = length(unique(hours))
    )
  }
  
  # Trip counts consistency (should be positive)
  if("n_trips" %in% names(mobility_data)) {
    trips <- mobility_data$n_trips
    consistency_checks$trips <- list(
      positive_trips = sum(trips > 0, na.rm = TRUE),
      zero_trips = sum(trips == 0, na.rm = TRUE),
      negative_trips = sum(trips < 0, na.rm = TRUE),
      mean_trips = round(mean(trips, na.rm = TRUE), 2),
      median_trips = round(median(trips, na.rm = TRUE), 2)
    )
  }
  
  # Distance consistency (valid categories)
  if("distance" %in% names(mobility_data)) {
    valid_distances <- c("0005-002", "002-005", "005-010", "010-050", "050-100", "100+")
    distances <- mobility_data$distance
    consistency_checks$distances <- list(
      valid_categories = sum(distances %in% valid_distances, na.rm = TRUE),
      invalid_categories = sum(!distances %in% valid_distances & !is.na(distances)),
      unique_categories = unique(distances)
    )
  }
  
  return(consistency_checks)
}

#' Validate data for statistical anomalies
#' @param mobility_data Data frame with mobility data
#' @param version Data version
#' @return List with anomaly validation results
#' @keywords internal
validate_data_anomalies <- function(mobility_data, version) {
  
  anomalies <- list()
  
  if("n_trips" %in% names(mobility_data)) {
    trips <- mobility_data$n_trips[!is.na(mobility_data$n_trips)]
    
    if(length(trips) > 0) {
      # Statistical outliers using IQR method
      Q1 <- quantile(trips, 0.25)
      Q3 <- quantile(trips, 0.75)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      outliers <- trips < lower_bound | trips > upper_bound
      
      anomalies$trips <- list(
        total_records = length(trips),
        outliers_count = sum(outliers),
        outliers_percentage = round(sum(outliers) / length(trips) * 100, 2),
        extreme_values = list(
          very_high = sum(trips > quantile(trips, 0.99)),
          very_low = sum(trips < quantile(trips, 0.01))
        ),
        statistics = list(
          mean = round(mean(trips), 2),
          median = round(median(trips), 2),
          std_dev = round(sd(trips), 2),
          min = min(trips),
          max = max(trips)
        )
      )
    }
  }
  
  return(anomalies)
}

#' Generate validation summary
#' @param validation_results List with all validation results
#' @return Character vector with summary
#' @keywords internal
generate_validation_summary <- function(validation_results) {
  
  summary <- character()
  
  # Structure summary
  if(!is.null(validation_results$structure)) {
    structure_status <- if(validation_results$structure$structure_valid) "[PASS]" else "[FAIL]"
    summary <- c(summary, paste("Structure validation:", structure_status))
  }
  
  # Completeness summary
  if(!is.null(validation_results$completeness)) {
    completeness_status <- if(validation_results$completeness$completeness_threshold_met) "[PASS]" else "[WARNING]"
    summary <- c(summary, paste("Completeness validation:", completeness_status, 
                               paste0("(", validation_results$completeness$overall_completeness_percentage, "%)")))
  }
  
  # Consistency summary
  if(!is.null(validation_results$consistency)) {
    summary <- c(summary, "Consistency validation: [CHECKED]")
  }
  
  # Anomalies summary
  if(!is.null(validation_results$anomalies)) {
    summary <- c(summary, "Anomaly detection: [COMPLETED]")
  }
  
  return(summary)
}

#' Generate validation recommendations
#' @param validation_results List with all validation results
#' @param version Data version
#' @return Character vector with recommendations
#' @keywords internal
generate_validation_recommendations <- function(validation_results, version) {
  
  recommendations <- character()
  
  # Structure recommendations
  if(!is.null(validation_results$structure)) {
    if(!validation_results$structure$structure_valid) {
      missing <- validation_results$structure$required_columns$missing
      recommendations <- c(recommendations, 
                          paste("Missing required columns:", paste(missing, collapse = ", ")))
    }
    
    if(length(validation_results$structure$unexpected_columns) > 0) {
      unexpected <- validation_results$structure$unexpected_columns
      recommendations <- c(recommendations,
                          paste("Unexpected columns found:", paste(unexpected, collapse = ", ")))
    }
  }
  
  # Completeness recommendations
  if(!is.null(validation_results$completeness)) {
    if(!validation_results$completeness$completeness_threshold_met) {
      recommendations <- c(recommendations,
                          paste("Data completeness below 95% threshold:",
                               validation_results$completeness$overall_completeness_percentage, "%"))
    }
    
    # Check individual columns with high missing rates
    high_missing <- names(validation_results$completeness$by_column)[
      sapply(validation_results$completeness$by_column, function(x) x$missing_percentage > 20)
    ]
    
    if(length(high_missing) > 0) {
      recommendations <- c(recommendations,
                          paste("Columns with >20% missing data:", paste(high_missing, collapse = ", ")))
    }
  }
  
  # Consistency recommendations
  if(!is.null(validation_results$consistency)) {
    if(!is.null(validation_results$consistency$trips)) {
      if(validation_results$consistency$trips$negative_trips > 0) {
        recommendations <- c(recommendations,
                            paste("Found", validation_results$consistency$trips$negative_trips, "negative trip counts"))
      }
    }
    
    if(!is.null(validation_results$consistency$hours)) {
      if(validation_results$consistency$hours$invalid_hours > 0) {
        recommendations <- c(recommendations,
                            paste("Found", validation_results$consistency$hours$invalid_hours, "invalid hours (not 0-23)"))
      }
    }
  }
  
  # Anomaly recommendations
  if(!is.null(validation_results$anomalies$trips)) {
    if(validation_results$anomalies$trips$outliers_percentage > 5) {
      recommendations <- c(recommendations,
                          paste("High outlier rate in trip counts:",
                               validation_results$anomalies$trips$outliers_percentage, "%"))
    }
  }
  
  # General recommendations
  if(length(recommendations) == 0) {
    recommendations <- c("Data validation passed all checks. Data appears to be high quality.")
  } else {
    recommendations <- c("Data validation completed with issues. Please review:",
                        recommendations)
  }
  
  return(recommendations)
}

#' Print method for Spanish mobility validation results
#' @param x Spanish mobility validation object
#' @param ... Additional arguments
#' @export
print.spanish_mobility_validation <- function(x, ...) {
  cat("Spanish Mobility Data Validation Report\n")
  cat("=====================================\n\n")
  
  cat("Summary:\n")
  cat(paste(x$summary, collapse = "\n"), "\n\n")
  
  cat("Recommendations:\n")
  cat(paste(x$recommendations, collapse = "\n"), "\n\n")
  
  if(!is.null(x$structure)) {
    cat("Structure Details:\n")
    cat("- Required columns present:", length(x$structure$required_columns$present), "/", 
        length(x$structure$required_columns$expected), "\n")
    cat("- Optional columns present:", length(x$structure$optional_columns$present), "/",
        length(x$structure$optional_columns$expected), "\n")
    if(length(x$structure$unexpected_columns) > 0) {
      cat("- Unexpected columns:", length(x$structure$unexpected_columns), "\n")
    }
    cat("\n")
  }
  
  if(!is.null(x$completeness)) {
    cat("Completeness Details:\n")
    cat("- Overall completeness:", x$completeness$overall_completeness_percentage, "%\n")
    cat("- Total rows:", x$completeness$total_rows, "\n")
    cat("- Total columns:", x$completeness$total_columns, "\n\n")
  }
  
  invisible(x)
}
