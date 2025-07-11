#' Simple Machine Learning for Mobility Prediction
#'
#' This file contains simplified machine learning functions for analytical purposes
#' in spatial data science applications. Designed for users.

#' Predict mobility patterns using simple machine learning
#'
#' A user-friendly function for predicting mobility patterns using basic ML models.
#' Designed for analytical purposes with clear steps, manageable complexity, and
#' automatic spatial visualization for users.
#'
#' @param mobility_data Data frame with historical mobility data for training. Must contain:
#'   \itemize{
#'     \item date - Date column (Date or character format)
#'     \item n_trips - Number of trips (numeric)
#'     \item Additional columns are automatically used for feature engineering
#'   }
#' @param prediction_dates Character vector of dates to predict mobility for.
#'   Examples: "2023-01-08" or c("2023-01-08", "2023-01-09")
#' @param model_type Character. Type of machine learning model:
#'   \itemize{
#'     \item "linear_regression" - Linear regression (default, fastest, most interpretable)
#'     \item "random_forest" - Random Forest (more complex, potentially more accurate)
#'   }
#' @param max_rows Numeric. Maximum number of rows to use for training 
#'   (default: 3000 for memory-efficient processing). Larger datasets are randomly sampled.
#' @param spatial_zones sf object with spatial zones for creating maps (optional).
#'   Use \code{get_zones()} to obtain. If provided, creates prediction maps.
#' @param create_map Logical. Whether to create spatial map if spatial_zones provided 
#'   (default: TRUE). Set to FALSE for faster processing without visualization.
#' @return List containing:
#'   \itemize{
#'     \item predictions - Data frame with hourly predictions for each date
#'     \item spatial_map - ggplot2 choropleth map (if spatial_zones provided)
#'     \item model_info - Information about the trained model
#'   }
#'   If no spatial_zones provided, returns only the predictions data frame.
#' @details
#' This function automatically:
#' \itemize{
#'   \item Creates temporal features (hour, weekday, weekend indicator)
#'   \item Handles missing values and memory management
#'   \item Trains simple ML models suitable for users
#'   \item Generates hourly predictions for specified dates
#'   \item Creates spatial maps when geometries are available
#'   \item Provides analytical step-by-step messaging
#' }
#' 
#' The models use basic temporal features that users can understand:
#' hour of day, day of week, and weekend indicator.
#' @export
#' @examples
#' \dontrun{
#' # Basic mobility prediction for users
#' mobility <- get_mobility(dates = c("2023-01-01", "2023-01-05"))
#' predictions <- predict_mobility_patterns(
#'   mobility_data = mobility,
#'   prediction_dates = c("2023-01-08", "2023-01-09"),
#'   model_type = "linear_regression"
#' )
#' head(predictions)
#' 
#' # With spatial mapping for analysis
#' zones <- get_zones("dist")
#' predictions <- predict_mobility_patterns(
#'   mobility_data = mobility,
#'   prediction_dates = "2023-01-08",
#'   model_type = "random_forest",
#'   spatial_zones = zones,
#'   create_map = TRUE
#' )
#' 
#' # View predictions and map
#' head(predictions$predictions)
#' print(predictions$spatial_map)
#' print(predictions$model_info)
#' }
predict_patterns <- function(mobility_data, prediction_dates,
                                      model_type = c("linear_regression", "random_forest"),
                                      max_rows = 3000,
                                      spatial_zones = NULL,
                                      create_map = TRUE) {
  
  model_type <- match.arg(model_type)
  
  # Input validation (analytical: show what we're checking)
  message("Step 1: Validating input data...")
  if(!"date" %in% names(mobility_data)) {
    stop("mobility_data must contain 'date' column")
  }
  if(!"n_trips" %in% names(mobility_data)) {
    stop("mobility_data must contain 'n_trips' column for prediction target")
  }
  
  # Memory management for memory-efficient processing
  if(nrow(mobility_data) > max_rows) {
    message("Step 2: Large dataset detected (", nrow(mobility_data), " rows). Sampling ", max_rows, " rows for training.")
    mobility_data <- mobility_data[sample(nrow(mobility_data), max_rows), ]
  }
  
  # Simple feature engineering (analytical: basic temporal features)
  message("Step 3: Creating simple temporal features...")
  mobility_data$hour <- lubridate::hour(mobility_data$date)
  mobility_data$weekday <- as.numeric(lubridate::wday(mobility_data$date))
  mobility_data$is_weekend <- lubridate::wday(mobility_data$date) %in% c(1, 7)
  
  # Remove missing values
  mobility_data <- mobility_data[complete.cases(mobility_data), ]
  
  # Prepare features (simple set for users)
  feature_cols <- c("hour", "weekday", "is_weekend")
  X_train <- mobility_data[, feature_cols, drop = FALSE]
  y_train <- mobility_data$n_trips
  
  # Train model (simplified)
  message("Step 4: Training ", model_type, " model...")
  if(model_type == "linear_regression") {
    model <- train_simple_linear_model(X_train, y_train)
  } else {
    model <- train_simple_random_forest(X_train, y_train)
  }
  
  # Create simple predictions (just for next few time periods)
  message("Step 5: Making predictions...")
  prediction_data <- create_simple_prediction_data(prediction_dates)
  
  # Make predictions
  X_predict <- prediction_data[, feature_cols, drop = FALSE]
  predictions <- predict(model, X_predict)
  
  # Combine results
  result <- data.frame(
    date = prediction_data$date,
    hour = prediction_data$hour,
    predicted_trips = as.numeric(predictions),
    model_type = model_type
  )
  
  # Create spatial map if requested and spatial zones provided
  if(create_map && !is.null(spatial_zones)) {
    message("Step 6: Creating spatial map of predictions...")
    
    # Aggregate predictions for mapping
    pred_summary <- result %>%
      dplyr::group_by(.data$hour) %>%
      dplyr::summarise(
        avg_predicted_trips = mean(.data$predicted_trips, na.rm = TRUE),
        total_predicted_trips = sum(.data$predicted_trips, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Create a simple version for mapping (assign to first few zones)
    if(nrow(pred_summary) > 0 && nrow(spatial_zones) > 0) {
      n_zones <- min(nrow(pred_summary), nrow(spatial_zones))
      map_data <- data.frame(
        id = spatial_zones$id[1:n_zones],
        predicted_trips = pred_summary$avg_predicted_trips[1:n_zones]
      )
      
      tryCatch({
        prediction_map <- create_spatial_map(
          map_data, spatial_zones, 
          map_type = "choropleth",
          title = paste("Predicted Mobility Patterns -", model_type)
        )
        
        result <- list(
          predictions = result,
          spatial_map = prediction_map,
          model_info = list(
            model_type = model_type,
            training_rows = nrow(X_train),
            features_used = feature_cols
          )
        )
        
        message("Step 6: Spatial map created successfully!")
        
      }, error = function(e) {
        message("Warning: Could not create spatial map: ", e$message)
      })
    }
  }
  
  message("Step 6: Prediction complete! Generated ", nrow(result), " predictions.")
  
  return(result)
}

#' Train a simple linear regression model 
#' @param X_train Training features
#' @param y_train Training target
#' @return Trained linear model
#' @keywords internal
train_simple_linear_model <- function(X_train, y_train) {
  # Combine features and target
  training_df <- data.frame(X_train, y = y_train)
  
  # Remove missing values
  training_df <- training_df[complete.cases(training_df), ]
  
  # Create simple formula
  feature_names <- names(X_train)
  formula_str <- paste("y ~", paste(feature_names, collapse = " + "))
  
  # Train model
  model <- lm(as.formula(formula_str), data = training_df)
  
  return(model)
}

#' Train a simple random forest model 
#' @param X_train Training features
#' @param y_train Training target
#' @return Trained random forest model
#' @keywords internal
train_simple_random_forest <- function(X_train, y_train) {
  if(!requireNamespace("randomForest", quietly = TRUE)) {
    stop("randomForest package required. Install with: install.packages('randomForest')")
  }
  
  # Handle missing values
  complete_rows <- complete.cases(X_train)
  X_train <- X_train[complete_rows, ]
  y_train <- y_train[complete_rows]
  
  # Memory management for memory-efficient processing
  if(nrow(X_train) > 1500) {
    message("Sampling 1500 rows for random forest training (user-friendly)...")
    sample_indices <- sample(nrow(X_train), 1500)
    X_train <- X_train[sample_indices, ]
    y_train <- y_train[sample_indices]
  }
  
  # Simple random forest with reduced complexity
  model <- randomForest::randomForest(
    x = X_train,
    y = y_train,
    ntree = 20,  # Small number of trees for speed
    nodesize = 15,  # Larger node size for simplicity
    maxnodes = 30  # Limit complexity
  )
  
  return(model)
}

#' Create simple prediction data 
#' @param prediction_dates Dates to predict
#' @return Simple prediction data frame
#' @keywords internal
create_simple_prediction_data <- function(prediction_dates) {
  # Create a simple prediction grid with basic time features
  prediction_data <- data.frame(
    date = rep(as.Date(prediction_dates), each = 24),
    hour = rep(0:23, length(prediction_dates))
  )
  
  # Add same features as training data
  prediction_data$weekday <- as.numeric(lubridate::wday(prediction_data$date))
  prediction_data$is_weekend <- lubridate::wday(prediction_data$date) %in% c(1, 7)
  
  return(prediction_data)
}

#' Simple anomaly detection for users 
#'
#' A simplified anomaly detection function focusing on statistical methods
#' that are easy to understand for users, with automatic spatial mapping
#' for spatial analysis applications.
#'
#' @param mobility_data Data frame with mobility data containing:
#'   \itemize{
#'     \item n_trips - Number of trips (numeric, required for anomaly detection)
#'     \item date - Date of observation (optional)
#'     \item id_origin - Origin zone identifier (optional, needed for spatial mapping)
#'     \item Other columns are preserved in output
#'   }
#' @param threshold Numeric. Z-score threshold for anomaly detection (default: 2.5).
#'   Common values:
#'   \itemize{
#'     \item 2.0 - More sensitive (detects more anomalies)
#'     \item 2.5 - Balanced (default, good for users)
#'     \item 3.0 - Conservative (detects fewer, strong anomalies only)
#'   }
#' @param spatial_zones sf object with spatial zones for creating maps (optional).
#'   Use \code{get_zones()} to obtain. Required for spatial visualization.
#' @param create_map Logical. Whether to create spatial map if spatial_zones provided 
#'   (default: TRUE). Set to FALSE for faster processing without visualization.
#' @return List containing:
#'   \itemize{
#'     \item anomalies - Data frame with original data plus anomaly flags
#'     \item spatial_map - ggplot2 choropleth map showing anomaly zones (if spatial_zones provided)
#'     \item summary - Summary statistics about detected anomalies
#'   }
#'   If no spatial_zones provided, returns only the anomalies data frame.
#' @details
#' This function uses statistical z-score method for anomaly detection:
#' \itemize{
#'   \item Calculates z-scores for trip counts (standardized values)
#'   \item Identifies observations with |z-score| > threshold as anomalies
#'   \item Creates spatial maps showing anomaly rates by zone
#'   \item Provides analytical messaging about the process
#' }
#' 
#' Z-scores represent how many standard deviations away from the mean each observation is.
#' This method is simple to understand and explain to users.
#' @export
#' @examples
#' \dontrun{
#' # Simple anomaly detection for users
#' mobility <- get_mobility(dates = "2023-01-01")
#' anomalies <- detect_simple_anomalies(mobility, threshold = 2.5)
#' summary(anomalies)
#' 
#' # With spatial mapping for analysis
#' zones <- get_zones("dist")
#' anomalies <- detect_simple_anomalies(
#'   mobility_data = mobility,
#'   threshold = 2.5,
#'   spatial_zones = zones,
#'   create_map = TRUE
#' )
#' 
#' # View anomalies and map
#' print(anomalies$summary)
#' print(anomalies$spatial_map)
#' head(anomalies$anomalies)
#' 
#' # Try different thresholds
#' strict_anomalies <- detect_simple_anomalies(mobility, threshold = 3.0)
#' loose_anomalies <- detect_simple_anomalies(mobility, threshold = 2.0)
#' }
detect_outliers <- function(mobility_data, threshold = 2.5, 
                                   spatial_zones = NULL, create_map = TRUE) {
  
  message("Step 1: Calculating z-scores for anomaly detection...")
  
  # Calculate z-scores (standardized values)
  z_scores <- abs(scale(mobility_data$n_trips))
  
  # Identify anomalies
  is_anomaly <- z_scores > threshold
  
  # Create result
  result <- data.frame(
    date = mobility_data$date,
    n_trips = mobility_data$n_trips,
    z_score = as.vector(z_scores),
    is_anomaly = is_anomaly,
    anomaly_type = ifelse(is_anomaly, "statistical_outlier", "normal")
  )
  
  # Add spatial information if available
  if("id_origin" %in% names(mobility_data)) {
    result$id_origin <- mobility_data$id_origin
  }
  if("id_destination" %in% names(mobility_data)) {
    result$id_destination <- mobility_data$id_destination
  }
  
  anomaly_count <- sum(is_anomaly, na.rm = TRUE)
  message("Step 2: Found ", anomaly_count, " anomalies (", 
          round(anomaly_count/nrow(result)*100, 1), "% of data)")
  
  # Create spatial map if requested and spatial zones provided
  if(create_map && !is.null(spatial_zones) && "id_origin" %in% names(result)) {
    message("Step 3: Creating spatial map of anomalies...")
    
    # Aggregate anomalies by zone for mapping
    anomaly_summary <- result %>%
      dplyr::group_by(.data$id_origin) %>%
      dplyr::summarise(
        anomaly_count = sum(.data$is_anomaly, na.rm = TRUE),
        total_observations = dplyr::n(),
        anomaly_rate = mean(.data$is_anomaly, na.rm = TRUE),
        avg_z_score = mean(.data$z_score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::rename(id = .data$id_origin)
    
    tryCatch({
      anomaly_map <- create_spatial_map(
        anomaly_summary, spatial_zones, 
        map_type = "choropleth",
        title = "Anomaly Detection by Zone"
      )
      
      result <- list(
        anomalies = result,
        spatial_map = anomaly_map,
        summary = list(
          total_anomalies = anomaly_count,
          anomaly_rate = round(anomaly_count/nrow(result)*100, 1),
          threshold_used = threshold,
          zones_analyzed = nrow(anomaly_summary)
        )
      )
      
      message("Step 3: Spatial map created successfully!")
      
    }, error = function(e) {
      message("Warning: Could not create spatial map: ", e$message)
    })
  }
  
  return(result)
}

#' Print method for simple predictions 
#' @param x Predictions object
#' @param ... Additional arguments
#' @export
print.simple_predictions <- function(x, ...) {
  cat("Simple Mobility Predictions \n")
  cat("========================================\n\n")
  
  cat("Model used:", x$model_type[1], "\n")
  cat("Total predictions:", nrow(x), "\n")
  cat("Prediction dates:", paste(unique(as.Date(x$date)), collapse = ", "), "\n")
  cat("Predicted trips range:", paste(round(range(x$predicted_trips, na.rm = TRUE), 1), collapse = " to "), "\n\n")
  
  cat("Sample predictions:\n")
  print(head(x, 10))
  
  invisible(x)
}
