#' @import glue
#' @import lubridate
#' @import dplyr
#' @import sf
#' @import ggplot2
#' @importFrom utils globalVariables head tail
#' @importFrom stats coef lm quantile reorder sd median weighted.mean aggregate as.formula complete.cases fitted frequency pnorm predict ts var
#' @importFrom rlang .data sym
#' @importFrom methods is
#' @importFrom scales percent
#' @importFrom digest digest
NULL

# Declare global variables to avoid R CMD check warnings
globalVariables(c(
  "id_origin", "n_trips", "id_destination", "internal_trips", 
  "total_trips", "containment", ".", "id", "lon", "lat", 
  "geometry", "sample_zones", "weekday", "date", "total_outflow",
  "total_inflow", "external_trips", "n_destinations", "net_flow",
  "connectivity_index", "area_km2", "trip_density", "internal_density",
  "z_score", "is_anomaly", "distance_km", "hour", "flow_type",
  "destination_name", "datetime", "name", "avg_outbound", "avg_inbound",
  "avg_internal", "unique_destinations", "unique_origins", "connected_zone",
  "daily_trips", "avg_daily_trips", "max_daily_trips", "min_daily_trips",
  "origin", "dest", "containment_index", "connectivity", "anomaly_count",
  "total_observations", "anomaly_rate", "anomaly_type", "predicted_trips",
  "avg_predicted_trips", "total_predicted_trips", "model_type", "lon_origin",
  "lat_origin", "lon_dest", "lat_dest", "is_weekend", "centroid"
))
