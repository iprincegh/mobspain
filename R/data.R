#' Sample Spatial Zones Dataset
#'
#' A sample dataset containing spatial zones with geographic boundaries for 
#' demonstration and testing purposes in the mobspain package.
#'
#' @format A data frame (sf object) with 3 rows and 5 variables:
#' \describe{
#'   \item{id}{Zone identifier (character)}
#'   \item{name}{Zone name (character)}
#'   \item{population}{Population count (numeric)}
#'   \item{geometry}{Spatial geometry (sf geometry column)}
#'   \item{area_km2}{Area in square kilometers (numeric)}
#' }
#'
#' @details
#' This dataset provides example spatial zones with point geometries that can be 
#' used for testing spatial analysis functions in the mobspain package. The zones 
#' include basic attributes like population and calculated area.
#'
#' @source Generated for demonstration purposes
#'
#' @examples
#' data(sample_zones)
#' 
#' # View the structure
#' str(sample_zones)
#' 
#' # Plot the zones
#' if(require(sf)) {
#'   plot(sample_zones$geometry)
#' }
#'
"sample_zones"
