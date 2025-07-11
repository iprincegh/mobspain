.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "mobspain v", utils::packageVersion("mobspain"), " loaded.\n",
    "Use configure_mobspain() to set up caching and parallel processing.\n",
    "Use mobspain_status() to check package configuration.\n",
    "Note: CSV access is used by default for reliability.\n",
    "For help getting started, run: ?mobspain or vignette('introduction', package = 'mobspain')"
  )
}

# Global variable declarations for R CMD check
utils::globalVariables(c(
  "total_origins",
  "total_destinations"
))

.onLoad <- function(libname, pkgname) {
  # Set default options if not already set
  if(is.null(getOption("mobspain.cache_dir"))) {
    options(mobspain.cache_dir = tempdir())
  }
  if(is.null(getOption("mobspain.max_cache_size"))) {
    options(mobspain.max_cache_size = 500)  # 500 MB default
  }
  if(is.null(getOption("mobspain.parallel"))) {
    options(mobspain.parallel = FALSE)
  }
  if(is.null(getOption("mobspain.cache_max_age_days"))) {
    options(mobspain.cache_max_age_days = 7)
  }
  if(is.null(getOption("mobspain.use_csv"))) {
    options(mobspain.use_csv = TRUE)  # Default to CSV access for reliability
  }
}
