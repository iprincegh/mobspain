test_that("Package configuration works", {
  # Test basic configuration
  expect_no_error(configure_mobspain())
  
  # Test configuration with custom cache directory
  temp_cache <- tempdir()
  expect_no_error(configure_mobspain(cache_dir = temp_cache))
  
  # Test configuration with parameters
  expect_no_error(configure_mobspain(max_cache_size = 100, parallel = FALSE))
})

test_that("Sample data is available", {
  # Test that sample_zones dataset exists and has correct structure
  data("sample_zones", package = "mobspain")
  expect_s3_class(sample_zones, "data.frame")
  expect_true("id" %in% names(sample_zones))
  expect_true("name" %in% names(sample_zones))
  expect_true("population" %in% names(sample_zones))
})

test_that("Mobility data function parameters validate correctly", {
  # Test that get_mobility validates input parameters
  expect_error(get_mobility(dates = "invalid-date"))
  expect_error(get_mobility(level = "invalid-level"))
  expect_error(get_mobility(max_rows = -1))
})

test_that("Mobility status function works", {
  # Test that mobspain_status returns appropriate information
  status <- mobspain_status()
  expect_s3_class(status, "mobspain_status")
  expect_true(is.list(status))
})

test_that("Validation functions work correctly", {
  # Test Spanish mobility data validation
  expect_error(validate_mitma_data(NULL))
  
  # Test with minimal valid data structure
  test_data <- data.frame(
    id_origin = c("28079", "08019"),
    id_destination = c("08019", "28079"),
    date = as.Date(c("2023-01-01", "2023-01-01")),
    n_trips = c(100, 150)
  )
  
  # Expect a warning about weekend/weekday distribution for minimal test data
  expect_warning(validate_mitma_data(test_data), "Unusual weekend/weekday data distribution")
})
