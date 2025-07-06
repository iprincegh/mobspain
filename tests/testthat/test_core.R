test_that("Database connection handles errors gracefully", {
  # Unset data directory
  options(mobspain.data_dir = NULL)

  # Test without initialization
  expect_error(connect_mobility_db(), "Run init_data_dir")

  # Test with valid directory
  test_dir <- tempfile()
  dir.create(test_dir)
  init_data_dir(test_dir)

  # Should return connection
  con <- suppressWarnings(connect_mobility_db())
  expect_s4_class(con, "DBIConnection")
  DBI::dbDisconnect(con, shutdown = TRUE)

  unlink(test_dir, recursive = TRUE)
})

test_that("Mobility data retrieval works", {
  skip_on_cran()
  skip_if_offline()

  test_dir <- tempfile()
  init_data_dir(test_dir)

  # Use small date range
  dates <- c("2023-01-01", "2023-01-01")

  # Use fallback method
  data <- suppressWarnings(
    get_mobility_matrix(dates, level = "dist")
  )

  expect_s3_class(data, "data.frame")
  expect_true("n_trips" %in% names(data))

  unlink(test_dir, recursive = TRUE)
})
