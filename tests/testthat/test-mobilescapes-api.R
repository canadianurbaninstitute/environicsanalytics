test_that("credentials initialize properly", {
  readRenviron(testthat::test_path("mobilescapes-test-data", ".env"))
  init_credentials()
})

test_that("you can discover geofences and pull an origins extract", {
  readRenviron(testthat::test_path("mobilescapes-test-data", ".env"))
  init_credentials()

  # NOTE: replace with a real vintage and a filter matching your BIAs.
  vintage <- "2026"

  geofences <- discover_mobilescapes_geofences(
    filter_definition = "PRCDCSD_NAME IN ('Oshawa, ON (CY)')",
    page_size = 25,
    vintage = vintage
  )

  pull_mobilescapes(
    geofence_ids = geofences$geofenceId,
    start_date = "2025-01-01",
    end_date = "2025-01-31",
    vintage = vintage
  )
})

test_that("you can request an origins report", {
  readRenviron(testthat::test_path("mobilescapes-test-data", ".env"))
  init_credentials()

  get_mobilescapes_origins(
    geofence_ids = c("E12345", "E12346"),
    start_date = "2025-01-01",
    end_date = "2025-01-31",
    geo_level_code = "FSA",
    vintage = "2026"
  )
})
