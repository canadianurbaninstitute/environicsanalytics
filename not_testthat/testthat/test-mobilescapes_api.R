test_that("credentials initialize properly", {
  dotenv::load_dot_env(testthat::test_path("mobilescapes-test-data", ".env"))
  init_credentials()
})

test_that("you can pull a geography under 5M sqft", {
  dotenv::load_dot_env(testthat::test_path("mobilescapes-test-data", ".env"))
  init_credentials()
  geojson <- "tests/testthat/mobilescapes-test-data/oshawa_bia.geojson"
  dt <- c("2025-01-01 00:00:00", "2025-01-01 12:00:00")
  pull_mobilescapes(
    start_datetime = dt[1],
    end_datetime = dt[2],
    geojson = geojson,
    aggregate_polygons = FALSE,
    append_prizm_segmentation = "prizm"
  )
})

test_that("you can pull a geography over 5M sqft", {
  dotenv::load_dot_env(testthat::test_path("mobilescapes-test-data", ".env"))
  init_credentials()
  geojson <- "tests/testthat/mobilescapes-test-data/downtown_moncton_bia.geojson"
  dt <- c("2025-01-01 00:00:00", "2025-01-01 12:00:00")
  pull_mobilescapes(
    start_datetime = dt[1],
    end_datetime = dt[2],
    geojson = geojson,
    aggregate_polygons = FALSE,
    append_prizm_segmentation = "prizm"
  )
})
