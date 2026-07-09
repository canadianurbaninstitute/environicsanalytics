# Tests for the new Data Extract endpoint code. These run entirely offline —
# they lock in the request-body shape from the sample API specification, since
# the endpoint itself is not yet live.

test_that("extract request body matches the sample specification", {
  body <- environicsanalytics:::.make_extract_request_body(
    geofence_ids = c("E12345", "E12346"),
    start_date = "2026-01-01",
    end_date = "2026-01-31",
    days_of_week = c("Mon", "Tue", "Wed", "Thu", "Fri"),
    time_of_day = "AllDay",
    dwell = "Any",
    target_set = list(
      list(
        targetGroupId = "group_1",
        segmentCodes = c("01", "02", "03")
      )
    )
  )

  expect_equal(body$geofenceIds, list("E12345", "E12346"))
  expect_equal(body$startDate, "2026-01-01")
  expect_equal(body$endDate, "2026-01-31")
  expect_equal(body$daysOfWeek, list("Mon", "Tue", "Wed", "Thu", "Fri"))
  expect_equal(body$timeOfDay, "AllDay")
  expect_equal(body$dwell, "Any")
  expect_equal(body$targetSet[[1]]$targetGroupId, "group_1")
  expect_equal(body$targetSet[[1]]$segmentCodes, c("01", "02", "03"))

  # JSON serialization should match the sample spec's field names exactly
  json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  expect_true(grepl('"geofenceIds":\\["E12345","E12346"\\]', json))
  expect_true(grepl('"startDate":"2026-01-01"', json))
  expect_true(grepl('"targetSet":\\[\\{"targetGroupId":"group_1"', json))
})

test_that("optional fields are omitted when not provided", {
  body <- environicsanalytics:::.make_extract_request_body(
    geofence_ids = "E12345",
    start_date = "2026-01-01",
    end_date = "2026-01-31"
  )

  expect_named(body, c("geofenceIds", "startDate", "endDate"))
})

test_that("extract request body validation catches bad input", {
  ok <- list(
    geofence_ids = "E12345",
    start_date = "2026-01-01",
    end_date = "2026-01-31"
  )

  # Missing geofence IDs
  expect_error(
    environicsanalytics:::.make_extract_request_body(NULL, ok$start_date, ok$end_date),
    "geofence_ids must be provided"
  )

  # Over the 300-geofence-per-request limit
  expect_error(
    environicsanalytics:::.make_extract_request_body(
      paste0("E", 1:301), ok$start_date, ok$end_date
    ),
    "at most 300 geofences"
  )

  # Duplicate geofence IDs
  expect_error(
    environicsanalytics:::.make_extract_request_body(
      c("E12345", "E12345"), ok$start_date, ok$end_date
    ),
    "duplicate"
  )

  # v4-style datetimes are rejected — this endpoint is date-only
  expect_error(
    environicsanalytics:::.make_extract_request_body(
      ok$geofence_ids, "2026-01-01 00:00:00", ok$end_date
    ),
    "YYYY-MM-DD"
  )

  # Reversed date range
  expect_error(
    environicsanalytics:::.make_extract_request_body(
      ok$geofence_ids, "2026-02-01", "2026-01-01"
    ),
    "on or before"
  )

  # Invalid day names
  expect_error(
    environicsanalytics:::.make_extract_request_body(
      ok$geofence_ids, ok$start_date, ok$end_date,
      days_of_week = c("Monday", "Tue")
    ),
    "Invalid days_of_week"
  )

  # Malformed target set
  expect_error(
    environicsanalytics:::.make_extract_request_body(
      ok$geofence_ids, ok$start_date, ok$end_date,
      target_set = list(list(segmentCodes = c("01")))
    ),
    "targetGroupId"
  )
})

test_that("endpoint URLs follow the new path scheme", {
  urls <- environicsanalytics:::.extract_endpoints("ca", "2026", "abc-123")

  expect_equal(
    urls$submit,
    "https://api.environicsanalytics.com/mobilescapes/ca/2026/extracts/csv"
  )
  expect_equal(
    urls$status,
    "https://api.environicsanalytics.com/mobilescapes/ca/2026/extracts/abc-123/status"
  )
  expect_equal(
    urls$results,
    "https://api.environicsanalytics.com/mobilescapes/ca/2026/extracts/abc-123/results"
  )
})

test_that("extract CSV chunks are merged by report name", {
  download_dir <- file.path(tempdir(), "extract-merge-test-download")
  output_dir <- file.path(tempdir(), "extract-merge-test-output")
  unlink(c(download_dir, output_dir), recursive = TRUE)
  dir.create(download_dir, recursive = TRUE)

  readr::write_csv(data.frame(a = 1, b = "x"), file.path(download_dir, "report_1.csv"))
  readr::write_csv(data.frame(a = 2, b = "y"), file.path(download_dir, "report_2.csv"))
  readr::write_csv(data.frame(c = 3), file.path(download_dir, "other.csv"))

  result <- environicsanalytics:::.merge_extract_csvs(download_dir, output_dir, "test")

  expect_named(result, c("other", "report"), ignore.order = TRUE)
  merged <- readr::read_csv(result$report, show_col_types = FALSE)
  expect_equal(nrow(merged), 2)

  # Raw chunks preserved, temp download dir removed
  expect_length(list.files(file.path(output_dir, "raw")), 3)
  expect_false(dir.exists(download_dir))

  unlink(output_dir, recursive = TRUE)
})
