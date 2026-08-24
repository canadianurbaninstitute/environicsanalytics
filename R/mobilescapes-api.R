# MobileScapes API v5 base URL. Country and vintage are path segments,
# e.g. https://api.environicsanalytics.com/mobilescapes/v5/ca/2026/origins
.MOBILESCAPES_BASE_URL <- "https://api.environicsanalytics.com/mobilescapes/v5"

# Request Helpers ###############################################################

#' Build a MobileScapes API URL
#'
#' @param country Character. 2-digit country code (e.g. "ca" for Canada).
#' @param vintage Character or numeric. Dataset vintage (e.g. "2026").
#' @param ... Additional path segments (e.g. "origins", "extracts", "csv").
#'
#' @return Character. Full request URL.
#' @keywords internal
.mobilescapes_url <- function(country, vintage, ...) {
  paste(.MOBILESCAPES_BASE_URL, country, vintage, ..., sep = "/")
}

#' Build a MobileScapes httr2 Request
#'
#' Attaches the bearer token and standard error handling. If `body` is
#' provided the request becomes a POST (httr2 sets this automatically);
#' otherwise it is a GET. If `query` is provided, it is added as URL
#' query parameters.
#'
#' @param url Character. Full request URL.
#' @param body List. Optional. JSON request body.
#' @param query List. Optional. Named list of query parameters.
#'
#' @return An httr2 request object.
#' @keywords internal
.build_request <- function(url, body = NULL, query = NULL) {
  bearer_token <- .quietly_get_bearer_token()

  req <- httr2::request(url) |>
    httr2::req_auth_bearer_token(bearer_token) |>
    # v5 rate-limits aggressively and returns a bare {"status":429} with no
    # Retry-After header, so back off exponentially on our own.
    httr2::req_retry(
      max_tries = 5,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503),
      backoff = function(attempt) min(60, 2^attempt)
    ) |>
    httr2::req_error(body = function(resp) {
      error_content <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
      paste0(
        "API Error: ",
        if (!is.null(error_content$errorCode)) paste0("[", error_content$errorCode, "] "),
        if (!is.null(error_content$message)) error_content$message else httr2::resp_body_string(resp)
      )
    })

  if (!is.null(query)) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }

  if (!is.null(body)) {
    req <- req |> httr2::req_body_json(body)
  }

  req
}

#' Perform a MobileScapes Request and Parse the JSON Response
#'
#' @param req An httr2 request object.
#'
#' @return Parsed JSON response body (as a list).
#' @keywords internal
.perform_request <- function(req) {
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Build a MobileScapes Report Request Body
#'
#' Constructs the shared request body fields used by the Origins,
#' Destinations, Destination Summary, and Origins Extract endpoints.
#'
#' @param geofence_ids Character vector. EA geofence IDs.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param days_of_week Character vector. Optional. "Mon" through "Sun".
#' @param time_of_day Character. Optional. One of "AllDay", "Morning",
#'   "Afternoon", "Evening" (verified against the API; hour boundaries are
#'   undocumented). This is a filter, not a breakdown - each bucket needs its
#'   own request, and the three named buckets cover ~99% of "AllDay", leaving
#'   an overnight residual with no selectable bucket.
#' @param dwell Character. Optional. One of "Any", "Short", "Medium", "Long"
#'   (verified against the API; minute thresholds are undocumented).
#' @param target_set List. Optional. List of `list(targetGroupId = ..., segmentCodes = ...)`.
#' @param geo_level_code Character. Optional. Geographic level for aggregation.
#'   One of "PRCDDA", "PRCDADA", "FSA", "PRCDCSD", "PRCD", "CMACA", "PR".
#'
#' @return List. Request body.
#' @keywords internal
.report_body <- function(
    geofence_ids,
    start_date,
    end_date,
    days_of_week = NULL,
    time_of_day = NULL,
    dwell = NULL,
    target_set = NULL,
    geo_level_code = NULL
) {
  if (is.null(geofence_ids) || length(geofence_ids) == 0) {
    stop("Error: geofence_ids must be provided (a character vector of EA geofence IDs)")
  }

  # as.list() keeps these as JSON arrays. httr2 serialises with
  # auto_unbox = TRUE, so a length-1 vector would otherwise become a bare
  # string and the API rejects it ("must be an array of JSON strings").
  body <- list(
    geofenceIds = as.list(as.character(geofence_ids)),
    startDate = start_date,
    endDate = end_date
  )

  if (!is.null(geo_level_code)) body$geoLevelCode <- geo_level_code
  if (!is.null(days_of_week)) body$daysOfWeek <- as.list(as.character(days_of_week))
  if (!is.null(time_of_day)) body$timeOfDay <- time_of_day
  if (!is.null(dwell)) body$dwell <- dwell
  if (!is.null(target_set)) body$targetSet <- target_set

  body
}

#' Reject Legacy v4 Arguments
#'
#' `pull_mobilescapes()` and `test_query_mobilescapes()` kept their v4 names
#' across the v5 rewrite even though every parameter except `geofence_ids`
#' changed shape or was removed outright. Without this, passing old-style
#' arguments (e.g. `geojson = ...`) would only surface R's generic "unused
#' argument" error - this gives a specific, per-argument migration hint
#' instead, and does it before any network call is made.
#'
#' @param dots The caller's `...`, as `list(...)`.
#' @param fn_name Character. Name of the calling function, for the message.
#'
#' @keywords internal
.reject_legacy_args <- function(dots, fn_name) {
  if (length(dots) == 0) return(invisible(NULL))

  hints <- list(
    start_datetime = "removed - use `start_date` (\"YYYY-MM-DD\", no time component)",
    end_datetime   = "removed - use `end_date` (\"YYYY-MM-DD\", no time component)",
    geojson        = "removed - MobileScapes v5 only accepts EA Geofence Library IDs; resolve them with discover_mobilescapes_geofences() and pass `geofence_ids`",
    wkt_list       = "removed - MobileScapes v5 only accepts EA Geofence Library IDs; resolve them with discover_mobilescapes_geofences() and pass `geofence_ids`",
    use_weights    = "removed - v5 always applies calibrated weighting",
    aggregate_polygons = "removed - v5's origins report always pools across all `geofence_ids`; destinations and the extract never pool",
    aggregate_polygon_name = "removed - use `output_name` instead",
    append_prizm_segmentation = "removed - no v5 report or extract returns a PRIZM segment",
    daily_time_filter = "removed - use `days_of_week` and `time_of_day` (\"AllDay\"/\"Morning\"/\"Afternoon\"/\"Evening\" only - coarser than v4's day-parts)",
    ping_filter    = "removed - `dwell` (\"Any\"/\"Short\"/\"Medium\"/\"Long\") is a different, new filter (visit duration), not a replacement",
    report_type    = "removed - the v5 extract only produces Origins data; \"celcdl\"/\"geofencepings\" have no v5 equivalent",
    data_vintage   = "renamed to `vintage`, and now selects the API dataset itself rather than an appended output column"
  )

  offending <- intersect(names(dots), names(hints))
  if (length(offending) > 0) {
    stop(
      sprintf(
        "%s(): the following argument(s) are from the MobileScapes v4 API and no longer exist:\n%s",
        fn_name,
        paste(sprintf("  - `%s`: %s", offending, unlist(hints[offending])), collapse = "\n")
      ),
      call. = FALSE
    )
  }

  unknown <- setdiff(names(dots), names(hints))
  if (length(unknown) > 0) {
    stop(sprintf("%s(): unused argument(s): %s", fn_name, paste(unknown, collapse = ", ")), call. = FALSE)
  }

  invisible(NULL)
}

# Config & Geofence Discovery ###################################################

#' Get MobileScapes Report Configuration
#'
#' Returns dataset constraints for the given country and vintage, including
#' the allowed date range and the maximum number of geofence IDs / months
#' that can be requested at once.
#'
#' @param country Character. 2-digit country code. Default "ca".
#' @param vintage Character or numeric. Dataset vintage (e.g. "2026").
#'
#' @return List with keys such as `startDate`, `endDate`, `h3endDate`,
#'   `maxGeofenceIds`, `maxDateRangeMonths`.
#'
#' @export
get_mobilescapes_config <- function(country = "ca", vintage) {
  url <- .mobilescapes_url(country, vintage, "config")
  req <- .build_request(url)
  .perform_request(req)
}

#' Discover MobileScapes Geofences
#'
#' Searches the EA Geofence Library for geofences matching the given filter,
#' returning IDs that can be used in other MobileScapes API calls.
#'
#' @param filter_definition Character. Optional. Filter expression, e.g.
#'   `"PRCDCSD_NAME IN ('Toronto, ON (C)') AND PR_NAME IN ('Ontario')"`.
#' @param page Numeric. Page number to start from. Default 1.
#' @param page_size Numeric. Results per page. Default 100. The API does not
#'   document a maximum; if a large value errors, lower it.
#' @param sort_by Character. One of GEOFENCE_ID, GEOFENCE_NAME, PRCDCSD_NAME,
#'   CMACA_NAME, PR_NAME, BANNER, PARENT_COMPANY, CATEGORY, SUB_CATEGORY,
#'   GEOFENCE_TYPE, IS_PRIMARY_POLYGON, GEOFENCE_SQUARE_FOOTAGE. Default "GEOFENCE_ID".
#' @param sort_direction Character. "asc" or "desc". Default "asc".
#' @param all_pages Logical. If TRUE (default) keep requesting successive
#'   pages until the API returns a short page, so the full matching set is
#'   returned. If FALSE, return only the single page given by `page`.
#' @param max_pages Numeric. Safety cap on the number of pages fetched when
#'   `all_pages = TRUE`. Default 100. A warning is issued if it is hit.
#' @param country Character. 2-digit country code. Default "ca".
#' @param vintage Character or numeric. Dataset vintage (e.g. "2026").
#'
#' @return Data frame with columns `geofenceId` and `geofenceName`.
#'
#' @export
discover_mobilescapes_geofences <- function(
    filter_definition = NULL,
    page = 1,
    page_size = 100,
    sort_by = "GEOFENCE_ID",
    sort_direction = "asc",
    all_pages = TRUE,
    max_pages = 100,
    country = "ca",
    vintage
) {
  url <- .mobilescapes_url(country, vintage, "geofences")

  fetch_page <- function(page_number) {
    query <- list(
      Page = page_number,
      PageSize = page_size,
      SortBy = sort_by,
      SortDirection = sort_direction
    )
    if (!is.null(filter_definition)) query$FilterDefinition <- filter_definition

    .perform_request(.build_request(url, query = query))
  }

  if (!all_pages) {
    return(dplyr::bind_rows(fetch_page(page)$items))
  }

  # The API reports `page`/`pageSize` but no total count, so page until a
  # short (or empty) page comes back. Without this the caller silently gets
  # only the first page_size geofences for the filter.
  collected <- list()
  pages_fetched <- 0

  repeat {
    result <- fetch_page(page + pages_fetched)
    items <- result$items
    pages_fetched <- pages_fetched + 1

    if (length(items) > 0) {
      collected[[length(collected) + 1]] <- dplyr::bind_rows(items)
    }

    if (length(items) < page_size) break

    if (pages_fetched >= max_pages) {
      warning(
        "discover_mobilescapes_geofences(): stopped at max_pages = ", max_pages,
        " (", sum(vapply(collected, nrow, integer(1))), " geofences). ",
        "Results may be truncated - raise max_pages or narrow filter_definition.",
        call. = FALSE
      )
      break
    }
  }

  if (length(collected) == 0) {
    return(data.frame(geofenceId = character(0), geofenceName = character(0)))
  }

  dplyr::bind_rows(collected)
}

# Synchronous Report Endpoints ###################################################

#' Get MobileScapes Origins Visits Report
#'
#' Returns aggregated origin visit counts showing where visitors are coming
#' from to visit the selected geofence(s), grouped by the requested
#' geographic level.
#'
#' Note: results are pooled across all `geofence_ids` in the request - the
#' response carries no per-geofence breakdown. For a per-area origin profile,
#' call this once per area.
#'
#' @param geofence_ids Character vector. EA geofence IDs.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param geo_level_code Character. Required. Geographic level to group origins
#'   by. Allowed values, finest to coarsest (verified against the v5 API):
#'   \itemize{
#'     \item `"PRCDDA"`   - Dissemination Area (finest available)
#'     \item `"PRCDADA"`  - Aggregate Dissemination Area
#'     \item `"FSA"`      - Forward Sortation Area (first 3 postal characters)
#'     \item `"PRCDCSD"`  - Census Subdivision (municipality)
#'     \item `"PRCD"`     - Census Division
#'     \item `"CMACA"`    - Census Metropolitan Area / Census Agglomeration
#'     \item `"PR"`       - Province
#'   }
#'   This report returns aggregated counts only, one level per request. For
#'   coordinates, postal code, census tract, or a per-geofence breakout, use
#'   the CSV extract via `pull_mobilescapes()` instead - it still carries
#'   `LATITUDE`/`LONGITUDE` and the full geography hierarchy.
#' @param days_of_week Character vector. Optional. e.g. c("Sat", "Sun").
#' @param time_of_day Character. Optional. One of "AllDay", "Morning",
#'   "Afternoon", "Evening". Default "AllDay".
#' @param dwell Character. Optional. One of "Any", "Short", "Medium",
#'   "Long". Default "Any".
#' @param target_set List. Optional. List of `list(targetGroupId = ..., segmentCodes = ...)`.
#' @param country Character. 2-digit country code. Default "ca".
#' @param vintage Character or numeric. Dataset vintage (e.g. "2026").
#'
#' @return Data frame with columns `geoCode` and `visits`.
#'
#' @export
get_mobilescapes_origins <- function(
    geofence_ids,
    start_date,
    end_date,
    geo_level_code,
    days_of_week = NULL,
    time_of_day = "AllDay",
    dwell = "Any",
    target_set = NULL,
    country = "ca",
    vintage
) {
  body <- .report_body(
    geofence_ids, start_date, end_date,
    days_of_week = days_of_week, time_of_day = time_of_day, dwell = dwell,
    target_set = target_set, geo_level_code = geo_level_code
  )

  url <- .mobilescapes_url(country, vintage, "origins")
  req <- .build_request(url, body)
  result <- .perform_request(req)

  dplyr::bind_rows(result$data)
}

#' Get MobileScapes Destination Visits Report
#'
#' Returns visit metrics for each destination geofence, including total
#' visits, optional year-over-year change, and coordinates for mapping.
#'
#' @param geofence_ids Character vector. EA geofence IDs.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param days_of_week Character vector. Optional. "Mon" through "Sun".
#' @param time_of_day Character. Optional. One of "AllDay", "Morning",
#'   "Afternoon", "Evening". Default "AllDay".
#' @param dwell Character. Optional. One of "Any", "Short", "Medium",
#'   "Long". Default "Any".
#' @param target_set List. Optional. List of `list(targetGroupId = ..., segmentCodes = ...)`.
#' @param country Character. 2-digit country code. Default "ca".
#' @param vintage Character or numeric. Dataset vintage (e.g. "2026").
#'
#' @return Data frame with columns `geofenceId`, `visits`, `percentChange`,
#'   `latitude`, `longitude`.
#'
#' @export
get_mobilescapes_destinations <- function(
    geofence_ids,
    start_date,
    end_date,
    days_of_week = NULL,
    time_of_day = "AllDay",
    dwell = "Any",
    target_set = NULL,
    country = "ca",
    vintage
) {
  body <- .report_body(
    geofence_ids, start_date, end_date,
    days_of_week = days_of_week, time_of_day = time_of_day, dwell = dwell,
    target_set = target_set
  )

  url <- .mobilescapes_url(country, vintage, "destinations")
  req <- .build_request(url, body)
  result <- .perform_request(req)

  dplyr::bind_rows(result$data)
}

#' Get MobileScapes Destination Summary Report
#'
#' Returns a detailed summary for one or more destination geofences,
#' including visit totals, weekday/weekend split, visitor origins,
#' top audience segments, visit breakdowns, and demographics.
#'
#' @param geofence_ids Character vector. EA geofence IDs.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param days_of_week Character vector. Optional. "Mon" through "Sun".
#' @param time_of_day Character. Optional. One of "AllDay", "Morning",
#'   "Afternoon", "Evening". Default "AllDay".
#' @param dwell Character. Optional. One of "Any", "Short", "Medium",
#'   "Long". Default "Any".
#' @param country Character. 2-digit country code. Default "ca".
#' @param vintage Character or numeric. Dataset vintage (e.g. "2026").
#'
#' @return List with keys `visitSummary`, `weekdayWeekendSummary`,
#'   `originSummary`, `topSegments`, `visitBreakdowns`, `demographics`.
#'   Returned as a list (not flattened) since the structure is nested.
#'
#' @export
get_mobilescapes_destination_summary <- function(
    geofence_ids,
    start_date,
    end_date,
    days_of_week = NULL,
    time_of_day = "AllDay",
    dwell = "Any",
    country = "ca",
    vintage
) {
  body <- .report_body(
    geofence_ids, start_date, end_date,
    days_of_week = days_of_week, time_of_day = time_of_day, dwell = dwell
  )

  url <- .mobilescapes_url(country, vintage, "destinations", "summary")
  req <- .build_request(url, body)
  .perform_request(req)
}

#' Get MobileScapes Related Visits Report
#'
#' Returns insights on where else visitors went in addition to visiting the
#' selected location, broken down by month. This endpoint supports a single
#' geofence ID per request.
#'
#' @param geofence_id Character. A single EA geofence ID.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param country Character. 2-digit country code. Default "ca".
#' @param vintage Character or numeric. Dataset vintage (e.g. "2026").
#'
#' @return List with keys `data` (per-month related-visit slices) and
#'   `slices` (slice key / report month lookup). Returned as a list (not
#'   flattened) since the structure is nested.
#'
#' @export
get_mobilescapes_related_visits <- function(
    geofence_id,
    start_date,
    end_date,
    country = "ca",
    vintage
) {
  if (length(geofence_id) != 1) {
    stop("Error: get_mobilescapes_related_visits() accepts exactly one geofence_id")
  }

  body <- list(
    geofenceIds = list(geofence_id),
    startDate = start_date,
    endDate = end_date
  )

  url <- .mobilescapes_url(country, vintage, "relatedvisits")
  req <- .build_request(url, body)
  .perform_request(req)
}

# Origins Extract (Async CSV) Helper Functions ###################################

#' Submit a MobileScapes Origins Extract Request
#'
#' @keywords internal
.submit_origins_extract <- function(
    geofence_ids, start_date, end_date, days_of_week, time_of_day, dwell,
    country, vintage
) {
  body <- .report_body(
    geofence_ids, start_date, end_date,
    days_of_week = days_of_week, time_of_day = time_of_day, dwell = dwell
  )

  url <- .mobilescapes_url(country, vintage, "extracts", "csv")
  req <- .build_request(url, body)
  .perform_request(req)
}

#' Get MobileScapes Extract Request Status
#'
#' @keywords internal
.get_extract_status <- function(request_id, country, vintage) {
  url <- .mobilescapes_url(country, vintage, "extracts", request_id, "status")
  req <- .build_request(url)
  .perform_request(req)
}

#' Get MobileScapes Extract Result Download Info
#'
#' Retrieves Azure storage details for a completed extract request.
#'
#' @keywords internal
.get_extract_result <- function(request_id, country, vintage) {
  cat("Getting result information...\n")

  url <- .mobilescapes_url(country, vintage, "extracts", request_id, "result")
  req <- .build_request(url)
  .perform_request(req)
}

#' Download MobileScapes Extract Files
#'
#' v5 serves extract output from a custom CDN host
#' (`https://cdn.environicsanalytics.com`) with the container nested under a
#' path prefix, rather than from `<account>.blob.core.windows.net`. AzureStor
#' infers the storage service from the hostname and rejects that URL with
#' "Unknown endpoint service", so the container is read directly over HTTP
#' using the pre-signed `blobList` URL the API returns.
#'
#' @keywords internal
.download_extract_files <- function(request_id, country, vintage, output_dir = "temp") {
  cat("\n========================================\n")
  cat("Downloading results...\n")
  cat("========================================\n")

  result_info <- .get_extract_result(request_id, country, vintage)

  if (is.null(result_info)) {
    cat("ERROR: Could not get result information\n")
    return(NULL)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }

  # The container's real base URL is only discoverable from blobList - it
  # carries the path prefix that storageUrl and containerName omit.
  container_base <- sub("\\?.*$", "", result_info$blobList)

  cat("Listing available files...\n")
  blob_names <- .list_extract_blobs(result_info$blobList)

  if (length(blob_names) == 0) {
    cat("WARNING: No files found in container\n")
    return(NULL)
  }

  cat("Found", length(blob_names), "file(s) to download\n")

  downloaded_files <- c()

  for (i in seq_along(blob_names)) {
    blob_name <- blob_names[i]
    output_file <- file.path(output_dir, basename(blob_name))

    cat(sprintf("  [%d/%d] Downloading: %s\n", i, length(blob_names), blob_name))

    blob_url <- paste0(container_base, "/", utils::URLencode(blob_name),
                       "?", result_info$sasToken)

    ok <- tryCatch({
      httr2::request(blob_url) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform(path = output_file)
      TRUE
    }, error = function(e) {
      cat("    ERROR downloading", blob_name, ":", conditionMessage(e), "\n")
      FALSE
    })

    if (ok) downloaded_files <- c(downloaded_files, output_file)
  }

  cat("\n========================================\n")
  cat("Download complete! Downloaded", length(downloaded_files), "file(s)\n")
  cat("========================================\n")

  downloaded_files
}

#' List Blob Names in an Extract Container
#'
#' Reads the Azure container listing XML from the pre-signed `blobList` URL,
#' following `NextMarker` continuations.
#'
#' @param blob_list_url Character. The `blobList` URL from the result payload.
#'
#' @return Character vector of blob names.
#' @keywords internal
.list_extract_blobs <- function(blob_list_url) {
  names_found <- character(0)
  marker <- NULL

  repeat {
    url <- if (is.null(marker)) {
      blob_list_url
    } else {
      paste0(blob_list_url, "&marker=", utils::URLencode(marker, reserved = TRUE))
    }

    xml <- httr2::request(url) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform() |>
      httr2::resp_body_string()

    hits <- regmatches(xml, gregexpr("<Name>[^<]*</Name>", xml))[[1]]
    names_found <- c(names_found, gsub("</?Name>", "", hits))

    nm <- regmatches(xml, regexpr("<NextMarker>[^<]*</NextMarker>", xml))
    marker <- if (length(nm) == 0) NULL else gsub("</?NextMarker>", "", nm)
    if (is.null(marker) || !nzchar(marker)) break
  }

  unique(names_found)
}

# Data Processing Functions ######################################################

#' Merge Extract Result Chunks into a Consolidated CSV
#'
#' Combines multiple CSV chunks from an Origins Extract into a single
#' consolidated file. Moves raw chunks to a 'raw' subdirectory.
#'
#' @keywords internal
.merge_extract_chunks <- function(download_dir, output_dir = "output", output_name) {
  cat("\n========================================\n")
  cat("Merging API result chunks...\n")
  cat("========================================\n")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created directory:", output_dir, "\n")
  }

  raw_dir <- file.path(output_dir, "raw")
  if (!dir.exists(raw_dir)) {
    dir.create(raw_dir, recursive = TRUE)
    cat("Created raw files directory in output directory:", raw_dir, "\n")
  }

  # Find all CSV chunks in download directory
  chunk_files <- list.files(download_dir, pattern = "\\.csv(\\.gz)?$", full.names = TRUE)
  cat("Found", length(chunk_files), "chunk file(s)\n")

  # Move files to raw directory
  for (f in chunk_files) {
    file.rename(f, file.path(raw_dir, basename(f)))
  }

  cat("All files moved to raw directory\n")

  chunk_files_raw <- list.files(raw_dir, pattern = "\\.csv(\\.gz)?$", full.names = TRUE)

  if (length(chunk_files_raw) == 0) {
    cat("No chunk files to process\n")
    unlink(download_dir, recursive = TRUE)
    return(NULL)
  }

  cat("Processing", length(chunk_files_raw), "chunk file(s)...\n")
  chunk_data <- lapply(chunk_files_raw, function(f) {
    readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  })
  combined <- dplyr::bind_rows(chunk_data)
  cat("Combined data:", nrow(combined), "rows\n")

  cat("\nWriting consolidated CSV file...\n")
  output_file <- file.path(output_dir, paste0(output_name, ".csv"))
  readr::write_csv(combined, output_file)
  cat("Saved file:", output_file, "\n")

  cat("\n========================================\n")
  cat("Merge complete!\n")
  cat("========================================\n")

  unlink(download_dir, recursive = TRUE)
  rm(chunk_data, combined)
  gc()

  output_file
}

# Core API Calling Functions ######################################################

#' Create MobileScapes Origins Extract "Dry" Request
#'
#' Shows exactly what httr2 package will send to the Environics API
#' without sending anything. Primarily for debugging.
#'
#' @param geofence_ids Character vector. EA geofence IDs.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param days_of_week Character vector. Optional. "Mon" through "Sun".
#' @param time_of_day Character. Optional. One of "AllDay", "Morning",
#'   "Afternoon", "Evening". Default "AllDay".
#' @param dwell Character. Optional. One of "Any", "Short", "Medium",
#'   "Long". Default "Any".
#' @param country Character. 2-digit country code. Default "ca".
#' @param vintage Character or numeric. Dataset vintage (e.g. "2026").
#'
#' @param ... Not used. Present only so that passing a removed v4 argument
#'   (e.g. `geojson`, `use_weights`, `daily_time_filter`) fails immediately
#'   with a migration hint instead of a generic "unused argument" error.
#'
#' @return Invisibly returns NULL. Outputs dry run to "test_mobilescapes_query.txt".
#'
#' @export
test_query_mobilescapes <- function(
    geofence_ids,
    start_date,
    end_date,
    days_of_week = NULL,
    time_of_day = "AllDay",
    dwell = "Any",
    country = "ca",
    vintage,
    ...
) {
  .reject_legacy_args(list(...), "test_query_mobilescapes")

  cat("Saving DRY MobileScapes request...\n")

  body <- .report_body(
    geofence_ids, start_date, end_date,
    days_of_week = days_of_week, time_of_day = time_of_day, dwell = dwell
  )

  url <- .mobilescapes_url(country, vintage, "extracts", "csv")
  req <- .build_request(url, body)

  sink("test_mobilescapes_query.txt")
  httr2::req_dry_run(req, quiet = FALSE, redact_headers = FALSE)
  sink()

  invisible(NULL)
}

#' Submit and Pull MobileScapes Origins Extract
#'
#' Authenticates with the API using a bearer token, submits an Origins
#' Extract request, polls for completion, downloads the CSV chunks from
#' Azure, and merges them into a single consolidated CSV file.
#'
#' @param geofence_ids Character vector. EA geofence IDs (see
#'   [discover_mobilescapes_geofences()]).
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param days_of_week Character vector. Optional. "Mon" through "Sun".
#' @param time_of_day Character. Optional. One of "AllDay", "Morning",
#'   "Afternoon", "Evening". Default "AllDay".
#' @param dwell Character. Optional. One of "Any", "Short", "Medium",
#'   "Long". Default "Any".
#' @param country Character. 2-digit country code. Default "ca".
#' @param vintage Character or numeric. Dataset vintage (e.g. "2026").
#' @param output_name Character. Optional. Base name for the output CSV file.
#'   Defaults to a name derived from the date range.
#' @param output_dir Character. Optional (default: "ea_output"). Output directory.
#' @param ... Not used. Present only so that passing a removed v4 argument
#'   (e.g. `geojson`, `use_weights`, `daily_time_filter`) fails immediately
#'   with a migration hint instead of a generic "unused argument" error.
#'
#' @return Creates output files in specified output directory. Returns
#'   directory of files as a character. The consolidated CSV is one row per
#'   origin postal code, verified against a real extract, with columns
#'   including `GeofenceName`, `Visits`, `PostalCode`, `LATITUDE`,
#'   `LONGITUDE`, `Sunday`...`Saturday`, `January`...`December`, and the full
#'   geography hierarchy (`PRCDDA`, `PRCDCSD`, `CMACT`, `FSA`, `PR`, etc. -
#'   each with a matching `_NAME` column). There is no PRIZM segment column
#'   and no time-of-day breakdown - see `time_of_day`/`dwell` above for the
#'   only filtering available on those dimensions. `GeofenceName` means a
#'   single extract can cover many geofences at once; unlike
#'   [get_mobilescapes_origins()], results are not pooled across
#'   `geofence_ids`.
#'
#' @export
pull_mobilescapes <- function(
    geofence_ids,
    start_date,
    end_date,
    days_of_week = NULL,
    time_of_day = "AllDay",
    dwell = "Any",
    country = "ca",
    vintage,
    output_name = NULL,
    output_dir = "ea_output",
    ...
) {
  .reject_legacy_args(list(...), "pull_mobilescapes")

  cat("\n########################################\n")

  # Print summary of submission
  cat("\n========= Request Summary ==========\n")
  cat(sprintf("Date Range: %s to %s\n", start_date, end_date))
  cat(sprintf("Geofence IDs: %d geofence(s)\n", length(geofence_ids)))
  cat(sprintf("Time of Day: %s\n", time_of_day))
  cat(sprintf("Dwell: %s\n", dwell))

  if (!is.null(days_of_week)) {
    cat(sprintf("Days of Week: %s\n", paste(days_of_week, collapse = ", ")))
  }

  cat("====================================\n")

  cat("\nSubmitting extract request...\n")

  # Submit request, being robust to 429 errors if query limit reached.
  result <- NULL
  max_attempts <- 3
  attempt <- 1

  while (attempt <= max_attempts && is.null(result)) {
    result <- tryCatch(
      .submit_origins_extract(
        geofence_ids, start_date, end_date, days_of_week, time_of_day, dwell,
        country, vintage
      ),
      error = function(e) {
        if (grepl("429", e$message)) {
          cat(sprintf("Access Denied (429). Attempt %d: Sleeping until 12:05 AM tomorrow...\n", attempt))
          current_time <- Sys.time()
          target_time <- as.POSIXct(paste(as.Date(current_time) + 1, "00:05:00"))
          Sys.sleep(as.numeric(difftime(target_time, current_time, units = "secs")))
          NULL
        } else {
          stop("Error: ", e$message, "\n")
        }
      }
    )
    attempt <- attempt + 1
  }

  if (is.null(result) || is.null(result$requestId)) {
    cat("ERROR: Failed to submit request after", max_attempts, "attempts. Stopping...\n")
    return(NULL)
  }

  request_id <- result$requestId
  cat("\nRequest successful!\nRequest ID:", request_id, "\n")

  # Poll API every 30s to check if data is ready
  cat("\n========================================\n")
  cat("Getting request updates...\n")
  cat("========================================\n")

  repeat {
    request_status <- .get_extract_status(request_id, country, vintage)

    if (is.null(request_status)) {
      cat("Error: Failed to get request status. Aborting pull.\n")
      return(NULL)
    }

    current_status <- request_status$requestStatus

    if (current_status == "Complete") {
      cat("\n========================================\n")
      cat("Result: Request", request_id, "completed successfully!\n")
      cat("========================================\n")
      break
    } else if (current_status == "Failed") {
      cat("\n========================================\n")
      cat("Result: Request", request_id, "failed.\n")
      cat("========================================\n")
      return(NULL)
    } else if (current_status == "Expired") {
      cat("\n========================================\n")
      cat("Result: Request expired.\n")
      cat("========================================\n")
      return(NULL)
    } else {
      cat("Status:", current_status, "(Next update in 30s)\n")
      Sys.sleep(30)
    }
  }

  # Basic output directory names
  output_base_dir <- output_dir
  temp_dir <- paste0(output_dir, "_temp")

  # Construct detailed name for output files
  start_clean <- gsub("[: ]", "_", start_date)
  end_clean <- gsub("[: ]", "_", end_date)
  geography_name <- if (!is.null(output_name)) {
    output_name
  } else {
    paste0("origins_", start_clean, "_to_", end_clean)
  }

  downloaded_files <- .download_extract_files(request_id, country, vintage, temp_dir)

  if (is.null(downloaded_files) || length(downloaded_files) == 0) {
    cat("ERROR: No files downloaded for", request_id, "\n")
    return(NULL)
  }

  # Create final output directory for this request's data
  final_output_dir <- file.path(output_base_dir, geography_name)
  .merge_extract_chunks(temp_dir, final_output_dir, geography_name)

  cat("\n########################################\n")
  cat("COMPLETED:", geography_name, "\n")
  cat("Output directory:", final_output_dir, "\n")
  cat("########################################\n")

  return(final_output_dir)
}
