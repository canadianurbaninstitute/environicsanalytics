# MobileScapes Data Extract API (new endpoint) #################################
#
# This module targets the overhauled MobileScapes "Data Extract" endpoint:
#   POST /{countryCode}/{vintage}/extracts/csv
#   GET  /{countryCode}/{vintage}/extracts/{requestId}/status
#   GET  /{countryCode}/{vintage}/extracts/{requestId}/results
#
# The endpoint is not yet live, so parts of this module are written against a
# sample specification and marked with TODO(new-api) comments where behaviour
# still needs to be verified against the real service:
#   - the base URL and results-link path
#   - the exact status vocabulary (assumed to match v4, compared case-insensitively)
#   - the naming of the output CSV files (merging is intentionally permissive)
#
# The v4 code in mobilescapes-api.R is untouched and remains the working path.

.EXTRACT_VALID_DAYS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

#' Build Endpoint URLs for the Data Extract API
#'
#' Single place where the new API's URL scheme is defined, so path corrections
#' only need to happen here once the endpoint is live.
#'
#' @param country_code Character. 2-digit country code (e.g. "ca").
#' @param vintage Character. Data vintage (path component).
#' @param request_id Character. Optional request id for status/results URLs.
#' @param base_url Character. API base URL.
#'
#' @return Named list with `submit`, `status` and `results` URLs.
#'
#' @keywords internal
.extract_endpoints <- function(country_code,
                               vintage,
                               request_id = NULL,
                               base_url = "https://api.environicsanalytics.com/mobilescapes") {
  root <- paste(base_url, country_code, vintage, "extracts", sep = "/")

  list(
    submit = paste0(root, "/csv"),
    # TODO(new-api): confirm status/results paths once the endpoint is live.
    status = if (!is.null(request_id)) paste0(root, "/", request_id, "/status"),
    results = if (!is.null(request_id)) paste0(root, "/", request_id, "/results")
  )
}

#' Make Request Body for the Data Extract API
#'
#' Constructs and validates the request body for the new Data Extract endpoint.
#' Unlike the v4 endpoint, geographies can only be specified via EA geofence IDs.
#'
#' @param geofence_ids Character vector. EA geofence IDs (max 300 per request).
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param days_of_week Character vector. Optional. Days to include, using
#'   3-letter capitalized names (e.g. c("Mon", "Tue")).
#' @param time_of_day Character. Optional. Time-of-day filter (e.g. "AllDay").
#' @param dwell Character. Optional. Dwell filter (e.g. "Any").
#' @param target_set List. Optional. List of target group objects, each a list
#'   with `targetGroupId` (character) and `segmentCodes` (character vector).
#'
#' @return List containing the request body parameters.
#'
#' @keywords internal
.make_extract_request_body <- function(
    geofence_ids,
    start_date,
    end_date,
    days_of_week = NULL,
    time_of_day = NULL,
    dwell = NULL,
    target_set = NULL
) {

  # Geofence IDs are the only supported geography input on this endpoint
  if (is.null(geofence_ids) || length(geofence_ids) == 0) {
    stop("Error: geofence_ids must be provided. The Data Extract endpoint only supports EA geofence IDs.")
  }
  if (length(geofence_ids) > 300) {
    stop("Error: A single request can include at most 300 geofences (got ",
         length(geofence_ids), ").")
  }
  if (anyDuplicated(geofence_ids)) {
    stop("Error: geofence_ids contains duplicate values.")
  }

  # Dates are date-only on this endpoint (v4 used full datetimes)
  date_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"
  if (!grepl(date_pattern, start_date) || !grepl(date_pattern, end_date)) {
    stop("Error: start_date and end_date must be in \"YYYY-MM-DD\" format.")
  }
  if (as.Date(start_date) > as.Date(end_date)) {
    stop("Error: start_date must be on or before end_date.")
  }

  if (!is.null(days_of_week)) {
    invalid_days <- setdiff(days_of_week, .EXTRACT_VALID_DAYS)
    if (length(invalid_days) > 0) {
      stop("Error: Invalid days_of_week value(s): ",
           paste(invalid_days, collapse = ", "),
           ". Use 3-letter capitalized names, e.g. \"Mon\".")
    }
  }

  if (!is.null(target_set)) {
    for (i in seq_along(target_set)) {
      group <- target_set[[i]]
      if (is.null(group$targetGroupId) || is.null(group$segmentCodes)) {
        stop("Error: Each target_set entry must be a list with targetGroupId and segmentCodes (entry ", i, " is invalid).")
      }
    }
  }

  body_list <- list(
    geofenceIds = as.list(geofence_ids),
    startDate = start_date,
    endDate = end_date
  )

  if (!is.null(days_of_week)) body_list$daysOfWeek <- as.list(days_of_week)
  if (!is.null(time_of_day)) body_list$timeOfDay <- time_of_day
  if (!is.null(dwell)) body_list$dwell <- dwell
  if (!is.null(target_set)) body_list$targetSet <- target_set

  return(body_list)
}

#' Get Data Extract Request Status
#'
#' Retrieves the current lifecycle status of a submitted extract request.
#'
#' @param bearer_token Character. OAuth bearer token.
#' @param request_id Character. The request ID from the submit call.
#' @param country_code Character. 2-digit country code.
#' @param vintage Character. Data vintage (path component).
#'
#' @return List with status information.
#'
#' @keywords internal
.get_extract_status <- function(bearer_token, request_id, country_code, vintage) {

  url <- .extract_endpoints(country_code, vintage, request_id)$status

  req <- httr2::request(url) |>
    httr2::req_auth_bearer_token(bearer_token) |>
    httr2::req_error(body = function(resp) {
      paste("Status check failed:", httr2::resp_body_string(resp))
    })

  resp <- httr2::req_perform(req)
  return(httr2::resp_body_json(resp))
}

#' Get Data Extract Download Links
#'
#' Retrieves time-limited Azure storage download details for a completed
#' extract request. Links expire quickly, so files should be downloaded
#' promptly after this call.
#'
#' @param bearer_token Character. OAuth bearer token.
#' @param request_id Character. The request ID.
#' @param country_code Character. 2-digit country code.
#' @param vintage Character. Data vintage (path component).
#'
#' @return List with Azure storage details (storageUrl, containerName, sasToken, blobList).
#'
#' @keywords internal
.get_extract_results <- function(bearer_token, request_id, country_code, vintage) {
  cat("Getting result information...\n")

  url <- .extract_endpoints(country_code, vintage, request_id)$results

  req <- httr2::request(url) |>
    httr2::req_auth_bearer_token(bearer_token) |>
    httr2::req_error(body = function(resp) {
      paste("Failed to get results:", httr2::resp_body_string(resp))
    })

  resp <- httr2::req_perform(req)
  return(httr2::resp_body_json(resp))
}

#' Download Data Extract Results from Azure Blob Storage
#'
#' Downloads all extract files from Azure Blob Storage using the storage
#' details returned by the results endpoint.
#'
#' @param result_info List. Azure storage details (storageUrl, containerName, sasToken).
#' @param output_dir Character. Directory to save downloaded files.
#'
#' @return Character vector of downloaded file paths, or NULL if error.
#'
#' @keywords internal
.download_extract_results <- function(result_info, output_dir = "temp") {
  cat("\n========================================\n")
  cat("Downloading results...\n")
  cat("========================================\n")

  if (is.null(result_info)) {
    cat("ERROR: Could not get result information\n")
    return(NULL)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }

  cat("Connecting to Azure Blob Storage...\n")

  endpoint <- AzureStor::storage_endpoint(result_info$storageUrl, sas = result_info$sasToken)
  container <- AzureStor::storage_container(endpoint, result_info$containerName)

  cat("Listing available files...\n")
  blob_list <- AzureStor::list_blobs(container)

  if (nrow(blob_list) == 0) {
    cat("WARNING: No files found in container\n")
    return(NULL)
  }

  cat("Found", nrow(blob_list), "file(s) to download\n")

  downloaded_files <- c()

  for (i in 1:nrow(blob_list)) {
    blob_name <- blob_list$name[i]
    output_file <- file.path(output_dir, basename(blob_name))

    cat(sprintf("  [%d/%d] Downloading: %s\n", i, nrow(blob_list), blob_name))

    tryCatch({
      AzureStor::storage_download(container, blob_name, output_file, overwrite = TRUE)
      downloaded_files <- c(downloaded_files, output_file)
    }, error = function(e) {
      cat("    ERROR downloading", blob_name, ":", e$message, "\n")
    })
  }

  cat("\n========================================\n")
  cat("Download complete! Downloaded", length(downloaded_files), "file(s)\n")
  cat("========================================\n")

  return(downloaded_files)
}

#' Merge Data Extract CSV Chunks into Consolidated Files
#'
#' Groups downloaded CSV files by report name (chunk-number suffixes stripped)
#' and merges each group into a single consolidated CSV. Raw chunks are moved
#' to a 'raw' subdirectory.
#'
#' The exact output file naming of the new endpoint is not yet known, so this
#' merge is intentionally permissive: any *.csv / *.csv.gz file is picked up,
#' and files that share a name once trailing chunk numbers are removed (e.g.
#' "report_1.csv.gz", "report_2.csv.gz") are combined.
#'
#' @param download_dir Character. Directory containing downloaded files.
#' @param output_dir Character. Directory for consolidated output files.
#' @param extract_name Character. Prefix for consolidated output files.
#'
#' @return Named list of consolidated file paths, keyed by report name.
#'
#' @keywords internal
.merge_extract_csvs <- function(download_dir, output_dir, extract_name) {
  cat("\n========================================\n")
  cat("Merging extract files...\n")
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

  csv_files <- list.files(download_dir, pattern = "\\.csv(\\.gz)?$", full.names = TRUE)
  cat("Found", length(csv_files), "CSV file(s)\n")

  for (f in csv_files) {
    file.rename(f, file.path(raw_dir, basename(f)))
  }
  cat("All files moved to raw directory\n")

  raw_files <- list.files(raw_dir, pattern = "\\.csv(\\.gz)?$", full.names = TRUE)

  # Group files by report name: strip extension and any trailing chunk number
  report_of <- function(path) {
    name <- sub("\\.csv(\\.gz)?$", "", basename(path))
    sub("[_-]\\d+$", "", name)
  }
  groups <- split(raw_files, vapply(raw_files, report_of, character(1)))

  result_files <- list()

  for (report_name in names(groups)) {
    files <- groups[[report_name]]
    cat(sprintf("Processing report '%s' (%d file(s))...\n", report_name, length(files)))

    data <- lapply(files, function(f) {
      readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    })
    combined <- dplyr::bind_rows(data)
    cat("Combined data:", nrow(combined), "rows\n")

    output_file <- file.path(output_dir, paste0(extract_name, "_", report_name, ".csv"))
    readr::write_csv(combined, output_file)
    cat("Saved file:", output_file, "\n")

    result_files[[report_name]] <- output_file

    rm(data, combined)
  }

  cat("\n========================================\n")
  cat("Merge complete!\n")
  cat("========================================\n")

  unlink(download_dir, recursive = TRUE)
  gc()
  return(result_files)
}

#' Create Data Extract API "Dry" Request
#'
#' Shows exactly what httr2 will send to the new Data Extract endpoint without
#' sending anything. Primarily for debugging while the endpoint is unavailable.
#' Outputs the dry run to "test_extract_query.txt".
#'
#' @param geofence_ids Character vector. EA geofence IDs (max 300 per request).
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param days_of_week Character vector. Optional. Days to include (e.g. c("Mon", "Tue")).
#' @param time_of_day Character. Optional. Time-of-day filter (e.g. "AllDay").
#' @param dwell Character. Optional. Dwell filter (e.g. "Any").
#' @param target_set List. Optional. List of target group objects, each a list
#'   with `targetGroupId` and `segmentCodes`.
#' @param country_code Character. 2-digit country code. Default is "ca".
#' @param vintage Character. Data vintage (path component of the new API).
#'
#' @return Invisibly returns NULL. Outputs dry run to "test_extract_query.txt".
#'
#' @export
test_query_extract <- function(
    geofence_ids,
    start_date,
    end_date,
    days_of_week = NULL,
    time_of_day = NULL,
    dwell = NULL,
    target_set = NULL,
    country_code = "ca",
    vintage
) {
  cat("Saving DRY Data Extract request...\n")

  bearer_token <- .quietly_get_bearer_token()

  body <- .make_extract_request_body(
    geofence_ids = geofence_ids,
    start_date = start_date,
    end_date = end_date,
    days_of_week = days_of_week,
    time_of_day = time_of_day,
    dwell = dwell,
    target_set = target_set
  )

  req <- httr2::request(.extract_endpoints(country_code, vintage)$submit) |>
    httr2::req_auth_bearer_token(bearer_token) |>
    httr2::req_body_json(body) |>
    httr2::req_error(body = function(resp) {
      error_content <- httr2::resp_body_json(resp)
      paste0(
        "API Error: ",
        if (!is.null(error_content$errorCode)) paste0("[", error_content$errorCode, "] "),
        if (!is.null(error_content$message)) error_content$message else httr2::resp_body_string(resp)
      )
    })

  sink("test_extract_query.txt")
  dry_run <- httr2::req_dry_run(
    req,
    quiet = FALSE,
    redact_headers = FALSE
  )
  sink()

  invisible(NULL)
}

#' Submit and Pull a MobileScapes Data Extract (New Endpoint)
#'
#' Submits an extract request to the overhauled MobileScapes Data Extract
#' endpoint, polls for completion, downloads the CSV files from Azure storage,
#' and merges chunks into consolidated CSV files. Unlike [pull_mobilescapes()],
#' geographies can only be specified via EA geofence IDs, and the time range is
#' date-only.
#'
#' NOTE: The Data Extract endpoint is not yet live. This function is written
#' against a sample of the new API specification and has not been verified
#' against the real service.
#'
#' @param geofence_ids Character vector. EA geofence IDs (max 300 per request).
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param days_of_week Character vector. Optional. Days to include, using
#'   3-letter capitalized names (e.g. c("Mon", "Tue", "Wed", "Thu", "Fri")).
#' @param time_of_day Character. Optional. Time-of-day filter (e.g. "AllDay").
#' @param dwell Character. Optional. Dwell filter (e.g. "Any").
#' @param target_set List. Optional. List of target group objects, each a list
#'   with `targetGroupId` (character) and `segmentCodes` (character vector).
#' @param country_code Character. 2-digit country code. Default is "ca".
#' @param vintage Character. Data vintage (path component of the new API).
#' @param output_dir Character. Optional (default: "ea_output"). Output directory.
#'
#' @return Creates output files in the specified output directory. Returns the
#'   directory of files as a character, or NULL on failure.
#'
#' @export
pull_extract <- function(
    geofence_ids,
    start_date,
    end_date,
    days_of_week = NULL,
    time_of_day = NULL,
    dwell = NULL,
    target_set = NULL,
    country_code = "ca",
    vintage,
    output_dir = "ea_output"
) {
  cat("\n########################################\n")

  bearer_token <- .quietly_get_bearer_token()

  body <- .make_extract_request_body(
    geofence_ids = geofence_ids,
    start_date = start_date,
    end_date = end_date,
    days_of_week = days_of_week,
    time_of_day = time_of_day,
    dwell = dwell,
    target_set = target_set
  )

  req <- httr2::request(.extract_endpoints(country_code, vintage)$submit) |>
    httr2::req_auth_bearer_token(bearer_token) |>
    httr2::req_body_json(body) |>
    httr2::req_error(body = function(resp) {
      error_content <- httr2::resp_body_json(resp)
      paste0(
        "API Error: ",
        if (!is.null(error_content$errorCode)) paste0("[", error_content$errorCode, "] "),
        if (!is.null(error_content$message)) error_content$message else httr2::resp_body_string(resp)
      )
    })

  cat("\nSending API request...\n")

  cat("\n========= Request Summary ==========\n")
  cat(sprintf("Date Range: %s to %s\n", start_date, end_date))
  cat(sprintf("Geofence IDs: %d geofence(s)\n", length(geofence_ids)))
  cat(sprintf("Country / Vintage: %s / %s\n", country_code, vintage))
  if (!is.null(days_of_week)) {
    cat(sprintf("Days of Week: %s\n", paste(days_of_week, collapse = ", ")))
  }
  if (!is.null(time_of_day)) cat(sprintf("Time of Day: %s\n", time_of_day))
  if (!is.null(dwell)) cat(sprintf("Dwell: %s\n", dwell))
  if (!is.null(target_set)) {
    cat(sprintf("Target Set: %d target group(s)\n", length(target_set)))
  }
  cat("====================================\n")

  # Perform request, being robust to 429 errors if query limit reached.
  resp <- NULL
  max_attempts <- 3
  attempt <- 1

  while (attempt <= max_attempts) {
    tryCatch(
      {
        resp <- httr2::req_perform(req)
        result <- httr2::resp_body_json(resp)

        if (!is.null(result$requestId)) {
          break  # Got a successful submission
        }
      },
      error = function(e) {
        if (grepl("429", e$message)) {
          cat(sprintf("Access Denied (429). Attempt %d: Sleeping until 12:05 AM tomorrow...\n", attempt))
          current_time <- Sys.time()
          target_time <- as.POSIXct(paste(as.Date(current_time) + 1, "00:05:00"))
          Sys.sleep(as.numeric(difftime(target_time, current_time, units = "secs")))
        } else {
          stop("Error: ", e$message, "\n")
        }
      }
    )
    attempt <- attempt + 1
  }

  if (is.null(resp)) {
    cat("ERROR: Failed to submit request after", max_attempts, "attempts. Stopping... \n")
    return(NULL)
  }

  result <- httr2::resp_body_json(resp)
  request_id <- result$requestId

  if (is.null(request_id)) {
    cat("ERROR: Failed to submit request.")
    return(NULL)
  }

  cat("\nRequest successful!\nRequest ID:", request_id, "\n")

  # Poll API every 30s to check if data is ready
  cat("\n========================================\n")
  cat("Getting request updates...\n")
  cat("========================================\n")

  repeat {
    bearer_token <- .quietly_get_bearer_token()
    request_status <- .get_extract_status(bearer_token, request_id, country_code, vintage)

    if (is.null(request_status)) {
      cat("Error: Failed to get request status. Aborting pull.\n")
      return(NULL)
    }

    # Status casing differs between API versions ("COMPLETE" vs "Complete"),
    # so compare case-insensitively.
    current_status <- toupper(request_status$requestStatus)

    if (current_status == "COMPLETE") {
      cat("\n========================================\n")
      cat("Result: Request", request_id, "completed successfully!\n")
      cat("========================================\n")
      break
    } else if (current_status == "FAILED") {
      cat("\n========================================\n")
      cat("Result: Request", request_id, "failed.\n")
      cat("========================================\n")
      return(NULL)
    } else if (current_status == "EXPIRED") {
      cat("\n========================================\n")
      cat("Result: Request expired.\n")
      cat("========================================\n")
      return(NULL)
    } else {
      cat("Status:", request_status$requestStatus, "(Next update in 30s)\n")
      Sys.sleep(30)
    }
  }

  # Download links are time-limited, so fetch them and download immediately
  temp_dir <- paste0(output_dir, "temp")

  extract_name <- paste0("extract_", start_date, "_to_", end_date)

  result_info <- .get_extract_results(bearer_token, request_id, country_code, vintage)
  downloaded_files <- .download_extract_results(result_info, temp_dir)

  if (is.null(downloaded_files) || length(downloaded_files) == 0) {
    cat("ERROR: No files downloaded for", request_id, "\n")
    return(NULL)
  }

  final_output_dir <- paste0(output_dir, "/", extract_name)
  .merge_extract_csvs(temp_dir, final_output_dir, extract_name)

  cat("\n########################################\n")
  cat("COMPLETED:", extract_name, "\n")
  cat("Output directory:", final_output_dir, "\n")
  cat("########################################\n")

  rm(body, result, resp, downloaded_files, request_id, bearer_token, req, result_info)
  gc()

  return(final_output_dir)
}
