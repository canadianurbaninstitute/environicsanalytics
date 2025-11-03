# GeoJson Helper Functions #####################################################
#' Clean GeoJSON for API Compliance (RFC 7946)
#'
#' @param geography sf object or GeoJSON list
#'
#' @return A cleaned GeoJSON list object
#' @keywords internal
.clean_geojson <- function(geography) {
  cat("\nCleaning geography for API...\n")

  # Handle sf objects - convert to GeoJSON list
  if (inherits(geography, "sf")) {
    # Transform back to WGS84 (EPSG:4326) for GeoJSON output
    # RFC 7946 requires WGS84 coordinates
    if (sf::st_crs(geography)$epsg != 4326) {
      cat("Transforming back to WGS84 (EPSG:4326) for GeoJSON output...\n")
      geography <- sf::st_transform(geography, 4326)
    }

    geojson <- jsonlite::fromJSON(
      geojsonio::geojson_json(geography),
      simplifyVector = FALSE
    )
  } else {
    geojson <- geography
  }

  # Validate input structure
  if (!is.list(geojson) || geojson$type != "FeatureCollection") {
    stop("Input must be a FeatureCollection GeoJSON or sf object")
  }

  # Create clean GeoJSON with only RFC 7946 standard properties
  clean_geojson <- list(
    type = "FeatureCollection",
    features = list()
  )

  # Track IDs to ensure uniqueness
  used_ids <- character()

  # Process each feature
  for (i in seq_along(geojson$features)) {
    feature <- geojson$features[[i]]

    # Validate geometry exists
    if (is.null(feature$geometry)) {
      warning(sprintf("Feature %d has no geometry - skipping", i))
      next
    }

    # Create clean feature with required properties
    clean_feature <- list(
      type = "Feature",
      geometry = feature$geometry
    )

    # Use Name property as ID if available. Clean to have no special characters.
    if (!is.null(feature$properties$Name) && !is.na(feature$properties$Name)) {
      proposed_id <- as.character(gsub("[^[:alnum:][:space:]]","",feature$properties$Name))
    } else if (!is.null(feature$id) && !is.na(feature$id)) {
      proposed_id <- as.character(feature$id)
    } else {
      proposed_id <- paste0("feature_", i)
    }

    # Ensure ID uniqueness (shouldn't happen if Names are unique, but safety check)
    if (proposed_id %in% used_ids) {
      warning(sprintf("Duplicate ID '%s' found, appending suffix", proposed_id))
      proposed_id <- paste0(proposed_id, "_dup_", i)
    }

    clean_feature$id <- proposed_id
    used_ids <- c(used_ids, proposed_id)

    # Add to clean features list
    clean_geojson$features[[length(clean_geojson$features) + 1]] <- clean_feature
  }

  cat("Successfully cleaned", length(clean_geojson$features), "feature(s)\n")

  # Validate output has features
  if (length(clean_geojson$features) == 0) {
    warning("No valid features in output")
  }

  clean_geojson
}

#' Process GeoJSON File: Split Large Features and Return Clean GeoJSON
#'
#' Reads in the GeoJson provided at the filepath, checks and (if needed) splits
#' the features so that they are under the API's limit (5M sq ft), and processes
#' the file to be
#'
#' @param filepath Character. Path to input GeoJSON file
#' @param max_area_sqft Numeric. Maximum area in square feet (default: 5000000)
#' @param safety_factor Numeric. Safety factor for splitting (default: 1.05)
#' @param output_filepath Character. Optional path to save output GeoJSON
#'
#' @return List. Clean GeoJSON object with all features < max_area_sqft
#'
#' @examples
#' \dontrun{
#' result <- process_geojson_file("large_area.geojson", output_filepath = "split_output.geojson")
#' }
#' @export
process_geojson_file <- function(filepath,
                                 max_area_sqft = 5000000,
                                 safety_factor = 1.05,
                                 output_filepath = NULL) {

  cat(sprintf("Reading GeoJSON from: %s\n", filepath))

  # Read GeoJSON file as sf object
  geography_sf <- sf::st_read(filepath, quiet = TRUE)

  cat(sprintf("Loaded %d feature(s)\n", nrow(geography_sf)))

  # Check if splitting is needed
  cat("\n========================================\n")
  cat("Checking feature areas... ")
  needs_processing <- !.check_feature_area(geography_sf, max_area_sqft)
  cat("========================================\n")

  if (needs_processing) {
    # Split large features
    geography_split <- .split_large_geographies(geography_sf, max_area_sqft, safety_factor)
  } else {
    geography_split <- geography_sf
  }

  # Convert to clean GeoJSON
  clean_json <- .clean_geojson(geography_split)

  # Optionally save to file
  if (!is.null(output_filepath)) {
    cat(sprintf("\nSaving result to: %s\n", output_filepath))
    jsonlite::write_json(clean_json, output_filepath, pretty = TRUE, auto_unbox = TRUE)
    cat("\u2713 File saved successfully\n")
  }

  invisible(clean_json)
}

# API Helper Functions #########################################################

#' Make Request Body for MobileScapes API
#'
#' Constructs the request body list for MobileScapes API calls.
#'
#' @param start_datetime Character. Start date/time in "YYYY-MM-DD hh:mm:ss" format.
#' @param end_datetime Character. End date/time in "YYYY-MM-DD hh:mm:ss" format.
#' @param geojson Character or list. Path to GeoJSON file or GeoJSON list object.
#' @param geofence_ids Vector. Optional. EA geofence IDs.
#' @param wkt_list List. Optional. Well-Known Text polygon definitions.
#' @param use_weights Logical. Apply weights per device.
#' @param aggregate_polygons Logical. Aggregate results across polygons.
#' @param aggregate_polygon_name Character. Optional. Name for aggregated results.
#' @param append_prizm_segmentation Character. Optional. PRIZM segmentation.
#' @param daily_time_filter List. Optional. Filter for specific times/days.
#' @param ping_filter Character. Optional. Ping filter ("first" or NULL).
#' @param report_type Character. Report type.
#' @param data_vintage Character. Optional. Data vintage.
#'
#' @return List containing the request body parameters.
#'
#' @keywords internal
.make_request_body <- function(
    start_datetime,
    end_datetime,
    geojson,
    geofence_ids,
    wkt_list,
    use_weights,
    aggregate_polygons,
    aggregate_polygon_name,
    append_prizm_segmentation,
    daily_time_filter,
    ping_filter,
    report_type,
    data_vintage
) {

  # Validate that at least one polygon type is provided
  if (is.null(geofence_ids) && is.null(wkt_list) && is.null(geojson)) {
    stop("Error: Must provide at least one of: geofence_ids, wkt_list, or geojson")
  }

  # Build basic request body
  body_list <- list(
    startDateTime = start_datetime,
    endDateTime = end_datetime,
    useWeights = use_weights,
    aggregatePolygons = aggregate_polygons,
    reportType = report_type
  )

  # Add optional spatial polygon parameters

  ## EA Geofence IDs
  if (!is.null(geofence_ids)) {
    body_list$geofenceIds <- geofence_ids
  }

  ## WKT Polygons
  if (!is.null(wkt_list)) {
    body_list$wktList <- wkt_list
  }

  # Clean GeoJSON for API compliance
  if (!is.null(geojson)) {
    body_list$geoJson <- process_geojson_file(
      geojson,
      5000000,
      1.05
    )
  }

  # Add other optional parameters
  if (!is.null(aggregate_polygon_name)) body_list$aggregatePolygonName <- aggregate_polygon_name
  if (!is.null(daily_time_filter)) body_list$dailyTimeFilter <- daily_time_filter
  if (!is.null(ping_filter)) body_list$pingFilter <- ping_filter
  if (!is.null(data_vintage)) body_list$dataVintage <- data_vintage
  if (!is.null(append_prizm_segmentation)) body_list$appendSegmentation <- append_prizm_segmentation

  body_list
}

#' Get MobileScapes Request Status
#'
#' Retrieves the current processing status of a submitted request.
#'
#' @param bearer_token Character. OAuth bearer token.
#' @param request_id Character. The request ID from query_mobilescapes().
#'
#' @return List with status information, or NULL if error.
#'
#' @keywords internal
.get_request_status <- function(bearer_token, request_id) {

  API_BASE_URL <- "https://api.environicsanalytics.com/mobilescapes/v4/ca"

  req <- httr2::request(paste0(API_BASE_URL, "/requests/", request_id, "/status")) |>
    httr2::req_auth_bearer_token(bearer_token) |>
    httr2::req_error(body = function(resp) {
      paste("Status check failed:", httr2::resp_body_string(resp))
    })

  resp <- httr2::req_perform(req)
  return(httr2::resp_body_json(resp))
}

#' Get MobileScapes Request Results Information
#'
#' Retrieves Azure storage information from completed request for downloading queried files.
#'
#' @param bearer_token Character. OAuth bearer token.
#' @param request_id Character. The request ID.
#'
#' @return List with Azure storage details (storageUrl, containerName, sasToken, blobList).
#'
#' @keywords internal
.get_request_results <- function(bearer_token, request_id) {
  cat("Getting result information...\n")

  API_BASE_URL <- "https://api.environicsanalytics.com/mobilescapes/v4/ca"

  req <- httr2::request(paste0(API_BASE_URL, "/requests/", request_id, "/resultInfo")) |>
    httr2::req_auth_bearer_token(bearer_token) |>
    httr2::req_error(body = function(resp) {
      paste("Failed to get results:", httr2::resp_body_string(resp))
    })

  resp <- httr2::req_perform(req)
  return(httr2::resp_body_json(resp))
}

#' Download MobileScapes Results from Azure Blob Storage
#'
#' Downloads all result files from Azure Blob Storage using AzureStor package.
#'
#' @param bearer_token Character. OAuth bearer token.
#' @param request_id Character. The request ID.
#' @param output_dir Character. Directory to save downloaded files. Default is "temp".
#'
#' @return Character vector of downloaded file paths, or NULL if error.
#'
#' @keywords internal
.download_results <- function(bearer_token, request_id, output_dir = "temp") {
  cat("\n========================================\n")
  cat("Downloading results...\n")
  cat("========================================\n")

  # Get result information
  result_info <- .get_request_results(bearer_token, request_id)

  if (is.null(result_info)) {
    cat("ERROR: Could not get result information\n")
    return(NULL)
  }

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }

  # Connect to Azure Blob Storage using AzureStor
  cat("Connecting to Azure Blob Storage...\n")

  endpoint <- AzureStor::storage_endpoint(result_info$storageUrl, sas = result_info$sasToken)
  container <- AzureStor::storage_container(endpoint, result_info$containerName)

  # List all blobs in the container
  cat("Listing available files...\n")
  blob_list <- AzureStor::list_blobs(container)

  if (nrow(blob_list) == 0) {
    cat("WARNING: No files found in container\n")
    return(NULL)
  }

  cat("Found", nrow(blob_list), "file(s) to download\n")

  # Download each blob
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

# DATA PROCESSING FUNCTIONS

#' Merge API Result Chunks into Consolidated Files
#'
#' Combines multiple CEL and CDL CSV chunks into single consolidated files.
#' Moves raw chunks to a 'raw' subdirectory.
#'
#' @param download_dir Character. Directory containing downloaded chunks.
#' @param output_dir Character. Directory for consolidated output files.
#' @param geography_name Character. Name for output files (sanitized).
#' @param start_datetime Character. Start datetime for naming convention.
#' @param end_datetime Character. End datetime for naming convention.
#'
#' @return Named list with paths to consolidated 'cel' and 'cdl' files.
#'
#' @keywords internal
.merge_api_chunks <- function(download_dir, output_dir = "output", geography_name, start_datetime, end_datetime) {
  cat("\n========================================\n")
  cat("Merging API result chunks...\n")
  cat("========================================\n")

  # Create output directory structure
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created directory:", output_dir, "\n")
  }

  raw_dir <- file.path(output_dir, "raw")
  if (!dir.exists(raw_dir)) {
    dir.create(raw_dir, recursive = TRUE)
    cat("Created raw files directory in output directory:", raw_dir, "\n")
  }

  # Find all cel and cdl files in download directory
  cel_files <- list.files(download_dir, pattern = "^cel_report_.*\\.csv\\.gz$", full.names = TRUE)
  cdl_files <- list.files(download_dir, pattern = "^cdl_report_.*\\.csv\\.gz$", full.names = TRUE)

  cat("Found", length(cel_files), "CEL file(s)\n")
  cat("Found", length(cdl_files), "CDL file(s)\n")

  # Move files to raw directory
  for (f in cel_files) {
    file.rename(f, file.path(raw_dir, basename(f)))
  }
  for (f in cdl_files) {
    file.rename(f, file.path(raw_dir, basename(f)))
  }

  cat("All files moved to raw directory\n")

  # Get file paths from raw directory
  cel_files_raw <- list.files(raw_dir, pattern = "^cel_report_.*\\.csv\\.gz$", full.names = TRUE)
  cdl_files_raw <- list.files(raw_dir, pattern = "^cdl_report_.*\\.csv\\.gz$", full.names = TRUE)

  combined_cel <- NULL
  combined_cdl <- NULL

  # Process CEL files
  if (length(cel_files_raw) > 0) {
    cat("Processing", length(cel_files_raw), "CEL file(s)...\n")
    cel_data <- lapply(cel_files_raw, function(f) {
      readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    })
    combined_cel <- dplyr::bind_rows(cel_data)
    cat("Combined CEL data:", nrow(combined_cel), "rows\n")
  } else {
    cat("No CEL files to process\n")
  }

  # Process CDL files
  if (length(cdl_files_raw) > 0) {
    cat("Processing", length(cdl_files_raw), "CDL file(s)...\n")
    cdl_data <- lapply(cdl_files_raw, function(f) {
      readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    })
    combined_cdl <- dplyr::bind_rows(cdl_data)
    cat("Combined CDL data:", nrow(combined_cdl), "rows\n")
  } else {
    cat("No CDL files to process\n")
  }

  # Output consolidated CSV files
  cat("\nWriting consolidated CSV files...\n")

  result_files <- list()

  # Save CEL file
  if (!is.null(combined_cel)) {
    output_cel <- file.path(output_dir, paste0(geography_name, "_cel.csv"))
    readr::write_csv(combined_cel, output_cel)
    cat("Saved CEL file:", output_cel, "\n")
    result_files$cel <- output_cel
  }

  # Save CDL file
  if (!is.null(combined_cdl)) {
    output_cdl <- file.path(output_dir, paste0(geography_name, "_cdl.csv"))
    readr::write_csv(combined_cdl, output_cdl)
    cat("Saved CDL file:", output_cdl, "\n")
    result_files$cdl <- output_cdl
  }

  cat("\n========================================\n")
  cat("Merge complete!\n")
  cat("========================================\n")


  unlink(download_dir, recursive = TRUE)
  return(result_files)
}

# Core API Calling Functions ######################################################

#' Create MobileScapes API "Dry" Request
#'
#' Shows exactly what httr2 package will send to the Environics API
#' without sending anything. Primarily for debugging.
#'
#' @param bearer_token Character. OAuth bearer token.
#' @param start_datetime Character. Start date/time in "YYYY-MM-DD hh:mm:ss" format.
#' @param end_datetime Character. End date/time in "YYYY-MM-DD hh:mm:ss" format.
#' @param geojson List. GeoJSON object (will be cleaned automatically).
#' @param geofence_ids Vector. Optional. EA geofence IDs.
#' @param wkt_list List. Optional. Well-Known Text polygon definitions.
#' @param use_weights Logical. Apply weights per device. Default is TRUE.
#' @param aggregate_polygons Logical. Aggregate results across polygons. Default is TRUE.
#' @param aggregate_polygon_name Character. Optional. Name for aggregated results.
#' @param append_prizm_segmentation Character. Optional. PRIZM segmentation.
#' @param daily_time_filter List. Optional. Filter for specific times/days.
#' @param ping_filter Character. Optional. Ping filter ("first" or NULL).
#' @param report_type Character. Report type. Default is "celcdl".
#' @param data_vintage Character. Optional. Data vintage.
#'
#' @return Invisibly returns NULL. Outputs dry run to "test_query.txt".
#'
#' @export
test_query_mobilescapes <- function(
    start_datetime,
    end_datetime,
    geojson = NULL,
    geofence_ids = NULL,
    wkt_list = NULL,
    use_weights = TRUE,
    aggregate_polygons = TRUE,
    aggregate_polygon_name = NULL,
    append_prizm_segmentation = "prizm",
    daily_time_filter = NULL,
    ping_filter = NULL,
    report_type = "celcdl",
    data_vintage = NULL
) {
  cat("Saving DRY MobileScapes request...\n")

  API_BASE_URL <- "https://api.environicsanalytics.com/mobilescapes/v4/ca"

  bearer_token <- .quietly_get_bearer_token()

  body <- .make_request_body(
    start_datetime = start_datetime,
    end_datetime = end_datetime,
    geojson = geojson,
    geofence_ids = geofence_ids,
    wkt_list = wkt_list,
    use_weights = use_weights,
    aggregate_polygons = aggregate_polygons,
    aggregate_polygon_name = aggregate_polygon_name,
    append_prizm_segmentation = append_prizm_segmentation,
    daily_time_filter = daily_time_filter,
    ping_filter = ping_filter,
    report_type = report_type,
    data_vintage = data_vintage
  )

  # Create httr2 request
  req <- httr2::request(paste0(API_BASE_URL, "/requests")) |>
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

  sink("test_mobilescapes_query.txt")
  dry_run <- httr2::req_dry_run(
    req,
    quiet = FALSE,
    redact_headers = FALSE
  )
  sink()
}


#' Submit and Pull MobileScapes API Request
#'
#' Authenticates with API using bearer token, submits a request to the MobileScapes API,
#' handles GeoJSON splitting if needed, polls for completion, downloads data from Azure,
#' and merges chunks into consolidated CEL and CDL CSV files.
#'
#' @param start_datetime Character. Start date/time in "YYYY-MM-DD hh:mm:ss" format.
#' @param end_datetime Character. End date/time in "YYYY-MM-DD hh:mm:ss" format.
#' @param geojson Character. Path to GeoJSON file.
#' @param geofence_ids Vector. Optional. EA geofence IDs.
#' @param wkt_list List. Optional. Well-Known Text polygon definitions.
#' @param use_weights Logical. Apply weights per device. Default is TRUE.
#' @param aggregate_polygons Logical. Aggregate results across polygons. Default is TRUE.
#' @param aggregate_polygon_name Character. Optional. Name for aggregated results.
#' @param append_prizm_segmentation Character. Optional. PRIZM segmentation.
#' @param daily_time_filter List. Optional. Filter for specific times/days.
#' @param ping_filter Character. Optional. Ping filter ("first" or NULL).
#' @param report_type Character. Report type. Default is "celcdl".
#' @param data_vintage Character. Optional. Data vintage.
#'
#' @return Invisibly returns NULL. Creates output files in output directory.
#'
#' @export
pull_mobilescapes <- function(
    start_datetime,
    end_datetime,
    geojson = NULL,
    geofence_ids = NULL,
    wkt_list = NULL,
    use_weights = TRUE,
    aggregate_polygons = TRUE,
    aggregate_polygon_name = NULL,
    append_prizm_segmentation = "prizm",
    daily_time_filter = NULL,
    ping_filter = NULL,
    report_type = "celcdl",
    data_vintage = NULL
) {
  cat("\n########################################\n")

  bearer_token <- get_bearer_token()

  body <- .make_request_body(
    start_datetime = start_datetime,
    end_datetime = end_datetime,
    geojson = geojson,
    geofence_ids = geofence_ids,
    wkt_list = wkt_list,
    use_weights = use_weights,
    aggregate_polygons = aggregate_polygons,
    aggregate_polygon_name = aggregate_polygon_name,
    append_prizm_segmentation = append_prizm_segmentation,
    daily_time_filter = daily_time_filter,
    ping_filter = ping_filter,
    report_type = report_type,
    data_vintage = data_vintage
  )

  API_BASE_URL <- "https://api.environicsanalytics.com/mobilescapes/v4/ca"

  # Create httr2 request
  req <- httr2::request(paste0(API_BASE_URL, "/requests")) |>
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

  cat("\nSubmitting API request...\n")

  # Print summary of submission
  cat("\n========= Request Summary ==========\n")
  cat(sprintf("Date Range: %s to %s\n", start_datetime, end_datetime))

  # Geography parameters
  if (!is.null(geofence_ids)) {
    cat(sprintf("Geofence IDs: %d geofences\n", length(geofence_ids)))
  }
  if (!is.null(wkt_list)) {
    cat(sprintf("WKT List: %d polygons\n", length(wkt_list)))
  }
  if (!is.null(geojson)) {
    # Read the original file to count features
    original_geojson <- sf::st_read(geojson, quiet = TRUE)
    original_count <- nrow(original_geojson)

    # Count features in the split version (from body)
    split_count <- length(body$geoJson$features)

    if (original_count == split_count) {
      cat(sprintf("GeoJSON: %d feature(s)\n", split_count))
    } else {
      cat(sprintf("GeoJSON: %d feature(s) (split from %d original)\n",
                  split_count, original_count))
    }
  }

  # Processing parameters
  cat(sprintf("Report Type: %s\n", report_type))
  cat(sprintf("Use Weights: %s\n", use_weights))
  cat(sprintf("Aggregate Polygons: %s\n", aggregate_polygons))

  if (!is.null(aggregate_polygon_name)) {
    cat(sprintf("Aggregate Polygon Name: %s\n", aggregate_polygon_name))
  }

  if (!is.null(data_vintage)) {
    cat(sprintf("Data Vintage: %s\n", data_vintage))
  }

  # Segmentation
  if (!is.null(append_prizm_segmentation)) {
    cat(sprintf("PRIZM Segmentation: %s\n", append_prizm_segmentation))
  }

  # Filtering parameters
  if (!is.null(daily_time_filter)) {
    cat(sprintf("Daily Time Filter: %d filter(s) applied\n", length(daily_time_filter)))
  }

  if (!is.null(ping_filter)) {
    cat(sprintf("Ping Filter: %s\n", ping_filter))
  }

  cat("====================================\n")


  # Perform request
  resp <- httr2::req_perform(req)
  result <- httr2::resp_body_json(resp)

  request_id <- result$requestId
  cat("\nRequest successful!\nRequest ID:", request_id, "\n")

  if (is.null(request_id)) {
    cat("ERROR: Failed to submit request.")
    return(NULL)
  }

  # Poll API to check if ready
  cat("\n========================================\n")
  cat("Getting request status updates...\n")
  cat("========================================\n")

  repeat {
    bearer_token <- .quietly_get_bearer_token()
    request_status <- .get_request_status(bearer_token, request_id)

    if (is.null(request_status)) {
      cat("Error: Failed to get request status. Aborting.\n")
      return(NULL)
    }

    current_status <- request_status$requestStatus

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
      cat("Request status:", current_status, "(waiting 30s)\n")
      Sys.sleep(30)
    }

  }

  # Pull from Environics Azure blob to temp directory for further processing
  geojson_data <- jsonlite::fromJSON(readLines(geojson), simplifyVector = FALSE)

  # Basic output directory names
  temp_dir <- "temp"
  output_base_dir <- "output"

  # Construct detailed name for API call
  start_time_clean <- gsub("[: ]", "_", start_datetime)
  end_time_clean <- gsub("[: ]", "_", end_datetime)
  geography_name <- paste0(geojson_data$name, "_", start_time_clean, "_to_", end_time_clean)

  downloaded_files <- .download_results(bearer_token, request_id, temp_dir)

  if (is.null(downloaded_files) || length(downloaded_files) == 0) {
    cat("ERROR: No files downloaded for", request_id, "\n")
    return(NULL)
  }

  # Create final output directory for this geography's data
  final_output_dir <- paste0(output_base_dir, "/", geography_name)
  .merge_api_chunks(temp_dir, final_output_dir, geography_name, start_datetime, end_datetime)

  cat("\n########################################\n")
  cat("COMPLETED:", geography_name, "\n")
  cat("Output directory:", final_output_dir, "\n")
  cat("########################################\n")

  invisible(NULL)
}
