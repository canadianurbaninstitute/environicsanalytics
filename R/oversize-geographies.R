#' Get Area of Features in Square Feet
#'
#' Transforms spatial features to EPSG:3347 (Canada metric projection) and
#' calculates area in square feet for accurate measurements.
#'
#' @param area An sf object containing spatial features
#'
#' @return Numeric vector of areas in square feet for each feature
#'
#' @importFrom sf st_transform st_area
#'
#' @keywords internal
.get_area_square_feet <- function(area) {
  # Transform to metric projected CRS for accurate area calculation (EPSG:3347 for Canada)
  geo_sf_3347 <- sf::st_transform(area, 3347)

  # Calculate and return area for each individual feature in square ft (1 m² = 10.7639 ft²)
  as.numeric(sf::st_area(geo_sf_3347)) * 10.7639
}


#' Check if All Individual Features are Less Than Maximum Area
#'
#' Validates that all features in an sf object are below a specified area threshold.
#' Prints diagnostic information about features that exceed the limit.
#'
#' @param feature An sf object containing spatial features
#' @param max_area_sqft Numeric. Maximum allowed area in square feet per feature.
#'   Default is 5000000 (5 million square feet)
#'
#' @return Logical. TRUE if ALL features are less than max_area_sqft, FALSE otherwise
#'
#' @keywords internal
.check_feature_area <- function(feature, max_area_sqft = 5000000) {
  # Calculate area for each individual feature in square ft
  feature_areas_sqft <- .get_area_square_feet(feature)

  # Check if all features are within the limit
  features_over_limit <- which(feature_areas_sqft >= max_area_sqft)
  all_features_valid <- length(features_over_limit) == 0

  if (all_features_valid) {
    cat("\u2713 All features within the limit\n")
    return(TRUE)
  } else {
    cat(sprintf("\u2717 %d feature(s) EXCEED the limit!\n", length(features_over_limit)))
    cat(sprintf("Feature indices exceeding limit: %s\n", paste(features_over_limit, collapse = ", ")))
    return(FALSE)
  }
}


#' Split Large Feature into Smaller Pieces
#'
#' Recursively splits a single large spatial feature into smaller pieces using a
#' grid-based approach until all pieces are below the maximum area threshold of
#' the Environics Analytics API (5 million ft^2)
#'
#' @param feature An sf object containing a single spatial feature
#' @param max_area_sqft Numeric. Maximum area in square feet for each piece
#' @param safety_factor Numeric. Safety factor applied to grid cell size calculation
#'   to ensure resulting pieces are below the maximum area. Values > 1 create
#'   smaller grid cells
#'
#' @return An sf object containing multiple smaller features, each with a unique
#'   piece_id column
#'
#' @importFrom sf st_transform st_make_grid st_intersection st_sf
#'
#' @keywords internal
.split_large_feature_grid <- function(feature, max_area_sqft, safety_factor) {

  # Transform to EPSG:3347 (Canada) for consistent area calculations
  feature_3347 <- sf::st_transform(feature, 3347)

  # Calculate grid cell size needed (in meters, since EPSG:3347 is metric)
  grid_cell_area_sqft <- max_area_sqft / safety_factor
  grid_cell_area_sqm <- grid_cell_area_sqft / 10.7639  # Convert to square meters
  cell_side_m <- sqrt(grid_cell_area_sqm)

  cat(sprintf("Grid cell size: %.2f sq ft (%.2f meters per side)\n",
              grid_cell_area_sqft, cell_side_m))

  # Create square grid and intersect with geography
  grid <- sf::st_make_grid(feature_3347, cellsize = cell_side_m, square = TRUE)
  pieces <- sf::st_intersection(feature_3347, grid)

  # Convert to sf object with IDs
  if (!inherits(pieces, "sf")) {
    pieces <- sf::st_sf(geometry = pieces)
  }
  pieces$piece_id <- seq_len(nrow(pieces))

  cat(sprintf("Created %d initial pieces from grid intersection\n", nrow(pieces)))

  # Check if all pieces are below max_area_sqft. If not, split recursively.
  pieces_list <- list()

  for (i in seq_len(nrow(pieces))) {
    piece_sf <- pieces[i, ]
    piece_area_sqft <- .get_area_square_feet(piece_sf)

    if (piece_area_sqft >= max_area_sqft) {
      cat(sprintf("Piece %d still exceeds max area (%.2f sq ft). Splitting recursively...\n",
                  i, piece_area_sqft))
      pieces_list[[i]] <- .split_large_feature_grid(piece_sf, max_area_sqft, safety_factor)
    } else {
      pieces_list[[i]] <- piece_sf
    }
  }

  # Combine all split pieces into single sf object
  result <- do.call(rbind, pieces_list)
  result$piece_id <- seq_len(nrow(result))

  max_area <- max(.get_area_square_feet(result))
  cat(sprintf("Final result: %d features, max area: %.2f sq ft\n", nrow(result), max_area))

  result
}


#' Split Large Geographies into Smaller Pieces
#'
#' Main function to process an sf object and split any features exceeding the
#' maximum area threshold of the Environics Analytics API into smaller pieces.
#' Features below the threshold are retained unchanged.
#'
#' @param geography An sf object containing one or more spatial features
#' @param max_area_sqft Numeric. Maximum area in square feet for each piece.
#'   Default is 5000000 (5 million square feet)
#' @param safety_factor Numeric. Safety factor for grid calculation, applied when
#'   splitting features. Default is 1.05. Higher values create smaller grid cells
#'   to ensure pieces stay below the maximum area
#'
#' @return An sf object with all features below max_area_sqft. Split features will
#'   have suffixed names (e.g., "original_name_1", "original_name_2")
#'
#' @keywords internal
.split_large_geographies <- function(geography, max_area_sqft = 5000000, safety_factor = 1.05) {

  cat(sprintf("\n=== Processing geography with %d features ===\n", nrow(geography)))

  # Calculate areas for all features
  feature_areas <- .get_area_square_feet(geography)

  # Identify which features need splitting
  needs_splitting <- which(feature_areas >= max_area_sqft)

  if (length(needs_splitting) == 0) {
    cat("\u2713 All features are already below the maximum area!\n")
    return(geography)
  }

  cat(sprintf("Found %d feature(s) that need splitting\n", length(needs_splitting)))

  # Store all features (split and non-split)
  all_features <- list()

  for (i in seq_len(nrow(geography))) {
    feature_sf <- geography[i, ]
    feature_area <- feature_areas[i]

    # Get the feature name from the Name column
    feature_name <- if ("Name" %in% names(geography)) {
      as.character(feature_sf$Name)
    } else {
      paste0("feature_", i)
    }

    if (i %in% needs_splitting) {
      cat(sprintf("\n--- Feature %d (%s): %.2f sq ft (EXCEEDS LIMIT) ---\n", i, feature_name, feature_area))
      split_pieces <- .split_large_feature_grid(feature_sf, max_area_sqft, safety_factor)

      # Add Name column to split pieces with suffixes
      split_pieces$Name <- paste0(feature_name, "_", seq_len(nrow(split_pieces)))

      all_features[[i]] <- split_pieces
    } else {
      cat(sprintf("Feature %d (%s): %.2f sq ft (OK)\n", i, feature_name, feature_area))

      # Ensure Name column exists
      if (!"Name" %in% names(feature_sf)) {
        feature_sf$Name <- feature_name
      }

      all_features[[i]] <- feature_sf
    }
  }

  # Combine all features
  result <- do.call(rbind, all_features)

  cat(sprintf("\n=== FINAL RESULT ===\n"))
  cat(sprintf("Input features: %d\n", nrow(geography)))
  cat(sprintf("Output features: %d\n", nrow(result)))
  cat(sprintf("Max area: %.2f sq ft\n", max(.get_area_square_feet(result))))

  # Verify all features are below limit
  if (.check_feature_area(result, max_area_sqft)) {
    cat("\u2713 Success! All features are now below the maximum area.\n")
  } else {
    warning("Some features still exceed the maximum area. Consider lowering the safety_factor.")
  }

  result
}
