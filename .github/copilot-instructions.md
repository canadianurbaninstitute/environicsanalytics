# Copilot Instructions for environicsanalytics

## Project Overview
- This is an R package providing a wrapper for the Environics Analytics MobileScapes API.
- The package handles authentication, API requests, Azure Blob Storage downloads, and data post-processing.
- Only the MobileScapes endpoint is currently supported.

## Key Components
- `R/authentication.R`: Manages OAuth2 credentials and bearer token retrieval. Use `init_credentials()` to set up credentials at session start.
- `R/mobilescapes_api.R`: Core API logic for submitting requests, polling status, downloading results, and merging chunked files. Main user functions: `pull_mobilescapes()`, `download_results()`, `merge_api_chunks()`.
- `R/oversize_geographies.R`: (If present) Handles splitting/filtering of large geographies for API efficiency.
- `man/`: Roxygen2-generated documentation for all exported functions.
- `tests/`: Uses `testthat` for unit testing. Entry point: `tests/testthat.R`.

## Developer Workflows
- **Authentication:**
  - Call `init_credentials()` with `client_id` and `client_secret` (or set as env vars) before any API calls.
  - Use `clear_credentials()` to reset credentials in the session.
- **API Usage:**
  - Use `pull_mobilescapes()` for end-to-end data retrieval (submits, polls, downloads, merges).
  - For debugging, use `test_query_mobilescapes()` to dry-run and inspect the request body.
- **Testing:**
  - Run all tests with `devtools::test()` or `testthat::test_dir('tests/testthat')`.
  - Add new tests in `tests/testthat/` using `test_that()` blocks.
- **Documentation:**
  - All user-facing functions are documented with Roxygen2. Run `devtools::document()` to update `man/` and `NAMESPACE`.

## Project Conventions
- All API calls use the `httr2` package for HTTP requests.
- Azure Blob downloads use the `AzureStor` package.
- Data is processed with `dplyr` and `readr`.
- Large geographies may require splitting; see `oversize_geographies.R` and related helpers.
- Output files are written to `output/` with subdirectories named by geography and date range.
- Temporary downloads are stored in `temp/`.

## Integration Points
- Requires valid Environics Analytics API credentials.
- Downloads results from Azure Blob Storage using SAS tokens provided by the API.
- Expects input geographies as GeoJSON files or objects.

## Examples
- See function documentation in `man/` or use `?pull_mobilescapes` in R for usage examples.
- Example workflow:
  ```r
  init_credentials(client_id = "...", client_secret = "...")
  pull_mobilescapes(start_datetime = ..., end_datetime = ..., geojson = "my.geojson")
  ```

## Special Notes
- The package is in early development; some functions are placeholders or not yet implemented.
- All configuration is via function arguments or environment variables; no config files.
- For new API endpoints or features, follow the structure in `mobilescapes_api.R`.
