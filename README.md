# environicsanalytics

R wrapper for the Environics Analytics API. Currently supports the
MobileScapes endpoint for querying location-based mobility data.

## Installation

``` r
# Install Dependencies (if needed)
install.packages(c("AzureStor","dplyr","geojsonio","httr2","jsonlite","readr","sf","dotenv"))

# Install from GitHub
devtools::install_github("canadianurbaninstitute/environicsanalytics")
```

## Quick Start

### Authentication

Store your Environics Analytics credentials in a `.env` file:

```         
CLIENT_ID=your_client_id_here
CLIENT_SECRET=your_client_secret_here
SCOPE=mobilescapes
```

Initialize credentials at the start of your session:

``` r
library(environicsanalytics)
library(dotenv)

# Initialize credentials (.env is loaded automatically)
init_credentials()
```

### Basic Usage

``` r
# Pull MobileScapes data for a specific time period and geography
pull_mobilescapes(
  start_datetime = "2024-01-01 00:00:00",
  end_datetime = "2024-01-31 23:59:59",
  geojson = "path/to/your/geography.geojson",
  use_weights = TRUE,
  aggregate_polygons = TRUE
)
```

This will authenticate, submit the request, poll for completion, and
download the results to your working directory in a folder called
"output".

Review the [Environics API Documentation](https://developers.environicsanalytics.com/) to get more specifics on how the parameters work in detail.

## Function Reference

### Authentication Functions

**`init_credentials()`**\
Sets up OAuth credentials for the package. Must be called once per
session before making API requests.

**`get_bearer_token()`**\
Returns a valid bearer token, automatically refreshing if expired. Used
internally by query functions.

**`clear_credentials()`**\
Removes all stored credentials and tokens from memory. Useful for
switching between accounts or testing.

### Query Functions

**`pull_mobilescapes()`**\
Main function for submitting and retrieving MobileScapes data. Accepts
geography via GeoJSON file, geofence IDs, or WKT polygons. Handles
request submission, polling, Azure download, and file consolidation.
Returns CEL and CDL CSV files in the output directory.

Parameters include: 
- `start_datetime`, `end_datetime`: Time range in "YYYY-MM-DD hh:mm:ss" format
- `geojson`: Path to GeoJSON file (features will be split if they exceed size limits)
- `geofence_ids`: Vector of EA geofence IDs (alternative to GeoJSON)
- `wkt_list`: List of Well-Known Text polygon definitions (alternative to GeoJSON)
- `use_weights`: Apply PI mobile device weighting (default: TRUE)
- `aggregate_polygons`: Aggregate results across polygons (default: TRUE)
- `aggregate_polygon_name`: Custom name for aggregated results
- `append_prizm_segmentation`: Add PRIZM segmentation (default: "prizm")
- `daily_time_filter`: Filter for specific times/days of week
- `ping_filter`: First ping filter ("first" or NULL)
- `report_type`: Report type (default: "celcdl")
- `data_vintage`: Specify data vintage

Review the [Environics API Documentation](https://developers.environicsanalytics.com/) to get more specifics on how the parameters work in detail.

### Debugging Functions

**`process_geojson_file()`**\
Processes and validates GeoJSON files. Automatically splits features
that exceed the API's 5M square foot limit. Optionally saves cleaned
output.

```         
# Process and split large features
result <- process_geojson_file(
  filepath = "large_area.geojson",
  max_area_sqft = 5000000,
  output_filepath = "split_output.geojson"
)
```

**`test_query_mobilescapes()`**\
Creates a dry run of an API request without submitting. Outputs the
exact request that would be sent to "test_query.txt" for inspection and
debugging.

## Contact

-   **Maintainer**: Luca Carnegie
    ([lcarnegie\@canurb.org](mailto:lcarnegie@canurb.org))
-   **GitHub**:
    <https://github.com/canadianurbaninstitute/environicsanalytics>
-   **Issues**:
    <https://github.com/canadianurbaninstitute/environicsanalytics/issues>
