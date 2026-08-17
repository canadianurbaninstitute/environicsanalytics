# environicsanalytics

R wrapper for the Environics Analytics API. Currently supports the
MobileScapes v5 endpoint for querying location-based mobility data.

> **Note:** As of v5, MobileScapes only accepts EA Geofence Library IDs
> (`geofence_ids`) as geography input. Custom GeoJSON/WKT polygon submission
> is no longer supported by the API. Use `discover_mobilescapes_geofences()`
> to find geofence IDs that fall within an area of interest (e.g. a BIA
> boundary) before calling the report/extract functions below.

## Installation

``` r
# Install Dependencies (if needed)
install.packages(c("AzureStor","dplyr","httr2","readr","dotenv"))

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

# Load environment variables
dotenv::load_dot_env(".env")

# Initialize credentials
init_credentials()
```

### Basic Usage

``` r
# Find geofence IDs for an area of interest (e.g. a BIA boundary)
geofences <- discover_mobilescapes_geofences(
  filter_definition = "PRCDCSD_NAME IN ('Oshawa, ON (CY)')",
  vintage = "2026"
)

# Pull a bulk Origins CSV extract for a specific time period and geofences
pull_mobilescapes(
  geofence_ids = geofences$geofenceId,
  start_date = "2026-01-01",
  end_date = "2026-01-31",
  vintage = "2026"
)
```

This will authenticate, submit the extract request, poll for completion, and
download the consolidated CSV to your working directory in a folder called
"ea_output".

Review the [Environics API Documentation](https://developers.mobilescapes.envision.environicsanalytics.com/) to get more specifics on how the parameters work in detail.

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

### Discovery Functions

**`get_mobilescapes_config()`**\
Returns dataset constraints (allowed date range, max geofence IDs per
request, max date range in months) for a given `country`/`vintage`.

**`discover_mobilescapes_geofences()`**\
Searches the 130,000+ geofence EA Geofence Library by filter expression
(e.g. `"PRCDCSD_NAME IN ('Toronto, ON (C)')"`), returning a data frame of
`geofenceId`/`geofenceName` pairs to use in the functions below.

### Report Functions

These call the synchronous v5 report endpoints and return results directly
(no polling needed):

- **`get_mobilescapes_origins()`** — Where visitors to the given geofence(s) come from, grouped by geography level (e.g. FSA).
- **`get_mobilescapes_destinations()`** — Visit counts, YoY change, and coordinates for each destination geofence.
- **`get_mobilescapes_destination_summary()`** — Detailed per-geofence summary: visit totals, weekday/weekend split, origin market share, top segments, demographics.
- **`get_mobilescapes_related_visits()`** — Where else visitors to a single geofence also went, by month. Accepts exactly one `geofence_id`.

All accept `geofence_ids`/`geofence_id`, `start_date`/`end_date` ("YYYY-MM-DD"),
and `country`/`vintage`; most also accept `days_of_week`, `time_of_day`, and
`dwell` filters.

### Bulk Extract Functions

**`pull_mobilescapes()`**\
Main function for submitting and retrieving a bulk Origins CSV extract.
Handles request submission, polling, Azure download, and file
consolidation into a single CSV in the output directory.

Parameters:
- `geofence_ids`: Vector of EA geofence IDs (see `discover_mobilescapes_geofences()`)
- `start_date`, `end_date`: Date range in "YYYY-MM-DD" format
- `days_of_week`: Optional vector of day filters (e.g. `c("Sat", "Sun")`)
- `time_of_day`: Optional time-of-day filter (default: "AllDay")
- `dwell`: Optional dwell filter (default: "Any")
- `country`: 2-digit country code (default: "ca")
- `vintage`: Dataset vintage, e.g. "2026" (required)
- `output_name`: Optional custom name for the output CSV
- `output_dir`: Directory, relative to working directory, that files will be written to (default: "ea_output")

It is certainly worth 5-10 mins to review the [Environics API Documentation](https://developers.mobilescapes.envision.environicsanalytics.com/) to get more specifics on how each parameter works in detail.

### Debugging Functions

**`test_query_mobilescapes()`**\
Creates a dry run of an Origins Extract request without submitting. Outputs
the exact request that would be sent to "test_mobilescapes_query.txt" for
inspection and debugging.

## API Notes

The Environics Analytics MobileScapes API enforces **daily quotas** and **rate limits**. All quotas reset at **05:00 AM UTC**. Call `get_mobilescapes_config()` for the current, authoritative `maxGeofenceIds`/`maxDateRangeMonths` limits for your country/vintage — the values below are general guidance only.

### Daily Quotas

- **Call Volume Quota:**  
  Up to **1,000 requests per user per day**, provided the geofence quota is not exceeded.

- **Geofences Quota:**  
  Up to **20,000 geofences per user per day**, distributed across multiple requests.

**Best Practice:**  Spread requests evenly throughout the day. Sending large batches of geofences in rapid bursts will likely trigger automatic throttling, introducing delays denoted by "QUEUED" statuses on requests, or otherwise. The total daily geofence quota remains guaranteed even if throttling occurs.

### Rate Limits

Each API endpoint also enforces its own **rate limits** to maintain overall system stability.  
If too many requests are made in a short time, the API may return a **`429 Too Many Requests`** error.  
Response details for rate limit errors may vary by endpoint.


## Contact

-   **Maintainer**: Luca Carnegie
    ([lcarnegie\@canurb.org](mailto:lcarnegie@canurb.org))
-   **GitHub**:
    <https://github.com/canadianurbaninstitute/environicsanalytics>
-   **Issues**:
    <https://github.com/canadianurbaninstitute/environicsanalytics/issues>
