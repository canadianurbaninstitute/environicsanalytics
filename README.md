# environicsanalytics

R wrapper for the Environics Analytics API. Currently supports the
MobileScapes v5 endpoint for querying location-based mobility data.

> **Note:** As of v5, MobileScapes only accepts EA Geofence Library IDs
> (`geofence_ids`) as geography input. Custom GeoJSON/WKT polygon submission
> is no longer supported by the API. Use `discover_mobilescapes_geofences()`
> to find geofence IDs that fall within an area of interest (e.g. a BIA
> boundary) before calling the report/extract functions below.
>
> **Upgrading from v4?** See
> [Migrating Downstream Packages (v4 → v5)](#migrating-downstream-packages-v4--v5)
> below for what downstream code must change,
> [docs/v4-to-v5-changes.md](docs/v4-to-v5-changes.md) for the complete
> API-level change reference, and
> [docs/msmdata-v5-impact.md](docs/msmdata-v5-impact.md) for a plain-language
> assessment of which msmdata outputs survive the move.

## Installation

``` r
# Install Dependencies (if needed)
install.packages(c("dplyr","httr2","readr"))

# Install from GitHub
devtools::install_github("canadianurbaninstitute/environicsanalytics")
```

## Quick Start

### Authentication

Store your Environics Analytics credentials in a project-level `.Renviron`
file (not `.env` - no extra package needed, and R loads it automatically at
session startup):

```         
CLIENT_ID=your_client_id_here
CLIENT_SECRET=your_client_secret_here
```

`SCOPE` and `TOKEN_URL` don't need to be set - `init_credentials()` defaults
to the v5 Microsoft Entra endpoint automatically. Only add them if you need
to override that (e.g. pointing at a different tenant). **Do not set
`SCOPE=mobilescapes`** - that was the v4 value, and the package will refuse
to authenticate with it rather than fail with a confusing error later.

Since `.Renviron` only loads at session startup, restart R (or run
`readRenviron(".Renviron")`) after creating or editing it. Add `.Renviron` to
`.gitignore` - it holds credentials and should never be committed.

Initialize credentials at the start of your session:

``` r
library(environicsanalytics)

# Only needed if you didn't restart R after creating/editing .Renviron
readRenviron(".Renviron")

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

By default it **paginates automatically** and returns every match, requesting
successive pages until the API returns a short one. Pass `all_pages = FALSE`
to get just the single page named by `page`. `max_pages` (default 100) caps
the loop and warns if hit. This endpoint is the most aggressively rate-limited
in v5 — expect `429`s on large sweeps.

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

## Migrating Downstream Packages (v4 → v5)

v5 is a **breaking change**. Any package that calls this one — `msmdata` in
particular — will not run unchanged. The changes below are ordered roughly by
how much downstream work each one causes.

### 1. Authentication moved to Microsoft Entra (blocking)

v5 does not accept the v4 token endpoint. Credentials that work in v5 return
`{"error":"invalid_client"}` against
`https://login.environicsanalytics.com/connect/token`.

| | v4 | v5 |
|---|---|---|
| Token URL | `login.environicsanalytics.com/connect/token` | `login.microsoftonline.com/eaplatform.onmicrosoft.com/oauth2/v2.0/token` |
| `SCOPE` | `mobilescapes` | `https://eaplatform.onmicrosoft.com/api/.default` |

The package defaults now point at Entra, and both can be overridden by
environment variable (`SCOPE`, `TOKEN_URL`) or argument. **Any downstream
package or deployment that hardcodes the old token URL, or ships a `.env`
with `SCOPE=mobilescapes`, must be updated or it will fail at
`init_credentials()`.**

### 2. Geography input: GeoJSON/WKT are gone

This is the largest change. v4 accepted arbitrary polygons; v5 accepts **only
EA Geofence Library IDs**.

Removed from the package entirely:

| Removed | Replacement |
|---|---|
| `pull_mobilescapes(geojson = ...)` | `pull_mobilescapes(geofence_ids = ...)` |
| `pull_mobilescapes(wkt_list = ...)` | `pull_mobilescapes(geofence_ids = ...)` |
| `process_geojson_file()` (exported) | No replacement — no longer needed |
| Oversize-geography splitting (`R/oversize-geographies.R`) | No replacement — the 5,000,000 sq ft polygon cap no longer applies, EA geofences are pre-sized |

Downstream code that stored a GeoJSON per area and passed the file path must
now resolve that area to a **set of geofence IDs** first, via
`discover_mobilescapes_geofences()`. Because geofence discovery is a filter
expression against EA's library (not a spatial intersection against your
polygon), this is a genuine modelling change, not a mechanical find-and-replace:
you must decide, per area, which filter expression defines it, and store that
mapping. See [Recommended pattern for many areas](#recommended-pattern-for-many-areas)
below.

### 3. Origin geography: depends which path you use

The two v5 paths differ enormously, and the choice drives most of the
downstream work.

**Async CSV extract (`pull_mobilescapes()`) — keeps v4's detail.** One row per
origin postal code, verified against a real extract (791 rows x 55 columns for
one geofence over one month):

```
GeofenceName, Visits, PostalCode, LATITUDE, LONGITUDE,
Sunday…Saturday, January…December,
GEOCODETYPE, ISGEOCODED, ISBUSINESS, ISRETIRED, ISAPARTMENT, INDEMOGRAPHIC, ISLICENSED,
CAN, REG, PR, CMACA, PRCD, PRCDCSD, CMACT, PRCDADA, PRCDDA, PRFED, FSA, FSALDU
  (each with a matching _NAME column)
```

Column renames are mechanical — drop the `CEL_` prefix and change case:
`COMMON_EVENING_LAT` → `LATITUDE`, `CEL_PRCDDA` → `PRCDDA`, `VISIT` → `Visits`,
and so on. `GeofenceName` is a column, so **one extract can cover many areas**.

Genuinely gone from the extract:

| v4 column | Status |
|---|---|
| `EARLYMORNING`…`LATEEVENING` (7 day-parts) | **No equivalent** — `timeOfDay` is a filter with 4 values |
| `SEGMENT` (PRIZM) | **No equivalent** — `appendSegmentation` removed |
| `VISITOR` | **No equivalent** |
| `WEEKDAY`, `WEEKEND` | Derive from the day columns |

**Synchronous origins report (`get_mobilescapes_origins()`) — much coarser.**
Returns `{geoCode, visits}` at one level, no coordinates, no per-geofence
breakout. Allowed `geo_level_code` values, verified against the API, finest to
coarsest:

**`PRCDDA`** (Dissemination Area) → `PRCDADA` → `FSA` → `PRCDCSD` → `PRCD` →
`CMACA` → `PR`

Census tract, `REG` and `PRFED` are **not** available on this path, and results
are **pooled across all `geofence_ids`** in the request — so per-area profiles
need one call per area.

`visits` is **fractional** on both paths (modelled estimates, e.g. `6.14`), not
the integer counts v4 produced.

**Rule of thumb:** if you need coordinates, postal code, census tract, or
per-geofence breakout, use the extract. Use the synchronous report for quick
aggregates where one geography level suffices.

### 4. `pull_mobilescapes()` signature

``` r
# v4
pull_mobilescapes(
  start_datetime = "2024-01-01 00:00:00",
  end_datetime   = "2024-01-31 23:59:59",
  geojson        = "path/to/area.geojson",
  use_weights    = TRUE,
  aggregate_polygons = TRUE,
  report_type    = "celcdl",
  data_vintage   = NULL
)

# v5
pull_mobilescapes(
  geofence_ids = c("E12345", "E12346"),
  start_date   = "2024-01-01",
  end_date     = "2024-01-31",
  vintage      = "2026",          # now REQUIRED
  output_name  = "oshawa_bia"     # new; controls the output folder/file name
)
```

| v4 parameter | v5 status |
|---|---|
| `start_datetime` / `end_datetime` (`"YYYY-MM-DD hh:mm:ss"`) | Renamed to `start_date` / `end_date`, format is now **date-only** `"YYYY-MM-DD"` |
| `data_vintage` (optional) | Renamed to `vintage` and is now **required** on every call |
| `geojson`, `wkt_list` | Removed (see above) |
| `use_weights` | Removed — weighting is applied by the API |
| `aggregate_polygons`, `aggregate_polygon_name` | Removed — use `output_name` to control output naming |
| `append_prizm_segmentation` | Removed — see `target_set` on the report functions |
| `daily_time_filter` | Replaced by `days_of_week` (e.g. `c("Sat","Sun")`) and `time_of_day` (default `"AllDay"`) |
| `ping_filter` | **Removed.** `dwell` is a *new, different* filter (visit duration, default `"Any"`) — not a rename. v4's ping de-duplication (`first`/`firstlast`/`none`) has no v5 equivalent |
| `report_type` (`"celcdl"`) | Removed — the async extract endpoint returns Origins data only |
| — | New: `country` (default `"ca"`), `output_name` |

`output_dir` is unchanged (default `"ea_output"`), and the return value is
still the full path to the request's output directory.

### 5. Output file layout

v4 wrote **two** files per request; v5 writes **one**.

```
# v4
ea_output/<geojson$name>_<start_datetime>_to_<end_datetime>/
  <name>_cel.csv
  <name>_cdl.csv
  raw/cel_report_*.csv.gz
  raw/cdl_report_*.csv.gz

# v5
ea_output/<output_name or origins_<start>_to_<end>>/
  <output_name>.csv
  raw/*.csv(.gz)
```

Downstream consequences:

- Any code globbing `*_cel.csv` / `*_cdl.csv` will find nothing. There is no
  CEL/CDL split in v5 — the extract is Origins data in a single consolidated CSV
  (see §3 for its columns).
- The default folder name no longer embeds the geography name from the GeoJSON's
  `name` field (there is no GeoJSON). It defaults to
  `origins_<start_date>_to_<end_date>`, which is **identical across areas** for
  the same date range. If you loop over areas, you **must** pass `output_name`
  per area or each iteration will collide with the last.
- Timestamps in the default folder name are now dates, not datetimes, so any
  downstream regex parsing the folder name for a time range needs updating.
- The extract column schema **has now been verified** against a real v5
  extract — see §3 for the full header and the v4 mapping.

### 6. Dependencies

`sf`, `geojsonio`, and `jsonlite` were dropped from `Imports` (no polygon
handling remains). If a downstream package used any of these but relied on
them arriving transitively via `environicsanalytics`, add them to that
package's own `DESCRIPTION`.

### 7. New capability: synchronous report endpoints

If a downstream package only needs aggregate numbers — visit counts, origin
market share, demographics — it may no longer need the async extract at all.
`get_mobilescapes_origins()`, `get_mobilescapes_destinations()`,
`get_mobilescapes_destination_summary()`, and
`get_mobilescapes_related_visits()` return data frames/lists directly with no
submit-poll-download-merge cycle and no Azure dependency. Worth checking
before porting an extract-based pipeline as-is.

### Recommended pattern for many areas

For a set of areas you already have geofences for (e.g. ~100 BIAs), resolve
the area → geofence-ID mapping **once**, persist it, and reuse it. Discovery
calls count against your daily quota, so re-resolving on every run is wasteful.

``` r
# Step 1 (once): build and save an area -> geofence_ids lookup.
vintage <- "2026"

lookup <- lapply(areas$filter_definition, function(f) {
  discover_mobilescapes_geofences(
    filter_definition = f,
    vintage = vintage
  )   # paginates automatically; returns the full matching set
})
names(lookup) <- areas$area_name
saveRDS(lookup, "geofence_lookup.rds")

# Step 2 (per run): pull one extract per area, naming outputs explicitly.
lookup <- readRDS("geofence_lookup.rds")

for (area_name in names(lookup)) {
  pull_mobilescapes(
    geofence_ids = lookup[[area_name]]$geofenceId,
    start_date   = "2026-01-01",
    end_date     = "2026-01-31",
    vintage      = vintage,
    output_name  = area_name   # REQUIRED to keep areas from overwriting each other
  )
}
```

Two limits to respect when scaling this up:

- **`maxGeofenceIds` per request.** Call `get_mobilescapes_config()` for the
  authoritative value for your country/vintage. If an area's geofence set
  exceeds it, split the IDs into chunks, call `pull_mobilescapes()` per chunk
  with distinct `output_name`s, and bind the resulting CSVs downstream. The
  package does **not** chunk for you.
- **`maxDateRangeMonths`.** Also from `get_mobilescapes_config()`. Longer
  ranges must be split into multiple requests and recombined.

Both are hard API-side limits in v5 and are the most likely first failure when
porting a v4 loop that assumed arbitrary polygon size and date range.

### Migration checklist

- [ ] Point auth at Entra: update `SCOPE`, drop any hardcoded v4 token URL
      (the package now detects and rejects the v4 endpoint/scope with a clear
      error before making a network call, and `pull_mobilescapes()`/
      `test_query_mobilescapes()` reject removed v4 arguments by name instead
      of a generic "unused argument" error)
- [ ] Rename extract columns: `COMMON_EVENING_LAT`/`LON` → `LATITUDE`/`LONGITUDE`, drop `CEL_` prefixes
- [ ] Replace time-of-day day-parts with the 4-value `time_of_day` filter
- [ ] Rework PRIZM segment analysis — no per-visitor `SEGMENT` in v5
- [ ] If using the sync origins report, call it once per area (results pool); the extract needs no such split
- [ ] Replace every GeoJSON/WKT input with a persisted area → `geofence_ids` mapping
- [ ] Drop calls to `process_geojson_file()` and any oversize-splitting logic
- [ ] Rename `start_datetime`/`end_datetime` → `start_date`/`end_date` and truncate to `YYYY-MM-DD`
- [ ] Add a required `vintage` to every call
- [ ] Remove `use_weights`, `aggregate_polygons`, `append_prizm_segmentation`, `report_type`
- [ ] Map `daily_time_filter` → `days_of_week`/`time_of_day`; drop `ping_filter` (no equivalent)
- [ ] Pass `output_name` per area to prevent output collisions
- [ ] Update readers: one consolidated CSV, no `_cel`/`_cdl` split; revalidate column names
- [ ] Add `sf`/`geojsonio`/`jsonlite` to your own `DESCRIPTION` if still used
- [ ] Add chunking for `maxGeofenceIds` and `maxDateRangeMonths`
- [ ] Consider whether the synchronous report endpoints remove the need for extracts

## API Notes

### Limits (authoritative)

Call `get_mobilescapes_config()` for the real limits for your country/vintage.
Observed for `ca`/`2026` (2026-08-18):

| Key | Value |
|---|---|
| `maxGeofenceIds` | 500 per request |
| `maxDateRangeMonths` | 12 |
| `startDate` | 2023-01-01 |
| `endDate` | 2026-08-09 |
| `h3endDate` | 2026-08-09 |

These are hard API-side limits. The package does not chunk for you.

### Rate limits

v5 rate-limits and returns a bare `{"status":429}` body with no `Retry-After`
header. The geofence discovery endpoint in particular returns 429 readily and
can stay limited for minutes at a time. Package requests retry `429`/`503`
automatically up to 5 times with exponential backoff, but sustained bursts
will still fail — spread large jobs out.

### Daily quotas (v4 documentation — unverified for v5)

> The figures below come from the **v4** documentation. The v5 documentation
> contains **no quota section at all**, and none of these numbers have been
> confirmed against v5. Treat them as a rough planning guide only, and rely on
> `get_mobilescapes_config()` for anything authoritative.

- **Call volume:** up to 1,000 requests per user per day
- **Geofences:** up to 20,000 geofences per user per day
- **Per request:** 300 geofences (v4 figure; v5 `config` reports 500)
- **Reset:** 05:00 AM UTC daily

v4 also surfaced an `ENQUEUED` status when throttled. v5 documents only
`Submitted`, `Processing`, `Complete`, `Failed`, and `Expired` — there is no
queued state, so throttling shows up as `429` responses instead.

## Contact

-   **Maintainer**: Luca Carnegie
    ([lcarnegie\@canurb.org](mailto:lcarnegie@canurb.org))
-   **GitHub**:
    <https://github.com/canadianurbaninstitute/environicsanalytics>
-   **Issues**:
    <https://github.com/canadianurbaninstitute/environicsanalytics/issues>
