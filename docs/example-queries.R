## MobileScapes v5 API - Guided Examples
##
## Walk through this file top to bottom, one call at a time, to get a feel
## for the API. Every geofence ID and number of visits below is real,
## verified output from this package - not placeholder data. Companion to
## docs/v4-to-v5-changes.md, which explains the "why" behind each gotcha
## demonstrated here.
##
## Prerequisites: a project-level .Renviron with CLIENT_ID/CLIENT_SECRET
## (see README.md). Loaded automatically when you library() the package.

library(environicsanalytics)
init_credentials()

VINTAGE <- "2026"

# A handful of real, confirmed-working "EA Standard" geofence IDs, drawn
# from the Main Street Index project. Safe to reuse for exploration.
BARRIE <- "E2182542" # Downtown Barrie BIA
HALIFAX <- "E2182592" # Downtown Halifax BIA
JAMAICA <- "E2182783" # Little Jamaica (Toronto)
BIA_124 <- "E2182571" # 124 Street BIA (Edmonton)

# 1. CONFIG - always start here =========================================
#
# Cheap, fast, no quota concerns. Tells you the real, current limits for
# your country/vintage rather than trusting anything documented elsewhere.

get_mobilescapes_config(vintage = VINTAGE)
# Expect something like:
#   maxGeofenceIds: 500       <- max geofence_ids per request
#   maxDateRangeMonths: 12    <- max span between start_date and end_date
#   startDate / endDate       <- allowed request date bounds

# 2. DESTINATIONS - visit counts for a place, with coordinates ==========
#
# One row per geofence_id. Use a WIDE window - see gotcha #1 below for why.

get_mobilescapes_destinations(
  geofence_ids = c(BARRIE, HALIFAX, JAMAICA, BIA_124),
  start_date = "2025-07-01",
  end_date = "2026-06-30", # 12 months - the max allowed
  vintage = VINTAGE
)
# Expect 4 rows: geofenceId, visits, percentChange (YoY), latitude, longitude.
# Real result at time of writing: 2.2M-12.8M visits per BIA over the year -
# a whole downtown core aggregates far more traffic than one store.

# 3. GOTCHA #1 - a short window can return NOTHING for a valid ID =======
#
# Same geofence, same package, just a narrower date range:

get_mobilescapes_destinations(
  geofence_ids = BARRIE,
  start_date = "2026-06-01",
  end_date = "2026-06-30", # just one month
  vintage = VINTAGE
)
# Returns a 0-row tibble - NOT an error, NOT a visits=0 row. This does not
# mean the ID is invalid. Compare to the 12-month call above, which returns
# a real row for the same ID. Always widen the window before concluding a
# geofence ID doesn't work.

# 4. GOTCHA #2 - "Custom" geofence IDs are rejected outright ============
#
# The Envision UI shows "EA Standard" (E-prefix) and "Custom" (C-prefix)
# geofences side by side, as if interchangeable. They are not, for
# MobileScapes v5:

tryCatch(
  get_mobilescapes_destinations(
    geofence_ids = "C12720", # a real Custom geofence ID
    start_date = "2026-06-01",
    end_date = "2026-06-30",
    vintage = VINTAGE
  ),
  error = function(e) message("Got the expected error:\n", conditionMessage(e))
)
# Expect: 400 Bad Request, "Invalid geofence ID(s): C12720" - reproduces
# for this ID alone, not just when mixed with valid ones. Cause unconfirmed
# (see docs/v4-to-v5-changes.md) - only use E-prefixed IDs for now.

# 5. ORIGINS - where do a place's visitors come from? ===================
#
# geo_level_code controls the aggregation level - one request, one level.
# Finest available: PRCDDA (Dissemination Area).

get_mobilescapes_origins(
  geofence_ids = BARRIE,
  start_date = "2025-07-01",
  end_date = "2026-06-30",
  geo_level_code = "PRCDDA",
  vintage = VINTAGE
)
# One row per origin DA: geoCode, visits (fractional - a modelled estimate,
# not a raw count).

# Same call, coarser level - compare row counts and total visits (should
# match the destinations() total above, since it's the same underlying data):
get_mobilescapes_origins(
  geofence_ids = BARRIE,
  start_date = "2025-07-01",
  end_date = "2026-06-30",
  geo_level_code = "FSA",
  vintage = VINTAGE
)

# Try an invalid level to see the API's own validation message:
tryCatch(
  get_mobilescapes_origins(
    geofence_ids = BARRIE, start_date = "2025-07-01", end_date = "2026-06-30",
    geo_level_code = "NEIGHBOURHOOD", vintage = VINTAGE
  ),
  error = function(e) message(conditionMessage(e))
)
# Lists the full allowed set: PRCDDA, PRCDADA, FSA, PRCDCSD, PRCD, PR, CMACA

# 6. DESTINATION SUMMARY - the richest single call =======================
#
# Total/average visits, weekday vs weekend, origin market share, top
# segments, demographics - all in one response, for one geofence.

summary <- get_mobilescapes_destination_summary(
  geofence_ids = BARRIE,
  start_date = "2025-07-01",
  end_date = "2026-06-30",
  vintage = VINTAGE
)
str(summary, max.level = 2)

# 7. RELATED VISITS - where else did this place's visitors go? ==========
#
# Exactly one geofence_id only - this is the one function in the package
# that enforces that (others silently pool multiple IDs together instead).

related <- get_mobilescapes_related_visits(
  geofence_id = BARRIE,
  start_date = "2025-07-01",
  end_date = "2026-06-30",
  vintage = VINTAGE
)
str(related, max.level = 3)
# Look at related$data[[1]]$slices - one entry per month, each with top-5
# co-visited geofences/banners inside and outside the primary's category.

# 8. DISCOVER GEOFENCES - search the library by attribute ================
#
# This is by far the most heavily rate-limited MobileScapes endpoint. In
# testing this package, a single account hit 429s on every call across an
# entire multi-hour session, surviving 5 retries with exponential backoff
# each time - this can be a sustained block, not just an occasional one. If
# it happens to you, it's very likely account/quota-wide, not something
# retrying harder will fix. There is no spatial search on this endpoint:
# only attribute filters like municipality, category, or banner.

discover_mobilescapes_geofences(
  filter_definition = "PRCDCSD_NAME IN ('Barrie, ON (C)')",
  vintage = VINTAGE
)
# Paginates automatically (all_pages = TRUE by default) - returns every
# match, not just the first page.

# 9. THE EXTRACT - bulk, async, richer schema (run this one separately) ==
#
# Unlike the synchronous calls above, this submits a request, polls for
# completion (can take a few minutes), downloads, and merges a CSV to disk.
# It returns coordinates, postal code, and the full geography hierarchy -
# strictly more detail than get_mobilescapes_origins() offers.
#
pull_mobilescapes(
  geofence_ids = c(BARRIE, HALIFAX),
  start_date = "2026-05-01",
  end_date = "2026-05-31",
  vintage = VINTAGE,
  output_name = "example_pull"
)
