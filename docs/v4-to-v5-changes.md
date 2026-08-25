# MobileScapes API: v4 → v5 Change Reference

Complete inventory of what changed between MobileScapes API v4 and v5, compiled
from the published v4 and v5 documentation (both are Postman collections; the
underlying collection JSON was diffed), the v4→v5 refactor of this package, and
direct probing of the live v5 API.

**Compiled:** 2026-08-18 · **Probed against:** `ca` / vintage `2026`

See also [msmdata-v5-impact.md](msmdata-v5-impact.md) for what these changes
mean for each dashboard output msmdata builds.

Sources:
- v5 docs — <https://developers.mobilescapes.envision.environicsanalytics.com/>
- v4 docs — <https://developers.mobilescapes.environicsanalytics.com/>

Claims below are marked **[docs]** where the documentation states them and
**[verified]** where they were confirmed against the live API. Anything marked
**[unverified]** is a gap in the v5 documentation that has not been tested.

---

## 0. Executive summary

The six changes that actually cost work:

1. **Authentication moved to Microsoft Entra.** v4 credentials flow no longer
   works; the old token endpoint returns `invalid_client`. **[verified]**
2. **Custom geography is gone.** No GeoJSON, no WKT. Only EA Geofence Library
   IDs. **[docs]**
3. **"Custom" geofence IDs (`C`-prefix) don't work in MobileScapes v5 at
   all** - only `"EA Standard"` IDs (`E`-prefix) do, even though both types
   are presented identically in the Envision UI. See §4. **[verified]**
4. **Origin detail depends on which path you use.** The *synchronous* origins
   report returns counts aggregated to one standard geography (finest:
   `PRCDDA`) with no coordinates. The *async CSV extract* still returns
   `LATITUDE`/`LONGITUDE`, full postal code, and the whole geography hierarchy,
   as v4 did. **[verified]**
5. **The extract is Origins-only.** The v4 CEL/CDL pair and the
   `geofencepings` report have no v5 equivalent. **[docs]**
6. **Data methodology changed.** v4 and v5 numbers are not a continuous time
   series. **[docs]**

---

## 1. Authentication

| | v4 | v5 |
|---|---|---|
| Provider | Environics IdentityServer | Microsoft Entra (Azure AD) |
| Token URL | `https://login.environicsanalytics.com/connect/token` | `https://login.microsoftonline.com/eaplatform.onmicrosoft.com/oauth2/v2.0/token` |
| Scope | `mobilescapes` | `https://eaplatform.onmicrosoft.com/api/.default` |
| Grant | `client_credentials` | `client_credentials` (unchanged) |
| Token lifetime | — | 3599s **[verified]** |

Posting v5 credentials to the v4 endpoint returns `{"error":"invalid_client"}`.
**[verified]**

v5 additionally enforces an API scope per endpoint — "Envision or MobileScapes"
— returning `403` if the authenticated user lacks it. **[docs]**

---

## 2. Endpoints

| Purpose | v4 | v5 |
|---|---|---|
| Submit extract | `POST /v4/{country}/requests` | `POST /v5/{country}/{vintage}/extracts/csv` |
| Extract status | `GET .../requests/{id}/status` | `GET .../extracts/{requestId}/status` |
| Extract result | `GET .../requests/{id}/resultInfo` | `GET .../extracts/{requestId}/result` |
| Geofence lookup | `GET .../geofences?$filter=` | `GET .../geofences?FilterDefinition=` |
| Origins report | — | `POST .../origins` *(new)* |
| Destinations report | — | `POST .../destinations` *(new)* |
| Destination summary | — | `POST .../destinations/summary` *(new)* |
| Related visits | — | `POST .../relatedvisits` *(new)* |
| Dataset config | — | `GET .../config` *(new)* |

**Path structure:** v5 inserts a required `{vintage}` segment after
`{country}` — `/mobilescapes/v5/ca/2026/origins`.

**No legacy fallback.** The v4 collection published v2 endpoints alongside v4;
the v5 collection publishes only v5.

---

## 3. Request body — extract submission

| v4 field | v5 |
|---|---|
| `startDateTime` / `endDateTime` (`YYYY-MM-DD hh:mm:ss`, polygon local time) | `startDate` / `endDate`, date-only `YYYY-MM-DD` |
| `geoJson` (RFC 7946, per-feature `id`) | **Removed** |
| `wktList` (POLYGON / MULTIPOLYGON) | **Removed** |
| `geofenceIds` | The only accepted geography input |
| `dataVintage` (body; vintage of geography to append) | `{vintage}` path segment; different meaning (dataset vintage), required |
| `useWeights` | **Removed** — data is always modelled/calibrated |
| `aggregatePolygons`, `aggregatePolygonName` | **Removed** |
| `appendSegmentation` (`prizm`, `prizmda`, `prizmqc`, `prizmqcda`) | **Removed** from extract; `targetSet` on the origins/destinations reports instead |
| `dailyTimeFilter` (dayPart array: `daysOfWeek` + `startTime`/`endTime` in `hh:mm`) | `daysOfWeek` + `timeOfDay`. **Arbitrary hour windows are no longer possible** |
| `pingFilter` (`first` / `firstlast` / `none`) | **Removed.** `dwell` is a new, semantically different filter (visit duration) |
| `reportType` (`celcdl`, `geofencepings`) | **Removed.** Extract is Origins-only; `geofencepings` has no equivalent |
| — | `geoLevelCode` *(new — origins report only; see §5)* |

### Filter value enums

v5 documents **no allowed values** for `timeOfDay` or `dwell`. Both are .NET
enums server-side and invalid values return a type-conversion error that does
not enumerate the valid set, so these were established by probing **[verified]**:

| Field | Accepted values |
|---|---|
| `timeOfDay` | `AllDay`, `Morning`, `Afternoon`, `Evening` |
| `dwell` | `Any`, `Short`, `Medium`, `Long` |
| `daysOfWeek` | `Mon`…`Sun` **[docs]** |

Rejected: `EarlyMorning`, `MorningCommute`, `LateMorning`, `Midday`,
`EveningCommute`, `LateEvening`, `Night`, `Overnight`, `Daytime` — so v4's
seven day-parts do not carry over.

**`timeOfDay` is a filter, not a breakdown.** No report returns a time-of-day
split; each bucket costs its own request. The three named buckets are also not
exhaustive — measured over one geofence for January 2026, `Morning + Afternoon
+ Evening` = 98.9% of `AllDay`, leaving a 1.1% overnight residual with no
selectable bucket. Derive it as `AllDay - (Morning + Afternoon + Evening)`.

The hour boundaries of each bucket, and the minute thresholds behind
`Short`/`Medium`/`Long`, remain undocumented. **[unverified]**

---

## 4. Geofence discovery

| | v4 | v5 |
|---|---|---|
| Filter syntax | OData `$filter` (`eq`, `ne`, `gt`, `ge`, `lt`, `le`, `and`, `or`, `not`) | `FilterDefinition`, SQL-like (`PRCDCSD_NAME IN ('Toronto, ON (C)') AND PR_NAME IN ('Ontario')`) |
| Pagination | Azure continuation tokens, 1,000-result limit | `Page` / `PageSize` |
| Sorting | — | `SortBy` / `SortDirection` (12 sortable fields) |
| Response | list of id/name | `{items: [{geofenceId, geofenceName}], page, pageSize}` |

**Saved v4 filter strings will not work in v5** — the syntax is different.

Two gaps:
- The response carries **no total-count field**, so a client cannot know how
  many pages exist without paging until a short page returns.
- **No maximum `PageSize` is documented.** **[unverified]**

Sortable fields: `GEOFENCE_ID`, `GEOFENCE_NAME`, `PRCDCSD_NAME`, `CMACA_NAME`,
`PR_NAME`, `BANNER`, `PARENT_COMPANY`, `CATEGORY`, `SUB_CATEGORY`,
`GEOFENCE_TYPE`, `IS_PRIMARY_POLYGON`, `GEOFENCE_SQUARE_FOOTAGE`.

### "Custom" geofences are not usable in MobileScapes v5 **[verified]**

The EA Geofence Library (as seen in the Envision UI) contains at least two
`GEOFENCE_TYPE` values: `"EA Standard"` (EA's own pre-existing library
entries, ID prefix `E`, e.g. `E2182542`) and `"Custom"` (geofences
registered for a specific client/project, ID prefix `C`, e.g. `C12720`).
Both types are shown side by side in the Envision UI and both have real,
resolvable geofence IDs.

**Only `E`-prefixed ("EA Standard") IDs work against MobileScapes v5.**
Submitting a `C`-prefixed ("Custom") ID to any report endpoint or the
extract returns an explicit `400`:

```json
{"problemDetails":"Invalid geofence ID(s): C12720"}
```

This is not a batching artifact - it reproduces identically for a single
`C`-prefixed ID submitted alone, and does not affect `E`-prefixed IDs
submitted in the same request. Practical effect on a project with a mix of
both (Main Street Index: 60 `E`-prefixed, 37 `C`-prefixed out of 97 resolved
areas): **the `C`-prefixed third of the areas cannot currently be pulled
from MobileScapes v5 at all**, regardless of how the request is built. The
cause is unconfirmed - most likely "Custom" geofences belong to a different
EA product/API within Envision that happens to share the same UI picker and
ID-lookup surface as MobileScapes, rather than being MobileScapes-specific.
This needs confirmation from EA, not further client-side troubleshooting.

### A short date range can silently return zero rows for a valid geofence **[verified]**

`get_mobilescapes_destinations()` (and by extension anything built on the
same report pattern) returns an **empty result with no error** for a valid,
correctly-typed `E`-prefixed geofence ID if that geofence had no recorded
visits in the requested window - it does not return a `visits = 0` row.

Confirmed directly: the same real geofence ID returned zero rows for a
single month, then returned a substantial, clearly real visit count for a
12-month window ending on the same date:

| Window | Result |
|---|---|
| `2026-06-01` to `2026-06-30` (1 month) | 0 rows |
| `2025-07-01` to `2026-06-30` (12 months, the max allowed) | 1 row, `visits = 2,175,612` |

**A missing row from a narrow-window test is not evidence the ID is
invalid** - it can just mean "no traffic in that specific window." Test
wider windows before concluding a geofence ID doesn't work, and don't
silently treat request-ID-count vs. response-row-count mismatches as
errors when building automated pipelines - they can be legitimate.

---

## 5. Origin geography — depends on the path

**The two paths differ enormously.** Choose deliberately.

**v4** returned one row per origin location carrying actual coordinates plus
the full geography hierarchy in the same row:

```
COMMON_EVENING_LAT, COMMON_EVENING_LON,
CEL_PRCDDA, CEL_PRCDADA, CEL_PRCDCSD, CEL_PRCD,
CEL_PR, CEL_CMACT, CEL_CMACA, CEL_REG, CEL_PRFED
```

**v5 synchronous origins report** returns `{geoCode, visits}` aggregated to
**one** requested level, with no coordinates.

**v5 async CSV extract** returns essentially the v4 shape — one row per origin
postal code with `LATITUDE`, `LONGITUDE`, `PostalCode`, `FSALDU`, and every
hierarchy level from `CAN` down to `PRCDDA`, plus `GeofenceName` so multiple
geofences can share one extract. Verified by downloading a real extract:
791 rows x 55 columns for one geofence over one month. See
[msmdata-v5-impact.md §2.1](msmdata-v5-impact.md) for the full header.

**Multi-geofence extracts are confirmed to split correctly by `GeofenceName`
- not just inferred from the column existing.** Submitted one extract with
two real geofence IDs (Downtown Barrie BIA, Downtown Halifax BIA) over the
same 12-month window: `GeofenceName` cleanly separated 170,897 rows into
the two areas, and each area's summed `Visits` matched its
`get_mobilescapes_destinations()` total exactly (Barrie: 2,175,612 both
ways; Halifax: 12,800,443 both ways). One extract call, submitted at or
under `maxGeofenceIds` (500), is a genuine substitute for N per-area calls.
**[verified]**

### Allowed `geoLevelCode` values **[verified]**

Finest → coarsest:

| Code | Geography | Approx. size |
|---|---|---|
| **`PRCDDA`** | **Dissemination Area** | **400–700 people — finest available** |
| `PRCDADA` | Aggregate Dissemination Area | 5,000–15,000 people |
| `FSA` | Forward Sortation Area | first 3 postal characters |
| `PRCDCSD` | Census Subdivision | municipality |
| `PRCD` | Census Division | county/region |
| `CMACA` | Census Metropolitan Area / Agglomeration | metro |
| `PR` | Province | — |

Confirmed by the API's own validation message:

> `'geoLevelCode' must be one of: PRCDDA, PRCDADA, FSA, PRCDCSD, PRCD, PR, CMACA.`

### Verified response shape at DA level **[verified]**

`get_mobilescapes_origins(geo_level_code = "PRCDDA")` for one geofence over
January 2026 returned 412 rows:

| geoCode | visits |
|---|---|
| 13080075 | 0.90 |
| 24250238 | 1.57 |
| 35020091 | 2.72 |

- `geoCode` is the 8-digit StatCan DAUID.
- **`visits` is fractional**, not an integer count — these are modelled,
  calibrated estimates. Downstream code that assumes integer visit counts, or
  that sums and compares against v4 integers, needs adjusting.

### What was lost

- **Nothing on the extract path**, geography-wise — coordinates, census tract
  (`CMACT`), `REG` and `PRFED` are all present. The genuine losses are the seven
  day-part columns and the PRIZM `SEGMENT` column.
- **On the synchronous report path**: coordinates, census tract, `REG` and
  `PRFED` are all unavailable — that path offers only the seven
  `geoLevelCode` values above.

### What was gained

- `FSA` is a directly selectable level on the report path.
- Full 6-character postal code (`PostalCode` / `FSALDU`) on the extract path —
  finer than anything v4 exposed as a selectable geography.

### Working guidance

- **If you need coordinates, postal code, census tract, or per-geofence
  breakout, use the extract.** The synchronous report cannot provide any of them.
- **Pull at `PRCDDA`** when using the report path. Every coarser standard level
  except `FSA` can be rolled up from DA locally with a correspondence file.
- **`FSA` needs its own request** — FSAs do not nest cleanly within DAs.
- **One level per request** — v4's single extract carried the whole hierarchy.
- **The origins report pools across all `geofenceIds` in the request.** The
  response has no per-geofence column, so a per-area profile requires **one
  request per area**. **[docs]**
- For heatmaps previously built on origin points, DA centroids or DA polygons
  are the closest substitute. This is a real loss of resolution.

---

## 6. Status and results

| | v4 | v5 |
|---|---|---|
| Status values | `SUBMITTED`, `ENQUEUED`, `PROCESSING`, `COMPLETE`, `FAILED`, `EXPIRED` | `Submitted`, `Processing`, `Complete`, `Failed`, `Expired` |
| Casing | UPPERCASE | TitleCase |
| Queued state | `ENQUEUED` when throttled | **Removed** — throttling surfaces as `429` |
| Partial success | `geofencesSucceeded` / `geofencesFailed` | Not documented **[unverified]** |
| Result payload | `blobList`, `sasToken`, `storageUrl`, `containerName` | Same four fields |
| File format | several files zipped together | CSV files |
| Link expiry | SAS expires within an hour | "time-limited", unspecified |
| Polling guidance | — | every 10–30s; most complete within 5 min |

**Output is served from a custom CDN host, not an Azure blob domain.**
`storageUrl` is `https://cdn.environicsanalytics.com` and the container sits
under a path prefix that neither `storageUrl` nor `containerName` includes — it
appears only in `blobList`. `AzureStor::storage_endpoint()` infers the service
from the hostname and fails with "Unknown endpoint service". Derive the
container base by stripping the query string from `blobList`. **[verified]**

**Submission is not validated synchronously.** `POST /extracts/csv` returns
`202` with a `requestId` even for a geofence ID that does not exist, and
silently ignores unknown body fields such as `geoLevelCode`. Bad input surfaces
only later as a failed request. **[verified]**

---

## 7. Limits, quotas, errors

### Config endpoint (new, authoritative)

`GET /{country}/{vintage}/config` — observed for `ca`/`2026` **[verified]**:

```json
{"maxGeofenceIds":500,"h3endDate":"2026-08-09","startDate":"2023-01-01",
 "endDate":"2026-08-09","maxDateRangeMonths":12}
```

| Key | Meaning |
|---|---|
| `startDate` / `endDate` | Allowed request date bounds |
| `h3endDate` | Latest `endDate` when using an H3 geofence layer |
| `maxGeofenceIds` | Max geofence IDs per request (500) |
| `maxDateRangeMonths` | Max span between start and end (12) |

### Quotas

v4 documented: 1,000 requests/user/day, 20,000 geofences/user/day, **300
geofences per request**, reset 05:00 UTC.

**v5 documents no quotas at all.** The per-request cap moved into `config`
(now 500). The daily figures are unconfirmed for v5. **[unverified]**

### Rate limiting

v5 returns a bare `{"status":429}` with no `Retry-After` header. The geofence
discovery endpoint is limited noticeably harder than the others — it returned
`429` on a first call in a fresh session and stayed limited across several
minutes of backoff, while `config` succeeded throughout. **[verified]**

### Error codes

v4 published a 15-entry error-code table (`01` ServiceUnavailable … `15`
InvalidCombination). **v5 publishes no such table** — only per-endpoint `400`,
`403`, `404`. Validation errors return RFC 9110 problem-details JSON with a
per-field `errors` object and a `traceId`. **[verified]**

Polygon-specific v4 codes (`InvalidPolygon`, `GeofenceTooBig`) are gone along
with polygon support.

---

## 8. Data and methodology

| | v4 | v5 |
|---|---|---|
| Source | Consent-based SDK data enhanced with cellular geo-location | Cellular network signals |
| Calibration | Weighted to general population | Calibrated to Canadian household population aged 15+ |
| Update frequency | — | Weekly |
| Geofence library | — | 130,000+ |

v4 itself removed the Worker, Worker Visit, Resident, Resident Visit, Total and
Total Visit fields relative to v2.

**v4 and v5 figures are not a continuous time series.** Any trend analysis
spanning the cutover needs an explicit break.

`config` exposing `h3endDate` implies an H3 hexagon geofence layer with an
earlier data cutoff than the standard layer. Not otherwise documented, and not
available as an origin `geoLevelCode`. **[unverified]**

---

## 9. Effect on this package

| Area | Change |
|---|---|
| Auth defaults | Now Entra; overridable via `SCOPE` / `TOKEN_URL` env vars |
| `discover_mobilescapes_geofences()` | Paginates automatically (`all_pages = TRUE`, `max_pages = 100`); default `page_size` 25 → 100 |
| All requests | Retry `429`/`503` five times with exponential backoff |
| Request bodies | `geofenceIds` / `daysOfWeek` now always serialise as JSON arrays — a length-1 vector previously became a bare string and the API rejected it |
| `pull_mobilescapes()` | Geofence IDs only; date-only range; `vintage` required; single consolidated CSV out |
| Removed | `process_geojson_file()` and all oversize-polygon splitting |
| Extract download | Rewritten to plain HTTP against the pre-signed `blobList` URL; handles `NextMarker` paging |
| Dependencies | Dropped `sf`, `geojsonio`, `jsonlite`, and now `AzureStor` |

### Still open

- **Why "Custom" geofences are rejected by MobileScapes v5 is unconfirmed**
  (see §4). Needs an answer from EA - whether Custom geofences require a
  different vintage/country, a different endpoint, or simply aren't a
  MobileScapes-compatible geofence type regardless of how they're
  registered.
- The extract contained 377 distinct DAs where the synchronous report returned
  412, for the same geofence and period, despite identical visit totals. Some
  rows carry placeholder geography codes (`0000000.00`, "Rest of Canada").
  Understand this before using either as a denominator.
- `timeOfDay` and `dwell` accepted values are now known (see §3), but their
  hour/minute boundaries are not.
- Maximum `PageSize` for geofence discovery is unknown.
- v5 daily quota figures are unconfirmed.
- Token caching: `.quietly_get_bearer_token()` fetches a fresh token on every
  request rather than reusing one until expiry. Harmless at low volume, but it
  doubles request count on paginated sweeps.
