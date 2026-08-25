# What MobileScapes v5 Means for msmdata

An assessment of every visitor-data product msmdata currently builds, and
whether it survives the move from MobileScapes v4 to v5.

**Compiled:** 2026-08-18 · Verified against the live v5 API (`ca` / `2026`),
including a real Origins extract downloaded and inspected.
Companion to [v4-to-v5-changes.md](v4-to-v5-changes.md).

> **Correction (2026-08-18):** an earlier version of this assessment said
> point-level visitor origins were gone. That was wrong. It was based on the
> synchronous origins report and the published documentation. The **async CSV
> extract** — the same path `pull_mobilescapes()` already uses — still returns
> coordinates, postal codes, and the full geography hierarchy. The heat map
> survives. Details in §2.4.

---

## Part 1 — In plain language

### What changed, in one paragraph

Much less than first feared. The bulk CSV download we already use still tells
us **where visitors came from, down to the postal code, with map coordinates** —
the same detail we have today. What changed is the plumbing: how we ask for the
data, how we log in, and how areas are identified. The one real content loss is
**time-of-day detail**, which drops from seven slots to four. Everything else we
publish either survives intact or gets better.

### The short version

| | |
|---|---|
| ✅ **Still works** | Visitor heat map, visits by postal code / neighbourhood, distance travelled, visit counts, busiest day of the week, comparisons over time |
| ⚠️ **Works, but coarser** | Time of day — four slots instead of seven |
| ❌ **Gone** | Visitor lifestyle (PRIZM) segments attached to individual visitors |
| ✨ **New, didn't have before** | Year-over-year comparisons, weekday vs weekend splits, built-in demographics, and "where else did these visitors go" |

### What to flag to stakeholders

**1. Numbers before and after the switch are not comparable.** Environics
rebuilt how the data is produced — it now comes from cellular network signals
rather than phone apps. Visit counts will shift for reasons that have nothing
to do with real foot traffic. Any chart spanning the changeover needs a visible
break or a note, or it will read as a trend when it isn't.

**2. Visit numbers now have decimal places** (e.g. 6.14 visits from a given
postal code) because they are modelled estimates rather than raw counts. Totals
remain meaningful; small individual numbers should be treated as estimates and
rounded for display.

**3. The time-of-day chart loses detail.** We currently show four time bands
built from seven underlying slots. We can still show four bands, but they are
now the vendor's own definitions and we don't know exactly which hours they
cover.

**4. The visitor lifestyle (PRIZM) chart needs a caveat or a rethink.** The new
data doesn't tell us each visitor's segment. We can estimate it from the
neighbourhood they came from, but that's an inference, not a measurement.

---

## Part 2 — Capability-by-capability assessment

| msmdata function | Dashboard output | Verdict | v5 route |
|---|---|---|---|
| `process_overall_visit_levels()` | Total visits over time | ✅ **Keep** | Extract `Visits`, or destinations report |
| `process_day_of_week()` | Visits by day of week | ✅ **Keep** | Extract `Sunday`…`Saturday` columns |
| `process_visitor_fsa_summary()` | Visits by FSA | ✅ **Keep** | Extract `FSA` column |
| `process_visitor_heatmap()` | Visitor origin heat map | ✅ **Keep** | Extract `LATITUDE`/`LONGITUDE` — see §2.4 |
| `process_visitor_quartiles()` | Visitor distance bands | ✅ **Keep** | Same coordinates, same method |
| `create_summary_cards()` | Total / busiest day / busiest time | ⚠️ **Mostly** | Total and busiest day direct; busiest time needs extra calls |
| `process_time_of_day()` | Visits by time of day | ⚠️ **Rebuild** | 4 buckets, filter-only — see §2.5 |
| `process_top_prizm_segments()` | Top PRIZM segments by distance | ❌ **Rebuild** | No per-visitor segment — see §2.6 |

### 2.1 The extract schema (verified)

A real extract for one geofence over January 2026 returned **791 rows × 55
columns**, one row per origin postal code:

```
GeofenceName, Visits, PostalCode, LATITUDE, LONGITUDE,
Sunday…Saturday, January…December,
GEOCODETYPE, ISGEOCODED, ISBUSINESS, ISRETIRED, ISAPARTMENT, INDEMOGRAPHIC, ISLICENSED,
CAN, REG, PR, CMACA, PRCD, PRCDCSD, CMACT, PRCDADA, PRCDDA, PRFED, FSA, FSALDU
  (each with a matching _NAME column)
```

Example row:

```
"JYSK at Niagara Square - 7555 Montrose Rd, Niagara Falls, ON", 6.14,
"L0S1S1", 42.96341, -79.03565, …
```

Three things this settles:

- **Coordinates are still there.** `LATITUDE`/`LONGITUDE` per origin record.
- **`GeofenceName` is a column**, so one extract covers many areas and can be
  split downstream. There is no one-call-per-area penalty on the extract path.
  Confirmed with a real two-geofence extract: `GeofenceName` cleanly split
  the output and each area's total matched its own `destinations()` call
  exactly. **[verified]**
- **The full geography hierarchy survives**, including `CMACT` (census tract)
  and `PRFED` (federal riding), which the synchronous report does not offer.

Cross-check: extract visits totalled **6,290.08**, exactly matching the
synchronous origins report for the same geofence and period.

### 2.2 v4 → v5 column mapping

| v4 column | v5 extract |
|---|---|
| `COMMON_EVENING_LAT` / `LON` | `LATITUDE` / `LONGITUDE` |
| `CEL_PRCDDA` | `PRCDDA` |
| `CEL_PRCDADA` | `PRCDADA` |
| `CEL_PRCDCSD` | `PRCDCSD` |
| `CEL_PRCD` | `PRCD` |
| `CEL_PR` | `PR` |
| `CEL_CMACT` | `CMACT` |
| `CEL_CMACA` | `CMACA` |
| `CEL_PRFED` | `PRFED` |
| `CEL_REG` | `REG` |
| `CEL_FSA` | `FSA` |
| — | `FSALDU`, `PostalCode` (full 6-character postal code) |
| `VISIT` | `Visits` |
| `SUNDAY`…`SATURDAY` | `Sunday`…`Saturday` |
| `JANUARY`…`DECEMBER` | `January`…`December` |
| `VISITOR` | **No equivalent** |
| `WEEKDAY`, `WEEKEND` | **No equivalent** (derive from day columns) |
| `EARLYMORNING`…`LATEEVENING` (7 day-parts) | **No equivalent** — see §2.5 |
| `SEGMENT` (PRIZM) | **No equivalent** — see §2.6 |

The rename is mostly mechanical: drop the `CEL_` prefix, change case. Note the
column names are now **mixed case** (`Visits`, `Sunday`) where v4 was uppercase.

### 2.3 A caveat on granularity

Rows are keyed to postal code, and `GEOCODETYPE` reads `Unique ePCCF` with
`ISGEOCODED = true`. The coordinates are therefore **geocoded postal-code
locations, not raw device positions** — which is also what v4's "common evening
location" was. Functionally equivalent for our purposes, since
`process_visitor_heatmap()` rounds to 4 decimals and aggregates anyway.

One discrepancy worth checking before relying on either source: the extract
contained **377 distinct DAs** while the synchronous origins report returned
**412** for the same geofence and period, even though total visits matched
exactly. Some rows also carry placeholder geography codes (e.g. `0000000.00`,
"Rest of Canada"). Worth understanding before using DA counts as a denominator.

### 2.4 Visitor heat map — survives

`process_visitor_heatmap()` rounds `COMMON_EVENING_LAT`/`LON` to 4 decimals and
sums visits per coordinate. The equivalent v5 columns are `LATITUDE`/`LONGITUDE`.
**The function needs a column rename and nothing else.** Same for
`process_visitor_quartiles()` and `calculate_visitor_distance()`.

### 2.5 Time of day — the real loss

v4 gave seven day-parts as **columns**. The v5 extract has **no time columns at
all**, and no report returns a time-of-day breakdown. `timeOfDay` exists only as
a **filter**, with this complete set of accepted values **[verified]**:

`AllDay`, `Morning`, `Afternoon`, `Evening`

Consequences:

1. **One request per bucket** — 4 calls where v4 read 7 columns from one file.
2. **No early-morning bucket, but it is derivable.** Measured on a real geofence
   over January 2026:

   | Bucket | Visits |
   |---|---|
   | AllDay | 6,290.08 |
   | Morning | 1,453.87 |
   | Afternoon | 3,049.06 |
   | Evening | 1,718.89 |
   | **Morning + Afternoon + Evening** | **6,221.82 (98.9%)** |
   | **Residual (overnight)** | **68.26 (1.1%)** |

   The fourth bar = `AllDay − (Morning + Afternoon + Evening)`, so the existing
   four-category chart survives structurally.
3. **Bucket boundaries are undocumented.** Our current axis labels
   ("Morning: 6am – 12pm") are v4 definitions and may not match v5's cut points.
   Either confirm with EA or drop the explicit hours from the labels.

### 2.6 PRIZM segments — no direct replacement

v4 carried a `SEGMENT` code per origin record. The v5 extract has no segment
column, and `appendSegmentation` is gone.

Two partial routes:

- **Destination summary → `topSegments`** gives top segments by share of visits
  per geofence, but **not** split by distance band.
- **Join extract rows to our own DA-level PRIZM data.** msmdata already works at
  `PRCDDA` in `process_ea_data()`, and the extract carries `PRCDDA` on every
  row — so the join is direct, and distance bands still come from the row's own
  coordinates.

The second route reproduces the current chart's shape but is an **ecological
approximation**: it assumes visitors from a DA match that DA's overall profile.
Label it as an estimate.

---

## Part 3 — What we gain

| New capability | Endpoint |
|---|---|
| Year-over-year change, computed by EA | Destinations (`percentChange`), destination summary |
| Weekday vs weekend split | Destination summary |
| Origin share inside vs outside market area | Destination summary |
| Visitor demographic profile, indexed against a base | Destination summary |
| **Where else visitors went** — top related locations and banners | Related visits |
| Dwell-time filter — `Any`, `Short`, `Medium`, `Long` **[verified]** | All reports |
| Full 6-character postal code on every origin row | Extract (`PostalCode`, `FSALDU`) |
| Quick aggregate numbers with no extract lifecycle | Synchronous reports |

---

## Part 4 — Request budget for 100 areas

The extract carries `GeofenceName`, so **areas do not each need their own
extract**. With `maxGeofenceIds = 500` per request, 100 BIAs' worth of geofences
may fit in a single extract, or a handful of chunked ones.

| Purpose | Calls |
|---|---|
| Origins extract covering all areas (chunked by 500 geofences) | 1 per chunk |
| Time of day, if needed (`Morning`, `Afternoon`, `Evening`, `AllDay`) | 4 per area |
| Destination summary, if using EA's demographics / YoY | 1 per area |

The extract path is dramatically cheaper than the per-area synchronous route. If
time-of-day and EA demographics are dropped or reduced in frequency, a full
100-area refresh is a handful of calls rather than several hundred.

Still applies: `maxDateRangeMonths = 12`, and geofence discovery is heavily
rate-limited — resolve area → geofence IDs once and cache it.

---

## Part 5 — Structural gotchas

1. **The synchronous origins report pools across all geofence IDs** in a
   request — no per-geofence column. Per-area output needs one call per area.
   **The extract does not have this problem** (`GeofenceName` column).
   **[verified]**
2. **A narrow date window can silently drop an area from the extract, not
   just from synchronous reports.** Confirmed directly: a two-geofence
   extract over a one-month window where one area had zero visits that
   month produced output for only the other area - no error, no empty row
   for the missing one. Re-running the same extract over a 12-month window
   (both areas confirmed non-zero) produced both. Always sanity-check that
   the number of distinct `GeofenceName` values in extract output matches
   the number of `geofence_ids` submitted. **[verified]**
3. **The extract does not validate on submit.** `POST /extracts/csv` returns
   `202` with a request ID even for a nonexistent geofence, and silently ignores
   unknown fields such as `geoLevelCode`. Errors surface later as a failed
   request.
3. **Extract output is served from a custom CDN host**
   (`cdn.environicsanalytics.com`), with the container under a path prefix.
   AzureStor cannot parse it — the package now downloads over plain HTTP using
   the pre-signed URL, and no longer depends on AzureStor.

---

## Part 6 — Still unknown

- **`timeOfDay` bucket boundaries** — the four values are confirmed, the hours
  they represent are not.
- **`dwell` thresholds** — `Short`/`Medium`/`Long` accepted; minute ranges
  undocumented.
- **The 377 vs 412 DA discrepancy** between extract and synchronous report
  (§2.3).
- **v5 daily quota figures** — v5 documents none; the v4 numbers are unconfirmed.
- **Whether the extract respects a `targetSet` filter**, which might partially
  restore segment-based analysis.
