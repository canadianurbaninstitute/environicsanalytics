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
**time-of-day detail**, which drops from seven slots to three overlapping ones.
Everything else we publish either survives intact or gets better.

### The short version

| | |
|---|---|
| ✅ **Still works** | Visitor heat map, visits by postal code / neighbourhood, distance travelled, visit counts, busiest day of the week, comparisons over time, visitor lifestyle (PRIZM) segments |
| ⚠️ **Works, but coarser** | Time of day — three bars instead of four, and they overlap |
| ❌ **Gone** | The Early Morning (12am–6am) time band |
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

**3. The time-of-day chart loses a bar.** We currently show four time bands
built from seven underlying slots. v5 offers three, they are the vendor's own
definitions, we don't know which hours they cover, and they overlap — so the
overnight band is gone and the percentages are "share across the three bands"
rather than "share of all visits".

**4. The visitor lifestyle (PRIZM) chart is fine.** Environics added segment
codes back to the bulk download, so this works as it did before — no caveat and
no rethink needed.

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
| `process_time_of_day()` | Visits by time of day | ⚠️ **Adapt** | 3 overlapping buckets, filter-only — see §2.5 |
| `process_top_prizm_segments()` | Top PRIZM segments by distance | ✅ **Keep** | `PZMLLIC` on the extract — see §2.6 |

### 2.1 The extract schema (verified)

A real extract for one geofence over January 2026 returned **791 rows × 55
columns**, one row per origin postal code. EA has since added PRIZM
segmentation, so the current schema is **57 columns** — re-verified 2026-09-15
against `C12401` over 2026-04-01→2026-06-30 (8,663 rows × 57 columns):

```
GeofenceName, Visits, PostalCode, LATITUDE, LONGITUDE,
Sunday…Saturday, January…December,
GEOCODETYPE, ISGEOCODED, ISBUSINESS, ISRETIRED, ISAPARTMENT, INDEMOGRAPHIC, ISLICENSED,
PZMLLIC, INSEGMENTATION_PZMLLIC,
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
- **PRIZM segmentation is back** as `PZMLLIC` (plus an `INSEGMENTATION_PZMLLIC`
  flag) — see §2.6.

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
| `EARLYMORNING`…`LATEEVENING` (7 day-parts) | **No equivalent** — `timeOfDay` is a 3-bucket filter, and the buckets overlap; see §2.5 |
| `SEGMENT` (PRIZM) | `PZMLLIC` — restored, see §2.6 |

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

The filter **is** honoured on the extract path **[verified 2026-09-15]** — the
same geofence and window returned 8,663 rows / 330,489 visits at `AllDay` and
4,009 rows / 117,253 visits at `Morning` — so per-bucket extracts work and keep
full origin detail.

> **Correction (2026-09-15):** an earlier revision of this section said the
> fourth (overnight) bar was derivable as
> `AllDay − (Morning + Afternoon + Evening)`, based on a 98.9% / 1.1% split
> measured on one geofence for January 2026. **That is wrong.** The three
> buckets *overlap* and sum to *more* than `AllDay`.

Re-measured over 2025-07-01→2026-06-30 via `get_mobilescapes_destinations()`:

| Geofence | `AllDay` | `M+A+E` | ratio |
|---|---|---|---|
| `E2182542` (Barrie) | 2,175,612 | 2,349,746 | **1.080** |
| `E2182592` (Halifax) | 12,800,443 | 15,528,720 | **1.213** |
| `C12401` (Baby Point Gates) | 1,492,628 | 1,693,266 | **1.134** |

Stable per geofence (Barrie held at 1.079 over two other 12-month windows) but
varying 8%–21% between them, so it is not a fixed correction factor either.

Consequences for `process_time_of_day()`:

1. **The chart drops from four bars to three.** Early Morning (12am–6am) is
   gone and cannot be reconstructed — the subtraction returns a negative number.
2. **The existing percentage calculation still works unchanged.** The function
   already computes `Percentage = AvgVisits / sum(AvgVisits) * 100`, i.e. share
   within the bucket set. That operation is identical with three categories.
   But because the buckets overlap, `sum(AvgVisits)` is no longer a true total —
   it is a share-of-buckets figure, not share-of-visits, and should be labelled
   that way.
3. **Drop the hour labels.** The current labels ("Morning: 6am - 12pm") are v4
   definitions. v5 does not document its cut points, and the overlap proves they
   are not contiguous — so asserting explicit hours is unsupported.
4. **Cost: one request per bucket.** On the extract path that is 3 extracts per
   period, but each carries `GeofenceName` and up to 500 geofence IDs, so 3
   extracts cover *all* areas for a period — not 3 per area.

`get_mobilescapes_destinations()` also accepts `timeOfDay` and returns one row
per geofence, which would be 3 cheap synchronous calls for all areas. **But it
returns zero rows on narrow windows** — verified: Barrie returned nothing for
both a 1-month and a 3-month window while returning 2.17M visits over 12 months.
For a monthly chart, use the extract.

### 2.6 PRIZM segments — restored as `PZMLLIC`

> **Correction (2026-09-15):** this section previously said PRIZM had no v5
> equivalent and `process_top_prizm_segments()` needed rebuilding as an
> ecological approximation. EA has since added segmentation to the extract.

The extract now carries **`PZMLLIC`** — the PRIZM segment code on each origin
record, the direct equivalent of v4's `SEGMENT` — plus **`INSEGMENTATION_PZMLLIC`**,
a boolean flag for whether the record is in the segmentation base.

Verified 2026-09-15 on `C12401` over 2026-04-01→2026-06-30: `PZMLLIC` has **67
distinct values** (the full Canadian PRIZM segment set), **zero NAs** across
8,663 rows, most common codes `31`, `18`, `22`, `06`, `61`. `INSEGMENTATION_PZMLLIC`
was `TRUE` for every row in that sample.

This makes `process_top_prizm_segments()` a **direct port**, not a rebuild:
`SEGMENT` → `PZMLLIC`, and the distance bands still come from each row's own
`LATITUDE`/`LONGITUDE`. No ecological approximation and no caveat about
inferring segment from neighbourhood is needed.

Two things to confirm before relying on it:

- **Code format.** Values are zero-padded two-character strings (`"06"`, not
  `6`). Read as character, or leading zeros will be lost and joins to a PRIZM
  lookup will silently miss.
- **Whether `INSEGMENTATION_PZMLLIC` is ever `FALSE`.** It was uniformly `TRUE`
  in the one sample checked. If it can be `FALSE`, decide whether to filter on
  it before aggregating.


## Part 3 — What we gain

| New capability | Endpoint |
|---|---|
| PRIZM segment per origin record, restored | Extract (`PZMLLIC`) |
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
| Time of day, if needed (`Morning`, `Afternoon`, `Evening`) | 3 per **chunk**, not per area — the extract honours `timeOfDay` and carries `GeofenceName` |
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
  they represent are not. They are known *not* to be a clean partition: the
  three named buckets overlap and sum to 8%–21% more than `AllDay` (§2.5).
- **Whether `INSEGMENTATION_PZMLLIC` is ever `FALSE`**, and the authoritative
  `PZMLLIC` code → segment-name lookup for v5 (§2.6).
- **Why an extract can complete with no files.** A Barrie extract over
  2026-04-01→2026-06-30 reported success but produced no output, matching the
  same geofence returning zero rows from `destinations()` on 1- and 3-month
  windows while returning 2.17M visits over 12 months. Consistent with the
  narrow-window behaviour in Part 5 §2, but the threshold is unknown.
- **`dwell` thresholds** — `Short`/`Medium`/`Long` accepted; minute ranges
  undocumented.
- **The 377 vs 412 DA discrepancy** between extract and synchronous report
  (§2.3).
- **v5 daily quota figures** — v5 documents none; the v4 numbers are unconfirmed.
- **Whether the extract respects a `targetSet` filter.** Less pressing now that
  `PZMLLIC` restores segment-based analysis directly.
