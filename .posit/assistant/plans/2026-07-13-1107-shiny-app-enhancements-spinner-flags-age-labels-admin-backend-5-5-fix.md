# Plan: Shiny App Enhancements

## 1. Loading spinner for heatmap_submissions

The heatmap already has `shinycssloaders::withSpinner()` wrapping the entire fluidRow (line 328). However, this wraps the whole row including the legend and the quick guide. The spinner should target just the `uiOutput("heatmap_submissions")` so the rest of the page is still visible.

**Change:** Move `withSpinner()` to wrap only `uiOutput("heatmap_submissions")` instead of the whole fluidRow.

## 2. Data flags (B, E, P, etc.) on the data submission page

Add a dropdown next to each year-input cell allowing countries to flag data points:
- B = Break in series
- E = Estimate
- P = Provisional
- (blank) = no flag

**Approach:**
- Add a small `<select>` dropdown (or a clickable flag icon that cycles through options) next to each year-input field in `make_year_inputs()`.
- Store flags in `session_data$flags` as `list(measure = list(row_type = list(year = flag_code)))`.
- Pass flags through `submitMeasure()` JS function alongside values.
- Save/restore flags from session RDS files.
- Display flags in the admin "Data entries" table as an additional column.
- Add a legend/key explaining the flag codes.

## 3. Dynamic age group labels based on country groupings

Currently `row_defs()` (lines 994-1031) uses static labels "Young", "Middle-aged", "Old" for all measures.

**Change:** Update `row_defs()` to look up the measure in `young_16_29`, `young_15_24`, `young_16_24` and set labels accordingly:

| Group | young_16_29 | young_15_24 | young_16_24 |
|-------|-------------|-------------|-------------|
| Young | Young (16-29 years) | Young (15-24 years) | Young (16-24 years) |
| Middle-aged | Middle-aged (30-49 years) | Middle-aged (25-64 years) | Middle-aged (25-54 years) |
| Old | Old (50+ years) | Old (65+ years) | Old (55+ years) |

Also add a way for countries to flag when their age groups differ from the standard. **Approach:** Add a small text input or note field in the panel where countries can indicate their actual age group boundaries if they differ.

## 4. "No data update" visible in admin backend

Currently `no_updates` are saved in session RDS but NOT shown in the admin table. The admin `admin_all_data()` reactive does not parse `s$no_updates`.

**Change:** 
- Add a "No updates" table option to the admin table selector.
- Parse `s$no_updates` in the admin data aggregation loop.
- Display as a table with columns: country, iso, measure, declared (TRUE/FALSE).

## 5. Fix 5_5 visibility

Measure `5_5` is listed in `xlsx_measures`, `pct_indics`, `all_rows`, and `young_15_24`. However, it may be missing from `dict` (dictionary.xlsx), which feeds `measure_list`, which controls which measures appear in the heatmap.

**Investigation needed:** Check whether `5_5` is in the dictionary file. If not, it needs to be added. If it is, the filtering at lines 37-40 (`!grepl("_DEP|_VER|11_3_")`) should not exclude it, so we need to trace further.

**Likely fix:** Add `5_5` to dictionary.xlsx, or if it's present, check if it's being filtered out of `dat` (line 42-45) because it has no matching rows in the final dataset. The `complete()` at line 939 should still create rows for it, but the `merge()` at line 1171 with `dict` would fail if 5_5 isn't in dict.

## Implementation Order

1. Investigate & fix 5_5 visibility (may be a data issue)
2. Spinner fix (quick)
3. Age group labels (moderate)
4. No-update in admin (moderate)
5. Data flags feature (largest change — JS + server + admin)
