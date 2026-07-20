# Plan: Remove global filters, add heatmap-specific filters

## Goal
Remove the demographic (`sex == "_T"`, `age == "_T"`, `education_lev == "_T"`) and measure (`_DEP`, `_VER`) filters from `global.R` so that `dat` contains all breakdowns. Add equivalent filters only where the heatmap code needs them, so heatmaps still show only `_T` aggregates and no `_DEP`/`_VER` measures.

## Changes

### 1. global.R — `measure_list` (lines 45-48)
Remove `!grepl("_DEP", measure), !grepl("_VER", measure)` from the filter. Keep the `!grepl("11_3_", measure)` filter.

### 2. global.R — `dat` (lines 51-56)
Remove `filter(sex == "_T", age == "_T", education_lev == "_T")`. Keep `measure %in% unique(measure_list$measure)` so we still exclude `11_3_` measures.

### 3. app.R — Heatmap `observe` block (line 1553+)
At the top of this block, after `d <- dat_rv()`:
- Filter `d` to `sex == "_T", age == "_T", education_lev == "_T"` for the heatmap only.
- Create a local `heatmap_measures` vector that excludes `_DEP` and `_VER` from `measure_list$measure`.
- Replace the three references to `unique(measure_list$measure)` (lines ~1605, 1609, 1618) with `heatmap_measures`.

### 4. app.R — Excel template `val_lookup` (line 1222)
Filter `d` to `_T` aggregates before building `val_lookup`, since the template pre-fill for `country_avg` should still use the total row. (The template's `val_lookup` currently assumes one row per measure/year.)
