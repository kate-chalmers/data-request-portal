library(tidyverse)
library(shiny)
library(shinyjs)
library(echarts4r)
library(DT)
library(writexl)
library(pins)

# ── Persistent session storage (pins) ─────────────────────────────────────────
# On hosted platforms (Posit Connect Cloud, shinyapps.io) the local file
# system is ephemeral: anything written at runtime is lost when the worker
# restarts. Saved sessions therefore live on a pins board backed by a Google
# Drive folder.
#
# Authentication uses a cached OAuth token for a *real* Google account.
# (A service account cannot be used here: Google gives service accounts no
# Drive storage quota, so their writes to a My Drive folder fail with 403.)
#
# One-time setup — run this interactively in the console:
#   googledrive::drive_auth(cache = "drive-token")
# and log in as the Google account that owns the folder below, granting Drive
# access. The token lands in "drive-token/". The folder name must NOT start
# with a dot: rsconnect excludes hidden files from deployment bundles, which
# silently breaks persistence on the server. Keep it out of git but IN the
# deployment bundle (deploy with rsconnect::deployApp(), not via GitHub).
#
# If no cached token is found, a local folder board under "sessions/" is used,
# so development needs no credentials (but nothing persists on a server!).

gdrive_sessions_folder <-
  "https://drive.google.com/drive/folders/1eKx4D63BnWe-npz2XrXgJaXwpuTbGCHe"

using_gdrive_board <- dir.exists("drive-token") && length(list.files("drive-token")) > 0

session_board <- if (using_gdrive_board) {
  options(gargle_oauth_cache = "drive-token", gargle_oauth_email = TRUE)
  googledrive::drive_auth()
  board_gdrive(gdrive_sessions_folder)
} else {
  message("Session storage: using LOCAL folder board (no cached Drive token found). ",
          "Fine for development; sessions will NOT persist on a hosted server.")
  board_folder("sessions")
}

# ── Async Drive access (mirai) ────────────────────────────────────────────────
# A Drive round-trip takes several seconds, and shinyapps.io serves multiple
# users from one R process, so a synchronous pin_write() inside an observer
# freezes the UI for *everyone* on that worker. When the Drive board is in
# use, reads/writes are instead shipped to a small pool of mirai daemons that
# hold their own authenticated copy of the board; results come back as
# promises. If mirai is unavailable or daemon setup fails, the app falls back
# to the synchronous helpers above (slow but safe). The local folder board is
# effectively instant, so it never needs daemons.
async_board_enabled <- using_gdrive_board &&
  requireNamespace("mirai", quietly = TRUE) &&
  tryCatch({
    mirai::daemons(2)
    mirai::everywhere({
      library(pins)
      options(gargle_oauth_cache = "drive-token", gargle_oauth_email = TRUE)
      googledrive::drive_auth()
      # mirai evaluates expressions in a throwaway environment, so the board
      # must be placed in the daemon's global env explicitly to persist.
      assign(".session_board", board_gdrive(.gdrive_folder), envir = .GlobalEnv)
    }, .gdrive_folder = gdrive_sessions_folder)
    TRUE
  }, error = function(e) {
    message("Async Drive access disabled (falling back to synchronous writes): ",
            conditionMessage(e))
    FALSE
  })

# Async counterparts of session_write()/session_read(). Each returns a mirai
# (promise-compatible): resolve with promises::then(). The write resolves to
# TRUE/FALSE like session_write(); the read resolves to the object or NULL.
# Only call these when async_board_enabled is TRUE.
session_write_async <- function(x, name) {
  mirai::mirai({
    tryCatch({
      suppressMessages(
        pins::pin_write(.session_board, x, name = name, type = "rds",
                        versioned = FALSE)
      )
      TRUE
    }, error = function(e) FALSE)
  }, x = x, name = name)
}

session_read_async <- function(name) {
  mirai::mirai({
    tryCatch(pins::pin_read(.session_board, name), error = function(e) NULL)
  }, name = name)
}

# Read a stored object by pin name; NULL if absent or unreadable. (No upfront
# pin_exists() check: that would double the API round-trips per read, and
# pin_read() errors on missing pins anyway.)
session_read <- function(name) {
  tryCatch(pin_read(session_board, name), error = function(e) NULL)
}

# Write an object; returns TRUE on success, FALSE (with a warning) on failure.
session_write <- function(x, name) {
  tryCatch({
    suppressMessages(
      pin_write(session_board, x, name = name, type = "rds", versioned = FALSE)
    )
    TRUE
  }, error = function(e) {
    warning("Failed to write pin '", name, "': ", conditionMessage(e))
    FALSE
  })
}

session_delete <- function(name) {
  tryCatch({ pin_delete(session_board, name); TRUE }, error = function(e) FALSE)
}

# Saved country sessions are pins named by three-letter ISO code.
session_list_countries <- function() {
  tryCatch(grep("^[A-Z]{3}$", pin_list(session_board), value = TRUE),
           error = function(e) character(0))
}

# Everything the app stores: country sessions plus the utility pins.
session_list_all <- function() {
  tryCatch(grep("^([A-Z]{3}|passwords|feedback|frozen)$", pin_list(session_board),
                value = TRUE),
           error = function(e) character(0))
}

# ── Admin freeze flags ────────────────────────────────────────────────────────
# Freeze/unfreeze state lives in one tiny "frozen" pin: a named list of
# iso → POSIXct frozen-at timestamp (absence = not frozen). Keeping it out of
# the large per-country session pins means (a) the in-app freeze poll reads a
# few hundred bytes instead of re-downloading the whole session, and (b) an
# admin freeze can never clobber a country's concurrent edits.
#
# Migration: sessions saved before this change carry a legacy `frozen` field.
# That field is honoured only while the "frozen" pin does not exist yet; the
# first admin freeze/unfreeze creates the pin, which is authoritative from
# then on.
frozen_map_read <- function() {
  session_read("frozen")  # NULL = pin absent → legacy fallback applies
}

frozen_map_set <- function(iso, frozen) {
  m <- frozen_map_read()
  if (is.null(m)) m <- list()
  m[[iso]] <- if (frozen) Sys.time() else NULL
  session_write(m, "frozen")
}

oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST", "FIN",
                    "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR",
                    "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK",
                    "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "CRI")

eu_silc_countries <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
                      "DEU", "GRC", "HUN", "ISL", "IRL", "ITA", "LVA", "LTU", "LUX",
                      "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "SRB", "SVK", "SVN",
                      "ESP", "SWE", "CHE", "TUR", "HRV", "MKD", "MNE", "ALB", "BIH",
                      "XKX")

oecd_names <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Czechia", "Denmark", "Estonia",
                "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan",
                "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Türkiye", "United Kingdom",
                "United States", "Costa Rica")

partner_countries <- c("BRA", "ARG", "BGR", "HRV", "PER", "ROU", "IDN", "THA", "ZAF", "UKR")

partner_names <- c("Brazil", "Argentina", "Bulgaria", "Croatia", "Peru", "Romania", "Indonesia", "Thailand", "South Africa", "Ukraine")

country_name_vector <- c(
  setNames(oecd_countries, oecd_names),
  setNames(partner_countries, partner_names)
)

dict <- readxl::read_excel("data/dictionary.xlsx") %>%
  mutate(question = NA)

defs_lookup <- readxl::read_excel("data/definitions.xlsx") %>%
  filter(!grepl("_DEP$|_VER$", measure)) %>%
  select(measure, definition, indicator, unit)

xlsx_response_format <- readRDS("./data/response_input.RDS")

measure_list <- dict %>% 
  distinct(measure) %>% 
  filter(!grepl("11_3_", measure)) %>%
  # Remove duplicates with no data
  filter(!measure %in% c("6_6", "10_2_GAP"))

# Measures to hide from the coverage heatmap (not yet published)
coverage_hidden <- c("5_5")


dat <- readRDS("./data/final dataset.RDS") %>%
  select(-base_per) %>%
  rbind(readRDS("./data/5_5 Request data.RDS")) %>%
  filter(measure %in% unique(measure_list$measure)) %>%
  mutate(time_period = as.numeric(time_period),
         time_period = ifelse(ref_area == "POL" & measure == "4_3" & is.na(time_period), 2023, time_period)) %>%
  add_row(
    ref_area = "HUN",
    measure = "5_5",
    unit_measure = "PT_POP_Y_GE15",
    sex = c("_T", "F", "M", rep("_T", 6), "_T", "F", "M", rep("_T", 6)),
    age = c(rep("_T", 3), "YOUNG", "MID", "OLD", rep("_T", 3), rep("_T", 3), "YOUNG", "MID", "OLD", rep("_T", 3)),
    education_lev = c(rep("_T", 6), "ISCED11_1", "ISCED11_2_3", "ISCED11_5T8", 
    rep("_T", 6), "ISCED11_1", "ISCED11_2_3", "ISCED11_5T8"),
    time_period = c(rep(2014, 9), rep(2019, 9)),
    obs_value = c(44.0, 39.4, 48.0, 32.6, 42.2, 57.2, 57.9, 43.3, 30.3, 36, 30.7, 40.9, 30.2, 34.8, 42.6,44.2, 33.3, 35.6),
    obs_status = "A"
  ) 


# ── Non-used responses (submitted but not incorporated) ──────────────────────
nonused_dat <- if (file.exists("data/non-used responses.RDS")) {
  readRDS("data/non-used responses.RDS") 
} else {
  data.frame(measure = character(), ref_area = character(),
             sex = character(), age = character(),
             education_lev = character(), time_period = numeric(),
             obs_value = numeric())
}

current_year <- format(Sys.Date(), "%Y")

xlsx_measures <- c("1_5", "2_9", "3_5","4_1", "4_2","4_3","4_4",
                   "5_4","5_5","7_2","7_3","7_4","8_2","9_1",
                   "11_1","14_1","14_2")

eu_silc_measures <- c("1_5", "2_9", "3_5", "4_4", "5_4",
                      "7_3", "7_4",
                      "11_1", "14_1")

time_use_measures <- c("4_1", "4_2", "4_3", "7_2")


# ── Validation ranges by indicator type ──────────────────────────────────────
# Used for client-side input validation on year-input fields
pct_indics         <- c("1_5", "3_5", "4_2", "5_4", "5_5", "7_4", "8_2", "9_1")
scale_indics       <- c("2_9", "4_4", "7_3", "11_1", "14_1", "14_2")
hours_day_indics   <- c("4_1")
hours_week_indics  <- c("7_2")
# 4_3 (gender gap in time use) has no validation range: countries enter raw
# minutes per week (e.g. 440) per sex and the gap is calculated by the OECD.

# Named list: measure → list(min, max)
validation_ranges <- c(
  setNames(lapply(pct_indics,         function(m) list(min = 0, max = 100)),  pct_indics),
  setNames(lapply(scale_indics,       function(m) list(min = 0, max = 10)),   scale_indics),
  setNames(lapply(hours_day_indics,   function(m) list(min = 0, max = 24)),   hours_day_indics),
  setNames(lapply(hours_week_indics,  function(m) list(min = 0, max = 168)),  hours_week_indics)
)

# Row-level overrides: a handful of breakdown rows use a different valid
# range than the rest of their measure's rows. Currently this only applies
# to the "Deprivation" row within all_rows_dep_vert measures (2_9, 4_4, 7_3,
# 11_1) - it's a share of people scoring <=4, i.e. a 0-100 percentage, even
# though those measures' other rows (Country average, population groups) use
# a 0-10 scale. "Vertical inequality" (row key "vert") is left on the
# measure's default 0-10 range. Keyed by row key (data-row), applied across
# any measure that uses that row key.
row_validation_overrides <- list(
  dep = list(min = 0, max = 100)
)

# ── Data-entry row types for xlsx measures ────────────────────────────────────
# Each xlsx measure belongs to exactly one category.
# country_average_only   : one row  - "Country average"
# no_country_average     : breakdowns only (M/F + age + education), no country avg
# gender_only            : three rows - Country average / Male / Female
# all_rows               : full set - Country avg + M/F + age + education
# all_rows_dep_vert      : full set + vertical inequality + deprivation
# Unassigned measures fall back to country_average_only.

country_average_only <- c("1_5", "3_5", "4_2", "9_1")
no_country_average <- c("8_2")

# ── Static per-indicator notes ────────────────────────────────────────────────
# Shown in a highlighted box at the top of the indicator's data-entry panel.
# measure code → note text (plain text; HTML is escaped).
measure_notes <- list(
  "8_2" = paste0(
    "For voter turnout we are only asking for data broken down by population ",
    "group (sex, age and education level), not country-level figures. ",
    "The heatmap on this tab therefore reflects population-group data only. ",
    "Country-level voter turnout shown in the Well-being Data Coverage tab is ",
    "sourced separately and may look different."
  ),
  "4_3" = paste0(
    "This indicator is calculated by subtracting women's total time from men's. ",
    "There is no need to provide values for men or women if the value is already ",
    "confirmed and available in the database."
  )
)
gender_only <- c("4_3")
all_rows <- c("4_1", "5_4", "5_5", "7_2", "7_4", "14_1", "14_2")
all_rows_dep_vert <- c("2_9", "4_4", "7_3", "11_1")


# ── Shared breakdown-row definitions ──────────────────────────────────────────
# Which rows an indicator is collected on, and how each row maps onto the
# published data's sex / age / education dimensions. Used by the Excel template
# download and by the record copy of a country's submission, so the two always
# describe the same rows.

dl_age_labels <- function(m) {
  if (m %in% young_15_24) {
    list(young = "Young (15-24 years)", middle_aged = "Middle-aged (25-64 years)", old = "Old (65+ years)")
  } else if (m %in% young_16_24) {
    list(young = "Young (16-24 years)", middle_aged = "Middle-aged (25-54 years)", old = "Old (55+ years)")
  } else {
    list(young = "Young (16-29 years)", middle_aged = "Middle-aged (30-49 years)", old = "Old (50+ years)")
  }
}

dl_row_defs <- function(m) {
  al <- dl_age_labels(m)
  edu_rows <- list(
    list(key = "primary",     label = "Primary (ISCED levels 0-2)"),
    list(key = "secondary",   label = "Secondary (ISCED levels 3-4)"),
    list(key = "tertiary",    label = "Tertiary (ISCED levels 5-8)")
  )
  demo_rows <- c(
    list(
      list(key = "male",        label = "Male"),
      list(key = "female",      label = "Female"),
      list(key = "young",       label = al$young),
      list(key = "middle_aged", label = al$middle_aged),
      list(key = "old",         label = al$old)
    ),
    edu_rows
  )
  if (m %in% no_country_average) {
    demo_rows
  } else if (m %in% all_rows) {
    c(list(list(key = "country_avg", label = "Country average")), demo_rows)
  } else if (m %in% all_rows_dep_vert) {
    c(list(list(key = "country_avg", label = "Country average"),
           list(key = "vert",        label = "Vertical inequality"),
           list(key = "dep",         label = "Deprivation")),
      demo_rows)
  } else if (m %in% gender_only) {
    list(
      list(key = "country_avg", label = "Country average"),
      list(key = "male",        label = "Male"),
      list(key = "female",      label = "Female")
    )
  } else {
    list(list(key = "country_avg", label = "Country average"))
  }
}

# Breakdown key → the sex / age / education combination it corresponds to in
# the published data. The vert/dep rows are separate measures (_VER/_DEP) and
# so are handled separately by callers.
breakdown_filter_map <- list(
  country_avg = list(sex = "_T", age = "_T", edu = "_T"),
  male        = list(sex = "M",  age = "_T", edu = "_T"),
  female      = list(sex = "F",  age = "_T", edu = "_T"),
  young       = list(sex = "_T", age = "YOUNG", edu = "_T"),
  middle_aged = list(sex = "_T", age = "MID",   edu = "_T"),
  old         = list(sex = "_T", age = "OLD",   edu = "_T"),
  primary     = list(sex = "_T", age = "_T", edu = "ISCED11_1"),
  secondary   = list(sex = "_T", age = "_T", edu = "ISCED11_2_3"),
  tertiary    = list(sex = "_T", age = "_T", edu = "ISCED11_5T8")
)


# ── Time Use Survey tables ────────────────────────────────────────────────────
# Rename columns below; data is entered interactively in the app.
# Cols 1-2 are free text; remaining cols are numeric.

time_use_col_names_1 <- c("Code", "Population (Total / Women / Men)", "Total\n(15-64 years old)", "Men\n(15-64 years old)", "Women\n(15-64 years old)")  # TODO: rename
time_use_col_names_2 <- c("Code", "Activity", "")                     

table_1_col_1 <- c(
  "1.0", "1.1", "1.2", "1.3", "1.4", "1.5", "1.6", 
  "2.0", "2.1", "2.2", "2.3", "2.3.1", "2.3.2", "2.4", "2.5", "2.6", "2.7", 
  "3.0", "3.1", "3.2", "3.3", 
  "4.0", "4.1", "4.2", "4.3", "4.4", "4.5", 
  "5.0", "5.1", "5.2", 
  "T"
)

table_1_col_2 <- c(
  "Paid work or study", "Paid work (all jobs)", "Travel to and from work/study", "Time in school or classes", "Research/homework", "Job search", "Other paid work or study-related",
  "Unpaid work", "Routine housework", "Shopping", "Care for household members", "Child care", "Adult care", "Care for non household members", "Volunteering", "Travel related to household activities", "Other unpaid",
  "Personal care", "Sleeping", "Eating & drinking", "Personal, household, and medical services + travel related to personal care",
  "Leisure", "Sports", "Participating / attending events", "Visiting or entertaining friends", "TV or radio at home", "Other leisure activities",
  "Other", "Religious / spiritual activities and civic obligations", "Other (no categories)",
  "Total"
)

young_16_29 <- c("2_9", "4_4", "7_3", "11_1",
                "4_1", "7_2", "7_4", "14_1", "14_2")

young_15_24 <- c("5_4", "5_5")

young_16_24 <- c("8_2")



# Fixed text for the first two (static) columns of each Time Use table.
# Edit values directly here - these display as read-only text in the app.
time_use_row_text_1 <- data.frame(
  Col_1 = table_1_col_1,   
  Col_2 = table_1_col_2,   
  stringsAsFactors = FALSE
)

time_use_row_text_2 <- data.frame(
  Col_1 = table_1_col_1[-31],   # TODO: fill in row labels
  Col_2 = table_1_col_2[-31],
  stringsAsFactors = FALSE
)

# ── Cross-country coverage counts ────────────────────────────────────────────
# For each measure × year, how many countries have data?  Used in the
# Well-being Data Coverage heatmap to highlight important gaps.
dat_all_country_avgs <- readRDS("./data/final dataset.RDS") %>%
  filter(sex == "_T", age == "_T", education_lev == "_T",
         !is.na(obs_value)) %>%
  filter(!grepl("_DEP$|_VER$", measure), !grepl("11_3_", measure))

n_total_countries <- n_distinct(dat_all_country_avgs$ref_area)

coverage_counts <- dat_all_country_avgs %>%
  group_by(measure, time_period) %>%
  summarise(n_countries = n_distinct(ref_area), .groups = "drop") %>%
  mutate(time_period = as.numeric(time_period))

rm(dat_all_country_avgs)

# Measures from time-use surveys - gaps are less concerning for these
time_use_no_concern <- c("4_1", "4_2", "4_3", "8_2")

# OECD average series - loaded when available; NULL otherwise
oecd_avg_file <- "data/oecd average.RDS"
oecd_avg <- if (file.exists(oecd_avg_file)) readRDS(oecd_avg_file) else NULL

# ── Pre-filled Country Question Format responses ────────────────────────────
# Load previous country responses and convert to the format expected by
# build_response_html:  country_prefill[["AUT"]][["1_5"]] → list("1" = val, ...)
# Matching is by question text (labels may be offset across countries).

.prev_resp_raw <- if (file.exists("data/previous responses.RDS")) {
  readRDS("data/previous responses.RDS") 
} else {
  list()
}

# The file is now one long tibble (question, response, measure, ref_area,
# year); it used to be a named list of per-country tibbles. Convert to the
# per-country list expected below, keeping only each country's most recent
# request round per measure.
if (is.data.frame(.prev_resp_raw)) {
  if ("year" %in% names(.prev_resp_raw)) {
    .prev_resp_raw <- .prev_resp_raw %>%
      mutate(.yr = suppressWarnings(as.numeric(year))) %>%
      group_by(ref_area, measure) %>%
      slice_max(.yr, with_ties = TRUE, na_rm = FALSE) %>%
      ungroup() %>%
      select(-.yr)
  }
  .prev_resp_raw <- split(.prev_resp_raw, .prev_resp_raw$ref_area)
}

country_prefill <- lapply(.prev_resp_raw, function(country_df) {
  measures <- unique(country_df$measure)
  setNames(lapply(measures, function(m) {
    prev   <- country_df[country_df$measure == m, , drop = FALSE]
    # Find the matching response format to get question labels + indices
    fmt_idx <- which(sapply(xlsx_response_format, function(x) x$indic) == m)
    if (length(fmt_idx) == 0) return(NULL)
    fmt_labels <- xlsx_response_format[[fmt_idx[1]]]$response$label

    out <- list()
    for (i in seq_along(fmt_labels)) {
      # Match by exact question text
      hit <- which(prev$question == fmt_labels[i])
      if (length(hit) > 0) {
        val <- prev$response[hit[1]]
        if (!is.na(val) && nzchar(val)) out[[as.character(i)]] <- val
      }
    }
    if (length(out) > 0) out else NULL
  }), measures)
})
rm(.prev_resp_raw)

# ── OECD comments per country × measure ─────────────────────────────────────
# Excel file with columns: ref_area (ISO3), measure, comment, and an optional
# reason column explaining why a figure could not be used or published (e.g.
# "5-point scale rather than 11-point scale used by the OECD").
# Loaded as two nested lists of the same shape:
#   oecd_comments[["AUT"]][["1_5"]] → "comment text"
#   oecd_reasons[["AUT"]][["1_5"]]  → "reason text"   (absent where blank)
oecd_comments_file <- "data/oecd_comments.xlsx"

.cmt_raw <- if (file.exists(oecd_comments_file)) {
  readxl::read_excel(oecd_comments_file) %>%
    filter(!is.na(comment), nzchar(comment))
} else {
  NULL
}

oecd_comments <- if (!is.null(.cmt_raw)) {
  lapply(split(.cmt_raw, .cmt_raw$ref_area),
         function(df) setNames(as.character(df$comment), df$measure))
} else {
  list()
}

# The reason column is optional, and is blank for most rows.
oecd_reasons <- if (!is.null(.cmt_raw) && "reason" %in% names(.cmt_raw)) {
  .rsn <- .cmt_raw %>% filter(!is.na(reason), nzchar(reason))
  if (nrow(.rsn) > 0) {
    lapply(split(.rsn, .rsn$ref_area),
           function(df) setNames(as.character(df$reason), df$measure))
  } else {
    list()
  }
} else {
  list()
}

rm(.cmt_raw)
if (exists(".rsn")) rm(.rsn)

# ── Most recent data request each country responded to ───────────────────────
# Long-format RDS with columns: ref_area (country NAME, not ISO3) and
# time_period (the year of the request round they last responded to).
# Countries absent from the file have never responded, so their portal starts
# unpopulated. Three names in the file differ from the labels used here, hence
# the alias map - without it those countries would wrongly read as "no
# response". Keyed by ISO3 so the app can look a country up directly.
latest_request_file <- "data/latest request.RDS"

.lr_aliases <- c("Slovakia" = "Slovak Republic",
                 "South Korea" = "Korea",
                 "Turkey" = "Türkiye")

latest_request <- if (file.exists(latest_request_file)) {
  .lr <- readRDS(latest_request_file) %>%
    # REMOVE EVENTUALLY
    mutate(
      ref_area = if_else(ref_area %in% names(.lr_aliases),
                         unname(.lr_aliases[ref_area]), ref_area),
      iso = unname(country_name_vector[match(ref_area, names(country_name_vector))]),
      time_period = as.numeric(time_period)
    ) %>%
    filter(!is.na(iso)) %>%
    # Keep the most recent round if a country appears more than once
    arrange(iso, desc(time_period)) %>%
    distinct(iso, .keep_all = TRUE)
  setNames(.lr$time_period, .lr$iso)
} else {
  setNames(numeric(0), character(0))
}

rm(.lr_aliases)
if (exists(".lr")) rm(.lr)

# ── Last time use survey previously submitted, per country ───────────────────
# Long-format RDS with columns: name, value, ref_area, year_used.
#   name == "Survey name"        → the survey's title
#   name == "Latest survey year" → when the survey took place (free text, e.g.
#                                  "2020-21", so kept as a string)
#   year_used                    → the data request round in which the country
#                                  submitted it
# Reshaped to one row per country (most recent round if a country appears in
# more than one) and shown read-only on the Time Use tab, so countries can
# confirm whether the survey we already hold is still their latest.
last_tu_survey_file <- "data/time use submissions.RDS"

last_tu_survey <- if (file.exists(last_tu_survey_file)) {
  .tu_raw <- readRDS(last_tu_survey_file) %>%
    filter(name %in% c("Survey name", "Latest survey year")) %>%
    mutate(
      field = if_else(name == "Survey name", "survey_name", "survey_year"),
      value = trimws(as.character(value))
    ) %>%
    distinct(ref_area, year_used, field, .keep_all = TRUE) %>%
    select(ref_area, year_used, field, value) %>%
    tidyr::pivot_wider(names_from = field, values_from = value) %>%
    mutate(
      .year_used_num = suppressWarnings(as.numeric(year_used)),
      survey_name    = na_if(survey_name, ""),
      survey_year    = na_if(survey_year, "")
    ) %>%
    group_by(ref_area) %>%
    slice_max(.year_used_num, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-.year_used_num) %>%
    # A country with no usable survey name has nothing to show
    filter(!is.na(survey_name), !survey_name %in% c("N.A.", "NA", "n.a."))
  res <- split(.tu_raw, .tu_raw$ref_area)
  rm(.tu_raw)
  res
} else {
  list()
}


