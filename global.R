library(tidyverse)
library(shiny)
library(shinyjs)
library(echarts4r)
library(DT)


oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST", "FIN",
                    "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR",
                    "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK",
                    "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "CRI")

eu_silc_countries <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
                       "DEU", "GRC", "HUN", "ISL", "IRL", "ITA", "LVA", "LTU", "LUX",
                       "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "SRB", "SVK", "SVN",
                       "ESP", "SWE", "CHE")

oecd_names <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Czech Republic", "Denmark", "Estonia",
                "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan",
                "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Türkiye", "United Kingdom",
                "United States", "Costa Rica")

partner_countries <- c("BRA", "ARG", "BGR", "HRV", "PER", "ROU", "IDN", "THA", "ZAF")

country_name_vector <- setNames(oecd_countries, oecd_names)

dict <- readxl::read_excel("data/dictionary.xlsx") %>%
  mutate(question = NA)

defs_lookup <- readxl::read_excel("data/definitions.xlsx") %>%
  filter(!grepl("_DEP$|_VER$", measure)) %>%
  select(measure, definition, indicator, unit)

xlsx_response_format <- readRDS("./data/response_input.RDS")

measure_list <- dict %>% 
  distinct(measure) %>% 
  filter(!grepl("_DEP", measure), !grepl("_VER", measure),
          !grepl("11_3_", measure)) 

dat <- readRDS("./data/final dataset.RDS") %>%
  filter(sex == "_T", age == "_T", education_lev == "_T",
         measure %in% unique(measure_list$measure)) %>%
  mutate(time_period = as.numeric(time_period))

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
minutes_day_indics <- c("4_3")

# Named list: measure → list(min, max)
validation_ranges <- c(
  setNames(lapply(pct_indics,         function(m) list(min = 0, max = 100)),  pct_indics),
  setNames(lapply(scale_indics,       function(m) list(min = 0, max = 10)),   scale_indics),
  setNames(lapply(hours_day_indics,   function(m) list(min = 0, max = 24)),   hours_day_indics),
  setNames(lapply(hours_week_indics,  function(m) list(min = 0, max = 168)),  hours_week_indics),
  setNames(lapply(minutes_day_indics, function(m) list(min = -120, max = 120)), minutes_day_indics)
)

# ── Data-entry row types for xlsx measures ────────────────────────────────────
# Each xlsx measure belongs to exactly one category.
# country_average_only : one row  — "Country average"
# gender_only          : three rows — Country average / Male / Female
# all_rows             : nine rows  — Country avg + M/F + age + education
# Unassigned measures fall back to country_average_only.

country_average_only <- c("1_5", "3_5", "4_2", "9_1")
gender_only <- c("4_3")
all_rows <- c("4_1", "5_4", "5_5", "7_2", "7_4", "8_2", "14_1", "14_2")
all_rows_dep_vert <- c("2_9", "4_4", "7_3", "11_1")


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

# Fixed text for the first two (static) columns of each Time Use table.
# Edit values directly here — these display as read-only text in the app.
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

# OECD average series — loaded when available; NULL otherwise
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