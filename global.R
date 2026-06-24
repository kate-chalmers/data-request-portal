library(tidyverse)
library(shiny)
library(shinyjs)
library(echarts4r)
library(DT)

getwd()

oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST", "FIN",
                    "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR",
                    "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK",
                    "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "CRI")

oecd_names <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Czech Republic", "Denmark", "Estonia",
                "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan",
                "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Türkiye", "United Kingdom",
                "United States", "Costa Rica")

partner_countries <- c("BRA", "ARG", "BGR", "HRV", "PER", "ROU", "IDN", "THA", "ZAF")

country_name_vector <- setNames(oecd_countries, oecd_names)

dict <- readxl::read_excel("data/dictionary.xlsx") %>%
  mutate(question = NA)

xlsx_response_format <- readRDS("./data/response_input.RDS")


measure_list <- dict %>% distinct(measure) %>% filter(!grepl("_DEP", measure), !grepl("_VER", measure))


dat <- readRDS("./data/final dataset.RDS") %>%
  filter(ref_area == "AUS", sex == "_T", age == "_T", education_lev == "_T",
         measure %in% unique(measure_list$measure)) %>%
  mutate(time_period = as.numeric(time_period))

current_year <- format(Sys.Date(), "%Y")

xlsx_measures <- c("1_5","3_5","4_1","2_9","4_2","4_3","4_4",
                   "5_4","5_5","7_2","7_3","7_4","8_2","9_1",
                   "11_1","14_1","14_2")

time_use_measures <- c("4_1", "4_2", "4_3", "7_2")

# ── Data-entry row types for xlsx measures ────────────────────────────────────
# Each xlsx measure belongs to exactly one category.
# country_average_only : one row  — "Country average"
# gender_only          : three rows — Country average / Male / Female
# all_rows             : nine rows  — Country avg + M/F + age + education
# Unassigned measures fall back to country_average_only.

country_average_only <- xlsx_measures   # default: move measures to the right bucket below
gender_only          <- character(0)    # TODO: assign measures with gender breakdown
all_rows             <- character(0)    # TODO: assign measures with full breakdown

country_average_only <- c("1_5", "3_5", "4_2", "9_1")
gender_only <- c("4_3")
all_rows <- c("4_1", "5_4", "5_5", "7_2", "7_4", "8_2", "14_1", "14_2")
all_rows_dep_vert <- c("2_9", "4_4", "7_3", "11_1")


# ── Time Use Survey tables ────────────────────────────────────────────────────
# Rename columns below; data is entered interactively in the app.
# Cols 1-2 are free text; remaining cols are numeric.

time_use_col_names_1 <- c("Code", "Population (Total / Women / Men)", "Total\n(15-64 years old)", "Men\n(15-64 years old)", "Women\n(15-64 years old)")  # TODO: rename
time_use_col_names_2 <- c("Code", "Paid work or study", "")                     # TODO: rename

# Fixed text for the first two (static) columns of each Time Use table.
# Edit values directly here — these display as read-only text in the app.
time_use_row_text_1 <- data.frame(
  Col_1 = rep("", 32),   # TODO: fill in row labels (e.g. activity codes)
  Col_2 = rep("", 32),   # TODO: fill in row descriptions
  stringsAsFactors = FALSE
)

time_use_row_text_2 <- data.frame(
  Col_1 = rep("", 29),   # TODO: fill in row labels
  Col_2 = rep("", 29),
  stringsAsFactors = FALSE
)

# OECD average series — loaded when available; NULL otherwise
oecd_avg_file <- "data/oecd_average.RDS"
oecd_avg <- if (file.exists(oecd_avg_file)) readRDS(oecd_avg_file) else NULL
