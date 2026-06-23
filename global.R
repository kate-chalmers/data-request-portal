library(tidyverse)
library(highcharter)

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


measure_list <- dict %>% distinct(measure) %>% filter(!grepl("_DEP", measure), !grepl("_VER", measure))


dat <- readRDS("./data/final dataset.RDS") %>%
  filter(ref_area == "AUS", sex == "_T", age == "_T", education_lev == "_T",
         measure %in% unique(measure_list$measure)) %>%
  mutate(time_period = as.numeric(time_period))

current_year <- format(Sys.Date(), "%Y")

xlsx_measures <- c("1_5","3_5","4_1","2_9","4_2","4_3","4_4",
                   "5_4","5_5","7_2","7_3","7_4","8_2","9_1",
                   "11_1","14_1","14_2")
