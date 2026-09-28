library(tidyverse)

# Now stored as one long tibble that already contains ref_area
prev_response <- readRDS("data/previous responses.RDS") %>%
  filter(question %in% "If yes, could you provide the exact question wording and response scale used?",
        !is.na(response)) %>%
  select(ref_area, measure, country_response = response)

used_in_database <- readRDS("data/non-used responses.RDS") %>%
  filter(sex == "_T", education_lev == "_T", age == "_T", 
        !grepl("_DEP", measure), !grepl("_VER", measure)) %>%
  group_by(measure, ref_area) %>%
  #mutate(min_year = min(time_period), max_year = max(time_period)) %>%
  #mutate(time_frame = paste0(min_year, "-", max_year)) %>%
  select(measure, ref_area) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(used_in_db = "no")

used_in_database %>% filter(ref_area == "NZL", measure == "2_9")
  
oecd_comments <- readxl::read_excel("data/oecd_comments.xlsx")

oecd_format <- readRDS("./data/response_input.RDS") |>
  map(\(x) bind_rows(label = x$label, response = x$response, .id = "block") |>
        mutate(indic = x$indic, .before = 1)) |>
  list_rbind() |>
  filter(grepl("OECD question", label) | grepl("OECD response", label)) %>%
  select(measure = indic, label, response) %>%
  group_by(measure) %>% 
  mutate(response = paste0(response, collapse = " ")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-label)

response_tidy <- oecd_comments %>% 
  filter(!is.na(reason)) %>% 
  merge(oecd_format) %>%
  merge(prev_response, by = c("ref_area", "measure"), all = T) %>%
  merge(used_in_database, by = c("ref_area", "measure"), all = T) %>%
  filter(!(ref_area == "CHE" & measure %in% c("1_5", "2_9", "3_5", "4_4", "7_3", "7_4", "11_1", "14_1", "14_2"))) %>%
  select(measure, "Used in database" = used_in_db ,"Country" = ref_area,  "OECD format" = response, "Country format" = country_response, "OECD comment" = comment, "Reason for exlcusion (if any)" = reason) %>%
  distinct() %>%
  mutate(measure = factor(measure, c("1_5", "2_9", "3_5", "4_4", "7_3", "7_4", "8_2", "9_1", "11_1", "14_1", "14_2")),
        `Country format` = trimws(`Country format`),
        Country = countrycode::countrycode(Country, "iso3c", "country.name"),
        `Used in database` = ifelse(is.na(`Used in database`), "Yes", "No"),
        `Used in database` = ifelse(grepl("flagged as available in latest", `OECD comment`), "Not provided in request", `Used in database`)) %>%
  arrange(measure, is.na(`OECD format`), Country) %>%
  mutate(`OECD format` = if_else(row_number() == 1, as.character(`OECD format`), ""),
         .by = measure) 

response_list <- split(response_tidy, response_tidy$measure)

openxlsx::write.xlsx(response_list, "./NO DEPLOY/flagged response patterns.xlsx")


