library(tidyverse)

# Now stored as one long tibble that already contains ref_area
prev_response <- readRDS("data/previous responses.RDS") %>%
  filter(question %in% "If yes, could you provide the exact question wording and response scale used?",
        !is.na(response)) %>%
  select(ref_area, measure, country_response = response)


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
  select(measure, ref_area, response, country_response, reason) %>%
  distinct() %>%
  mutate(measure = factor(measure, c("1_5", "2_9", "3_5", "4_4", "7_3", "7_4", "8_2", "9_1", "11_1", "14_1", "14_2")),
        country_response = trimws(country_response)) %>%
  arrange(measure, is.na(response), ref_area) %>%
  mutate(response = if_else(row_number() == 1, as.character(response), ""),
         .by = measure)


response_list <- split(response_tidy, response_tidy$measure)

openxlsx::write.xlsx(response_list, "./NO DEPLOY/flagged response patterns.xlsx")


