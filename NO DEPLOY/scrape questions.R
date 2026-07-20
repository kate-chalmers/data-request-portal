library(tidyverse)

bold_section <- function(x, section) {
  str_replace(x, fixed(section), paste0("<b>", section, "</b>"))
}

excel_file <- "~/OneDrive/WISE/Data requests/NO DEPLOY/MEX well-being and time use questionnaire - February 2025.xlsx"

indicator_sheets <- readxl::excel_sheets(excel_file) %>% .[grepl("Indicator", .)]

indicator_response <- list()

for(i in seq_along(indicator_sheets)) {
  
  dat <- readxl::read_excel(excel_file, sheet = indicator_sheets[i])
  
  dat_tidy <- dat %>% 
    select(label = 2, response = 3) %>%
    mutate(filter = ifelse(grepl("Indicator group", label), 1, NA),
           filter = zoo::na.locf(filter, na.rm = F)) %>%
    filter(is.na(filter)) %>%
    select(label, response) %>%
    drop_na(label) %>%
    filter(!label %in% c("Current Well-being", "Resources for Future Well-being")) %>%
    mutate(
      measure = indicator_sheets[i],          # <- you were missing this comma
      response = str_remove_all(response, "\r"),
      response = str_replace_all(response, "\n", "<br>"),
      response = case_when(
        grepl("1_5", measure) ~ bold_section(response, "1 With great difficulty<br>2 With difficulty"),
        grepl("3_5", measure) ~ bold_section(response, "2 No"),
        grepl("7_4", measure) ~ bold_section(response, "1 All of the time<br>2 Most of the time"),
        TRUE ~ response
      )
    ) 
  
  temp_list <- list(
    indic = dat_tidy %>% filter(label == "Indicator code") %>% pull(response),
    label = dat_tidy %>% drop_na(response),
    response = dat_tidy %>% filter(is.na(response))
  ) 
  
  indicator_response[[i]] <- temp_list
  
}


saveRDS(indicator_response, "/Users/Kate/OneDrive/WISE/Data request portal/data-request-portal/data/response_input.RDS")


