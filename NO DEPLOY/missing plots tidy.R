library(tidyverse)
library(oecdcountrycode)
library(ggtext)

dat <- readRDS("S:/Data/WDP/Well being database/Automated database/output/final dataset.RDS") %>%
  mutate(time_period = as.numeric(time_period)) %>%
  filter(!grepl("11_3_", measure),
         !grepl("_DEP", measure), !grepl("_VER", measure))

dat %>% distinct(education_lev)

inpath <- "S:/Data/WDP/Visualisations/R scripts"

names_indicators <- readxl::read_excel(paste0(inpath, "/data/dictionary.xlsx")) %>%
  mutate(unit_measure = str_remove_all(unit_measure, "_DEP"),
         label = case_when(
           is.na(label) & !is.na(indic) ~ indic,
           TRUE ~ label
         )) %>%
  distinct() %>%
  select(measure, label)

country_list <- unique(dat$ref_area)

for(iso3c_wanted in country_list) {
  
  country_name <- oecdcountrycode(iso3c_wanted, "iso3c", "country.name")
  
  country_len <- length(country_list)

  country_exist <- dat %>% 
    filter(sex == "_T", age == "_T", education_lev == "_T") %>%
    filter(!is.na(obs_value)) %>%
    filter(ref_area == iso3c_wanted) %>%
    select(measure, time_period) %>%
    distinct() %>%
    mutate(exist = "yes") 
  

  gradientPal <-function(x) rgb(colorRamp(c("#8dcc2e", "#EC7063"))(x), maxColorValue = 255) 
  

  plot_df <- dat %>% 
    filter(sex == "_T", age == "_T", education_lev == "_T") %>%
    select(measure, ref_area, time_period) %>%
    distinct() %>%
    group_by(measure, time_period) %>%
    mutate(n = n()) %>%
    slice(1) %>%
    select(-ref_area) %>%
    group_by(measure) %>%
    complete(time_period = 2004:2025) %>%
    ungroup() %>%
    left_join(., country_exist, by = c("measure", "time_period")) %>%
    mutate(
      exist = case_when(
        !measure %in% c("8_2", "4_1", "4_2", "4_3") & is.na(exist) & !is.na(n) ~ "no country",
        measure %in% c("8_2", "4_1", "4_2", "4_3") & is.na(exist) & !is.na(n) ~ "no concern",
        !is.na(exist) & !is.na(n) ~ "yes",
        is.na(exist) & is.na(n) ~ "no one",
        TRUE ~ "no one"
      ),
      n = case_when(exist %in% c("no country") ~ n, TRUE ~ NA),
      exist = factor(exist, levels = c("no one", "no concern", "no country", "yes")))
  
  gradient_colors <- plot_df %>%
    filter(exist == "no country") %>%
    mutate(
      n_norm = (n - 0) / (country_len - 0),
      color = ifelse(!is.na(n_norm), gradientPal(n_norm), NA),
      exist = case_when(
        !is.na(color) & exist == "no country" ~ paste0(exist, "_", n_norm),
        TRUE ~ exist
      )
    ) 
  
  no_country_name <- gradient_colors %>% filter(n_norm == max(n_norm)) %>% pull(exist) %>% unique()

  gradient_colors <- gradient_colors %>% select(-n_norm)
  
  color_conventions <- c("no country" = "#EC7063", 
                         "no one" = "grey90", 
                         "yes" = "#2ECC71",
                         "no concern" = "#FFCCCB")
  
  plot_dat <- plot_df %>%
    filter(!exist == "no country") %>%
    mutate(
      color = case_when(
        exist == "no one" ~ "grey90",
        exist == "yes" ~  "#2ECC71",
        exist == "no concern" ~ "#FFCCCB"
      )
    ) %>%
    rbind(gradient_colors) %>%
    left_join(., names_indicators, by = "measure") %>%
    # pivot_wider(names_from = "time_period", values_from = "n") %>%
    mutate(measure2 = measure) %>%
    separate(measure2, into=c("cat", "subcat")) %>%
    mutate(cat = as.numeric(cat),
           subcat = as.numeric(subcat),
           # obs_status = case_when(
           #   obs_status == "A" ~ "",
           #   TRUE ~ ""
           # ),
           time_period = as.factor(time_period),
           label = case_when(
             cat == 1 ~ paste0("<b style='color:#279fdb'>", measure, "</b> ", label),
             cat == 2 ~ paste0("<b style='color:#33ab9c'>", measure, "</b> ", label),
             cat == 3 ~ paste0("<b style='color:#2681c4'>", measure, "</b> ", label),
             cat == 4 ~ paste0("<b style='color:#942f29'>", measure, "</b> ", label),
             cat == 5 ~ paste0("<b style='color:#7c407e'>", measure, "</b> ", label),
             cat == 6 ~ paste0("<b style='color:#7ab253'>", measure, "</b> ", label),
             cat == 7 ~ paste0("<b style='color:#df5668'>", measure, "</b> ", label),
             cat == 8 ~ paste0("<b style='color:#d9a72c'>", measure, "</b> ", label),
             cat == 9 ~ paste0("<b style='color:#11b368'>", measure, "</b> ", label),
             cat == 10 ~ paste0("<b style='color:#606164'>", measure, "</b> ", label),
             cat == 11 ~ paste0("<b style='color:#e96c3b'>", measure, "</b> ", label),
             cat == 12 ~ paste0("<b style='color:darkblue'>", measure, "</b> ", label),
             cat == 13 ~ paste0("<b style='color:darkblue'>", measure, "</b> ", label),
             cat == 14 ~ paste0("<b style='color:darkblue'>", measure, "</b> ", label),
             cat == 15 ~ paste0("<b style='color:darkblue'>", measure, "</b> ", label),
             TRUE ~ label
           )) %>%
    arrange(-cat, -subcat) %>%
    select(-cat, -subcat) %>%
    mutate(label = fct_inorder(label)) 
  
  fill_colors <- setNames(plot_dat$color, plot_dat$exist) 

  p1 <- plot_dat %>%
    ggplot(aes(time_period, label, fill=exist)) +
    geom_tile(color="grey20") +
    geom_text(aes(label=n), size=2) +
    labs(y=NULL, x=NULL,
         title = paste0(country_name, " data availability, country averages"),
         caption = "Note: Number values in boxes refer to the number of countries currently available in database for that year") +
    scale_fill_manual(values= fill_colors,
                      breaks = c("no one", "no concern", no_country_name, "yes"),
                      labels = c("No data in database",
                                 "Intermittent data expected",
                                 paste0("No data available (color based on no. of other countries available)"),
                                 paste0("Data available"))) +
    scale_x_discrete(expand=c(0,0), breaks=c(2004:2025)) +
    scale_y_discrete(expand=c(0,0)) +
    guides(fill = guide_legend(nrow=2)) +
    theme(axis.text.x = element_text(hjust=1, angle = 45),
          legend.position = "top",
          legend.title = element_blank(),
          axis.text.y = element_markdown(),
          panel.grid = element_blank(),
          plot.title = element_markdown(),
          plot.background = element_rect(fill="white"),
          plot.caption.position = "plot")
  
  ggsave(paste0(iso3c_wanted, "_Data availability country averages.png"),
         path = paste0("./output/", iso3c_wanted, "/"),
         plot = p1, height=12, width=9)
  
  tots <- dat %>%
    filter(sex == "_T", age == "_T", education_lev == "_T")
  
  country_horiz_exist <- dat %>% 
    anti_join(tots) %>%
    filter(!is.na(obs_value)) %>%
    filter(ref_area == iso3c_wanted) %>%
    select(measure, sex, age, education = education_lev) %>%
    distinct() %>%
    mutate(exist = "yes") %>%
    distinct() %>%
    pivot_longer(!c(measure, exist)) %>%
    filter(!value == "_T") %>%
    select(-name) 
  
  
  p2 <- dat %>%
    anti_join(tots) %>%
    select(ref_area, measure, unit_measure, sex, age, education = education_lev) %>%
    distinct() %>%
    group_by(measure, unit_measure, sex, age, education) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    select(-ref_area) %>%
    distinct() %>%
    pivot_longer(!c(measure, unit_measure, n)) %>%
    filter(!value == "_T") %>%
    select(-name) %>%
    group_by(measure) %>%
    complete(value = c("F", "M", "YOUNG", "MID", "OLD", "ISCED11_1", "ISCED11_2_3", "ISCED11_5T8")) %>%
    ungroup() %>% 
    left_join(., country_horiz_exist, by = c("measure", "value")) %>% 
    drop_na(value) %>%
    mutate(
      exist = case_when(
        !measure %in% c("8_2", "4_1", "4_2", "4_3") & is.na(exist) & !is.na(value) & !is.na(n)  ~ "no country",
        measure %in% c("8_2", "4_1", "4_2", "4_3") & is.na(exist) & !is.na(value) & !is.na(n)  ~ "no concern",
        !is.na(exist) & !is.na(value) & !is.na(n) ~ "yes",
        is.na(exist) & is.na(value) & is.na(n) ~ "no one",
        TRUE ~ "no one"
      ),
      n = case_when(exist %in% c("no country") ~ n, TRUE ~ NA),
      exist = factor(exist, levels = c("no one", "no concern", "no country", "yes"))
    ) %>%
    left_join(., names_indicators, by = "measure") %>%
    mutate(measure2 = measure) %>%
    separate(measure2, into=c("cat", "subcat")) %>%
    mutate(cat = as.numeric(cat),
           subcat = as.numeric(subcat),
           value = factor(value, c("F", "M", "PRIMARY", "SECONDARY", "TERTIARY", "YOUNG", "MID", "OLD")),
           label = case_when(
             cat == 1 ~ paste0("<b style='color:#279fdb'>", measure, "</b> ", label),
             cat == 2 ~ paste0("<b style='color:#33ab9c'>", measure, "</b> ", label),
             cat == 3 ~ paste0("<b style='color:#2681c4'>", measure, "</b> ", label),
             cat == 4 ~ paste0("<b style='color:#7c407e'>", measure, "</b> ", label),
             cat == 5 ~ paste0("<b style='color:#7ab253'>", measure, "</b> ", label),
             cat == 6 ~ paste0("<b style='color:#11b368'>", measure, "</b> ", label),
             cat == 7 ~ paste0("<b style='color:#e96c3b'>", measure, "</b> ", label),
             cat == 8 ~ paste0("<b style='color:#606164'>", measure, "</b> ", label),
             cat == 9 ~ paste0("<b style='color:#942f29'>", measure, "</b> ", label),
             cat == 10 ~ paste0("<b style='color:#df5668'>", measure, "</b> ", label),
             cat == 11 ~ paste0("<b style='color:#d9a72c'>", measure, "</b> ", label),
             cat == 12 ~ paste0("<b style='color:darkblue'>", measure, "</b> ", label),
             cat == 13 ~ paste0("<b style='color:darkblue'>", measure, "</b> ", label),
             cat == 14 ~ paste0("<b style='color:darkblue'>", measure, "</b> ", label),
             cat == 15 ~ paste0("<b style='color:darkblue'>", measure, "</b> ", label),
             TRUE ~ label
           )) %>%
    arrange(-cat, -subcat) %>%
    select(-cat, -subcat) %>%  
    mutate(label = fct_inorder(label)) %>%
    drop_na(value) %>%
    ggplot(aes(value, label, fill=exist)) +
    geom_tile(color="grey20") +
    geom_text(aes(label=n), size=2) +
    labs(y=NULL, x=NULL,
         title = paste0(country_name, " data availability, horizontal inequalities"),
         caption = "Note: Number values in boxes refer to the number of countries currently available in database for that year") +
    scale_fill_manual(values= color_conventions, 
                      labels = c("Data not available in database",
                                 "Intermittent data expected",
                                 paste0("No data for ", country_name),
                                 paste0("Data available for ", country_name))) +
    guides(fill = guide_legend(nrow=2)) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    theme(axis.text.x = element_markdown(hjust=1, angle = 45),
          legend.position = "top",
          legend.title = element_blank(),
          axis.text.y = element_markdown(),
          plot.background = element_rect(fill="white"),
          panel.grid = element_blank())
  
  
  ggsave(paste0(iso3c_wanted, "_Data availability horizontal inequalities.png"), 
         path = paste0("./output/", iso3c_wanted, "/"),
         plot = p2,
         height=9, width=9)
  
  
}

