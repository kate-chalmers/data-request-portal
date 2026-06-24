
timeSeriesAverage <- function(full_df, dimension_name) {
  
  # dimension_name <- "_T"
  # full_df <- tidy_dat
  
  full_df <- full_df %>% filter(ref_area %in% oecd_countries)
  
  # Remove any years that are not complete enough to be considered
  tidy_df <- full_df %>%
    mutate(unit_measure = str_remove_all(unit_measure, "_SUB")) %>%
    # Calculate sample size
    group_by(measure, unit_measure, time_period, dimension) %>%
    mutate(n=n()) %>%
    ungroup() %>%
    # Remove any years that have < 10 sample (removes data from request reporting in non-SILC years or spotty recent data)
    filter(!n <= 10)
    
  # Monitor and update when data is less spotty
  tidy_df <- tidy_df %>% filter(!measure %in% c("5_3", "10_1", "13_3") | !time_period == 2023,
                                !measure %in% c("2_2", "2_8_DEP", "2_8_VER") | !time_period == 2024) 
  
  # Obesity and voter turnout have large gaps, fill in spaces to create OECD average
  gap_filled_indics <- tidy_df %>% 
    filter(measure %in% c("13_5", "8_2", "1_3", "1_3_VER", "1_6")) %>%
    # Update max years periodically
    mutate(max_year = case_when(
      measure == "1_3" ~ 2023,
      measure == "1_6" ~ 2023,
      measure == "1_3_VER" ~ 2023,
      measure == "8_2" ~ 2025,
      measure == "13_5" ~ 2023,
    )) %>%
    group_by(measure, ref_area, dimension) %>%
    complete(time_period = 2015:max_year,
             unit_measure = unit_measure) %>%
    mutate(obs_value = zoo::na.locf(obs_value, na.rm = F),
           obs_value = zoo::na.locf(obs_value, fromLast = T, na.rm=F)) %>%
    group_by(measure, unit_measure, dimension, time_period) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    select(-max_year)
  
  tidy_df <- tidy_df %>% 
    filter(!measure %in% c("13_5", "8_2", "1_3", "1_3_VER", "1_6")) %>%
    rbind(gap_filled_indics)
  
  # OECD time series by dimension -----------------------------------------
  
  prepped_df <- tidy_df %>%
    filter(dimension %in% dimension_name)
  
  # List measures available in dimensions specified
  measures_avail <- prepped_df %>% distinct(measure) %>% pull()
  
  cleaned_df <- c()
  for(measure_name in measures_avail) {
    
    # List unit measures available in measure specified
    unit_measures_avail <- prepped_df %>% filter(measure == measure_name) %>% distinct(unit_measure) %>% pull()
    
    for(unit_measure_name in unit_measures_avail) {
      
      # Filter by each measure, unit_measure and dimension from the FULL data available for indicator
      short_df <- tidy_df %>% filter(measure == measure_name,
                                     unit_measure == unit_measure_name,
                                     dimension %in% c(dimension_name, "_T"))
      
      # List full set of countries available for indicator for full dimension
      ref_area_full <- short_df %>% distinct(ref_area) %>% pull()
      
      # Remove any countries that are missing from at least one of the three time periods
      removals <- short_df %>%
        group_by(time_period, dimension) %>%
        complete(ref_area = ref_area_full) %>%
        ungroup() %>%
        filter(is.na(obs_value)) %>%
        distinct(ref_area) %>%
        pull()
      
      # Loop over dimension values
      for(dimension_name_short in dimension_name) {
        
        short_df <- prepped_df %>%
          filter(measure == measure_name,
                 unit_measure == unit_measure_name,
                 dimension == dimension_name_short) %>%
          # Remove countries that have missing values/aren't available for all dimension
          filter(!ref_area %in% removals) %>%
          group_by(measure, unit_measure, dimension, time_period) %>%
          mutate(n = n()) %>%
          ungroup()
        
        cleaned_df <- rbind(cleaned_df, short_df)
        
      }
    }
  }
  
  
  # Calculate the average by measure, dimension, time period
  horizontal_ts <- cleaned_df %>%
    group_by(measure, unit_measure, dimension, time_period) %>%
    mutate(obs_value = mean(obs_value, na.rm=T),
           countries = paste0(ref_area, collapse = ", "),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD")) %>%
    slice(1) %>%
    ungroup()
  
  # Check same value is used across age bands
  nonMatchHorizontal <- horizontal_ts %>%
    group_by(measure, unit_measure, dimension) %>%
    mutate(var = var(n)) %>%
    ungroup() %>%
    filter(!var == 0) %>%
    nrow()
  
  # Stop function if any number mismatches exist
  if(nonMatchHorizontal != 0L) {
    stop(print("Horizontal averages do not match! Check script"))
  }
  
  # Create list of countries used
  match_list <- horizontal_ts %>%
    select(measure, unit_measure, ref_area = countries) %>%
    separate_rows(ref_area) %>%
    distinct()
  
  # Match countries to create overall OECD average --------------------------
  # This is redundant if dimension is set to _T but useful for horizontal comparisons
  
  oecd_ts <- tidy_df %>%
    filter(dimension == "_T") %>%
    # Drop any countries not in match list by merging
    merge(match_list, by = c("measure", "unit_measure", "ref_area")) %>%
    group_by(measure, unit_measure, dimension, time_period) %>%
    mutate(n = n(),
           obs_value = mean(obs_value, na.rm=T),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD")) %>%
    slice(1) %>%
    ungroup()
  
  # Check same value is used across age bands
  nonMatchAverage <- oecd_ts %>%
    group_by(measure, unit_measure, dimension) %>%
    mutate(var = var(n)) %>%
    ungroup() %>%
    filter(!var == 0) %>%
    nrow()
  
  # Stop function if any number mismatches exist
  if(nonMatchAverage != 0L) {
    stop(print("OECD averages do not match! Check script"))
  }
  
  # Test if both horizontal and OECD averages match
  test_horiz <- horizontal_ts %>% select(measure, unit_measure, horizon = ref_area) %>% distinct()
  test_oecd <- oecd_ts %>% select(measure, unit_measure, average = ref_area) %>% distinct()
  
  # Count number of mismatches
  testMatch <- test_horiz %>% merge(test_oecd) %>% filter(!horizon == average) %>% nrow()
  
  # Stop function if any number mismatches exist
  if(testMatch != 0L) {
    stop(print("OECD and horizontal averages do not match! Check script"))
  }
  
  # Return oecd average only if _T otherwise horizontal averages + oecd average
  if(all(dimension_name == "_T")) {
    full_average <- oecd_ts %>% select(-n)
  } else {
    full_average <- oecd_ts %>%
      select(-n) %>%
      rbind(horizontal_ts %>% select(-n, -countries))
  }
  
  return(full_average)
  
  full_average %>% 
    mutate(n = parse_number(ref_area)) %>%
    filter(n < 15) %>% distinct(measure)
  
}

twoPointAverage <- function(full_df, dimension_name) {
  
  # full_df <- full_dat
  # dimension_name <- group_list
  
  full_df <- full_df %>% filter(ref_area %in% oecd_countries)
  
  # Remove any years that are not complete enough to be considered
  filtered_df <- full_df %>%
    group_by(measure, unit_measure, time_period, dimension) %>%
    mutate(n=n()) %>%
    ungroup() %>%
    filter(!n < 5)
  
  # Set earliest value band
  earliest_df <- filtered_df %>%
    filter(time_period <= 2019) %>%
    group_by(ref_area, measure, unit_measure, dimension) %>%
    filter(time_period == min(time_period)) %>%
    ungroup() %>%
    mutate(label = "earliest")
  
  # Set latest value band
  latest_df <- filtered_df %>%
    filter(time_period > 2019) %>%
    group_by(ref_area, measure, unit_measure, dimension) %>%
    filter(time_period == max(time_period)) %>%
    ungroup() %>%
    mutate(label = "latest")
  
  # Combine groups
  tidy_df <- rbind(earliest_df, latest_df) 
  
  # OECD average by dimension -----------------------------------------------
  
  # Filter dimensions wanted and remove _SUB in unit_measure (for matching)
  prepped_df <- tidy_df %>% filter(dimension %in% dimension_name, ref_area %in% oecd_countries)
  
  # List measures available in dimensions specified
  measures_avail <- prepped_df %>% distinct(measure) %>% pull()

  cleaned_df <- c()
  for(measure_name in measures_avail) {
    
    # List unit measures available in measure specified
    unit_measures_avail <- prepped_df %>% filter(measure == measure_name) %>% distinct(unit_measure) %>% pull()
    
    for(unit_measure_name in unit_measures_avail) {
      
      # Filter by each measure, unit_measure and dimension from the FULL data available for indicator
      short_df <- tidy_df %>% filter(measure == measure_name,
                                     unit_measure == unit_measure_name,
                                     dimension %in% c(dimension_name, "_T"))
      
      # List full set of countries available for indicator for full dimension
      ref_area_full <- short_df %>% distinct(ref_area) %>% pull()
      
      # Remove any countries that are missing from at least one of the three time periods
      removals <- short_df %>%
        group_by(label, dimension) %>%
        complete(ref_area = ref_area_full) %>%
        ungroup() %>%
        filter(is.na(obs_value)) %>%
        distinct(ref_area) %>%
        pull()
      
      # Loop over dimension values
      for(dimension_name_short in dimension_name) {
        
        short_df <- prepped_df %>%
          filter(measure == measure_name,
                 unit_measure == unit_measure_name,
                 dimension == dimension_name_short) %>%
          # Remove countries that have missing values/aren't available for all dimension
          filter(!ref_area %in% removals) %>%
          group_by(measure, unit_measure, dimension, label) %>%
          mutate(n = n()) %>%
          ungroup()
        
        cleaned_df <- rbind(cleaned_df, short_df)
        
      }
      
    }
  }
  
  # Calculate the average by measure, dimension, time period
  horizontal_average <- cleaned_df %>%
    group_by(measure, unit_measure, dimension, label) %>%
    mutate(obs_value = mean(obs_value, na.rm=T),
           countries = paste0(ref_area, collapse = ", "),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD")) %>%
    slice(1) %>%
    ungroup() %>%
    select(-time_period)
  
  horizontal_average <- cleaned_df %>%
    group_by(measure, unit_measure, dimension, label) %>%
    count(time_period) %>%
    filter(n == max(n)) %>%
    ungroup() %>%
    select(measure, dimension, label, time_period) %>%
    merge(horizontal_average, ., by = c("measure", "dimension", "label"))
  
  # Check same value is used across age bands
  nonMatchHorizontal <- horizontal_average %>%
    group_by(measure, unit_measure, dimension) %>%
    mutate(var = var(n)) %>%
    ungroup() %>%
    filter(!var == 0) %>%
    nrow()
  
  # Stop function if any number mismatches exist
  if(nonMatchHorizontal != 0L) {
    stop(print("Horizontal averages do not match! Check script"))
  }
  
  # Create list of countries used
  match_list <- horizontal_average %>%
    select(measure, unit_measure, ref_area = countries) %>%
    separate_rows(ref_area) %>%
    distinct()
  
  # Match countries to create overall OECD average --------------------------
  # This is redundant if dimension is set to _T but useful for horizontal comparisons
  
  getmode <- function(obs_value) {
    uniqv <- unique(obs_value)
    uniqv[which.max(tabulate(match(obs_value, uniqv)))]
  }

  oecd_average <- tidy_df %>%
    filter(dimension == "_T") %>%
    # Drop any countries not in match list by merging
    merge(match_list, by = c("measure", "unit_measure", "ref_area")) %>%
    group_by(measure, unit_measure, dimension, label) %>%
    mutate(n = n(),
           obs_value = mean(obs_value, na.rm=T),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD"),
           time_period = getmode(time_period)) %>%
    slice(1) %>%
    ungroup()
  
  # Check same value is used across age bands
  nonMatchAverage <- oecd_average %>%
    group_by(measure, unit_measure, dimension) %>%
    mutate(var = var(n)) %>%
    ungroup() %>%
    filter(!var == 0) %>%
    nrow()
  
  # Stop function if any number mismatches exist
  if(nonMatchAverage != 0L) {
    stop(print("OECD averages do not match! Check script"))
  }
  
  # Test if both horizontal and OECD averages match
  test_horiz <- horizontal_average %>% select(measure, unit_measure, horizon = ref_area) %>% distinct()
  test_oecd <- oecd_average %>% select(measure, unit_measure, average = ref_area) %>% distinct()
  
  # Count number of mismatches
  testMatch <- test_horiz %>% merge(test_oecd) %>% filter(!horizon == average) %>% nrow()
  
  # Stop function if any number mismatches exist
  if(testMatch != 0L) {
    stop(print("OECD and horizontal averages do not match! Check script"))
  }
  
  # Return oecd average only if _T otherwise horizontal averages + oecd average
  if(all(dimension_name == "_T")) {
    full_average <- oecd_average %>% select(-n)
  } else {
    full_average <- oecd_average %>%
      select(-n) %>%
      rbind(horizontal_average %>% select(-countries, -n))
  }
  
  return(full_average)
  
}

twoPointAverageTUS <- function(full_df, dimension_name) {
  
  # full_df <- tidy_dat
  # dimension_name <- "_T"
  
  full_df <- full_df %>% filter(ref_area %in% oecd_countries,
                                measure %in% c("4_1", "4_2", "4_3", "7_2"))
  
  
  modeFunction <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  full_average <- full_df %>%
    group_by(measure, ref_area, dimension) %>%
    filter(time_period == max(time_period)) %>%
    group_by(measure, dimension) %>%
    mutate(
      obs_value = mean(obs_value),
      ref_area = "OECD",
      time_period = modeFunction(time_period)
    ) %>%
    slice(1)

  return(full_average)
  
}


break_wrap <- function (string, width = 80, indent = 0, exdent = 0, whitespace_only = TRUE) {
  out <- stringi::stri_wrap(string, width = width, indent = indent,
                            exdent = exdent, whitespace_only = whitespace_only, simplify = FALSE)
  vapply(out, str_c, collapse = "<br>", character(1))
}


scatterplotCompare <- function(old_dat, new_dat, measure_name, south_europe_additions = F) {
  
  # old_dat <- hsl_dat
  # new_dat <- long_hours_data
  # measure_name <- "2_7"
  
  old_dat <- old_dat %>% filter(measure == measure_name) 
  label_name <- dict %>% filter(measure == measure_name) %>% pull(label)
  
  # Extract relevant columns with proper names
  old_vals <- old_dat %>% 
    select(ref_area, value_old = obs_value) %>%
    mutate(value_old = as.numeric(value_old))
  
  new_vals <- new_dat %>% 
    select(ref_area, value_new = replace_indic) %>%
    mutate(value_new = as.numeric(value_new)) %>%
    mutate(
      region = countrycode::countrycode(ref_area, "iso3c", "region"),
      continent = countrycode::countrycode(ref_area, "iso3c", "continent"),
      continent = case_when(
        ref_area %in% c("USA", "CAN") ~ "North America",
        ref_area %in% c("CHL","COL", "CRI", "MEX", "BRA", "ARG", "PER") ~ "Latin America",
        ref_area %in% c("AUS", "NZL") ~ "Oceania",
        ref_area %in% c("AUT","BEL","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN",
                        "ISR","IRL","ISL","ITA","LVA","LTU","LUX","NLD","NOR","POL",
                        "PRT","SVK","SVN","ESP","GBR","CHE","SWE","BGR","ROU","HRV") ~ "Europe",
        ref_area %in% c("JPN","KOR","TUR","CHN","IND","IDN") ~ "Asia",
        ref_area %in% c("ZAF") ~ "Africa",
        TRUE ~ "Other"
      ),
      cont_group = factor(continent),
      region_group = case_when(
        ref_area %in% c("DNK", "FIN", "ISL", "NOR", "SWE") ~ "North_Europe",
        ref_area %in% c("AUT", "BEL", "FRA", "DEU", 
                        "IRL", "LUX", "NLD", "CHE", 
                        "GBR") ~ "West_Europe",
        ref_area %in% c("ITA", "ESP", "PRT", "GRC") & south_europe_additions == F ~ "South_Europe",
        ref_area %in% c("ITA", "ESP", "PRT", "GRC", "TUR", "ISR") & south_europe_additions == T ~ "South_Europe",
        ref_area %in% c("CZE", "SVK", "POL", "HUN", "BGR", "SVN", 
                        "EST", "LVA", "LTU", "ROU", "HRV") ~ "East_Europe",
        ref_area %in% c("USA", "CAN") ~ "North_America",
        ref_area %in% c("MEX", "CHL", "COL", "CRI", "BRA", "ARG", "PER") ~ "Latin_America",
        ref_area %in% c("AUS", "NZL") ~ "Oceania",
        ref_area %in% c("JPN", "KOR", "ISR", "TUR") & south_europe_additions == F ~ "Asia",
        ref_area %in% c("JPN", "KOR") & south_europe_additions == T ~ "Asia",
        ref_area == "ZAF" ~ "Africa",
        TRUE ~ "Other"
      ),
      region_group = factor(region_group)
    ) %>%
    select(-region, -continent)
  
  # Merge datasets by ref_area
  dat <- merge(old_vals, new_vals, by = "ref_area")
  
  # Correlation
  cor_val <- cor(dat$value_new, dat$value_old, use = "complete.obs")
  cat("Correlation between observed and predicted values:\n")
  cat(sprintf("Pearson’s r = %.3f\n\n", cor_val))
  
  lm_spec <- linear_reg() %>% set_engine("lm")
  
  formulas <- list(
    baseline     = value_old ~ value_new,
    cont_group   = value_old ~ value_new + cont_group,
    cont_inter   = value_old ~ value_new * cont_group,
    region_group = value_old ~ value_new + region_group,
    region_inter = value_old ~ value_new * region_group
  )
  
  fits <- formulas %>% map(~ lm_spec %>% fit(as.formula(.x), data = dat))
  
  # --- ANOVA comparisons ---
  cat("ANOVA comparisons:\n")
  
  run_anova <- function(model_list, labels) {
    engines <- map(model_list, extract_fit_engine)
    result  <- do.call(anova, engines)
    # print(result)
    walk2(labels, result$`Pr(>F)`[-1], ~ cat(sprintf(
      "\n%s %s fit (p = %.4f).\n",
      .x,
      ifelse(.y < 0.05, "improves", "does NOT significantly improve"),
      .y
    )))
    invisible(result)
  }
  
  # Continent chain: baseline → group → interaction
  cat("\n── Continent models ───────────────────\n")
  baseline_cont_anova <- test <- run_anova(
    list(fits$baseline, fits$cont_group, fits$cont_inter),
    c("baseline → cont_group", "cont_group → cont_inter")
  )
  
  # Region chain: baseline → group → interaction
  cat("\n── Region models ──────────────────────\n")
  baseline_region_anova <- run_anova(
    list(fits$baseline, fits$region_group, fits$region_inter),
    c("baseline → region_group", "region_group → region_inter")
  )
  
  # Extract metrics for all models in one table
  metrics <- fits %>%
    imap_dfr(~ {
      .x %>%
        extract_fit_engine() %>%
        glance() %>%
        mutate(model = .y)
    }) %>%
    select(model, adj.r.squared)
  
  # fits %>% imap_dfr(~ tidy(.x) %>% mutate(model = .y))
  
  equations <- fits %>% 
    imap_dfr(~ tidy(.x) %>% mutate(model = .y)) %>%
    select(term, estimate, model) %>%
    filter(term %in% c("(Intercept)", "value_new")) %>%
    mutate(estimate = round(estimate, 2)) %>%
    pivot_wider(names_from = "term", values_from = "estimate") %>%
    merge(metrics) %>%
    mutate(
      adj.r.squared = round(adj.r.squared, 2),
      text = "y = ",
      text = case_when(
        model == "baseline" ~ paste0(text, `(Intercept)`, " + ", value_new, "x (Adj R2 = ", adj.r.squared, ")"),
        grepl("_group", model) ~ paste0(text, `(Intercept)`, " + ", value_new, "x + G (Adj R2 = ", adj.r.squared, ")"),
        grepl("_inter", model) ~ paste0(text, `(Intercept)`, " + ", value_new, "x + G + x:G (Adj R2 = ", adj.r.squared, ")"),
      )
    ) %>%
    select(model, text) 
  
  cont_eq_text <- equations %>% filter(grepl("cont", model)) %>% pull(text) %>% paste0(., collapse = "\n")
  reg_eq_text <- equations %>% filter(grepl("region", model)) %>% pull(text) %>% paste0(., collapse = "\n")
  
  cat("\nInterpretation:\n")
  cat("- Residuals = observed – predicted.\n")
  cat("- Standardized residuals scale this by model error:\n")
  cat("    • |Std. Residual| > 2 → moderate outlier.\n")
  cat("    • |Std. Residual| > 3 → severe outlier.\n\n")
  
  country_values <- fits %>%
    imap_dfr(~ .x %>% extract_fit_engine() %>% augment(data = dat) %>% mutate(model = .y)) %>%
    select(model, ref_area, fitted = .fitted, residuals = .resid, std_residuals = .std.resid) %>%
    mutate(
      outlier_flag = case_when(
        abs(std_residuals) > 3 ~ "Severe outlier",
        abs(std_residuals) > 2 ~ "Moderate outlier",
        TRUE ~ "Regular"
      ),
      outlier_flag = factor(outlier_flag, c("Severe outlier", "Moderate outlier", "Regular") %>% rev()),
      country = countrycode::countrycode(ref_area, "iso3c", "country.name")
    ) 
  
  country_values %>%
    filter(outlier_flag != "Regular", model == "baseline") %>%
    group_by(model, outlier_flag) %>%
    summarise(countries = paste(country, collapse = ", "), .groups = "drop") %>%
    arrange(model, outlier_flag) %>%                  
    group_by(model) %>%
    group_walk(~ {
      cat(sprintf("\n── %s model ──────────────────────────\n", .y$model))
      walk2(.x$outlier_flag, .x$countries, ~ cat(sprintf("  %s: %s\n", .x, .y)))
    })
  
  plot_df <- country_values %>% left_join(dat)
  
  # === PLOTS ===
  
  # Baseline plot -----------------------------------------------------------
  
  baseline_p1 <- ggplot(plot_df %>% filter(model == "baseline"), 
                        aes(x = value_new, y = value_old, label = ref_area, color = outlier_flag)) +
    geom_point(size = 3) +
    geom_text_repel(size = 3.5, show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
    annotate("text",
             # x = Inf, y = -Inf,
             # hjust = 1.1, vjust = -0.5,
             x = max(plot_df %>% filter(model == "baseline") %>% pull(value_new), na.rm = TRUE),
             y = min(plot_df %>% filter(model == "baseline") %>% pull(value_old), na.rm = TRUE),
             hjust = 1, vjust = 0,
             label = equations %>% filter(model == "baseline") %>% pull(text),
             size = 4, color = "black") +
    scale_color_manual(values = c("Regular" = "steelblue",
                                  "Moderate outlier" = "orange",
                                  "Severe outlier" = "tomato")) +
    labs(x = paste("Predicted (", measure_name, ")", sep = ""),
         y = paste("Observed (", measure_name, ")", sep = ""),
         title = "Baseline model",
         # subtitle = paste("Observed vs Predicted for", label_name),
         color = NULL) +
    theme_minimal() +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold"))
  
  # Residuals vs fitted plot
  baseline_p2 <- ggplot(plot_df %>% filter(model == "baseline"), 
                        aes(x = fitted, y = std_residuals, color = outlier_flag, label = ref_area)) +
    geom_point(size = 3) +
    geom_text_repel(size = 3, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "orange") +
    geom_hline(yintercept = c(-3, 3), linetype = "dotted", color = "red") +
    scale_color_manual(values = c("Regular" = "steelblue",
                                  "Moderate outlier" = "orange",
                                  "Severe outlier" = "tomato")) +
    labs(x = "Fitted values", 
         y = "Standardized residuals",
         # subtitle = "Residuals vs Fitted",
         color = "Point Type") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Combine side by side safely
  baseline_combined_plot <- baseline_p1 + baseline_p2
  
  # Continent ---------------------------------------------------------------
  
  continent_p1 <- ggplot(plot_df %>% filter(model == "cont_inter"), 
                         aes(x = value_new, y = value_old, label = ref_area, color = cont_group)) +
    geom_point(size = 3) +
    geom_text_repel(size = 3.5, show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE, aes(group = cont_group), linetype = "dashed") +
    annotate("text",
             x = max(plot_df %>% filter(model == "cont_inter") %>% pull(value_new), na.rm = TRUE),
             y = min(plot_df %>% filter(model == "cont_inter") %>% pull(value_old), na.rm = TRUE),
             hjust = 1, vjust = 0,
             label = cont_eq_text,
             size = 3.5, color = "black") +
    labs(x = paste("Predicted (", measure_name, ")", sep = ""),
         y = paste("Observed (", measure_name, ")", sep = ""),
         title = "Continent model (interaction)",
         # subtitle = paste("Observed vs Predicted for", measure_name),
         color = NULL, fill = NULL) +
    guides(fill = "none", color = guide_legend(nrow = 1)) +
    theme_minimal() +
    theme(legend.position = "top",
          legend.justification = "right",
          plot.title = element_text(face = "bold"))
  
  continent_p2 <- ggplot(plot_df %>% filter(model == "cont_inter"), 
                         aes(x = fitted, y = std_residuals, color = cont_group, label = ref_area)) +
    geom_point(size = 3) +
    geom_text_repel(size = 3, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "orange") +
    geom_hline(yintercept = c(-3, 3), linetype = "dotted", color = "red") +
    labs(x = "Fitted values", 
         y = "Standardized residuals",
         # subtitle = "Residuals vs Fitted (interaction model)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Combine side by side safely
  cont_combined_plot <- continent_p1 + continent_p2
  
  
  # Regional ----------------------------------------------------------------
  
  regional_p1 <- ggplot(plot_df %>% filter(model == "region_inter"), 
                        aes(x = value_new, y = value_old, label = ref_area, color = region_group)) +
    geom_point(size = 3) +
    geom_text_repel(size = 3.5, show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE, aes(group = region_group), linetype = "dashed") +
    annotate("text",
             x = max(plot_df %>% filter(model == "region_inter") %>% pull(value_new), na.rm = TRUE),
             y = min(plot_df %>% filter(model == "region_inter") %>% pull(value_old), na.rm = TRUE),
             hjust = 1, vjust = 0,
             label = reg_eq_text,
             size = 3.5, color = "black") +
    labs(x = paste("Predicted (", measure_name, ")", sep = ""),
         y = paste("Observed (", measure_name, ")", sep = ""),
         title = "Regional model (interaction)",
         # subtitle = paste("Observed vs Predicted for", measure_name),
         color = NULL, fill = NULL) +
    guides(fill = "none", color = guide_legend(nrow = 1)) +
    theme_minimal() +
    theme(legend.position = "top",
          legned.justification = "right",
          plot.title = element_text(face = "bold"))
  
  regional_p2 <- ggplot(plot_df %>% filter(model == "region_inter"), 
                        aes(x = fitted, y = std_residuals, color = region_group, label = ref_area)) +
    geom_point(size = 3) +
    geom_text_repel(size = 3, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "orange") +
    geom_hline(yintercept = c(-3, 3), linetype = "dotted", color = "red") +
    labs(x = "Fitted values", 
         y = "Standardized residuals",
         # subtitle = "Residuals vs Fitted (interaction model)",
         color = NULL) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Combine side by side safely
  region_combined_plot <- regional_p1 + regional_p2
  
  all_plot <- baseline_combined_plot / cont_combined_plot / region_combined_plot 
  print(all_plot)
  
  return(list(
    dat = plot_df %>% merge(metrics),
    fits = fits,
    correlation = cor_val,
    anova_base_cont = baseline_cont_anova,
    anova_base_region = baseline_region_anova
  ))
  
}


imputatorFunction <- function(old_dat, new_dat, measure_name, imputing_countries, 
                              model_type = "baseline", south_europe_additions = F) {
  
  # # For testing
  # old_dat <- hsl_dat
  # new_dat <- weekly_worked_2014
  # measure_name <- "7_2"
  # model_type <- "cont_inter"
  # imputing_countries <- imputing_iso3c
  # south_europe_additions <- F
  
  new_dat <- new_dat %>% 
    mutate(
      south_europe_additions = south_europe_additions,
      cont_group = case_when(
        ref_area %in% c("USA", "CAN") ~ "North America",
        ref_area %in% c("CHL","COL", "CRI", "MEX", "BRA", "ARG", "PER") ~ "Latin America",
        ref_area %in% c("AUS", "NZL") ~ "Oceania",
        ref_area %in% c("AUT","BEL","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN",
                        "ISR","IRL","ISL","ITA","LVA","LTU","LUX","NLD","NOR","POL",
                        "PRT","SVK","SVN","ESP","GBR","CHE","SWE","BGR","ROU","HRV") ~ "Europe",
        ref_area %in% c("JPN","KOR","TUR","CHN","IND","IDN") ~ "Asia",
        ref_area %in% c("ZAF") ~ "Africa",
        TRUE ~ "Other"
      ),
      cont_group = factor(cont_group),
      # cont_group = factor(cont_group, levels = c("Latin America", "North America", "Europe", "Africa", "Oceania", "Asia", "Other")),
      region_group = case_when(
        ref_area %in% c("DNK", "FIN", "ISL", "NOR", "SWE") ~ "North_Europe",
        ref_area %in% c("AUT", "BEL", "FRA", "DEU", 
                        "IRL", "LUX", "NLD", "CHE", 
                        "GBR") ~ "West_Europe",
        ref_area %in% c("ITA", "ESP", "PRT", "GRC") & south_europe_additions == F ~ "South_Europe",
        ref_area %in% c("ITA", "ESP", "PRT", "GRC", "TUR", "ISR") & south_europe_additions == T ~ "South_Europe",
        ref_area %in% c("CZE", "SVK", "POL", "HUN", "BGR", "SVN", 
                        "EST", "LVA", "LTU", "ROU", "HRV") ~ "East_Europe",
        ref_area %in% c("USA", "CAN") ~ "North_America",
        ref_area %in% c("MEX", "CHL", "COL", "CRI", "BRA", "ARG", "PER") ~ "Latin_America",
        ref_area %in% c("AUS", "NZL") ~ "Oceania",
        ref_area %in% c("JPN", "KOR", "ISR", "TUR") & south_europe_additions == F ~ "Asia",
        ref_area %in% c("JPN", "KOR") & south_europe_additions == T ~ "Asia",
        ref_area == "ZAF" ~ "Africa",
        TRUE ~ "Other"
      ),
      region_group = factor(region_group)
      # region_group = factor(region_group, levels = c("North_Europe", "West_Europe", "South_Europe",
      #                                                "East_Europe", "Latin_America", "North_America",
      #                                                "Oceania", "Asia", "Africa", "Other"))
    ) %>%
    select(-south_europe_additions)
  
  # --- Merge with old data for model fitting ---
  indic_relationship <- old_dat %>%
    select(-time_period) %>%
    filter(measure == measure_name) %>%
    select(ref_area, missing_indic = obs_value) %>%
    merge(new_dat, all = TRUE) 
  
  # --- Correlation between missing and replacement indicator ---
  corr_val <- indic_relationship %>%
    summarise(
      cor_val = cor(missing_indic, replace_indic, use = "complete.obs"),
      n_obs_used = sum(complete.cases(missing_indic, replace_indic))
    )
  
  cat("Correlation:", round(corr_val$cor_val, 3), "based on", corr_val$n_obs_used, "observations.\n")
  
  # --- Model formula ---
  formulas <- list(
    baseline     = missing_indic ~ replace_indic,
    cont_group   = missing_indic ~ replace_indic + cont_group,
    cont_inter   = missing_indic ~ replace_indic * cont_group,
    region_group = missing_indic ~ replace_indic + region_group,
    region_inter = missing_indic ~ replace_indic * region_group
  )
  
  formula_model <- formulas[[model_type]]
  
  # --- Fit linear regression model ---
  lm_spec <- linear_reg() %>% set_engine("lm")
  res <-  lm_spec %>% fit(formula_model, data = indic_relationship)
  
  # --- Model diagnostics ---
  r_squared_val <- round(summary(res$fit)$adj.r.squared, 4)
  cat("Adjusted R-squared:", r_squared_val, "\n")
  
  # Remap levels to match model training data to avoid unseen factor level errors
  if(grepl("cont", model_type)) {
    
    # --- Extract continent levels from the model ---
    model_continent_levels <- res$fit$xlevels[["cont_group"]]
    
    # --- Impute missing values for imputing countries ---
    imputing_df <- new_dat %>%
      mutate(
        cont_group = as.character(cont_group),
        cont_group = ifelse(cont_group %in% model_continent_levels, cont_group, model_continent_levels[1]),
        cont_group = factor(cont_group, levels = model_continent_levels)
      )
    
  } else if(grepl("region", model_type)) {
    
    # --- Extract continent levels from the model ---
    model_region_levels <- res$fit$xlevels[["region_group"]]
    
    # --- Impute missing values for imputing countries ---
    imputing_df <- new_dat %>%
      mutate(
        region_group = as.character(region_group),
        region_group = ifelse(region_group %in% model_region_levels, region_group, model_region_levels[1]),
        region_group = factor(region_group, levels = model_region_levels)
      )
    
  } else {
    imputing_df <- new_dat 
  }
  
  agreement_df <- imputing_df %>% filter(!ref_area %in% imputing_countries)
  
  index_agree <- agreement_df |> 
    select(ref_area) |> 
    bind_cols(predict(res$fit, newdata = agreement_df)) %>%
    rename(pred = 2) %>%
    merge(old_dat %>% filter(measure == measure_name) %>% select(ref_area, obs_value))
  
  index_agree_val <- metrica::d(index_agree, obs = obs_value, pred = pred)
  cat("The index of agreement is ", round(as.numeric(index_agree_val), 3), "\n", sep = "")
  
  # --- Refined Willmott Index ---
  refined_willmot <- index_agree %>%
    mutate(
      dr = 1 - sum(abs(pred - obs_value)) / sum(abs(pred - mean(obs_value)) + abs(obs_value - mean(obs_value))),
      dr = round(dr, 3),
      intepretation = case_when(
        dr > 0.9 ~ "excellent",
        dr >= 0.75 & dr <= 0.9 ~ "good",
        dr < 0.5 ~ "poor",
        TRUE ~ "moderate"
      )
    ) %>%
    select(intepretation, dr) %>%
    distinct()
  
  cat("The Refined Willmott Index of Agreement is ", refined_willmot$dr, 
      " (", refined_willmot$intepretation, " agreement)\n", sep = "")
  
  # Add AIC/BIC to test model fit
  imputing_df <- imputing_df %>% filter(ref_area %in% imputing_countries)
  
  indic_dat <- imputing_df |> 
    select(ref_area) |> 
    bind_cols(predict(res$fit, newdata = imputing_df)) %>%
    rename(obs_value = 2) %>%
    mutate(measure = measure_name)
  
  imputing_df$obs_value <- predict(res$fit, newdata = imputing_df)
  
  indic_dat <- imputing_df %>%
    mutate(measure = measure_name) %>%
    select(ref_area, obs_value, measure)
  
  # --- Summary table ---
  results_table <- data.frame(
    measure_name = measure_name,
    model_type = model_type,
    correlation_val = corr_val$cor_val,
    r_squared_val = r_squared_val,
    index_agree_val = index_agree_val,
    index_agree_refined = refined_willmot$dr
  )
  
  cat("\nSummary table row:\n")
  print(results_table)
  
  return(list(
    indic_dat = indic_dat,
    results_table = results_table
  ))
  
}




