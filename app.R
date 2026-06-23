source("./global.R")

ui <- navbarPage("Data request",
                 tags$head(
                   tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
                   tags$script(HTML("
      // Toggle collapsible panel
      function togglePanel(id) {
        var el = document.getElementById('panel_' + id);
        el.style.display = el.style.display === 'none' ? 'flex' : 'none';
      }

      // Collect all year inputs for a measure and send to Shiny
      function submitMeasure(safe_id, measure) {
        var container = document.getElementById('inputs_' + safe_id);
        var inputs = container.querySelectorAll('input[type=number]');
        var values = {};
        inputs.forEach(function(inp) {
          values[inp.dataset.year] = inp.value === '' ? null : parseFloat(inp.value);
        });
        Shiny.setInputValue('submitted_data', {
          measure: measure,
          values: values,
          timestamp: new Date().toISOString()
        }, {priority: 'event'});
      }
    "))
                 ),
                 tabPanel("Landing page",
                          fluidPage(
                            fluidRow(
                              column(2),
                              column(8, align= "center", 
                                     img(src="wise_logo.png", height = 100, width = 150), 
                                     br(), 
                                     img(src="oecd_logo.svg"),
                                     HTML("<br><br>The purpose of this questionnaire is to gather national data on 
                                                              different aspects of well-being in OECD member countries to ensure they are reflected in 
                                                              the OECD How's Life? Well-being Database and associated products such as upcoming editions 
                                                              of the How's Life? publication series and annually updated well-being country profiles. 
                                                              The OECD How's Life? Well-being Database federates over 80 indicators (see below), 
                                                              the majority of which are sourced from other OECD and external international data collections. 
                                                              To ensure a streamlined process, this questionnaire covers the indicators that are unique to the OECD How's 
                                                              Life? Well-being Database as well as relevant information for the OECD Time Use Database. 
                                                              It therefore excludes indicators that are managed through other OECD data collection activities, 
                                                              or by other external international data producers. All types of offical surveys are of interest to this 
                                                              exercise, including (but not limited to) household surveys, health surveys, general social surveys, 
                                                              time-use surveys and ad hoc surveys.")
                                     
                              ),
                              column(2)
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(3),
                              column(6,
                                     wellPanel(
                                       textInput("session_name", "Session name", value = paste0("session_", format(Sys.time(), "%Y%m%d_%H%M"))),
                                       actionButton("save_session", "💾 Save country session", class = "btn-primary btn-sm", style = "width:100%; margin-bottom:6px;"),
                                       br(),
                                       textInput("load_session_name", "Load session name"),
                                       actionButton("load_session", "📂 Load country session", class = "btn-default btn-sm", style = "width:100%;"),
                                       br(), br(),
                                       uiOutput("session_status")
                                     )
                              ),
                              column(3)
                            ),
                            br(),
                            fluidRow(
                              column(1),
                              column(10, align = "center",
                                     uiOutput("countryLoaded"),
                                     br(),
                                     br(),
                                     tags$div(
                                       style = "display:flex; gap:16px; font-size:11px; margin-bottom:8px; justify-content:center;",
                                       tags$span(tags$span(style="display:inline-block;width:12px;height:12px;background:darkgreen;border-radius:2px;margin-right:4px;"), "Existing data"),
                                       tags$span(tags$span(style="display:inline-block;width:12px;height:12px;background:#e67e22;border-radius:2px;margin-right:4px;"), "Submitted by you"),
                                       tags$span(tags$span(style="display:inline-block;width:12px;height:12px;background:lightgrey;border-radius:2px;margin-right:4px;"),  "No data")
                                     ),
                                     uiOutput("heatmap")
                              ),
                              column(1)
                            )
                          )
                 )
)

server <- function(input, output, session) {
  
  # Reactive store for all submitted data: list keyed by measure
  session_data <- reactiveValues(entries = list())
  
  # ── Build heatmap ──────────────────────────────────────────────────────────
  observe({
    
    entries <- session_data$entries
    years <- 2004:2026

    dat_tidy <- dat %>%
      select(measure, time_period, obs_value) %>%
      complete(measure = unique(measure_list$measure),
               time_period = years) %>%
      mutate(measure2 = measure) %>%
      separate(measure2, into = c("cat", "subcat")) %>%
      mutate(cat = as.numeric(cat), subcat = as.numeric(subcat)) %>%
      arrange(cat, subcat) %>%
      mutate(
        group = case_when(
          cat == 1  ~ "Income and wealth",
          cat == 2  ~ "Work and job quality",
          cat == 3  ~ "Housing",
          cat == 4  ~ "Work-life balance",
          cat == 5  ~ "Health",
          cat == 6  ~ "Knowledge and skills",
          cat == 7  ~ "Social connections",
          cat == 8  ~ "Civic engagement",
          cat == 9  ~ "Environmental quality",
          cat == 10 ~ "Safety",
          cat == 11 ~ "Subjective well-being",
          cat == 12 ~ "Natural capital",
          cat == 13 ~ "Human capital",
          cat == 14 ~ "Social capital",
          cat == 15 ~ "Economic capital"
        )
      )
    
    val_lookup <- dat %>%
      select(measure, time_period, obs_value) %>%
      mutate(time_period = as.numeric(time_period))
    
    # Build the year axis row once — sits above all groups
    label_every <- c(2004, 2008, 2012, 2016, 2020, 2024, 2026)
    
    year_axis_cells <- paste(
      sapply(years, function(yr) {
        label <- if (yr %in% label_every) as.character(yr) else ""
        paste0(
          "<div style='flex:1;text-align:center;font-size:9px;color:#888;'>", label, "</div>"
        )
      }),
      collapse = ""
    )
    
    axis_row <- paste0(
      "<div style='display:flex;flex-direction:row;align-items:center;width:100%;margin-bottom:4px;'>",
      "<div style='flex:0 0 20%;'></div>",
      "<div style='flex:1;display:flex;flex-direction:row;'>",
      year_axis_cells,
      "</div>",
      "<div style='flex:0 0 130px;'></div>",
      "</div>"
    )
    
    make_year_inputs <- function(m) {
      safe <- gsub("\\.", "_", m)
      vals <- val_lookup %>% filter(measure == m)
      
      # Override with session data if available
      saved <- session_data$entries[[m]]
      
      inputs <- sapply(years, function(yr) {
        v <- if (!is.null(saved) && !is.null(saved[[as.character(yr)]])) {
          saved[[as.character(yr)]]
        } else {
          row <- vals %>% filter(time_period == yr)
          if (nrow(row) > 0 && !is.na(row$obs_value[1])) row$obs_value[1] else NA
        }
        has_val <- !is.na(v)
        value_attr       <- if (has_val) paste0("value='", v, "'") else ""
        placeholder_attr <- if (!has_val) "placeholder='-'" else ""
        paste0(
          "<div style='display:flex;flex-direction:column;align-items:center;margin:1px;min-width:36px;'>",
          "<span style='font-size:8px;color:#666;'>", yr, "</span>",
          "<input type='number' data-year='", yr, "' ", value_attr, " ", placeholder_attr,
          " style='width:36px;padding:1px;border:1px solid #ccc;border-radius:3px;font-size:10px;text-align:center;'/>",
          "</div>"
        )
      })
      paste(inputs, collapse = "")
    }
    
    year_inputs_lookup <- setNames(
      lapply(unique(dat_tidy$measure), make_year_inputs),
      unique(dat_tidy$measure)
    )
    
    # Build a lookup df of submitted values so we can join onto dat_tidy
    submitted_df <- if (length(entries) > 0) {
      bind_rows(lapply(names(entries), function(m) {
        vals <- entries[[m]]
        bind_rows(lapply(names(vals), function(yr) {
          v <- vals[[yr]]
          data.frame(
            measure     = m,
            time_period = as.numeric(yr),
            submitted   = !is.null(v) && !is.na(v) && v != "",
            stringsAsFactors = FALSE
          )
        }))
      }))
    } else {
      data.frame(measure = character(), time_period = numeric(), submitted = logical())
    }
    
    html_heatmap <- dat_tidy %>%
      mutate(time_period = as.numeric(time_period)) %>%
      left_join(submitted_df, by = c("measure", "time_period")) %>%
      mutate(
        submitted = replace_na(submitted, FALSE),
        color = case_when(
          !is.na(obs_value) ~ "darkgreen",   # existing data
          submitted          ~ "#e67e22",     # user submitted
          TRUE               ~ "lightgrey"   # no data
        )
      ) %>%
      select(measure, time_period, color, cat, group) %>%
      group_by(measure) %>%
      mutate(
        boxes = paste0(
          "<div style='flex:1;height:15px;background:", color,
          ";margin:1px;border-radius:2.5px;'></div>",
          collapse = ""
        )
      ) %>%
      slice(1) %>%
      ungroup() %>%
      merge(dict %>% select(measure, label, question)) %>%
      arrange(cat) %>%
      mutate(
        needs_input = measure %in% xlsx_measures,
        safe_id     = gsub("\\.", "_", measure),
        year_inputs = unlist(year_inputs_lookup[measure]),
        
        # Row: highlight xlsx measures with orange left border + icon
        row_html = paste0(
          "<div onclick=\"togglePanel('", safe_id, "')\" style='cursor:pointer;display:flex;flex-direction:row;align-items:center;margin-bottom:1px;width:100%;padding:2px;border-radius:3px;",
          if_else(needs_input, "border-left:3px solid #e67e22;background:#fffaf5;", ""),
          "' onmouseover=\"this.style.background='#f0f0f0'\" onmouseout=\"this.style.background='", if_else(needs_input, "#fffaf5", ""), "'\">",
          
          # Label — now the first element, aligns with axis spacer
          "<div style='flex:0 0 20%;font-size:12px;padding-right:2px;text-align:right;'>", label, "</div>",
          
          # Boxes — flex:1, aligns with axis cells
          "<div style='flex:1;display:flex;flex-direction:row;'>", boxes, "</div>",
          
          # Badge on the right in a fixed-width container
          "<div style='flex:0 0 130px;text-align:left;padding-left:6px;'>",
          if_else(needs_input,
                  "<span title='New data required' style='font-size:9px;background:#e67e22;color:white;border-radius:3px;padding:1px 4px;white-space:nowrap;'>⚠ Data update requested</span>",
                  ""
          ),
          "</div>",
          "</div>",
          
          # Collapsible panel
          "<div id='panel_", safe_id, "' style='display:none;flex-direction:column;gap:12px;padding:12px;margin-bottom:8px;border:1px solid #ddd;border-radius:6px;background:#fafafa;",
          if_else(needs_input, "border-left:3px solid #e67e22;", ""), "'>",
          
          # Row 1: questions side by side
          "<div style='display:flex;flex-direction:row;gap:16px;'>",
          "<div style='flex:1;'>",
          "<strong style='font-size:13px;'>OECD Question Format</strong>",
          "<p style='font-size:12px;margin-top:6px;'>", question, "</p>",
          "</div>",
          "<div style='flex:1;'>",
          "<strong style='font-size:13px;'>Country Question Format</strong>",
          "<p style='font-size:12px;margin-top:6px;'>", question, "</p>",
          "</div>",
          "</div>",
          
          "<hr style='margin:0;border:none;border-top:1px solid #ddd;'/>",
          
          # Row 2: time series inputs
          "<div style='width:100%;'>",
          "<strong style='font-size:13px;'>Enter Data</strong>",
          if_else(needs_input,
                  "<span style='font-size:11px;color:#e67e22;margin-left:8px;'>⚠ New data required from questionnaire</span>",
                  ""
          ),
          "<div id='inputs_", safe_id, "' style='display:flex;flex-direction:row;flex-wrap:wrap;margin-top:8px;'>",
          year_inputs,
          "</div>",
          "<div style='margin-top:8px;'>",
          "<button onclick=\"submitMeasure('", safe_id, "','", measure, "')\" ",
          "style='background:#2c7bb6;color:white;border:none;padding:6px 16px;border-radius:4px;cursor:pointer;font-size:12px;'>",
          "✓ Submit</button>",
          "<span id='status_", safe_id, "' style='margin-left:10px;font-size:11px;color:green;'></span>",
          "</div>",
          "</div>",
          
          "</div>"
        )
      ) %>%
      group_by(cat, group) %>%
      summarise(
        group_html = paste(
          paste0("<h4 style='margin:12px 0 4px 0;text-align:left;'>", unique(group), "</h4>"),
          paste(row_html, collapse = ""),
          axis_row,   # <-- now appended after each group's rows
          collapse = ""
        ),
        .groups = "drop"
      ) %>%
      arrange(cat) %>%
      pull(group_html) %>%
      paste(collapse = "") 
    
    output$heatmap <- renderUI({ HTML(html_heatmap) })
  })
  
  # ── Capture submitted data ─────────────────────────────────────────────────
  observeEvent(input$submitted_data, {
    d <- input$submitted_data
    session_data$entries[[d$measure]] <- d$values
    
    # Confirm to user via JS
    runjs(paste0("
      var el = document.getElementById('status_", gsub("\\.", "_", input$submitted_data$measure), "');
      if(el) { el.innerText = '✓ Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })
  
  # ── Save session ────────────────────────────────────────────────────────────
  observeEvent(input$save_session, {
    req(input$session_name)
    dir.create("sessions", showWarnings = FALSE)
    path <- file.path("sessions", paste0(input$session_name, ".rds"))
    saveRDS(reactiveValuesToList(session_data), path)
    output$session_status <- renderUI({
      tags$span(style = "color:green;font-size:12px;", paste0("✓ Saved: ", input$session_name))
    })
  })
  
  # ── Load session ────────────────────────────────────────────────────────────
  observeEvent(input$load_session, {
    req(input$load_session_name)
    path <- file.path("sessions", paste0(input$load_session_name, ".rds"))
    if (file.exists(path)) {
      loaded <- readRDS(path)
      session_data$entries <- loaded$entries
      output$session_status <- renderUI({
        tags$span(style = "color:green;font-size:12px;", paste0("✓ Loaded: ", input$load_session_name))
      })
    } else {
      output$session_status <- renderUI({
        tags$span(style = "color:red;font-size:12px;", "✗ Session not found")
      })
    }
  })
  
  # Add here the country loaded by session
  observe({
    
    country_loaded_text <- dat %>% distinct(ref_area) %>% pull() %>% countrycode::countrycode(., "iso3c", "country.name")

    output$countryLoaded <- renderUI({ HTML(paste0("<span style='font-size:16px'><b>Current session:</b> ", country_loaded_text, "</span>")) })
    
  })
  
}

shinyApp(ui = ui, server = server)