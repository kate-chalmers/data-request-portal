source("./global.R")

ui <- navbarPage("Data request",
                 tags$head(
                   tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
                   tags$script(src = "https://cdn.jsdelivr.net/npm/echarts@5/dist/echarts.min.js"),
                   tags$script(HTML("
      // Deferred ECharts init: options stored on render, charts init when panel opens
      window.__chartOpts = window.__chartOpts || {};

      function togglePanel(id) {
        var el = document.getElementById('panel_' + id);
        var opening = el.style.display === 'none';
        el.style.display = opening ? 'flex' : 'none';
        if (opening) {
          setTimeout(function() {
            if (typeof echarts === 'undefined') return;
            el.querySelectorAll('.echart-container').forEach(function(div) {
              var inst = echarts.getInstanceByDom(div) || echarts.init(div);
              var opts = window.__chartOpts[div.id];
              if (opts) { inst.setOption(opts); inst.resize(); }
            });
          }, 80);
        }
      }

      // Collect year inputs by row key then year, send nested structure to Shiny
      function submitMeasure(safe_id, measure) {
        var container = document.getElementById('inputs_' + safe_id);
        var inputs = container.querySelectorAll('.year-input');
        var values = {};
        inputs.forEach(function(inp) {
          var row = inp.dataset.row || 'country_avg';
          var yr  = inp.dataset.year;
          if (!values[row]) values[row] = {};
          values[row][yr] = inp.value === '' ? null : parseFloat(inp.value);
        });
        Shiny.setInputValue('submitted_data', {
          measure: measure,
          values: values,
          timestamp: new Date().toISOString()
        }, {priority: 'event'});
      }

      // Collect all inputs in a time-use table and send to Shiny
      function submitTable(table_id) {
        var container = document.getElementById(table_id);
        var rows = container.querySelectorAll('tr[data-row]');
        var data = {};
        rows.forEach(function(row) {
          var r = row.dataset.row;
          data[r] = {};
          row.querySelectorAll('input').forEach(function(inp) {
            data[r]['c' + inp.dataset.col] = inp.value;
          });
        });
        Shiny.setInputValue('submitted_table', {
          table: table_id,
          data: data,
          timestamp: new Date().toISOString()
        }, {priority: 'event'});
      }

      // Collect Country Question Format text inputs and send to Shiny
      function submitResponses(measure) {
        var safe_id = measure.replace(/[^a-zA-Z0-9]/g, '_');
        var panel = document.getElementById('panel_' + safe_id);
        var inputs = panel ? panel.querySelectorAll('.resp-input') : [];
        var values = {};
        inputs.forEach(function(inp) { values[inp.dataset.idx] = inp.value; });
        Shiny.setInputValue('submitted_responses', {
          measure: measure,
          values: values,
          timestamp: new Date().toISOString()
        }, {priority: 'event'});
      }

      // Submit a free-text note for a measure
      function submitNote(safe_id, measure) {
        var el = document.getElementById('note_' + safe_id);
        Shiny.setInputValue('submitted_note', {
          measure: measure,
          note: el ? el.value : '',
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
                                     img(src="OECD_logo.svg"),
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
                 ),
                 tabPanel("Time Use",
                          fluidPage(
                            br(),
                            fluidRow(
                              column(1),
                              column(10,
                                     h3("Time Use Survey Tables"),
                                     p("For table 1 and 2 below, please provide supplemental data and information for your national time use surveys.",
                                       style = "color:#888;font-size:13px;"),
                                     br(),
                                     h4("Table 1. Time spent on daily activities (minutes)"),
                                     uiOutput("time_use_table1_ui"),
                                     br(), br(),
                                     h4("Table 2. Considering the activity coding list in the national time-use survey, please indicate which activity codes are grouped under each activity (e.g. 1.1. paid work)."),
                                     uiOutput("time_use_table2_ui")
                              ),
                              column(1)
                            )
                          )
                 )
)

server <- function(input, output, session) {
  
  # Reactive store for all submitted data: list keyed by measure
  session_data <- reactiveValues(entries = list(), notes = list(),
                                 responses = list(),
                                 time_use_1 = NULL, time_use_2 = NULL)
  
  # ── Time Use table builder ──────────────────────────────────────────────────
  # n_text_cols: number of leading columns rendered as static text (from row_text)
  # row_text:    data frame with n_rows rows supplying the static-text col values
  make_time_use_table <- function(n_rows, col_names, n_text_cols, table_id,
                                   row_text = NULL, saved = NULL) {
    n_cols <- length(col_names)
    th <- paste(sapply(col_names, function(cn) {
      paste0("<th style='font-size:11px;padding:4px 8px;border:1px solid #ddd;background:#f5f5f5;white-space:pre-wrap;'>", cn, "</th>")
    }), collapse = "")
    header <- paste0("<tr>", th, "</tr>")
    
    body <- paste(sapply(seq_len(n_rows), function(r) {
      cells <- paste(sapply(seq_len(n_cols), function(c) {
        if (c <= n_text_cols) {
          # Static text from row_text data frame
          txt <- if (!is.null(row_text) && r <= nrow(row_text)) row_text[r, c] else ""
          paste0("<td style='font-size:11px;padding:4px 6px;border:1px solid #eee;color:#333;'>", txt, "</td>")
        } else {
          saved_val <- if (!is.null(saved) && !is.null(saved[[as.character(r)]])) {
            val <- saved[[as.character(r)]][[paste0("c", c)]]
            if (!is.null(val)) val else ""
          } else ""
          paste0(
            "<td style='padding:2px;'>",
            "<input type='text' inputmode='decimal' class='tu-num year-input' data-row='", r, "' data-col='", c, "' value='", saved_val, "' ",
            "oninput=\"this.value=this.value.replace(/[^0-9.\\-]/g,'')\" ",
            "style='width:100%;min-width:60px;font-size:11px;border:1px solid #ccc;border-radius:3px;padding:2px 4px;text-align:right;'/>",
            "</td>"
          )
        }
      }), collapse = "")
      paste0("<tr data-row='", r, "'>", cells, "</tr>")
    }), collapse = "")
    
    paste0(
      "<div style='overflow-x:auto;margin-top:8px;'>",
      "<table id='", table_id, "' style='border-collapse:collapse;width:100%;'>",
      "<thead>", header, "</thead>",
      "<tbody>", body, "</tbody>",
      "</table></div>",
      "<div style='margin-top:8px;'>",
      "<button onclick=\"submitTable('", table_id, "')\" ",
      "style='background:#2c7bb6;color:white;border:none;padding:6px 16px;border-radius:4px;cursor:pointer;font-size:12px;'>",
      "&#10003; Submit table</button>",
      "<span id='status_", table_id, "' style='margin-left:10px;font-size:11px;color:green;'></span>",
      "</div>"
    )
  }
  
  # ── Response-format HTML builder ────────────────────────────────────────────
  # Produces OECD label table HTML and Country question HTML for one xlsx measure.
  # saved_resp: named list (idx → text) of previously submitted text answers.
  build_response_html <- function(resp, saved_resp = NULL) {
    # OECD format: render $label tibble as a compact label–value table
    oecd_rows <- paste(mapply(function(lbl, val) {
      paste0(
        "<tr>",
        "<td style='font-size:11px;font-weight:600;color:#555;padding:3px 8px 3px 0;vertical-align:top;white-space:nowrap;'>", lbl, "</td>",
        "<td style='font-size:11px;padding:3px 0;color:#333;'>", if (is.na(val)) "—" else val, "</td>",
        "</tr>"
      )
    }, resp$label$label, resp$label$response, SIMPLIFY = TRUE), collapse = "")
    oecd_html <- paste0("<table style='width:100%;border-collapse:collapse;'>", oecd_rows, "</table>")
    
    # Country format: $response tibble — NA rows become text inputs
    safe_indic <- gsub("\\.", "_", resp$indic)
    country_rows <- paste(sapply(seq_len(nrow(resp$response)), function(i) {
      q_lbl   <- resp$response$label[i]
      q_val   <- resp$response$response[i]
      inp_id  <- paste0("resp_", safe_indic, "_", i)
      pre_val <- if (!is.null(saved_resp) && !is.null(saved_resp[[as.character(i)]])) {
        saved_resp[[as.character(i)]]
      } else if (!is.na(q_val)) q_val else ""
      
      if (is.na(q_val)) {
        paste0(
          "<div style='margin-bottom:8px;'>",
          "<p style='font-size:11px;font-weight:600;color:#444;margin:0 0 3px;'>", q_lbl, "</p>",
          "<input type='text' id='", inp_id, "' class='resp-input' data-idx='", i, "' ",
          "value='", pre_val, "' placeholder='Enter response\u2026' ",
          "style='width:100%;font-size:11px;border:1px solid #ccc;border-radius:3px;padding:4px 6px;box-sizing:border-box;'/>",
          "</div>"
        )
      } else {
        paste0(
          "<div style='margin-bottom:6px;'>",
          "<span style='font-size:11px;font-weight:600;color:#444;'>", q_lbl, ":</span> ",
          "<span style='font-size:11px;color:#333;'>", q_val, "</span>",
          "</div>"
        )
      }
    }), collapse = "")
    
    country_html <- paste0(
      country_rows,
      "<div style='margin-top:10px;'>",
      "<button onclick=\"submitResponses('", resp$indic, "')\" ",
      "style='background:#2c7bb6;color:white;border:none;padding:5px 14px;border-radius:4px;cursor:pointer;font-size:12px;'>",
      "&#10003; Submit responses</button>",
      "<span id='resp_status_", safe_indic, "' style='margin-left:10px;font-size:11px;color:green;'></span>",
      "</div>"
    )
    
    list(oecd = oecd_html, country = country_html)
  }
  
  # Index xlsx_response_format by indic for fast lookup
  resp_by_indic <- setNames(xlsx_response_format, sapply(xlsx_response_format, `[[`, "indic"))
  
  # ── Non-reactive lookups (computed once per session) ────────────────────────
  val_lookup <- dat %>%
    select(measure, time_period, obs_value) %>%
    mutate(time_period = as.numeric(time_period))

  # Inline ECharts: store options in window.__chartOpts via <script>; charts are
  # initialised lazily by togglePanel() after the panel becomes visible.
  make_year_chart <- function(m) {
    all_years <- 2004:2026
    safe      <- gsub("\\.", "_", m)
    cv        <- val_lookup %>% filter(measure == m)

    country_vals <- sapply(all_years, function(yr) {
      row <- cv %>% filter(time_period == yr)
      if (nrow(row) > 0 && !is.na(row$obs_value[1])) as.character(round(row$obs_value[1], 4)) else "null"
    })

    has_oecd  <- !is.null(oecd_avg)
    oecd_ser  <- if (has_oecd) {
      ov <- oecd_avg %>% filter(measure == m)
      oecd_vals <- sapply(all_years, function(yr) {
        row <- ov %>% filter(time_period == yr)
        if (nrow(row) > 0 && !is.na(row$obs_value[1])) as.character(round(row$obs_value[1], 4)) else "null"
      })
      paste0(",{name:'OECD average',type:'line',data:[", paste(oecd_vals, collapse=","), "],",
             "connectNulls:true,itemStyle:{color:'#e67e22'},lineStyle:{type:'dashed',width:1.5},symbolSize:5}")
    } else ""

    paste0(
      # Container — full panel width minus grid margins
      "<div id='echart_", safe, "' class='echart-container' ",
      "style='width:100%;height:210px;margin-top:8px;'></div>",
      # Store options; chart initialised by togglePanel when panel opens
      "<script>",
      "window.__chartOpts=window.__chartOpts||{};",
      "window.__chartOpts['echart_", safe, "']={",
      "  grid:{left:55,right:16,top:22,bottom:28,containLabel:true},",
      "  tooltip:{trigger:'axis'},",
      "  legend:{show:", if (has_oecd) "true" else "false", ",top:2,right:16,textStyle:{fontSize:10}},",
      "  xAxis:{type:'category',data:[\"", paste(all_years, collapse='","'), "\"],",
      "    axisLabel:{fontSize:9,interval:3}},",
      "  yAxis:{type:'value',axisLabel:{fontSize:9},splitLine:{lineStyle:{color:'#eee'}}},",
      "  series:[{name:'Country',type:'line',data:[", paste(country_vals, collapse=","), "],",
      "    connectNulls:true,itemStyle:{color:'#2c7bb6'},lineStyle:{width:2},symbolSize:6}",
      oecd_ser, "]};",
      "</script>"
    )
  }

  year_charts_lookup <- setNames(
    lapply(unique(measure_list$measure), function(m) {
      if (m %in% xlsx_measures || m %in% time_use_measures) return("")
      make_year_chart(m)
    }),
    unique(measure_list$measure)
  )
  
  # ── Time Use tables ─────────────────────────────────────────────────────────
  output$time_use_table1_ui <- renderUI({
    HTML(make_time_use_table(31, time_use_col_names_1, 2, "tu_table1",
                              row_text = time_use_row_text_1,
                              saved    = session_data$time_use_1))
  })
  output$time_use_table2_ui <- renderUI({
    HTML(make_time_use_table(30, time_use_col_names_2, 2, "tu_table2",
                              row_text = time_use_row_text_2,
                              saved    = session_data$time_use_2))
  })
  
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
    
    # Icon filenames for each dimension group
    group_icons <- c(
      "Income and wealth"     = "income and wealth.png",
      "Work and job quality"  = "work and job quality.png",
      "Housing"               = "housing.png",
      "Work-life balance"     = "worklife balance.png",
      "Health"                = "health.png",
      "Knowledge and skills"  = "knowledge and skills.png",
      "Social connections"    = "social connections.png",
      "Civic engagement"      = "civic engagement.png",
      "Environmental quality" = "environmental quality.png",
      "Safety"                = "safety.png",
      "Subjective well-being" = "subjective wellbeing.png",
      "Natural capital"       = "natural capital.png",
      "Human capital"         = "human capital.png",
      "Social capital"        = "social capital.png",
      "Economic capital"      = "economic capital.png"
    )

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
    
    # Row-label definitions per measure type
    row_defs <- function(m) {
      if (m %in% all_rows) {
        list(
          list(key = "country_avg",  label = "Country average",           bold = TRUE),
          list(key = "male",         label = "Male",                      bold = FALSE),
          list(key = "female",       label = "Female",                    bold = FALSE),
          list(key = "young",        label = "Young",                     bold = FALSE),
          list(key = "middle_aged",  label = "Middle-aged",               bold = FALSE),
          list(key = "old",          label = "Old",                       bold = FALSE),
          list(key = "primary",      label = "Primary (ISCED 0-2)",       bold = FALSE),
          list(key = "secondary",    label = "Secondary (ISCED 3-4)",     bold = FALSE),
          list(key = "tertiary",     label = "Tertiary (ISCED 5-8)",      bold = FALSE)
        )
      }  else if(m %in% all_rows_dep_vert) {
        list(
          list(key = "country_avg",  label = "Country average",           bold = TRUE),
          list(key = "vert",         label = "Vertical inequality",       bold = FALSE),
          list(key = "dep",          label = "Deprivation",               bold = FALSE),
          list(key = "male",         label = "Male",                      bold = FALSE),
          list(key = "female",       label = "Female",                    bold = FALSE),
          list(key = "young",        label = "Young",                     bold = FALSE),
          list(key = "middle_aged",  label = "Middle-aged",               bold = FALSE),
          list(key = "old",          label = "Old",                       bold = FALSE),
          list(key = "primary",      label = "Primary (ISCED 0-2)",       bold = FALSE),
          list(key = "secondary",    label = "Secondary (ISCED 3-4)",     bold = FALSE),
          list(key = "tertiary",     label = "Tertiary (ISCED 5-8)",      bold = FALSE)
        )
    }else if (m %in% gender_only) {
        list(
          list(key = "country_avg",  label = "Country average",           bold = TRUE),
          list(key = "male",         label = "Male",                      bold = FALSE),
          list(key = "female",       label = "Female",                    bold = FALSE)
        )
      } else {
        list(list(key = "country_avg", label = "Country average",         bold = TRUE))
      }
    }

    make_year_inputs <- function(m) {
      vals  <- val_lookup %>% filter(measure == m)
      saved <- session_data$entries[[m]]
      rows  <- row_defs(m)

      # Year header
      yr_header <- paste(sapply(years, function(yr) {
        paste0("<div style='flex:1;text-align:center;font-size:8px;color:#888;min-width:32px;'>", yr, "</div>")
      }), collapse = "")

      header_html <- paste0(
        "<div style='display:flex;align-items:center;margin-bottom:2px;'>",
        "<div style='flex:0 0 150px;'></div>",
        "<div style='flex:1;display:flex;'>", yr_header, "</div>",
        "</div>"
      )

      row_htmls <- sapply(rows, function(r) {
        inputs <- sapply(years, function(yr) {
          v <- if (!is.null(saved) && !is.null(saved[[r$key]]) &&
                   !is.null(saved[[r$key]][[as.character(yr)]])) {
            saved[[r$key]][[as.character(yr)]]
          } else if (r$key == "country_avg") {
            existing <- vals %>% filter(time_period == yr)
            if (nrow(existing) > 0 && !is.na(existing$obs_value[1])) existing$obs_value[1] else NA
          } else NA

          has_val          <- !is.na(v)
          value_attr       <- if (has_val) paste0("value='", v, "'") else ""
          placeholder_attr <- if (!has_val) "placeholder='-'" else ""

          paste0(
            "<div style='flex:1;min-width:32px;padding:1px;'>",
            "<input type='text' inputmode='decimal' class='year-input' ",
            "data-row='", r$key, "' data-year='", yr, "' ",
            value_attr, " ", placeholder_attr,
            " oninput=\"this.value=this.value.replace(/[^0-9.\\-]/g,'')\"",
            " style='width:100%;padding:1px;border:1px solid #ccc;border-radius:3px;",
            "font-size:10px;text-align:center;'/>",
            "</div>"
          )
        })

        paste0(
          "<div style='display:flex;align-items:center;margin-bottom:2px;'>",
          "<div style='flex:0 0 150px;font-size:11px;color:#444;padding-right:6px;text-align:right;",
          if (r$bold) "font-weight:600;" else "", "'>", r$label, "</div>",
          "<div style='flex:1;display:flex;'>", paste(inputs, collapse = ""), "</div>",
          "</div>"
        )
      })

      paste0(
        "<div style='overflow-x:auto;margin-top:6px;'>",
        header_html,
        paste(row_htmls, collapse = ""),
        "</div>"
      )
    }
    
    year_inputs_lookup <- setNames(
      lapply(unique(dat_tidy$measure), make_year_inputs),
      unique(dat_tidy$measure)
    )
    
    # Build response HTML for xlsx measures (uses session_data$responses for pre-fill)
    response_html_lookup <- setNames(
      lapply(unique(dat_tidy$measure), function(m) {
        if (!m %in% xlsx_measures || is.null(resp_by_indic[[m]])) return(list(oecd = "", country = ""))
        build_response_html(resp_by_indic[[m]], session_data$responses[[m]])
      }),
      unique(dat_tidy$measure)
    )
    
    # Build submitted_df from the country_avg row of the nested entries structure
    submitted_df <- if (length(entries) > 0) {
      bind_rows(lapply(names(entries), function(m) {
        row_data <- entries[[m]][["country_avg"]]
        if (is.null(row_data)) return(data.frame())
        bind_rows(lapply(names(row_data), function(yr) {
          v <- row_data[[yr]]
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
      left_join(defs_lookup, by = "measure") %>%
      arrange(cat) %>%
      mutate(
        needs_input    = measure %in% xlsx_measures,
        is_time_use    = measure %in% time_use_measures,
        safe_id        = gsub("\\.", "_", measure),
        year_inputs    = unlist(year_inputs_lookup[measure]),
        year_chart     = unlist(year_charts_lookup[measure]),
        oecd_q_html    = sapply(measure, function(m) response_html_lookup[[m]]$oecd),
        country_q_html = sapply(measure, function(m) response_html_lookup[[m]]$country),
        def_text       = replace_na(definition, "Definition to be added."),
        tech_name      = replace_na(indicator,  "—"),
        unit_text      = replace_na(unit,       "—"),
        
        # Per-type row styling
        row_border  = case_when(
          needs_input ~ "border-left:3px solid #e67e22;background:#fffaf5;",
          is_time_use ~ "border-left:3px solid #2c7bb6;background:#f5f8ff;",
          TRUE        ~ ""
        ),
        row_hover   = case_when(
          needs_input ~ "#fffaf5",
          is_time_use ~ "#f5f8ff",
          TRUE        ~ ""
        ),
        badge_html  = case_when(
          needs_input ~ "<span title='New data required' style='font-size:9px;background:#e67e22;color:white;border-radius:3px;padding:1px 4px;white-space:nowrap;'>&#9888; Data update requested</span>",
          is_time_use ~ "<span title='Time use tables' style='font-size:9px;background:#2c7bb6;color:white;border-radius:3px;padding:1px 4px;white-space:nowrap;'>&#128203; Time use tables</span>",
          TRUE        ~ ""
        ),
        panel_border = case_when(
          needs_input ~ "border-left:3px solid #e67e22;",
          is_time_use ~ "border-left:3px solid #2c7bb6;",
          TRUE        ~ ""
        ),
        
        # 3-way panel body built row-by-row via mapply
        panel_body = mapply(function(ni, itu, sid, mn, yi, yc, q, oqh, cqh, def, tech, unt, lbl) {
          if (ni) {
            # ── xlsx: two-column question layout + editable inputs ────────────
            paste0(
              "<div style='display:flex;flex-direction:row;gap:16px;'>",
              "<div style='flex:1;overflow:auto;'><strong style='font-size:13px;'>OECD Question Format</strong>",
              "<div style='margin-top:6px;'>", oqh, "</div></div>",
              "<div style='flex:1;overflow:auto;'><strong style='font-size:13px;'>Country Question Format</strong>",
              "<div style='margin-top:6px;'>", cqh, "</div></div>",
              "</div>",
              "<hr style='margin:0;border:none;border-top:1px solid #ddd;'/>",
              "<div style='width:100%;'>",
              "<strong style='font-size:13px;'>Enter Data</strong>",
              "<span style='font-size:11px;color:#e67e22;margin-left:8px;'>&#9888; New data required from questionnaire</span>",
              "<div id='inputs_", sid, "' style='display:flex;flex-direction:row;flex-wrap:wrap;margin-top:8px;'>", yi, "</div>",
              "<div style='margin-top:8px;'>",
              "<button onclick=\"submitMeasure('", sid, "','", mn, "')\" ",
              "style='background:#2c7bb6;color:white;border:none;padding:6px 16px;border-radius:4px;cursor:pointer;font-size:12px;'>",
              "&#10003; Submit</button>",
              "<span id='status_", sid, "' style='margin-left:10px;font-size:11px;color:green;'></span>",
              "</div></div>"
            )
          } else if (itu) {
            # ── time use: redirect to Time Use tab ───────────────────────────
            paste0(
              "<div style='padding:28px;text-align:center;'>",
              "<p style='font-size:26px;margin:0;'>&#128203;</p>",
              "<p style='font-size:13px;margin-top:10px;'>",
              "This indicator is covered by the <strong>Time Use Survey tables</strong>.</p>",
              "<p style='font-size:12px;color:#666;margin-top:4px;'>",
              "Please navigate to the <strong>Time Use</strong> tab to complete the relevant tables.</p>",
              "</div>"
            )
          } else {
            # ── read-only: metadata + echart + note ──────────────────────────
            paste0(
              "<div style='width:100%;text-align:left;'>",
              "<span style='font-size:13px;color:#555;'>", lbl, "</span></div>",
              "<div><span style='font-size:11px;font-weight:600;'>Technical name: </span>",
              "<span style='font-size:13px;color:#555;'>", tech, "</span></div>",
              "<div><span style='font-size:11px;font-weight:600;'>Unit: </span>",
              "<span style='font-size:13px;color:#555;'>", unt, "</span></div>",
              "<strong style='font-size:13px;'>Definition</strong>",
              "<p style='font-size:12px;color:#333;margin-top:4px;'>", def, "</p>",
              "<div style='margin-top:10px;display:flex;flex-direction:column;gap:5px;'>",
              "<div><span style='font-size:11px;font-weight:600;'>Label: </span>",
              "</div></div>",
              "<hr style='margin:4px 0;border:none;border-top:1px solid #ddd;'/>",
              "<div style='width:100%;'><strong style='font-size:13px;'>Time Series</strong>",
              yc, "</div>",
              "<hr style='margin:4px 0;border:none;border-top:1px solid #ddd;'/>",
              "<div style='width:100%;'>",
              "<strong style='font-size:13px;'>Note</strong>",
              "<p style='font-size:11px;color:#888;margin:4px 0 6px;'>Add any relevant context, caveats, or source notes.</p>",
              "<textarea id='note_", sid, "' rows='3' ",
              "style='width:100%;font-size:12px;border:1px solid #ccc;border-radius:4px;padding:6px;box-sizing:border-box;resize:vertical;'></textarea>",
              "<div style='margin-top:6px;'>",
              "<button onclick=\"submitNote('", sid, "','", mn, "')\" ",
              "style='background:#2c7bb6;color:white;border:none;padding:5px 14px;border-radius:4px;cursor:pointer;font-size:12px;'>",
              "&#10003; Submit note</button>",
              "<span id='note_status_", sid, "' style='margin-left:10px;font-size:11px;color:green;'></span>",
              "</div></div>"
            )
          }
        }, needs_input, is_time_use, safe_id, measure, year_inputs, year_chart, question,
        oecd_q_html, country_q_html, def_text, tech_name, unit_text, label,
        SIMPLIFY = TRUE, USE.NAMES = FALSE),
        
        row_html = paste0(
          "<div onclick=\"togglePanel('", safe_id, "')\" ",
          "style='cursor:pointer;display:flex;flex-direction:row;align-items:center;margin-bottom:1px;width:100%;padding:2px;border-radius:3px;",
          row_border,
          "' onmouseover=\"this.style.background='#f0f0f0'\" onmouseout=\"this.style.background='", row_hover, "'\">",
          "<div style='flex:0 0 20%;font-size:12px;padding-right:2px;text-align:right;'>", label, "</div>",
          "<div style='flex:1;display:flex;flex-direction:row;'>", boxes, "</div>",
          "<div style='flex:0 0 130px;text-align:left;padding-left:6px;'>", badge_html, "</div>",
          "</div>",
          "<div id='panel_", safe_id, "' style='display:none;flex-direction:column;gap:12px;padding:12px;margin-bottom:8px;border:1px solid #ddd;border-radius:6px;background:#fafafa;",
          panel_border, "'>",
          panel_body,
          "</div>"
        )
      ) %>%
      group_by(cat, group) %>%
      summarise(
        group_html = paste(
          {
            grp      <- unique(group)
            icon_src <- group_icons[grp]
            icon_tag <- if (!is.na(icon_src))
              paste0("<img src='", icon_src, "' style='height:22px;width:22px;margin-right:7px;vertical-align:middle;object-fit:contain;'/>")
            else ""
            paste0("<h4 style='margin:16px 0 4px 0;text-align:left;display:flex;align-items:center;'>",
                   icon_tag, grp, "</h4>")
          },
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
  
  # ── Capture submitted notes ────────────────────────────────────────────────
  observeEvent(input$submitted_note, {
    d <- input$submitted_note
    session_data$notes[[d$measure]] <- d$note
    
    runjs(paste0("
      var el = document.getElementById('note_status_", gsub("\\.", "_", input$submitted_note$measure), "');
      if(el) { el.innerText = '✓ Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })
  
  # ── Capture submitted questionnaire responses ──────────────────────────────
  observeEvent(input$submitted_responses, {
    d <- input$submitted_responses
    session_data$responses[[d$measure]] <- d$values
    safe <- gsub("\\.", "_", d$measure)
    runjs(paste0("
      var el = document.getElementById('resp_status_", safe, "');
      if(el) { el.innerText = '\\u2713 Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })
  
  # ── Capture submitted time-use table data ──────────────────────────────────
  observeEvent(input$submitted_table, {
    d <- input$submitted_table
    if (d$table == "tu_table1") {
      session_data$time_use_1 <- d$data
    } else {
      session_data$time_use_2 <- d$data
    }
    runjs(paste0("
      var el = document.getElementById('status_", d$table, "');
      if(el) { el.innerText = '\\u2713 Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
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
      session_data$entries    <- loaded$entries
      session_data$notes      <- if (!is.null(loaded$notes))      loaded$notes      else list()
      session_data$responses  <- if (!is.null(loaded$responses))  loaded$responses  else list()
      session_data$time_use_1 <- if (!is.null(loaded$time_use_1)) loaded$time_use_1 else NULL
      session_data$time_use_2 <- if (!is.null(loaded$time_use_2)) loaded$time_use_2 else NULL
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