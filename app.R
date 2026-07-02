source("./global.R")

# ── Shared head & JS ─────────────────────────────────────────────────────────
shared_head <- tagList(
  useShinyjs(),
  tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
  tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
  tags$script(src = "https://cdn.jsdelivr.net/npm/echarts@5/dist/echarts.min.js"),
  tags$script(HTML("
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
      Shiny.setInputValue('submitted_data',
        { measure: measure, values: values, timestamp: new Date().toISOString() },
        {priority: 'event'});
    }

    function submitTable(table_id) {
      var container = document.getElementById(table_id);
      var rows = container.querySelectorAll('tr[data-row]');
      var data = {};
      rows.forEach(function(row) {
        var r = row.dataset.row; data[r] = {};
        row.querySelectorAll('input').forEach(function(inp) {
          data[r]['c' + inp.dataset.col] = inp.value;
        });
      });
      Shiny.setInputValue('submitted_table',
        { table: table_id, data: data, timestamp: new Date().toISOString() },
        {priority: 'event'});
    }

    function submitResponses(measure) {
      var safe_id = measure.replace(/[^a-zA-Z0-9]/g, '_');
      var panel = document.getElementById('panel_' + safe_id);
      var inputs = panel ? panel.querySelectorAll('.resp-input') : [];
      var values = {};
      inputs.forEach(function(inp) { values[inp.dataset.idx] = inp.value; });
      Shiny.setInputValue('submitted_responses',
        { measure: measure, values: values, timestamp: new Date().toISOString() },
        {priority: 'event'});
    }

    function submitNote(safe_id, measure) {
      var el = document.getElementById('note_' + safe_id);
      Shiny.setInputValue('submitted_note',
        { measure: measure, note: el ? el.value : '', timestamp: new Date().toISOString() },
        {priority: 'event'});
    }
  "))
)

# ── Reusable legend ───────────────────────────────────────────────────────────
heatmap_legend <- tags$div(
  style = "display:flex;gap:20px;font-size:11px;margin-bottom:10px;justify-content:center;align-items:center;color:#55606B;",
  tags$span(
    tags$span(style = "display:inline-block;width:11px;height:11px;background:#1F7A4D;border-radius:2px;margin-right:5px;vertical-align:middle;"),
    "Existing data"
  ),
  tags$span(
    tags$span(style = "display:inline-block;width:11px;height:11px;background:#F89C1C;border-radius:2px;margin-right:5px;vertical-align:middle;"),
    "Submitted this session"
  ),
  tags$span(
    tags$span(style = "display:inline-block;width:11px;height:11px;background:#D9DDE3;border:1px solid #c5cad0;border-radius:2px;margin-right:5px;vertical-align:middle;"),
    "No data"
  )
)

# ── Country choices ────────────────────────────────────────────────────────────
login_country_choices <- c(
  "\u2014 Select your country \u2014" = "",
  sort(setNames(
    oecd_countries,
    countrycode::countrycode(oecd_countries, "iso3c", "country.name", warn = FALSE)
  ))
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- tagList(
  shared_head,

  # ── Login screen ──────────────────────────────────────────────────────────
  tags$div(
    id = "login_screen",
    style = paste0(
      "position:fixed;top:0;left:0;width:100%;height:100%;z-index:9999;",
      "background:linear-gradient(150deg,#001f6e 0%,#003189 55%,#0055a0 100%);",
      "display:flex;align-items:center;justify-content:center;"
    ),
    tags$div(
      style = paste0(
        "background:#fff;border-radius:14px;padding:44px 48px 36px;",
        "width:420px;max-width:92vw;",
        "box-shadow:0 16px 48px rgba(0,0,0,0.28);"
      ),
      tags$div(
        style = "display:flex;align-items:center;justify-content:center;gap:28px;margin-bottom:26px;",
        img(src = "wise_logo.png", height = 48),
        img(src = "OECD_logo.svg", height = 34)
      ),
      tags$h4(
        style = "text-align:center;font-size:17px;color:#1F2B3A;font-weight:700;margin:0 0 6px;",
        "How\u2019s Life? Data Request Portal"
      ),
      tags$p(
        style = "text-align:center;font-size:12px;color:#55606B;margin:0 0 26px;line-height:1.5;",
        "Select your country and enter the access password."
      ),
      tags$div(
        style = "margin-bottom:14px;",
        tags$label("Country",
                   style = "font-size:11px;font-weight:600;color:#55606B;display:block;margin-bottom:5px;"),
        selectInput("login_country", NULL, choices = login_country_choices, width = "100%")
      ),
      tags$div(
        style = "margin-bottom:22px;",
        tags$label("Password",
                   style = "font-size:11px;font-weight:600;color:#55606B;display:block;margin-bottom:5px;"),
        passwordInput("login_password", NULL, width = "100%", placeholder = "Enter access password")
      ),
      actionButton("login_btn", "Enter \u2192",
                   class  = "btn-primary",
                   style  = "width:100%;font-size:14px;padding:10px 0;font-weight:700;"),
      tags$div(style = "margin-top:12px;min-height:20px;text-align:center;",
               uiOutput("login_error"))
    )
  ),

  # ── Main app (hidden until login) ─────────────────────────────────────────
  shinyjs::hidden(
    tags$div(
      id = "main_app",
      navbarPage(
        title = uiOutput("nav_title", inline = TRUE),
        id    = "main_navbar",

        # ── Tab 1: Data Submissions ────────────────────────────────────────
        tabPanel("Data Submissions",
          fluidPage(
            fluidRow(
              column(1),
              column(10,
                tags$div(class = "landing-hero",
                  tags$div(class = "landing-logo-row",
                    img(src = "wise_logo.png", height = 60),
                    img(src = "OECD_logo.svg", height = 36)
                  ),
                  tags$p(
                    "Please complete the data fields below for each indicator requiring national input.
                     Fields are pre-filled where existing data is available in the OECD",
                    tags$em("How\u2019s Life? Well-being Database."),
                    "Submit each section using the button provided."
                  )
                )
              ),
              column(1)
            ),
            br(),
            fluidRow(
              column(1),
              column(10, align = "center",
                tags$div(class = "heatmap-content",
                  heatmap_legend,
                  uiOutput("heatmap_submissions")
                )
              ),
              column(1)
            )
          )
        ),

        # ── Tab 2: Well-being Data Coverage ───────────────────────────────
        tabPanel("Well-being Data Coverage",
          fluidPage(
            fluidRow(
              column(1),
              column(10,
                tags$div(class = "landing-hero",
                  tags$p(
                    style = "margin:0;",
                    "Overview of all well-being indicators in the OECD",
                    tags$em("How\u2019s Life? Well-being Database"),
                    "and the current data coverage for your country.
                     Click any indicator to view its time series."
                  )
                )
              ),
              column(1)
            ),
            br(),
            fluidRow(
              column(1),
              column(10, align = "center",
                tags$div(class = "heatmap-content",
                  heatmap_legend,
                  uiOutput("heatmap_coverage")
                )
              ),
              column(1)
            )
          )
        ),

        # ── Tab 3: Time Use ────────────────────────────────────────────────
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
                fluidRow(
                  column(5,
                    tags$label("Survey name",
                               style = "font-size:13px;font-weight:600;display:block;margin-bottom:4px;"),
                    textInput("tu_survey_name", label = NULL,
                              placeholder = "e.g. Time Use Survey 2024", width = "100%")
                  ),
                  column(3,
                    tags$label("Latest survey year",
                               style = "font-size:13px;font-weight:600;display:block;margin-bottom:4px;"),
                    numericInput("tu_survey_year", label = NULL,
                                 value = NA, min = 1990, max = 2035, width = "100%")
                  )
                ),
                br(),
                h4(HTML("Table 1. Time spent on daily activities (<u>minutes</u>)")),
                uiOutput("time_use_table1_ui"),
                br(), br(),
                h4("Table 2. Considering the activity coding list in the national time-use survey, please indicate which activity codes are grouped under each activity (e.g. 1.1. paid work)."),
                uiOutput("time_use_table2_ui")
              ),
              column(1)
            )
          )
        )
      ) # end navbarPage
    )   # end main_app div
  )     # end hidden
)       # end tagList

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Authentication ──────────────────────────────────────────────────────────
  credentials <- reactiveValues(authenticated = FALSE, country = NULL, country_name = NULL)
  dat_rv      <- reactiveVal(NULL)

  output$nav_title <- renderUI({
    if (!credentials$authenticated) return(tags$span("How\u2019s Life?"))
    tags$span(
      "How\u2019s Life?",
      tags$span(
        style = "font-size:11px;font-weight:400;background:rgba(255,255,255,0.18);padding:2px 10px;border-radius:12px;margin-left:12px;",
        credentials$country_name
      )
    )
  })

  observeEvent(input$login_btn, {
    req(input$login_country, input$login_password)
    if (!nzchar(input$login_country)) {
      output$login_error <- renderUI(
        tags$p(style = "color:#E63312;font-size:12px;margin:0;", "Please select a country.")
      )
      return()
    }
    if (input$login_password != "oecd2026") {
      output$login_error <- renderUI(
        tags$p(style = "color:#E63312;font-size:12px;margin:0;", "Incorrect password. Please try again.")
      )
      return()
    }
    # Successful login
    iso   <- input$login_country
    cname <- countrycode::countrycode(iso, "iso3c", "country.name", warn = FALSE)
    credentials$authenticated <- TRUE
    credentials$country       <- iso
    credentials$country_name  <- cname

    # Filter dataset to this country
    dat_rv(dat %>% filter(ref_area == iso))

    # Auto-load country session if exists
    path <- file.path("sessions", paste0(iso, ".rds"))
    if (file.exists(path)) {
      loaded <- tryCatch(readRDS(path), error = function(e) NULL)
      if (!is.null(loaded)) {
        session_data$entries    <- loaded$entries    %||% list()
        session_data$notes      <- loaded$notes      %||% list()
        session_data$responses  <- loaded$responses  %||% list()
        session_data$time_use_1 <- loaded$time_use_1
        session_data$time_use_2 <- loaded$time_use_2
        if (!is.null(loaded$tu_survey_name))
          updateTextInput(session,   "tu_survey_name", value = loaded$tu_survey_name)
        if (!is.null(loaded$tu_survey_year))
          updateNumericInput(session, "tu_survey_year", value = loaded$tu_survey_year)
      }
    }

    shinyjs::hide("login_screen")
    shinyjs::show("main_app")
  })

  # ── Session data ─────────────────────────────────────────────────────────────
  session_data <- reactiveValues(
    entries    = list(),
    notes      = list(),
    responses  = list(),
    time_use_1 = NULL,
    time_use_2 = NULL
  )

  # Auto-save to sessions/{iso}.rds whenever any data changes
  observe({
    req(credentials$authenticated, credentials$country)
    # Touch all fields to create reactive dependencies
    list(session_data$entries, session_data$notes, session_data$responses,
         session_data$time_use_1, session_data$time_use_2)
    dir.create("sessions", showWarnings = FALSE)
    saveRDS(
      c(reactiveValuesToList(session_data),
        list(tu_survey_name = isolate(input$tu_survey_name),
             tu_survey_year = isolate(input$tu_survey_year))),
      file.path("sessions", paste0(credentials$country, ".rds"))
    )
  })

  # ── Helper: null coalescing ──────────────────────────────────────────────────
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # ── Time Use table builder ───────────────────────────────────────────────────
  make_time_use_table <- function(n_rows, col_names, n_text_cols, table_id,
                                   row_text = NULL, saved = NULL) {
    n_cols <- length(col_names)
    th <- paste(sapply(col_names, function(cn) {
      paste0("<th style='font-size:11px;padding:4px 8px;border:1px solid #ddd;background:#f5f5f5;white-space:pre-wrap;'>", cn, "</th>")
    }), collapse = "")
    header <- paste0("<tr>", th, "</tr>")

    body <- paste(sapply(seq_len(n_rows), function(r) {
      code_val   <- if (!is.null(row_text) && r <= nrow(row_text)) trimws(row_text[r, 1]) else ""
      is_divider <- grepl("\\.0$", code_val)

      if (is_divider) {
        txt2          <- if (!is.null(row_text) && r <= nrow(row_text) && n_text_cols >= 2) row_text[r, 2] else ""
        divider_label <- if (nchar(txt2) > 0) paste0(code_val, " \u2014 ", txt2) else code_val
        td_style      <- "font-size:11px;font-weight:600;color:white;padding:6px 10px;border:1px solid #0f2843;background:#003189;"
        paste0("<tr data-row='", r, "' style='background:#003189;'>",
               "<td colspan='", n_cols, "' style='", td_style, "'>", divider_label, "</td></tr>")
      } else {
        cells <- paste(sapply(seq_len(n_cols), function(c) {
          if (c <= n_text_cols) {
            txt <- if (!is.null(row_text) && r <= nrow(row_text)) row_text[r, c] else ""
            paste0("<td style='font-size:11px;padding:4px 6px;border:1px solid #eee;color:#333;'>", txt, "</td>")
          } else {
            saved_val <- if (!is.null(saved) && !is.null(saved[[as.character(r)]])) {
              val <- saved[[as.character(r)]][[paste0("c", c)]]
              if (!is.null(val)) val else ""
            } else ""
            paste0("<td style='padding:2px;'>",
                   "<input type='text' inputmode='decimal' class='tu-num year-input' ",
                   "data-row='", r, "' data-col='", c, "' value='", saved_val, "' ",
                   "oninput=\"this.value=this.value.replace(/[^0-9.\\-]/g,'')\" ",
                   "style='width:100%;min-width:60px;font-size:11px;border:1px solid #ccc;",
                   "border-radius:3px;padding:2px 4px;text-align:center;'/>",
                   "</td>")
          }
        }), collapse = "")
        paste0("<tr data-row='", r, "'>", cells, "</tr>")
      }
    }), collapse = "")

    paste0(
      "<div class='tu-table-wrap'>",
      "<div style='overflow-x:auto;margin-top:8px;'>",
      "<table id='", table_id, "' style='border-collapse:collapse;width:100%;'>",
      "<thead>", header, "</thead><tbody>", body, "</tbody>",
      "</table></div>",
      "<div style='margin-top:10px;'>",
      "<button onclick=\"submitTable('", table_id, "')\" ",
      "style='background:#009EDB;color:white;border:none;padding:6px 16px;border-radius:5px;cursor:pointer;font-size:12px;font-weight:600;'>",
      "&#10003; Submit table</button>",
      "<span id='status_", table_id, "' style='margin-left:10px;font-size:11px;color:#1F7A4D;font-weight:600;'></span>",
      "</div></div>"
    )
  }

  # ── Time Use table outputs ───────────────────────────────────────────────────
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

  # ── Response-format HTML builder ─────────────────────────────────────────────
  build_response_html <- function(resp, saved_resp = NULL) {
    oecd_rows <- paste(mapply(function(lbl, val) {
      paste0("<tr>",
             "<td style='font-size:11px;font-weight:600;color:#555;padding:3px 8px 3px 0;vertical-align:top;white-space:nowrap;'>", lbl, "</td>",
             "<td style='font-size:11px;padding:3px 0;color:#333;'>", if (is.na(val)) "\u2014" else val, "</td>",
             "</tr>")
    }, resp$label$label, resp$label$response, SIMPLIFY = TRUE), collapse = "")
    oecd_html <- paste0("<table style='width:100%;border-collapse:collapse;'>", oecd_rows, "</table>")

    safe_indic    <- gsub("\\.", "_", resp$indic)
    country_rows  <- paste(sapply(seq_len(nrow(resp$response)), function(i) {
      q_lbl   <- resp$response$label[i]
      q_val   <- resp$response$response[i]
      inp_id  <- paste0("resp_", safe_indic, "_", i)
      pre_val <- if (!is.null(saved_resp) && !is.null(saved_resp[[as.character(i)]])) {
        saved_resp[[as.character(i)]]
      } else if (!is.na(q_val)) q_val else ""
      if (is.na(q_val)) {
        paste0("<div style='margin-bottom:8px;'>",
               "<p style='font-size:11px;font-weight:600;color:#444;margin:0 0 3px;'>", q_lbl, "</p>",
               "<input type='text' id='", inp_id, "' class='resp-input' data-idx='", i, "' ",
               "value='", pre_val, "' placeholder='Enter response\u2026' ",
               "style='width:100%;font-size:11px;border:1px solid #ccc;border-radius:3px;padding:4px 6px;box-sizing:border-box;'/>",
               "</div>")
      } else {
        paste0("<div style='margin-bottom:6px;'>",
               "<span style='font-size:11px;font-weight:600;color:#444;'>", q_lbl, ":</span> ",
               "<span style='font-size:11px;color:#333;'>", q_val, "</span>",
               "</div>")
      }
    }), collapse = "")

    country_html <- paste0(
      country_rows,
      "<div style='margin-top:10px;'>",
      "<button onclick=\"submitResponses('", resp$indic, "')\" ",
      "style='background:#009EDB;color:white;border:none;padding:5px 14px;border-radius:4px;cursor:pointer;font-size:12px;'>",
      "&#10003; Submit responses</button>",
      "<span id='resp_status_", safe_indic, "' style='margin-left:10px;font-size:11px;color:green;'></span>",
      "</div>"
    )
    list(oecd = oecd_html, country = country_html)
  }

  resp_by_indic <- setNames(xlsx_response_format, sapply(xlsx_response_format, `[[`, "indic"))

  # ── Build both heatmaps ───────────────────────────────────────────────────────
  observe({
    req(credentials$authenticated)
    d <- dat_rv()
    req(!is.null(d) && nrow(d) > 0)

    entries <- session_data$entries
    years   <- 2004:2026

    # val_lookup: time series for this country
    val_lookup <- d %>%
      select(measure, time_period, obs_value) %>%
      mutate(time_period = as.numeric(time_period))

    # Inline ECharts chart HTML builder (for all non-time-use measures)
    make_year_chart <- function(m) {
      all_years    <- 2004:2026
      safe         <- gsub("\\.", "_", m)
      cv           <- val_lookup %>% filter(measure == m)
      country_vals <- sapply(all_years, function(yr) {
        row <- cv %>% filter(time_period == yr)
        if (nrow(row) > 0 && !is.na(row$obs_value[1])) as.character(round(row$obs_value[1], 4)) else "null"
      })
      has_oecd <- !is.null(oecd_avg)
      oecd_ser <- if (has_oecd) {
        ov        <- oecd_avg %>% filter(measure == m)
        oecd_vals <- sapply(all_years, function(yr) {
          row <- ov %>% filter(time_period == yr)
          if (nrow(row) > 0 && !is.na(row$obs_value[1])) as.character(round(row$obs_value[1], 4)) else "null"
        })
        paste0(",{name:'OECD average',type:'line',data:[", paste(oecd_vals, collapse=","), "],",
               "connectNulls:true,itemStyle:{color:'#F89C1C'},lineStyle:{type:'dashed',width:1.5},symbolSize:5}")
      } else ""
      paste0(
        "<div id='echart_", safe, "' class='echart-container' style='width:100%;height:210px;margin-top:8px;'></div>",
        "<script>",
        "window.__chartOpts=window.__chartOpts||{};",
        "window.__chartOpts['echart_", safe, "']={",
        "  grid:{left:55,right:16,top:22,bottom:28,containLabel:true},",
        "  tooltip:{trigger:'axis'},",
        "  legend:{show:", if (has_oecd) "true" else "false", ",top:2,right:16,textStyle:{fontSize:10}},",
        "  xAxis:{type:'category',data:[\"", paste(all_years, collapse='","'), "\"],axisLabel:{fontSize:9,interval:3}},",
        "  yAxis:{type:'value',axisLabel:{fontSize:9},splitLine:{lineStyle:{color:'#eee'}}},",
        "  series:[{name:'Country',type:'line',data:[", paste(country_vals, collapse=","), "],",
        "    connectNulls:true,itemStyle:{color:'#009EDB'},lineStyle:{width:2},symbolSize:6}",
        oecd_ser, "]};",
        "</script>"
      )
    }

    # Build chart HTML for ALL non-time-use measures (used in coverage mode for xlsx too)
    year_charts_lookup <- setNames(
      lapply(unique(measure_list$measure), function(m) {
        if (m %in% time_use_measures) return("")
        make_year_chart(m)
      }),
      unique(measure_list$measure)
    )

    # dat_tidy: full grid of measures × years
    dat_tidy <- d %>%
      select(measure, time_period, obs_value) %>%
      complete(measure = unique(measure_list$measure), time_period = years) %>%
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

    label_every      <- c(2004, 2008, 2012, 2016, 2020, 2024, 2026)
    year_axis_cells  <- paste(sapply(years, function(yr) {
      lbl <- if (yr %in% label_every) as.character(yr) else ""
      paste0("<div style='flex:1;text-align:center;font-size:9px;color:#888;'>", lbl, "</div>")
    }), collapse = "")
    axis_row <- paste0(
      "<div style='display:flex;flex-direction:row;align-items:center;width:100%;margin-bottom:4px;'>",
      "<div style='flex:0 0 20%;'></div>",
      "<div style='flex:1;display:flex;flex-direction:row;'>", year_axis_cells, "</div>",
      "<div style='flex:0 0 130px;'></div></div>"
    )

    # Row breakdown definitions
    row_defs <- function(m) {
      if (m %in% all_rows) {
        list(
          list(key="country_avg", label="Country average",       bold=TRUE),
          list(key="male",        label="Male",                  bold=FALSE),
          list(key="female",      label="Female",                bold=FALSE),
          list(key="young",       label="Young",                 bold=FALSE),
          list(key="middle_aged", label="Middle-aged",           bold=FALSE),
          list(key="old",         label="Old",                   bold=FALSE),
          list(key="primary",     label="Primary (ISCED 0-2)",   bold=FALSE),
          list(key="secondary",   label="Secondary (ISCED 3-4)", bold=FALSE),
          list(key="tertiary",    label="Tertiary (ISCED 5-8)",  bold=FALSE)
        )
      } else if (exists("all_rows_dep_vert") && m %in% all_rows_dep_vert) {
        list(
          list(key="country_avg", label="Country average",       bold=TRUE),
          list(key="vert",        label="Vertical inequality",   bold=FALSE),
          list(key="dep",         label="Deprivation",           bold=FALSE),
          list(key="male",        label="Male",                  bold=FALSE),
          list(key="female",      label="Female",                bold=FALSE),
          list(key="young",       label="Young",                 bold=FALSE),
          list(key="middle_aged", label="Middle-aged",           bold=FALSE),
          list(key="old",         label="Old",                   bold=FALSE),
          list(key="primary",     label="Primary (ISCED 0-2)",   bold=FALSE),
          list(key="secondary",   label="Secondary (ISCED 3-4)", bold=FALSE),
          list(key="tertiary",    label="Tertiary (ISCED 5-8)",  bold=FALSE)
        )
      } else if (m %in% gender_only) {
        list(
          list(key="country_avg", label="Country average", bold=TRUE),
          list(key="male",        label="Male",            bold=FALSE),
          list(key="female",      label="Female",          bold=FALSE)
        )
      } else {
        list(list(key="country_avg", label="Country average", bold=TRUE))
      }
    }

    make_year_inputs <- function(m) {
      vals  <- val_lookup %>% filter(measure == m)
      saved <- session_data$entries[[m]]
      rows  <- row_defs(m)
      yr_header <- paste(sapply(years, function(yr) {
        paste0("<div style='flex:1;text-align:center;font-size:8px;color:#888;min-width:32px;'>", yr, "</div>")
      }), collapse = "")
      header_html <- paste0(
        "<div style='display:flex;align-items:center;margin-bottom:2px;'>",
        "<div style='flex:0 0 150px;'></div>",
        "<div style='flex:1;display:flex;'>", yr_header, "</div></div>"
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
          "<div style='flex:1;display:flex;'>", paste(inputs, collapse=""), "</div></div>"
        )
      })
      paste0("<div style='overflow-x:auto;margin-top:6px;'>", header_html,
             paste(row_htmls, collapse=""), "</div>")
    }

    year_inputs_lookup <- setNames(
      lapply(unique(dat_tidy$measure), make_year_inputs),
      unique(dat_tidy$measure)
    )

    response_html_lookup <- setNames(
      lapply(unique(dat_tidy$measure), function(m) {
        if (!m %in% xlsx_measures || is.null(resp_by_indic[[m]]))
          return(list(oecd = "", country = ""))
        build_response_html(resp_by_indic[[m]], session_data$responses[[m]])
      }),
      unique(dat_tidy$measure)
    )

    completeness_df <- dat_tidy %>%
      filter(!measure %in% time_use_measures) %>%
      group_by(measure, cat) %>%
      summarise(has_data = any(!is.na(obs_value)), .groups = "drop") %>%
      group_by(cat) %>%
      summarise(n_total = n(), n_missing = sum(!has_data), .groups = "drop")

    submitted_df <- if (length(entries) > 0) {
      bind_rows(lapply(names(entries), function(m) {
        row_data <- entries[[m]][["country_avg"]]
        if (is.null(row_data)) return(data.frame())
        bind_rows(lapply(names(row_data), function(yr) {
          v <- row_data[[yr]]
          data.frame(measure = m, time_period = as.numeric(yr),
                     submitted = !is.null(v) && !is.na(v) && v != "",
                     stringsAsFactors = FALSE)
        }))
      }))
    } else {
      data.frame(measure = character(), time_period = numeric(), submitted = logical())
    }

    # ── Pipeline helper: build heatmap HTML ────────────────────────────────────
    # coverage_mode = TRUE  → show all measures, all read-only, no ⚠ badge
    # coverage_mode = FALSE → show only xlsx_measures, with data entry
    build_heatmap_html <- function(coverage_mode) {

      base <- dat_tidy %>%
        { if (!coverage_mode) filter(., measure %in% xlsx_measures) else . } %>%
        mutate(time_period = as.numeric(time_period)) %>%
        left_join(submitted_df, by = c("measure", "time_period")) %>%
        mutate(
          submitted = replace_na(submitted, FALSE),
          color = case_when(
            !is.na(obs_value) ~ "#1F7A4D",
            submitted          ~ "#F89C1C",
            TRUE               ~ "#D9DDE3"
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
          # In coverage mode: all measures are read-only (even xlsx)
          needs_input    = if (coverage_mode) FALSE else measure %in% xlsx_measures,
          is_time_use    = measure %in% time_use_measures,
          safe_id        = gsub("\\.", "_", measure),
          year_inputs    = unlist(year_inputs_lookup[measure]),
          year_chart     = unlist(year_charts_lookup[measure]),
          oecd_q_html    = sapply(measure, function(m) response_html_lookup[[m]]$oecd),
          country_q_html = sapply(measure, function(m) response_html_lookup[[m]]$country),
          def_text       = replace_na(definition, "Definition to be added."),
          tech_name      = replace_na(indicator,  "\u2014"),
          unit_text      = replace_na(unit,       "\u2014"),

          row_border = case_when(
            needs_input ~ "border-left:3px solid #F89C1C;background:#fffbf4;",
            is_time_use ~ "border-left:3px solid #009EDB;background:#f0faff;",
            TRUE        ~ ""
          ),
          row_hover = case_when(
            needs_input ~ "#fffbf4",
            is_time_use ~ "#f0faff",
            TRUE        ~ ""
          ),
          # No ⚠ badge in coverage mode
          badge_html = case_when(
            !coverage_mode & needs_input ~
              "<span title='New data required' style='font-size:9px;background:#F89C1C;color:white;border-radius:3px;padding:1px 4px;white-space:nowrap;'>&#9888; Data update requested</span>",
            is_time_use ~
              "<span title='Time use tables' style='font-size:9px;background:#009EDB;color:white;border-radius:3px;padding:1px 4px;white-space:nowrap;'>&#128203; Time use tables</span>",
            TRUE ~ ""
          ),
          panel_border = case_when(
            needs_input ~ "border-left:3px solid #F89C1C;",
            is_time_use ~ "border-left:3px solid #009EDB;",
            TRUE        ~ "border-left:3px solid #D4D9DF;"
          ),

          panel_body = mapply(function(ni, itu, sid, mn, yi, yc, q, oqh, cqh, def, tech, unt, lbl) {
            if (ni) {
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
                "<div id='inputs_", sid, "' style='display:flex;flex-direction:row;flex-wrap:wrap;margin-top:8px;'>", yi, "</div>",
                "<div style='margin-top:8px;'>",
                "<button onclick=\"submitMeasure('", sid, "','", mn, "')\" ",
                "style='background:#009EDB;color:white;border:none;padding:6px 16px;border-radius:4px;cursor:pointer;font-size:12px;'>",
                "&#10003; Submit</button>",
                "<span id='status_", sid, "' style='margin-left:10px;font-size:11px;color:green;'></span>",
                "</div></div>"
              )
            } else if (itu) {
              paste0(
                "<div style='padding:28px;text-align:center;'>",
                "<p style='font-size:26px;margin:0;'>&#128203;</p>",
                "<p style='font-size:13px;margin-top:10px;'>",
                "This indicator is covered by the <strong>Time Use Survey tables</strong>.</p>",
                "<p style='font-size:12px;color:#666;margin-top:4px;'>",
                "Please navigate to the <strong>Time Use</strong> tab.</p>",
                "</div>"
              )
            } else {
              paste0(
                "<div style='width:100%;text-align:left;'>",
                "<div><span style='font-size:13px;font-weight:600;'>Label: </span>",
                "<span style='font-size:12px;color:#555;'>", lbl, "</span></div>",
                "<div><span style='font-size:13px;font-weight:600;'>Technical name: </span>",
                "<span style='font-size:12px;color:#555;'>", tech, "</span></div>",
                "<div><span style='font-size:13px;font-weight:600;'>Unit: </span>",
                "<span style='font-size:12px;color:#555;'>", unt, "</span></div>",
                "<div><span style='font-size:13px;font-weight:600;'>Definition: </span>",
                "<span style='font-size:12px;color:#555;'>", def, "</span></div>",
                "</div>",
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
                "style='background:#009EDB;color:white;border:none;padding:5px 14px;border-radius:4px;cursor:pointer;font-size:12px;'>",
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
            "<div id='panel_", safe_id, "' class='collapsible-panel' style='display:none;flex-direction:column;gap:14px;padding:16px;margin-bottom:6px;",
            panel_border, "'>",
            panel_body,
            "</div>"
          )
        ) %>%
        group_by(cat, group) %>%
        summarise(rows_html = paste(row_html, collapse = ""), .groups = "drop") %>%
        arrange(cat) %>%
        left_join(completeness_df, by = "cat") %>%
        mutate(
          section = if_else(cat <= 11, "Current Well-Being", "Future Well-Being"),

          comp_tag = mapply(function(nm, nt) {
            if (is.na(nm) || is.na(nt) || nt == 0) return("")
            have     <- nt - nm
            pct      <- round(100 * have / nt)
            fill_cls <- if (nm == 0) "comp-fill comp-full" else "comp-fill"
            lbl_cls  <- if (nm == 0) "comp-label comp-ok"  else "comp-label comp-warn"
            tip      <- if (nm == 0) paste0("All ", nt, " indicators have data")
                        else paste0(nm, " of ", nt, " indicators missing data")
            paste0("<div class='comp-wrap' title='", tip, "'>",
                   "<div class='comp-bar'><div class='", fill_cls, "' style='width:", pct, "%'></div></div>",
                   "<span class='", lbl_cls, "'>", have, "/", nt, "</span>",
                   "<span class='comp-tooltip'>", if (nm == 0) "complete" else "indicators", "</span>",
                   "</div>")
          }, n_missing, n_total, SIMPLIFY = TRUE),

          group_html = mapply(function(grp, ct) {
            icon_src <- group_icons[grp]
            icon_tag <- if (!is.na(icon_src))
              paste0("<img src='", icon_src, "' style='height:22px;width:22px;margin-right:7px;vertical-align:middle;object-fit:contain;'/>")
            else ""
            paste0("<div class='dim-header'><h4>", icon_tag, grp, "</h4>", ct, "</div>")
          }, group, comp_tag, SIMPLIFY = TRUE),

          prev_section = lag(section, default = ""),
          section_div  = if_else(
            section != prev_section,
            paste0("<div class='wb-section-header'>",
                   if_else(section == "Current Well-Being",
                           "&#9679;&nbsp; Current Well-Being",
                           "&#9651;&nbsp; Future Well-Being"), "</div>"),
            ""
          ),
          full_html = paste0(section_div, group_html, rows_html, axis_row)
        ) %>%
        pull(full_html) %>%
        paste(collapse = "")

      base
    }

    output$heatmap_submissions <- renderUI({ HTML(build_heatmap_html(FALSE)) })
    output$heatmap_coverage    <- renderUI({ HTML(build_heatmap_html(TRUE))  })
  })

  # ── Data submission observers ─────────────────────────────────────────────────
  observeEvent(input$submitted_data, {
    d <- input$submitted_data
    session_data$entries[[d$measure]] <- d$values
    runjs(paste0("
      var el = document.getElementById('status_", gsub("\\.", "_", input$submitted_data$measure), "');
      if(el) { el.innerText = '\\u2713 Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })

  observeEvent(input$submitted_note, {
    d <- input$submitted_note
    session_data$notes[[d$measure]] <- d$note
    runjs(paste0("
      var el = document.getElementById('note_status_", gsub("\\.", "_", input$submitted_note$measure), "');
      if(el) { el.innerText = '\\u2713 Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })

  observeEvent(input$submitted_responses, {
    d    <- input$submitted_responses
    safe <- gsub("\\.", "_", d$measure)
    session_data$responses[[d$measure]] <- d$values
    runjs(paste0("
      var el = document.getElementById('resp_status_", safe, "');
      if(el) { el.innerText = '\\u2713 Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })

  observeEvent(input$submitted_table, {
    d <- input$submitted_table
    if (d$table == "tu_table1") session_data$time_use_1 <- d$data
    else                        session_data$time_use_2 <- d$data
    runjs(paste0("
      var el = document.getElementById('status_", d$table, "');
      if(el) { el.innerText = '\\u2713 Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })

}

shinyApp(ui = ui, server = server)
