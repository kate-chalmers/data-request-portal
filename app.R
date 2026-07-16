source("./global.R")

# ── Shared head & JS ─────────────────────────────────────────────────────────
shared_head <- tagList(
  useShinyjs(),
  tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
  tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
  tags$script(src = "https://cdn.jsdelivr.net/npm/echarts@5/dist/echarts.min.js"),
  tags$script(HTML(paste0(
    "window.__validRanges = ", jsonlite::toJSON(validation_ranges, auto_unbox = TRUE), ";"
  ))),
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

      // Validate against ranges if defined
      var range = window.__validRanges && window.__validRanges[measure];
      if (range) {
        var bad = [];
        inputs.forEach(function(inp) {
          if (inp.value === '') return;
          var v = parseFloat(inp.value);
          if (isNaN(v)) return;
          if (v < range.min || v > range.max) {
            bad.push(inp.dataset.year + ' (' + inp.dataset.row + '): ' + v);
            inp.style.border = '2px solid #E63312';
          } else {
            inp.style.border = '1px solid #ccc';
          }
        });
        if (bad.length > 0) {
          var statusEl = document.getElementById('status_' + safe_id);
          if (statusEl) {
            statusEl.style.color = '#E63312';
            statusEl.innerText = 'Values out of range (' + range.min + ' to ' + range.max + '): ' + bad.length + ' field(s). Please correct before submitting.';
          }
          return;
        }
      }

      var values = {};
      inputs.forEach(function(inp) {
        var row = inp.dataset.row || 'country_avg';
        var yr  = inp.dataset.year;
        if (!values[row]) values[row] = {};
        values[row][yr] = inp.value === '' ? null : parseFloat(inp.value);
        inp.style.border = '1px solid #ccc';
      });
      // Collect data flags (B, E, P, etc.)
      var flags = {};
      container.querySelectorAll('.flag-select').forEach(function(sel) {
        var row = sel.dataset.row || 'country_avg';
        var yr  = sel.dataset.year;
        if (!flags[row]) flags[row] = {};
        if (sel.value !== '') flags[row][yr] = sel.value;
      });
      // Also collect the Country Question Format responses from this panel
      var panel = document.getElementById('panel_' + safe_id);
      var responses = {};
      if (panel) {
        panel.querySelectorAll('.resp-input').forEach(function(inp) {
          responses[inp.dataset.idx] = inp.value;
        });
      }
      Shiny.setInputValue('submitted_data',
        { measure: measure, safe_id: safe_id, values: values, flags: flags, responses: responses,
          timestamp: new Date().toISOString() },
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

    function submitNote(safe_id, measure) {
      var el = document.getElementById('note_' + safe_id);
      Shiny.setInputValue('submitted_note',
        { measure: measure, safe_id: safe_id, note: el ? el.value : '', timestamp: new Date().toISOString() },
        {priority: 'event'});
    }

    function declareNoUpdate(safe_id, measure) {
      var btn = document.getElementById('noupdate_' + safe_id);
      var isActive = btn.classList.toggle('active');
      Shiny.setInputValue('no_update_declared',
        { measure: measure, safe_id: safe_id, active: isActive,
          timestamp: new Date().toISOString() },
        {priority: 'event'});
    }

    function declareTUNoUpdate() {
      var btn = document.getElementById('tu_no_update_btn');
      var isActive = btn.classList.toggle('active');
      Shiny.setInputValue('tu_no_update_declared',
        { active: isActive, timestamp: new Date().toISOString() },
        {priority: 'event'});
    }

    function toggleAdminLogin() {
      var countryRow = document.getElementById('login_country_row');
      var adminBack  = document.getElementById('admin_back_link');
      var adminLink  = document.getElementById('admin_link');
      var loginTitle = document.getElementById('login_title');
      var loginDesc  = document.getElementById('login_desc');
      var isAdmin = countryRow.style.display === 'none';
      if (isAdmin) {
        countryRow.style.display = 'block';
        adminLink.style.display  = 'block';
        adminBack.style.display  = 'none';
        loginTitle.textContent = 'OECD Well-being and Time Use Questionnaire Portal';
        loginDesc.textContent  = 'Select your country and enter the access password.';
        Shiny.setInputValue('login_mode', 'country');
      } else {
        countryRow.style.display = 'none';
        adminLink.style.display  = 'none';
        adminBack.style.display  = 'block';
        loginTitle.textContent = 'Admin Access';
        loginDesc.textContent  = 'Enter the admin password to view submitted data.';
        Shiny.setInputValue('login_mode', 'admin');
      }
    }

    // Yes/No toggle: set hidden value, highlight choice, and when 'No' is
    // chosen disable every other response box in the same panel.
    function setToggle(inputId, btn, val) {
      var input = document.getElementById(inputId);
      if (input) input.value = val;
      var group = btn.parentNode;
      group.querySelectorAll('.toggle-btn').forEach(function(b) { b.classList.remove('active'); });
      btn.classList.add('active');

      var panel = btn.closest('.collapsible-panel');
      if (!panel) return;
      var disable = (val === 'No');
      // Text response boxes
      panel.querySelectorAll('.resp-input').forEach(function(el) {
        if (el.id === inputId || el.type === 'hidden') return;
        el.disabled = disable;
        el.style.opacity = disable ? '0.45' : '1';
        el.style.background = disable ? '#f0f0f0' : '#fff';
      });
      // Other toggle groups
      panel.querySelectorAll('.toggle-group').forEach(function(g) {
        if (g.contains(btn)) return;
        g.querySelectorAll('.toggle-btn').forEach(function(b) {
          b.disabled = disable;
          b.style.opacity = disable ? '0.45' : '1';
        });
      });
    }

    // Auto-expand textareas: resize to fit content, with a comfortable minimum
    function autoResizeTextarea(el) {
      el.style.height = 'auto';
      var contentH = el.scrollHeight;
      // At rest: at least 32px (one line); when focused or has content: at least 60px
      var minH = (el === document.activeElement || el.value.trim() !== '') ? 60 : 32;
      el.style.height = Math.max(contentH, minH) + 'px';
    }
    // Delegate input/focus/blur events for dynamically created textareas
    document.addEventListener('input', function(e) {
      if (e.target.classList.contains('resp-textarea')) autoResizeTextarea(e.target);
    });
    document.addEventListener('focus', function(e) {
      if (e.target.classList.contains('resp-textarea')) autoResizeTextarea(e.target);
    }, true);
    document.addEventListener('blur', function(e) {
      if (e.target.classList.contains('resp-textarea')) autoResizeTextarea(e.target);
    }, true);
    // Auto-resize all textareas after Shiny renders new content
    $(document).on('shiny:value', function() {
      setTimeout(function() {
        document.querySelectorAll('.resp-textarea').forEach(autoResizeTextarea);
      }, 100);
    });
  ")),
  tags$script(HTML("
    // ── Time Use Table 1: auto-summation & 1440 check ──────────────
    (function() {
      var groups = {
        1:  [2, 3, 4, 5, 6, 7],
        8:  [9, 10, 11, 14, 15, 16, 17],
        18: [19, 20, 21],
        22: [23, 24, 25, 26, 27],
        28: [29, 30]
      };
      var subGroups = { 11: [12, 13] };
      var totalRow = 31;
      var groupRows = [1, 8, 18, 22, 28];
      var numCols = [3, 4, 5];

      function getVal(row, col) {
        var t = document.getElementById('tu_table1');
        if (!t) return NaN;
        var inp = t.querySelector('input[data-row=\"' + row + '\"][data-col=\"' + col + '\"]');
        if (!inp || inp.value === '') return NaN;
        return parseFloat(inp.value);
      }

      function setVal(row, col, val) {
        var t = document.getElementById('tu_table1');
        if (!t) return;
        var inp = t.querySelector('input[data-row=\"' + row + '\"][data-col=\"' + col + '\"]');
        if (!inp) return;
        inp.value = isNaN(val) ? '' : parseFloat(val.toFixed(4)).toString();
      }

      function setGroupSum(gr, col, val) {
        var cell = document.querySelector('.group-sum-cell[data-group-row=\"' + gr + '\"][data-col=\"' + col + '\"]');
        if (!cell) return;
        cell.textContent = isNaN(val) ? '\\u2014' : parseFloat(val.toFixed(2)).toString();
      }

      function sumOf(rows, col) {
        var s = 0, any = false;
        for (var i = 0; i < rows.length; i++) {
          var v = getVal(rows[i], col);
          if (!isNaN(v)) { s += v; any = true; }
        }
        return any ? s : NaN;
      }

      function recalc() {
        var ci, col, sg, gi, gs, grand, anyG;
        for (ci = 0; ci < numCols.length; ci++) {
          col = numCols[ci];
          for (sg in subGroups) {
            setVal(parseInt(sg), col, sumOf(subGroups[sg], col));
          }
          grand = 0; anyG = false;
          for (gi = 0; gi < groupRows.length; gi++) {
            gs = sumOf(groups[groupRows[gi]], col);
            setGroupSum(groupRows[gi], col, gs);
            if (!isNaN(gs)) { grand += gs; anyG = true; }
          }
          setVal(totalRow, col, anyG ? grand : NaN);
        }
        check1440();
      }

      function check1440() {
        var t = document.getElementById('tu_table1');
        var w = document.getElementById('tu1_1440_warning');
        if (!t || !w) return;
        var colNames = {3:'Total (15-64)', 4:'Men (15-64)', 5:'Women (15-64)'};
        var issues = [];
        for (var ci = 0; ci < numCols.length; ci++) {
          var col = numCols[ci];
          var inp = t.querySelector('input[data-row=\"' + totalRow + '\"][data-col=\"' + col + '\"]');
          if (!inp || inp.value === '') continue;
          var v = parseFloat(inp.value);
          if (!isNaN(v) && Math.abs(v - 1440) > 0.5) {
            issues.push(colNames[col] + ': ' + parseFloat(v.toFixed(2)));
          }
        }
        if (issues.length > 0) {
          w.style.display = 'block';
          var d = document.getElementById('tu1_1440_detail');
          if (d) d.textContent = 'Totals not equal to 1440 minutes: ' + issues.join(' | ');
        } else {
          w.style.display = 'none';
        }
      }

      document.addEventListener('input', function(e) {
        if (e.target.closest('#tu_table1') && e.target.classList.contains('tu-num') && !e.target.classList.contains('tu-computed')) {
          recalc();
        }
      });

      // Cap values at 0–1440 on blur
      document.addEventListener('blur', function(e) {
        if (!e.target.closest('#tu_table1') || !e.target.classList.contains('tu-num') || e.target.classList.contains('tu-computed')) return;
        var v = parseFloat(e.target.value);
        if (isNaN(v)) return;
        var clamped = false;
        if (v > 1440) { e.target.value = '1440'; clamped = true; }
        if (v < 0)    { e.target.value = '0';    clamped = true; }
        if (clamped) recalc();
      }, true);

      $(document).on('shiny:value', function(evt) {
        if (evt.name === 'time_use_table1_ui') {
          setTimeout(recalc, 200);
        }
      });
    })();
  "))
)

# ── Reusable legends ──────────────────────────────────────────────────────────
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

coverage_legend <- tags$div(
  style = "display:flex;flex-wrap:wrap;gap:16px;font-size:11px;margin-bottom:10px;justify-content:center;align-items:center;color:#55606B;",
  tags$span(
    tags$span(style = "display:inline-block;width:11px;height:11px;background:#1F7A4D;border-radius:2px;margin-right:5px;vertical-align:middle;"),
    "Data available"
  ),
  tags$span(
    style = "display:inline-flex;align-items:center;gap:4px;",
    tags$span(style = "display:inline-block;width:11px;height:11px;background:#FDE8C8;border-radius:2px;vertical-align:middle;"),
    tags$span(style = "display:inline-block;width:60px;height:11px;border-radius:2px;vertical-align:middle;background:linear-gradient(to right,#FDE8C8,#C0392B);"),
    tags$span(style = "display:inline-block;width:11px;height:11px;background:#C0392B;border-radius:2px;vertical-align:middle;"),
    tags$span(style = "margin-left:4px;", "Gap (few \u2192 many other countries have data)")
  ),
  tags$span(
    tags$span(style = "display:inline-block;width:11px;height:11px;background:#D9DDE3;border:1px solid #c5cad0;border-radius:2px;margin-right:5px;vertical-align:middle;"),
    "No data anywhere"
  )
)

# ── Country choices ────────────────────────────────────────────────────────────
# Sort by displayed English name, not ISO3C value
.oecd_choices <- setNames(
  oecd_countries,
  countrycode::countrycode(oecd_countries, "iso3c", "country.name", warn = FALSE)
)
.partner_choices <- setNames(
  partner_countries,
  countrycode::countrycode(partner_countries, "iso3c", "country.name", warn = FALSE)
)
login_country_choices <- list(
  "\u2014 Select your country \u2014" = "",
  "OECD countries" = as.list(.oecd_choices[order(names(.oecd_choices))]),
  "Partner countries" = as.list(.partner_choices[order(names(.partner_choices))])
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
        id = "login_title",
        style = "text-align:center;font-size:17px;color:#1F2B3A;font-weight:700;margin:0 0 6px;",
        "OECD Well-being and Time Use Questionnaire Portal"
      ),
      tags$p(
        id = "login_desc",
        style = "text-align:center;font-size:12px;color:#55606B;margin:0 0 26px;line-height:1.5;",
        "Select your country and enter the access password."
      ),
      tags$div(
        id = "login_country_row",
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
               uiOutput("login_error")),
      tags$div(
        id = "admin_link",
        style = "text-align:center;margin-top:16px;",
        tags$a(href = "#", onclick = "toggleAdminLogin(); return false;",
               style = "font-size:11px;color:#888;text-decoration:none;",
               "Admin access \u2192")
      ),
      tags$div(
        id = "admin_back_link",
        style = "text-align:center;margin-top:16px;display:none;",
        tags$a(href = "#", onclick = "toggleAdminLogin(); return false;",
               style = "font-size:11px;color:#888;text-decoration:none;",
               "\u2190 Back to country login")
      )
    )
  ),

  # ── Main app (hidden until login) ─────────────────────────────────────────
  shinyjs::hidden(
    tags$div(
      id = "main_app",
      tags$div(
        class = "navbar-right-utils",
        actionLink("change_pw_modal_btn", label = NULL, icon = icon("gear"),
                   title = "Change password"),
        actionLink("logout_btn", label = NULL, icon = icon("right-from-bracket"),
                   title = "Log out")
      ),
      navbarPage(
        title = uiOutput("nav_title", inline = TRUE),
        id    = "main_navbar",

        # ── Tab 1: Data Submissions ────────────────────────────────────────
        tabPanel("Well-being Data Submissions",
          fluidPage(
            fluidRow(
              column(1),
              column(10,
                tags$div(class = "landing-hero",
                  tags$div(class = "landing-logo-row",
                    img(src = "wise_logo.png", height = 60),
                    img(src = "OECD_logo.svg", height = 36)
                  ),
                  tags$p(HTML(
                    "The purpose of this questionnaire is to gather national data on different aspects of well-being in OECD member countries to ensure they are 
                    reflected in the OECD Well-being Database and associated products such as upcoming editions of the <a href='https://www.oecd.org/en/publications/serials/how-s-life_g1g317ee.html'>
                    How's Life? publication series</a>,
                    the <a href='https://www.oecd.org/en/data/tools/well-being-data-monitor.html'>OECD Well-being Data Monitor</a>
                    and annually updated <a href=''>well-being country profiles</a>.<br><br> The OECD Well-being Database includes over 80 indicators, the majority 
                    of which are sourced from other OECD and external international data collections. <b>To ensure a streamlined process, this questionnaire covers ONLY the 
                    indicators that are unique to the OECD Well-being Database as well as relevant information for the OECD Time Use Database. It therefore 
                    excludes indicators that are managed through other OECD data collection activities, or by other external international data producers.</b> All types of 
                    offical surveys are of interest to this exercise, including (but not limited to) household surveys, health surveys, general social surveys, time-use 
                    surveys and ad hoc surveys.
                    <br><br>
                    The OECD Well-being Database can be accessed <a href='http://data-explorer.oecd.org/s/fu'>here.</a><br>
                    The OECD Time Use Database can be accessed <a href='http://data-explorer.oecd.org/s/177'>here.</a><br><br>
                    For information about the OECD Well-being Framework, see <a href='https://www.oecd.org/wise/measuring-well-being-and-progress.htm'>here.</a><br>
                    For metadata and definitions of all indicators covered in the OECD Well-being Database, see <a href='https://www.oecd.org/content/dam/oecd/en/topics/policy-sub-issues/measuring-well-being-and-progress/oecd-well-being-database-definitions.pdf'>here</a><br>
                    ")
                  )
                )
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10,
                tags$div(
                  style = paste0(
                    "background:#f0f6ff;border:1px solid #c5d7ee;border-radius:8px;",
                    "padding:16px 22px;margin-bottom:20px;font-size:12px;color:#1F2B3A;line-height:1.6;"
                  ),
                  tags$p(style = "font-weight:700;margin:0 0 6px;font-size:13px;", "Quick guide"),
                  tags$ul(style = "margin:0;padding-left:18px;",
                    tags$li("Each indicator below is shown as a row in the heatmap.",
                            tags$b("Green"), "cells = existing data,",
                            tags$b("orange"), "= submitted this session,",
                            tags$b("grey"), "= no data."),
                    tags$li("Click any indicator row to expand its panel, enter values, and press",
                            tags$b("\u2713 Submit"), "to save."),
                    tags$li("Data and responses are pre-filled with previous submissions but can be overwritten."),
                    tags$li("Your progress is", tags$b("auto-saved"), "and will be restored if you log out and back in."),
                    tags$li("Indicators marked", tags$span(style = "font-size:9px;background:#F89C1C;color:white;border-radius:3px;padding:1px 4px;", "\u26A0 Awaiting data input"),
                            "still need your input.")
                  )
                )
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10, align = "center",
                tags$div(class = "heatmap-content",
                  heatmap_legend,
                  shinycssloaders::withSpinner(uiOutput("heatmap_submissions"))
                )
              ),
              column(1)
            )
          )
        ),

        # ── Tab 3: Time Use ────────────────────────────────────────────────
        tabPanel("Time Use Data Submissions",
          fluidPage(
            fluidRow(
              column(1),
              column(10,
                tags$div(class = "landing-hero",
                  tags$div(class = "landing-logo-row",
                    img(src = "wise_logo.png", height = 60),
                    img(src = "OECD_logo.svg", height = 36)
                  ),
                  tags$p(HTML(
                    "This section collects detailed time use data from national time use surveys.
                    Table 1 gathers time spent on daily activities (in minutes per day), while
                    Table 2 asks you to map your national activity codes to the OECD classification."
                  ))
                )
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10,
                tags$div(
                  style = paste0(
                    "background:#f0f6ff;border:1px solid #c5d7ee;border-radius:8px;",
                    "padding:16px 22px;margin-bottom:20px;font-size:12px;color:#1F2B3A;line-height:1.6;"
                  ),
                  tags$p(style = "font-weight:700;margin:0 0 6px;font-size:13px;", "Quick guide"),
                  tags$ul(style = "margin:0;padding-left:18px;",
                    tags$li("Table 1 collects time spent on daily activities in",
                            tags$b("minutes per day"), "for the total population (15\u201364), men, and women."),
                    tags$li("Group subtotals and the grand total are",
                            tags$b("calculated automatically"), "from the values you enter."),
                    tags$li("Subtotal rows (e.g. 2.3 Care for household members) are",
                            tags$b("auto-summed"), "from their sub-categories (2.3.1 + 2.3.2)."),
                    tags$li("The daily total should sum to",
                            tags$b("1440 minutes"), "(24 hours). If it differs, you will be asked to provide a brief explanation."),
                    tags$li("Table 2 asks you to map your national activity codes to each OECD activity category."),
                    tags$li("Click", tags$b("\u2713 Submit table"), "to save each table separately.",
                            "Your progress is", tags$b("auto-saved"), "and restored on your next login.")
                  )
                )
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10,
                tags$div(
                  style = "margin-bottom:18px;display:flex;align-items:center;gap:10px;",
                  tags$button(
                    id = "tu_no_update_btn",
                    onclick = "declareTUNoUpdate()",
                    class = "no-update-btn",
                    style = "background:#f5f5f5;color:#555;border:1px solid #ccc;padding:6px 14px;border-radius:4px;cursor:pointer;font-size:12px;",
                    "No time use data update to declare"
                  ),
                  tags$span(id = "tu_no_update_status",
                            style = "font-size:11px;color:#1F7A4D;font-weight:600;")
                ),
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
                h4(HTML("Table 1. Time spent on daily activities (<u>minutes per day</u>)")),
                tags$p(style = "font-size:11px;color:#888;margin:-4px 0 4px;",
                       "Blue-highlighted rows are calculated automatically. Enter values in the white rows only."),
                uiOutput("time_use_table1_ui"),
                tags$div(
                  id = "tu1_1440_warning",
                  style = paste0(
                    "display:none;background:#FFF8E1;border:1px solid #F5C518;border-radius:8px;",
                    "padding:14px 18px;margin-top:12px;margin-bottom:16px;"
                  ),
                  tags$div(
                    style = "display:flex;align-items:center;gap:8px;margin-bottom:8px;",
                    tags$span(style = "font-size:16px;color:#B8860B;", "\u26A0"),
                    tags$span(id = "tu1_1440_detail",
                              style = "font-size:12px;font-weight:600;color:#B8860B;",
                              "Daily totals do not sum to 1440 minutes (24 hours).")
                  ),
                  tags$p(style = "font-size:11px;color:#666;margin:0 0 6px;",
                         "This may be intentional (e.g. rounding, simultaneous activities, or survey methodology). Please provide a brief explanation:"),
                  textAreaInput("tu1_explanation", label = NULL, value = "", width = "100%",
                                rows = 2, placeholder = "Explain why the total differs from 1440 minutes\u2026")
                ),
                br(), br(),
                h4("Table 2. Considering the activity coding list in the national time-use survey, please indicate which activity codes are grouped under each activity (e.g. 1.1. paid work)."),
                uiOutput("time_use_table2_ui")
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
                                       "Overview of full set of well-being indicators in the OECD",
                                       tags$em("How\u2019s Life? Well-being Database"),
                                       "and the current data coverage for your country.",
                                       tags$b("No action is needed from you on this page."),
                                       "This tab is provided for reference only, to help you explore the full set of well-being indicators and see where data is currently available or missing.",
                                       "Any data gaps shown here fall outside the scope of this questionnaire and are managed through other OECD data collection processes.", 
                                       "Red-shaded cells indicate gaps where other countries have data - darker red means more countries have data for that year, highlighting higher-priority gaps.",
                                       "Click any indicator to view its time series."
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
                                     coverage_legend,
                                     uiOutput("heatmap_coverage")
                            )
                     ),
                     column(1)
                   )
                 )
        ),

      ), # end navbarPage

      # ── Page bottom padding ──────────────────────────────────────────
      tags$div(style = "height:60px;"),

      # ── Feedback / contact footer (visible on all tabs) ─────────────
      tags$div(
        style = paste0(
          "background:#f5f7fa;border-top:1px solid #dde1e6;padding:24px 0;",
          "margin-top:20px;"
        ),
        fluidRow(
          column(1),
          column(10,
            tags$div(
              style = "display:flex;flex-wrap:wrap;gap:28px;align-items:flex-start;",
              tags$div(
                style = "flex:1;min-width:260px;",
                tags$h5(style = "font-weight:700;margin:0 0 8px;font-size:14px;color:#1F2B3A;",
                        "Contact & Support"),
                tags$p(style = "font-size:12px;color:#55606B;line-height:1.7;margin:0;", HTML(
                  "For questions about the questionnaire or data submissions:<br>",
                  "<a href='mailto:kate.chalmers@oecd.org' style='color:#009EDB;'>kate.chalmers@oecd.org</a><br>",
                  "<a href='mailto:lara.fleischer@oecd.org' style='color:#009EDB;'>lara.fleischer@oecd.org</a><br>",
                  "<a href='mailto:wellbeing@oecd.org' style='color:#009EDB;'>wellbeing@oecd.org</a>"
                ))
              ),
              tags$div(
                style = "flex:1;min-width:300px;",
                tags$h5(style = "font-weight:700;margin:0 0 8px;font-size:14px;color:#1F2B3A;",
                        "Feedback"),
                tags$p(style = "font-size:11px;color:#888;margin:0 0 6px;",
                       "Let us know if you encounter any issues or have suggestions."),
                tags$textarea(
                  id = "feedback_text",
                  placeholder = "Type your feedback here\u2026",
                  rows = "3",
                  style = paste0(
                    "width:100%;font-size:12px;font-family:inherit;border:1px solid #ccc;",
                    "border-radius:4px;padding:8px;box-sizing:border-box;resize:vertical;"
                  )
                ),
                tags$div(
                  style = "margin-top:8px;display:flex;align-items:center;gap:10px;",
                  actionButton("send_feedback_btn", "Send feedback",
                               style = "background:#009EDB;color:white;border:none;padding:6px 16px;border-radius:4px;font-size:12px;font-weight:600;"),
                  uiOutput("feedback_status", inline = TRUE)
                )
              )
            )
          ),
          column(1)
        )
      )
    )   # end main_app div
  ),    # end hidden (main_app)

  # ── Admin app (hidden, accessed from login page) ──────────────────────────
  shinyjs::hidden(
    tags$div(
      id = "admin_app",
      style = "padding-top:20px;",
      tags$div(
        style = paste0(
          "background:var(--oecd-navy);padding:12px 20px;display:flex;",
          "align-items:center;justify-content:space-between;margin-bottom:20px;"
        ),
        tags$span(style = "color:white;font-weight:700;font-size:16px;",
                  "OECD Well-being Portal \u2014 Admin"),
        actionLink("admin_logout_btn", "\u2190 Log out",
                   style = "color:rgba(255,255,255,0.85);font-size:13px;font-weight:600;")
      ),
      fluidPage(
        fluidRow(
          column(1),
          column(10,
            tags$h3("Submitted Data", style = "font-weight:700;margin-bottom:4px;"),
            tags$p("Data entered by countries via the portal.",
                   style = "font-size:12px;color:#888;margin-bottom:16px;"),
            fluidRow(
              column(4,
                selectInput("admin_country_filter", "Country",
                            choices = c("All countries" = "ALL"),
                            width = "100%")
              ),
              column(4,
                selectInput("admin_table_select", "Table",
                            choices = c("Data entries" = "entries",
                                        "Data flags" = "flags",
                                        "Notes" = "notes",
                                        "No updates declared" = "no_updates",
                                        "Responses" = "responses",
                                        "Time-use table 1" = "tu1",
                                        "Time-use table 2" = "tu2",
                                        "Feedback" = "feedback"),
                            width = "100%")
              ),
              column(4, style = "padding-top:25px;",
                downloadButton("admin_download_csv", "Download CSV",
                               style = "width:100%;")
              )
            ),
            DT::dataTableOutput("admin_data_table"),
            tags$hr(style = "margin:30px 0 20px;border-color:#eee;"),
            tags$div(
              style = "display:flex;align-items:center;gap:16px;",
              actionButton("admin_reset_btn", "Reset All Submissions",
                           icon = icon("trash"),
                           style = "background:#E63312;color:white;border:none;font-weight:600;"),
              tags$span(
                style = "font-size:12px;color:#888;",
                "Permanently deletes all saved session files for every country."
              )
            ),
            uiOutput("admin_reset_feedback", style = "margin-top:10px;")
          ),
          column(1)
        )
      )
    )
  )
)       # end tagList

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Authentication ──────────────────────────────────────────────────────────
  credentials <- reactiveValues(authenticated = FALSE, country = NULL, country_name = NULL)
  dat_rv      <- reactiveVal(NULL)

  output$nav_title <- renderUI({
    if (!credentials$authenticated) return(tags$span("OECD Well-being Questionnaire Portal"))
    tags$span(
      style = "color:#ffffff !important;",
      "OECD Well-being Questionnaire Portal",
      tags$span(
        style = "font-size:13px;font-weight:500;color:#ffffff !important;background:rgba(255,255,255,0.22);padding:3px 12px;border-radius:12px;margin-left:14px;",
        credentials$country_name
      )
    )
  })

  observeEvent(input$login_btn, {
    req(input$login_password)

    # ── Admin login mode ─────────────────────────────────────────────────────
    if (identical(input$login_mode, "admin")) {
      if (input$login_password != "admin2026") {
        output$login_error <- renderUI(
          tags$p(style = "color:#E63312;font-size:12px;margin:0;", "Incorrect admin password.")
        )
        return()
      }
      admin_auth(TRUE)
      # Populate country filter
      rds_files <- list.files("sessions", pattern = "^[A-Z]{3}\\.rds$", full.names = FALSE)
      isos      <- sub("\\.rds$", "", rds_files)
      names(isos) <- countrycode::countrycode(isos, "iso3c", "country.name", warn = FALSE)
      choices <- c("All countries" = "ALL", isos[order(names(isos))])
      updateSelectInput(session, "admin_country_filter", choices = choices)

      shinyjs::hide("login_screen")
      shinyjs::show("admin_app")
      return()
    }

    # ── Country login mode ───────────────────────────────────────────────────
    req(input$login_country)
    if (!nzchar(input$login_country)) {
      output$login_error <- renderUI(
        tags$p(style = "color:#E63312;font-size:12px;margin:0;", "Please select a country.")
      )
      return()
    }
    pw_file   <- file.path("sessions", "passwords.rds")
    pw_store  <- if (file.exists(pw_file)) tryCatch(readRDS(pw_file), error = function(e) list()) else list()
    valid_pw  <- pw_store[[input$login_country]] %||% "oecd2026"
    if (input$login_password != valid_pw) {
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
        session_data$no_updates <- loaded$no_updates %||% list()
        session_data$flags      <- loaded$flags      %||% list()
        session_data$time_use_1   <- loaded$time_use_1
        session_data$time_use_2   <- loaded$time_use_2
        session_data$tu_no_update <- loaded$tu_no_update %||% FALSE
        if (!is.null(loaded$tu_survey_name))
          updateTextInput(session,   "tu_survey_name", value = loaded$tu_survey_name)
        if (!is.null(loaded$tu_survey_year))
          updateNumericInput(session, "tu_survey_year", value = loaded$tu_survey_year)
        if (!is.null(loaded$tu1_explanation) && nzchar(loaded$tu1_explanation))
          updateTextAreaInput(session, "tu1_explanation", value = loaded$tu1_explanation)
      }
    }

    shinyjs::hide("login_screen")
    shinyjs::show("main_app")

    # Restore time-use no-update button state
    if (isTRUE(session_data$tu_no_update)) {
      runjs("setTimeout(function() {
        var btn = document.getElementById('tu_no_update_btn');
        if(btn) btn.classList.add('active');
        var el = document.getElementById('tu_no_update_status');
        if(el) { el.style.color = '#1F7A4D'; el.innerText = '\\u2713 Marked as no update'; }
      }, 300);")
    }
  })

  # ── Logout ────────────────────────────────────────────────────────────────────
  observeEvent(input$logout_btn, {
    credentials$authenticated <- FALSE
    credentials$country       <- NULL
    credentials$country_name  <- NULL
    dat_rv(NULL)

    # Reset session data
    session_data$entries      <- list()
    session_data$notes        <- list()
    session_data$responses    <- list()
    session_data$flags        <- list()
    session_data$time_use_1   <- NULL
    session_data$time_use_2   <- NULL
    session_data$tu_no_update <- FALSE

    # Reset login form
    updateSelectInput(session, "login_country", selected = "")
    updateTextInput(session, "login_password", value = "")
    output$login_error <- renderUI(NULL)

    shinyjs::hide("main_app")
    shinyjs::show("login_screen")
  })

  # ── Change password (modal, only available when logged in) ───────────────────
  observeEvent(input$change_pw_modal_btn, {
    req(credentials$authenticated)
    showModal(modalDialog(
      title = paste0("Change password \u2014 ", credentials$country_name),
      size = "s",
      easyClose = TRUE,
      tags$div(
        style = "margin-bottom:12px;",
        tags$label("Current password",
                   style = "font-size:11px;font-weight:600;color:#55606B;display:block;margin-bottom:4px;"),
        passwordInput("pw_current", NULL, width = "100%", placeholder = "Enter current password")
      ),
      tags$div(
        style = "margin-bottom:12px;",
        tags$label("New password",
                   style = "font-size:11px;font-weight:600;color:#55606B;display:block;margin-bottom:4px;"),
        passwordInput("pw_new", NULL, width = "100%", placeholder = "Enter new password")
      ),
      tags$div(
        style = "margin-bottom:4px;",
        tags$label("Confirm new password",
                   style = "font-size:11px;font-weight:600;color:#55606B;display:block;margin-bottom:4px;"),
        passwordInput("pw_confirm", NULL, width = "100%", placeholder = "Confirm new password")
      ),
      uiOutput("change_pw_msg"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("change_pw_btn", "Update password",
                     style = "background:#009EDB;color:white;border:none;font-weight:600;")
      )
    ))
  })

  observeEvent(input$change_pw_btn, {
    req(credentials$authenticated, credentials$country)
    iso <- credentials$country

    pw_file  <- file.path("sessions", "passwords.rds")
    pw_store <- if (file.exists(pw_file)) tryCatch(readRDS(pw_file), error = function(e) list()) else list()
    current  <- pw_store[[iso]] %||% "oecd2026"

    if (!identical(input$pw_current, current)) {
      output$change_pw_msg <- renderUI(
        tags$p(style = "color:#E63312;font-size:11px;margin:4px 0 0;", "Current password is incorrect."))
      return()
    }
    if (!nzchar(input$pw_new) || nchar(input$pw_new) < 4) {
      output$change_pw_msg <- renderUI(
        tags$p(style = "color:#E63312;font-size:11px;margin:4px 0 0;", "New password must be at least 4 characters."))
      return()
    }
    if (!identical(input$pw_new, input$pw_confirm)) {
      output$change_pw_msg <- renderUI(
        tags$p(style = "color:#E63312;font-size:11px;margin:4px 0 0;", "Passwords do not match."))
      return()
    }

    dir.create("sessions", showWarnings = FALSE)
    pw_store[[iso]] <- input$pw_new
    saveRDS(pw_store, pw_file)
    output$change_pw_msg <- renderUI(
      tags$p(style = "color:#1F7A4D;font-size:11px;margin:4px 0 0;", "\u2713 Password updated successfully."))
    Sys.sleep(1.5)
    removeModal()
  })

  # ── Session data ─────────────────────────────────────────────────────────────
  session_data <- reactiveValues(
    entries      = list(),
    notes        = list(),
    responses    = list(),
    no_updates   = list(),
    flags        = list(),
    time_use_1   = NULL,
    time_use_2   = NULL,
    tu_no_update = FALSE
  )

  # Auto-save to sessions/{iso}.rds whenever any data changes
  observe({
    req(credentials$authenticated, credentials$country)
    # Touch all fields to create reactive dependencies
    list(session_data$entries, session_data$notes, session_data$responses,
         session_data$no_updates, session_data$flags, session_data$time_use_1, session_data$time_use_2,
         session_data$tu_no_update)
    dir.create("sessions", showWarnings = FALSE)
    saveRDS(
      c(reactiveValuesToList(session_data),
        list(tu_survey_name  = isolate(input$tu_survey_name),
             tu_survey_year  = isolate(input$tu_survey_year),
             tu1_explanation = isolate(input$tu1_explanation))),
      file.path("sessions", paste0(credentials$country, ".rds"))
    )
  })

  # ── Helper: null coalescing ──────────────────────────────────────────────────
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # ── Time Use table builder ───────────────────────────────────────────────────
  make_time_use_table <- function(n_rows, col_names, n_text_cols, table_id,
                                   row_text = NULL, saved = NULL,
                                   show_sums = FALSE, computed_codes = character(0)) {
    n_cols <- length(col_names)
    th <- paste(sapply(col_names, function(cn) {
      paste0("<th style='font-size:11px;padding:4px 8px;border:1px solid #ddd;background:#f5f5f5;white-space:pre-wrap;'>", cn, "</th>")
    }), collapse = "")
    header <- paste0("<tr>", th, "</tr>")

    body <- paste(sapply(seq_len(n_rows), function(r) {
      code_val   <- if (!is.null(row_text) && r <= nrow(row_text)) trimws(row_text[r, 1]) else ""
      is_divider <- grepl("\\.0$", code_val)
      is_computed <- show_sums && (code_val %in% computed_codes)

      if (is_divider) {
        txt2          <- if (!is.null(row_text) && r <= nrow(row_text) && n_text_cols >= 2) row_text[r, 2] else ""
        divider_label <- if (nchar(txt2) > 0) paste0(code_val, " - ", txt2) else code_val
        if (show_sums) {
          td_label_style <- "font-size:11px;font-weight:600;color:white;padding:6px 10px;border:1px solid #0f2843;background:#003189;"
          label_td <- paste0("<td colspan='", n_text_cols, "' style='", td_label_style, "'>", divider_label, "</td>")
          sum_tds <- paste(sapply((n_text_cols + 1):n_cols, function(c) {
            paste0("<td class='group-sum-cell' data-group-row='", r, "' data-col='", c, "' ",
                   "style='font-size:11px;font-weight:700;color:rgba(255,255,255,0.85);padding:6px 4px;",
                   "border:1px solid #0f2843;background:#003189;text-align:center;min-width:60px;'>\u2014</td>")
          }), collapse = "")
          paste0("<tr data-row='", r, "'>", label_td, sum_tds, "</tr>")
        } else {
          td_style <- "font-size:11px;font-weight:600;color:white;padding:6px 10px;border:1px solid #0f2843;background:#003189;"
          paste0("<tr data-row='", r, "' style='background:#003189;'>",
                 "<td colspan='", n_cols, "' style='", td_style, "'>", divider_label, "</td></tr>")
        }
      } else {
        cells <- paste(sapply(seq_len(n_cols), function(c) {
          if (c <= n_text_cols) {
            txt <- if (!is.null(row_text) && r <= nrow(row_text)) row_text[r, c] else ""
            td_style <- if (is_computed) {
              "font-size:11px;padding:4px 6px;border:1px solid #d0d8e2;color:#1F2B3A;font-weight:600;background:#e8eef5;"
            } else {
              "font-size:11px;padding:4px 6px;border:1px solid #eee;color:#333;"
            }
            paste0("<td style='", td_style, "'>", txt, "</td>")
          } else {
            saved_val <- if (!is.null(saved) && !is.null(saved[[as.character(r)]])) {
              val <- saved[[as.character(r)]][[paste0("c", c)]]
              if (!is.null(val)) val else ""
            } else ""
            if (is_computed) {
              paste0("<td style='padding:2px;background:#e8eef5;'>",
                     "<input type='text' class='tu-num year-input tu-computed' ",
                     "data-row='", r, "' data-col='", c, "' value='", saved_val, "' ",
                     "readonly tabindex='-1' ",
                     "style='width:100%;min-width:60px;font-size:11px;font-weight:600;border:1px solid #b8c4d0;",
                     "border-radius:3px;padding:2px 4px;text-align:center;background:#dce4ef;color:#1F2B3A;cursor:default;'/>",
                     "</td>")
            } else {
              paste0("<td style='padding:2px;'>",
                     "<input type='text' inputmode='decimal' class='tu-num year-input' ",
                     "data-row='", r, "' data-col='", c, "' value='", saved_val, "' ",
                     "oninput=\"this.value=this.value.replace(/[^0-9.\\-]/g,'')\" ",
                     "style='width:100%;min-width:60px;font-size:11px;border:1px solid #ccc;",
                     "border-radius:3px;padding:2px 4px;text-align:center;'/>",
                     "</td>")
            }
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
                              saved    = session_data$time_use_1,
                              show_sums = TRUE,
                              computed_codes = c("T")))
  })
  output$time_use_table2_ui <- renderUI({
    HTML(make_time_use_table(30, time_use_col_names_2, 2, "tu_table2",
                              row_text = time_use_row_text_2,
                              saved    = session_data$time_use_2))
  })

  # ── Response-format HTML builder ─────────────────────────────────────────────
  # Turn any http(s) URLs in a string into clickable links
  linkify <- function(x) {
    if (length(x) == 0) return(x)
    ifelse(is.na(x), x,
           gsub("(https?://[^\\s<>\"]+)",
                "<a href='\\1' target='_blank' style='color:#009EDB;word-break:break-all;'>\\1</a>",
                x, perl = TRUE))
  }

  build_response_html <- function(resp, saved_resp = NULL, prefill_resp = NULL) {
    oecd_rows <- paste(mapply(function(lbl, val) {
      paste0("<tr>",
             "<td style='font-size:11px;font-weight:600;color:#555;padding:3px 8px 3px 0;vertical-align:top;white-space:nowrap;'>", lbl, "</td>",
             "<td style='font-size:11px;padding:3px 0;color:#333;'>", if (is.na(val)) "\u2014" else linkify(val), "</td>",
             "</tr>")
    }, resp$label$label, resp$label$response, SIMPLIFY = TRUE), collapse = "")
    oecd_html <- paste0("<table style='width:100%;border-collapse:collapse;'>", oecd_rows, "</table>")

    safe_indic <- gsub("\\.", "_", resp$indic)

    # `disable_rest` becomes TRUE once a yes/no question is answered "No" on load,
    # greying out and disabling every following response box.
    disable_rest <- FALSE
    parts <- character(nrow(resp$response))
    for (i in seq_len(nrow(resp$response))) {
      q_lbl   <- resp$response$label[i]
      q_val   <- resp$response$response[i]
      inp_id  <- paste0("resp_", safe_indic, "_", i)
      # Priority: session save > prefill > default
      pre_val <- if (!is.null(saved_resp) && !is.null(saved_resp[[as.character(i)]])) {
        saved_resp[[as.character(i)]]
      } else if (!is.null(prefill_resp) && !is.null(prefill_resp[[as.character(i)]])) {
        prefill_resp[[as.character(i)]]
      } else if (!is.na(q_val)) q_val else ""
      is_yesno <- grepl("\\(yes\\s*/\\s*no\\)", q_lbl, ignore.case = TRUE)

      dis_attr  <- if (disable_rest) " disabled" else ""
      dis_style <- if (disable_rest) "opacity:0.45;background:#f0f0f0;" else ""

      # Escape HTML entities for safe embedding in textarea content
      safe_pre_val <- gsub("&", "&amp;", pre_val, fixed = TRUE)
      safe_pre_val <- gsub("<", "&lt;", safe_pre_val, fixed = TRUE)
      safe_pre_val <- gsub(">", "&gt;", safe_pre_val, fixed = TRUE)

      if (is.na(q_val) && is_yesno) {
        yes_active <- if (identical(tolower(pre_val), "yes")) " active" else ""
        no_active  <- if (identical(tolower(pre_val), "no"))  " active" else ""
        parts[i] <- paste0(
          "<div style='margin-bottom:8px;", dis_style, "'>",
          "<p style='font-size:11px;font-weight:600;color:#444;margin:0 0 4px;'>", q_lbl, "</p>",
          "<input type='hidden' id='", inp_id, "' class='resp-input' data-idx='", i, "' value='", pre_val, "'/>",
          "<div class='toggle-group'>",
          "<button type='button'", dis_attr, " class='toggle-btn", yes_active, "' onclick=\"setToggle('", inp_id, "', this, 'Yes')\">Yes</button>",
          "<button type='button'", dis_attr, " class='toggle-btn", no_active, "' onclick=\"setToggle('", inp_id, "', this, 'No')\">No</button>",
          "</div></div>"
        )
        # If this yes/no is answered "No", disable everything that follows
        if (identical(tolower(pre_val), "no")) disable_rest <- TRUE
      } else if (is.na(q_val)) {
        parts[i] <- paste0(
          "<div style='margin-bottom:8px;'>",
          "<p style='font-size:11px;font-weight:600;color:#444;margin:0 0 3px;", dis_style, "'>", q_lbl, "</p>",
          "<textarea id='", inp_id, "' class='resp-input resp-textarea' data-idx='", i, "'", dis_attr, " ",
          "placeholder='Enter response\u2026' rows='1' ",
          "style='width:100%;font-size:11px;border:1px solid #ccc;border-radius:4px;padding:6px 8px;",
          "box-sizing:border-box;resize:vertical;overflow:hidden;line-height:1.5;font-family:inherit;",
          "min-height:32px;transition:min-height 0.15s ease;", dis_style, "'>",
          safe_pre_val, "</textarea>",
          "</div>"
        )
      } else {
        parts[i] <- paste0(
          "<div style='margin-bottom:6px;'>",
          "<span style='font-size:11px;font-weight:600;color:#444;'>", q_lbl, ":</span> ",
          "<span style='font-size:11px;color:#333;'>", linkify(q_val), "</span>",
          "</div>"
        )
      }
    }

    list(oecd = oecd_html, country = paste(parts, collapse = ""))
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
    make_year_chart <- function(m, prefix = "") {
      all_years    <- 2004:2026
      safe         <- paste0(prefix, gsub("\\.", "_", m))
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

    # Build chart lookups with prefixed IDs for each heatmap
    make_charts_lookup <- function(prefix) {
      setNames(
        lapply(unique(measure_list$measure), function(m) {
          if (m %in% time_use_measures) return("")
          make_year_chart(m, prefix)
        }),
        unique(measure_list$measure)
      )
    }
    sub_charts_lookup <- make_charts_lookup("sub_")
    cov_charts_lookup <- make_charts_lookup("cov_")

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
    # Age labels depend on which age-group classification the measure uses
    age_labels <- function(m) {
      if (m %in% young_15_24) {
        list(young = "Young (15-24 years)", middle_aged = "Middle-aged (25-64 years)", old = "Old (65+ years)")
      } else if (m %in% young_16_24) {
        list(young = "Young (16-24 years)", middle_aged = "Middle-aged (25-54 years)", old = "Old (55+ years)")
      } else {
        # Default: young_16_29 grouping
        list(young = "Young (16-29 years)", middle_aged = "Middle-aged (30-49 years)", old = "Old (50+ years)")
      }
    }

    row_defs <- function(m) {
      al <- age_labels(m)
      if (m %in% all_rows) {
        list(
          list(key="country_avg", label="Country average",       bold=TRUE),
          list(key="male",        label="Male",                  bold=FALSE),
          list(key="female",      label="Female",                bold=FALSE),
          list(key="young",       label=al$young,                bold=FALSE),
          list(key="middle_aged", label=al$middle_aged,          bold=FALSE),
          list(key="old",         label=al$old,                  bold=FALSE),
          list(key="age_flag",    label="",                      bold=FALSE, is_age_flag=TRUE),
          list(key="primary",     label="Primary (ISCED levels 0-2)",   bold=FALSE),
          list(key="secondary",   label="Secondary (ISCED levels 3-4)", bold=FALSE),
          list(key="tertiary",    label="Tertiary (ISCED levels 5-8)",  bold=FALSE)
        )
      } else if (exists("all_rows_dep_vert") && m %in% all_rows_dep_vert) {
        list(
          list(key="country_avg", label="Country average",       bold=TRUE),
          list(key="vert",        label="Vertical inequality",   bold=FALSE),
          list(key="dep",         label="Deprivation",           bold=FALSE),
          list(key="male",        label="Male",                  bold=FALSE),
          list(key="female",      label="Female",                bold=FALSE),
          list(key="young",       label=al$young,                bold=FALSE),
          list(key="middle_aged", label=al$middle_aged,          bold=FALSE),
          list(key="old",         label=al$old,                  bold=FALSE),
          list(key="age_flag",    label="",                      bold=FALSE, is_age_flag=TRUE),
          list(key="primary",     label="Primary (ISCED levels 0-2)",   bold=FALSE),
          list(key="secondary",   label="Secondary (ISCED levels 3-4)", bold=FALSE),
          list(key="tertiary",    label="Tertiary (ISCED levels 5-8)",  bold=FALSE)
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
      saved_flags <- session_data$flags[[m]]
      rows  <- row_defs(m)
      yr_header <- paste(sapply(years, function(yr) {
        paste0("<div style='flex:1;text-align:center;font-size:8px;color:#888;min-width:32px;'>", yr, "</div>")
      }), collapse = "")
      header_html <- paste0(
        "<div style='display:flex;align-items:center;margin-bottom:2px;'>",
        "<div style='flex:0 0 150px;'></div>",
        "<div style='flex:1;display:flex;'>", yr_header, "</div></div>"
      )
      # Retrieve saved age-flag note for this measure
      saved_age_flag <- if (!is.null(saved) && !is.null(saved[["age_flag"]])) saved[["age_flag"]] else ""

      # Flag options
      flag_codes <- c("", "B", "E", "P", "D", "U")
      flag_labels <- c("\u2014", "B", "E", "P", "D", "U")

      row_htmls <- sapply(rows, function(r) {
        # Special row: age group difference flag (text input spanning full width)
        if (!is.null(r$is_age_flag) && isTRUE(r$is_age_flag)) {
          flag_val <- if (is.list(saved_age_flag)) "" else as.character(saved_age_flag)
          return(paste0(
            "<div style='display:flex;align-items:center;margin-bottom:4px;margin-top:2px;'>",
            "<div style='flex:0 0 150px;font-size:10px;color:#888;padding-right:6px;text-align:right;font-style:italic;'>",
            "Age groups differ?</div>",
            "<div style='flex:1;'>",
            "<input type='text' class='year-input' data-row='age_flag' data-year='note' ",
            "value='", htmltools::htmlEscape(flag_val, attribute = TRUE), "' ",
            "placeholder='If your age groups differ from the above, describe here' ",
            "style='width:100%;padding:3px 6px;border:1px solid #dde1e6;border-radius:4px;",
            "font-size:10px;color:#555;'/>",
            "</div></div>"
          ))
        }
        # Combined value + flag cells (stacked within each year column)
        cells <- sapply(years, function(yr) {
          v <- if (!is.null(saved) && !is.null(saved[[r$key]]) &&
                   !is.null(saved[[r$key]][[as.character(yr)]])) {
            saved[[r$key]][[as.character(yr)]]
          } else if (r$key == "country_avg") {
            existing <- vals %>% filter(time_period == yr)
            if (nrow(existing) > 0 && !is.na(existing$obs_value[1])) existing$obs_value[1] else NA
          } else NA
          has_val          <- !is.na(v)
          value_attr       <- if (has_val) paste0("value='", v, "'") else ""
          placeholder_attr <- if (!has_val) "placeholder='\u00b7'" else ""

          saved_f <- if (!is.null(saved_flags) && !is.null(saved_flags[[r$key]]) &&
                         !is.null(saved_flags[[r$key]][[as.character(yr)]])) {
            saved_flags[[r$key]][[as.character(yr)]]
          } else ""
          opts_html <- paste(mapply(function(code, lbl) {
            sel <- if (identical(code, saved_f)) " selected" else ""
            paste0("<option value='", code, "'", sel, ">", lbl, "</option>")
          }, flag_codes, flag_labels, SIMPLIFY = TRUE), collapse = "")

          paste0(
            "<div style='flex:1;min-width:32px;padding:0 1px;'>",
            "<input type='text' inputmode='decimal' class='year-input' ",
            "data-row='", r$key, "' data-year='", yr, "' ",
            value_attr, " ", placeholder_attr,
            " oninput=\"this.value=this.value.replace(/[^0-9.\\-]/g,'')\"",
            " style='width:100%;padding:2px 1px;border:1px solid #dde1e6;border-radius:4px 4px 0 0;",
            "font-size:10px;text-align:center;border-bottom:none;'/>",
            "<select class='flag-select' data-row='", r$key, "' data-year='", yr, "' ",
            "style='width:100%;padding:0;border:1px solid #dde1e6;border-radius:0 0 4px 4px;",
            "font-size:7px;text-align:center;color:#999;background:#fafbfc;cursor:pointer;",
            "line-height:1;height:14px;-webkit-appearance:none;appearance:none;'>",
            opts_html, "</select>",
            "</div>"
          )
        })
        paste0(
          "<div style='display:flex;align-items:center;margin-bottom:3px;'>",
          "<div style='flex:0 0 150px;font-size:11px;color:#444;padding-right:6px;text-align:right;",
          if (r$bold) "font-weight:600;" else "", "'>", r$label, "</div>",
          "<div style='flex:1;display:flex;'>", paste(cells, collapse=""), "</div></div>"
        )
      })
      # Flag legend
      flag_legend <- paste0(
        "<div style='font-size:9px;color:#999;margin-top:6px;padding:4px 8px;",
        "background:#f8f9fa;border-radius:4px;display:inline-block;'>",
        "<strong style='color:#666;'>Flags:</strong> ",
        "B = Break in series &nbsp;&middot;&nbsp; ",
        "E = Estimate &nbsp;&middot;&nbsp; ",
        "P = Provisional &nbsp;&middot;&nbsp; ",
        "D = Definition differs &nbsp;&middot;&nbsp; ",
        "U = Low reliability",
        "</div>"
      )
      paste0("<div style='overflow-x:auto;margin-top:6px;'>", header_html,
             paste(row_htmls, collapse=""), flag_legend, "</div>")
    }

    year_inputs_lookup <- setNames(
      lapply(unique(dat_tidy$measure), make_year_inputs),
      unique(dat_tidy$measure)
    )

    # Load prefill responses for this country (if available)
    country_iso <- credentials$country
    prefill_for_country <- country_prefill[[country_iso]]

    response_html_lookup <- setNames(
      lapply(unique(dat_tidy$measure), function(m) {
        if (!m %in% xlsx_measures || is.null(resp_by_indic[[m]]))
          return(list(oecd = "", country = ""))
        build_response_html(
          resp_by_indic[[m]],
          saved_resp   = session_data$responses[[m]],
          prefill_resp = if (!is.null(prefill_for_country)) prefill_for_country[[m]] else NULL
        )
      }),
      unique(dat_tidy$measure)
    )

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

    # Measures that already have at least one submitted country-average value
    submitted_measures <- names(entries)[vapply(names(entries), function(m) {
      ca <- entries[[m]][["country_avg"]]
      !is.null(ca) && any(vapply(ca, function(v) !is.null(v) && !is.na(v) && v != "", logical(1)))
    }, logical(1))]

    # Measures marked "no data update to declare"
    no_update_measures <- names(session_data$no_updates)[
      vapply(session_data$no_updates, isTRUE, logical(1))
    ]

    # Combined: measures that are "done" (either submitted or no-update)
    done_measures <- union(submitted_measures, no_update_measures)

    # ── Pipeline helper: build heatmap HTML ────────────────────────────────────
    # coverage_mode = TRUE  → show all measures, all read-only, no ⚠ badge
    # coverage_mode = FALSE → show only xlsx_measures, with data entry
    #   For EU-SILC countries, eu_silc_measures are excluded from submissions
    is_eu_silc_country <- credentials$country %in% eu_silc_countries

    build_heatmap_html <- function(coverage_mode) {

      # Prefix IDs to avoid duplicates between the two heatmaps
      id_prefix <- if (coverage_mode) "cov_" else "sub_"

      # Determine which measures to show on the submissions tab
      submission_measures <- if (!coverage_mode && is_eu_silc_country) {
        setdiff(xlsx_measures, eu_silc_measures)
      } else {
        xlsx_measures
      }

      base <- dat_tidy %>%
        { if (!coverage_mode) filter(., measure %in% submission_measures) else . } %>%
        mutate(time_period = as.numeric(time_period)) %>%
        left_join(submitted_df, by = c("measure", "time_period")) %>%
        { if (coverage_mode)
            left_join(., coverage_counts, by = c("measure", "time_period"))
          else
            mutate(., n_countries = NA_integer_)
        } %>%
        mutate(
          submitted = replace_na(submitted, FALSE),
          n_countries = replace_na(n_countries, 0L),
          # Fraction of countries with data (0-1), used for gap gradient
          n_frac = pmin(n_countries / max(n_total_countries, 1), 1),
          is_no_concern = measure %in% time_use_no_concern,
          # Pre-compute gradient color (amber #FDE8C8 → red #C0392B)
          gap_color = rgb(
            253 + (192 - 253) * n_frac,
            232 + ( 57 - 232) * n_frac,
            200 + ( 43 - 200) * n_frac,
            maxColorValue = 255
          ),
          color = case_when(
            !is.na(obs_value)                               ~ "#1F7A4D",
            submitted                                       ~ "#F89C1C",
            coverage_mode & is_no_concern & n_countries > 0 ~ "#FCE4B8",
            coverage_mode & n_countries > 0                 ~ gap_color,
            TRUE                                            ~ "#D9DDE3"
          ),
          tooltip = case_when(
            !is.na(obs_value)               ~ "",
            coverage_mode & n_countries > 0 ~ paste0(n_countries, " of ", n_total_countries, " countries have data"),
            TRUE                            ~ ""
          )
        ) %>%
        select(measure, time_period, color, tooltip, cat, group) %>%
        group_by(measure) %>%
        mutate(
          boxes = paste0(
            "<div style='flex:1;height:15px;background:", color,
            ";margin:1px;border-radius:2.5px;'",
            if_else(nchar(tooltip) > 0, paste0(" title='", tooltip, "'"), ""),
            "></div>",
            collapse = ""
          )
        ) %>%
        slice(1) %>%
        ungroup() %>%
        merge(dict %>% select(measure, label, question)) %>%
        left_join(defs_lookup, by = "measure") %>%
        arrange(cat) %>%
        mutate(
          # In coverage mode: all measures are read-only (even xlsx / time-use)
          needs_input    = if (coverage_mode) FALSE else measure %in% xlsx_measures,
          is_time_use    = if (coverage_mode) FALSE else measure %in% time_use_measures,
          safe_id        = paste0(id_prefix, gsub("\\.", "_", measure)),
          year_inputs    = unlist(year_inputs_lookup[measure]),
          year_chart     = unlist((if (coverage_mode) cov_charts_lookup else sub_charts_lookup)[measure]),
          oecd_q_html    = sapply(measure, function(m) response_html_lookup[[m]]$oecd),
          country_q_html = sapply(measure, function(m) response_html_lookup[[m]]$country),
          def_text       = linkify(replace_na(definition, "Definition to be added.")),
          tech_name      = replace_na(indicator,  "-"),
          unit_text      = replace_na(unit,       "-"),

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
          # Only show the "data update requested" flag on submissions, and only
          # while a measure has not yet been submitted. No badges in coverage mode.
          is_done = measure %in% done_measures,
          badge_html = case_when(
            coverage_mode ~ "",
            !needs_input  ~ "",
            is_done       ~ "<span style='font-size:9px;background:#1F7A4D;color:white;border-radius:3px;padding:1px 4px;white-space:nowrap;'>&#10003; Complete</span>",
            TRUE          ~ "<span title='New data required' style='font-size:9px;background:#F89C1C;color:white;border-radius:3px;padding:1px 4px;white-space:nowrap;'>&#9888; Awaiting data input</span>"
          ),
          panel_border = case_when(
            needs_input ~ "border-left:3px solid #F89C1C;",
            is_time_use ~ "border-left:3px solid #009EDB;",
            TRUE        ~ "border-left:3px solid #D4D9DF;"
          ),

          is_no_update = measure %in% no_update_measures,

          panel_body = mapply(function(ni, itu, sid, mn, yi, yc, q, oqh, cqh, def, tech, unt, lbl, is_nu) {
            if (ni) {
              nu_active <- if (is_nu) " active" else ""
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
                "<div style='margin-top:10px;display:flex;align-items:center;flex-wrap:wrap;gap:8px;'>",
                "<button onclick=\"submitMeasure('", sid, "','", mn, "')\" ",
                "style='background:#009EDB;color:white;border:none;padding:6px 16px;border-radius:4px;cursor:pointer;font-size:12px;font-weight:600;'>",
                "&#10003; Submit data</button>",
                "<span style='font-size:11px;color:#888;'>or</span>",
                "<button id='noupdate_", sid, "' onclick=\"declareNoUpdate('", sid, "','", mn, "')\" ",
                "class='no-update-btn", nu_active, "' ",
                "style='background:#f5f5f5;color:#555;border:1px solid #ccc;padding:6px 14px;border-radius:4px;cursor:pointer;font-size:11px;'>",
                "No data update to declare</button>",
                "<span id='status_", sid, "' style='margin-left:4px;font-size:11px;color:green;'></span>",
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
          oecd_q_html, country_q_html, def_text, tech_name, unit_text, label, is_no_update,
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
        mutate(
          section = if_else(cat <= 11, "Current Well-Being", "Future Well-Being"),

          group_html = vapply(group, function(grp) {
            icon_src <- group_icons[grp]
            icon_tag <- if (!is.na(icon_src))
              paste0("<img src='", icon_src, "' style='height:22px;width:22px;margin-right:7px;vertical-align:middle;object-fit:contain;'/>")
            else ""
            paste0("<div class='dim-header'><h4>", icon_tag, grp, "</h4></div>")
          }, character(1)),

          prev_section = lag(section, default = ""),
          section_div  = if_else(
            section != prev_section,
            paste0("<div class='wb-section-header'>",
                   if_else(section == "Current Well-Being",
                           "&nbsp; Current Well-Being",
                           "&nbsp; Future Well-Being"), "</div>"),
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
    # Data flags (B, E, P, etc.)
    if (!is.null(d$flags)) session_data$flags[[d$measure]] <- d$flags
    # Country Question Format responses are submitted alongside the data
    if (!is.null(d$responses)) session_data$responses[[d$measure]] <- d$responses
    runjs(paste0("
      var el = document.getElementById('status_", d$safe_id, "');
      if(el) { el.style.color = '#1F7A4D'; el.innerText = '\\u2713 Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })

  observeEvent(input$submitted_note, {
    d <- input$submitted_note
    session_data$notes[[d$measure]] <- d$note
    runjs(paste0("
      var el = document.getElementById('note_status_", d$safe_id, "');
      if(el) { el.innerText = '\\u2713 Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })

  observeEvent(input$no_update_declared, {
    d <- input$no_update_declared
    session_data$no_updates[[d$measure]] <- d$active
    runjs(paste0("
      var el = document.getElementById('status_", d$safe_id, "');
      if(el) {
        if(", tolower(d$active), ") {
          el.style.color = '#1F7A4D';
          el.innerText = '\\u2713 Marked as no update at ", format(Sys.time(), "%H:%M:%S"), "';
        } else {
          el.innerText = '';
        }
      }
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

  # ── Time-use no-update observer ────────────────────────────────────────────
  observeEvent(input$tu_no_update_declared, {
    d <- input$tu_no_update_declared
    session_data$tu_no_update <- d$active
    runjs(paste0("
      var el = document.getElementById('tu_no_update_status');
      if(el) {
        if(", tolower(d$active), ") {
          el.style.color = '#1F7A4D';
          el.innerText = '\\u2713 Marked as no update at ", format(Sys.time(), "%H:%M:%S"), "';
        } else {
          el.innerText = '';
        }
      }
    "))
  })

  # ── Tab completion badges ──────────────────────────────────────────────────
  observe({
    req(credentials$authenticated)

    entries    <- session_data$entries
    no_updates <- session_data$no_updates

    # Well-being: count remaining indicators
    submitted_measures <- names(entries)[vapply(names(entries), function(m) {
      ca <- entries[[m]][["country_avg"]]
      !is.null(ca) && any(vapply(ca, function(v) !is.null(v) && !is.na(v) && v != "", logical(1)))
    }, logical(1))]
    no_update_measures <- names(no_updates)[vapply(no_updates, isTRUE, logical(1))]

    is_eu_silc <- credentials$country %in% eu_silc_countries
    sub_measures <- if (is_eu_silc) setdiff(xlsx_measures, eu_silc_measures) else xlsx_measures
    done_wb <- union(submitted_measures, no_update_measures)
    remaining_wb <- length(setdiff(sub_measures, done_wb))

    # Time use: done if table 1 submitted OR no-update declared
    tu_done <- !is.null(session_data$time_use_1) || isTRUE(session_data$tu_no_update)

    wb_badge_js <- if (remaining_wb > 0) paste0("'", remaining_wb, "'") else "null"
    tu_badge_js <- if (!tu_done) "'!'" else "null"

    runjs(sprintf("
      setTimeout(function() {
        function setBadge(sel, text) {
          var tab = document.querySelector(sel);
          if (!tab) return;
          var b = tab.querySelector('.tab-badge');
          if (!b) { b = document.createElement('span'); b.className = 'tab-badge'; tab.appendChild(b); }
          if (text) {
            b.textContent = text;
            b.style.cssText = 'font-size:9px;background:#F89C1C;color:white;border-radius:8px;padding:1px 6px;margin-left:6px;font-weight:600;display:inline;';
          } else {
            b.style.display = 'none';
          }
        }
        setBadge('a[data-value=\"Well-being Data Submissions\"]', %s);
        setBadge('a[data-value=\"Time Use Data Submissions\"]', %s);
      }, 100);
    ", wb_badge_js, tu_badge_js))
  })

  # ── Admin ───────────────────────────────────────────────────────────────────
  admin_auth <- reactiveVal(FALSE)

  observeEvent(input$admin_logout_btn, {
    admin_auth(FALSE)
    updateTextInput(session, "login_password", value = "")
    output$login_error <- renderUI(NULL)
    # Reset login page to country mode
    runjs("
      var cr = document.getElementById('login_country_row');
      var al = document.getElementById('admin_link');
      var ab = document.getElementById('admin_back_link');
      var lt = document.getElementById('login_title');
      var ld = document.getElementById('login_desc');
      if(cr) cr.style.display = 'block';
      if(al) al.style.display = 'block';
      if(ab) ab.style.display = 'none';
      if(lt) lt.textContent = 'OECD Well-being and Time Use Questionnaire Portal';
      if(ld) ld.textContent = 'Select your country and enter the access password.';
      Shiny.setInputValue('login_mode', 'country');
    ")
    Sys.sleep(0.1)
    shinyjs::hide("admin_app")
    shinyjs::show("login_screen")
  })

  # Read all session files and assemble into tidy tables
  admin_all_data <- reactive({
    req(admin_auth())
    # Re-read every time the reactive fires (invalidated by table selection / filter)
    input$admin_country_filter
    input$admin_table_select

    rds_files <- list.files("sessions", pattern = "^[A-Z]{3}\\.rds$", full.names = TRUE)
    empty <- list(entries = data.frame(), flags = data.frame(),
                  notes = data.frame(), no_updates = data.frame(),
                  responses = data.frame(), tu1 = data.frame(),
                  tu2 = data.frame(), feedback = data.frame())
    if (length(rds_files) == 0) return(empty)

    all_entries    <- list()
    all_flags      <- list()
    all_notes      <- list()
    all_no_updates <- list()
    all_responses  <- list()
    all_tu1        <- list()
    all_tu2        <- list()

    for (f in rds_files) {
      iso <- sub("\\.rds$", "", basename(f))
      s   <- tryCatch(readRDS(f), error = function(e) NULL)
      if (is.null(s)) next
      cname <- countrycode::countrycode(iso, "iso3c", "country.name", warn = FALSE)

      # Entries
      if (!is.null(s$entries) && length(s$entries) > 0) {
        for (m in names(s$entries)) {
          row_data <- s$entries[[m]]
          if (!is.list(row_data)) next
          for (rk in names(row_data)) {
            yr_data <- row_data[[rk]]
            if (!is.list(yr_data)) next
            for (yr in names(yr_data)) {
              v <- yr_data[[yr]]
              if (!is.null(v) && !is.na(v) && v != "") {
                all_entries[[length(all_entries) + 1]] <-
                  data.frame(country = cname, iso = iso, measure = m,
                             row_type = rk, year = as.integer(yr),
                             value = as.numeric(v), stringsAsFactors = FALSE)
              }
            }
          }
        }
      }

      # Flags (B, E, P, etc.)
      if (!is.null(s$flags) && length(s$flags) > 0) {
        for (m in names(s$flags)) {
          flag_data <- s$flags[[m]]
          if (!is.list(flag_data)) next
          for (rk in names(flag_data)) {
            yr_data <- flag_data[[rk]]
            if (!is.list(yr_data)) next
            for (yr in names(yr_data)) {
              v <- yr_data[[yr]]
              if (!is.null(v) && nzchar(v)) {
                all_flags[[length(all_flags) + 1]] <-
                  data.frame(country = cname, iso = iso, measure = m,
                             row_type = rk, year = as.integer(yr),
                             flag = v, stringsAsFactors = FALSE)
              }
            }
          }
        }
      }

      # Notes
      if (!is.null(s$notes) && length(s$notes) > 0) {
        for (m in names(s$notes)) {
          n <- s$notes[[m]]
          if (!is.null(n) && nzchar(n)) {
            all_notes[[length(all_notes) + 1]] <-
              data.frame(country = cname, iso = iso, measure = m,
                         note = n, stringsAsFactors = FALSE)
          }
        }
      }

      # No updates
      if (!is.null(s$no_updates) && length(s$no_updates) > 0) {
        for (m in names(s$no_updates)) {
          if (isTRUE(s$no_updates[[m]])) {
            all_no_updates[[length(all_no_updates) + 1]] <-
              data.frame(country = cname, iso = iso, measure = m,
                         no_update = TRUE, stringsAsFactors = FALSE)
          }
        }
      }

      # Responses
      if (!is.null(s$responses) && length(s$responses) > 0) {
        for (m in names(s$responses)) {
          resp <- s$responses[[m]]
          if (!is.list(resp)) next
          for (idx in names(resp)) {
            val <- resp[[idx]]
            if (!is.null(val) && nzchar(val)) {
              all_responses[[length(all_responses) + 1]] <-
                data.frame(country = cname, iso = iso, measure = m,
                           question_index = as.integer(idx), response = val,
                           stringsAsFactors = FALSE)
            }
          }
        }
      }

      # Time-use helper
      parse_tu <- function(tu) {
        if (is.null(tu) || !is.list(tu)) return(data.frame())
        rows <- list()
        for (rk in names(tu)) {
          cols <- tu[[rk]]
          if (!is.list(cols)) next
          for (ck in names(cols)) {
            val <- cols[[ck]]
            if (!is.null(val) && nzchar(val)) {
              rows[[length(rows) + 1]] <-
                data.frame(country = cname, iso = iso,
                           row = as.integer(rk), col = ck, value = val,
                           stringsAsFactors = FALSE)
            }
          }
        }
        if (length(rows) > 0) bind_rows(rows) else data.frame()
      }

      tu1 <- parse_tu(s$time_use_1)
      tu2 <- parse_tu(s$time_use_2)
      if (nrow(tu1) > 0) all_tu1[[length(all_tu1) + 1]] <- tu1
      if (nrow(tu2) > 0) all_tu2[[length(all_tu2) + 1]] <- tu2
    }

    # Load feedback
    fb_file <- file.path("sessions", "feedback.rds")
    fb_df <- if (file.exists(fb_file)) {
      fb_list <- tryCatch(readRDS(fb_file), error = function(e) list())
      if (length(fb_list) > 0) {
        bind_rows(lapply(fb_list, function(fb) {
          cname <- countrycode::countrycode(fb$country, "iso3c", "country.name", warn = FALSE)
          data.frame(
            country   = if (!is.na(cname)) cname else fb$country,
            iso       = fb$country,
            timestamp = format(fb$timestamp, "%Y-%m-%d %H:%M:%S"),
            message   = fb$message,
            stringsAsFactors = FALSE
          )
        }))
      } else data.frame()
    } else data.frame()

    list(
      entries    = if (length(all_entries)    > 0) bind_rows(all_entries)    else data.frame(country = character(), iso = character(), measure = character(), row_type = character(), year = integer(), value = numeric()),
      flags      = if (length(all_flags)      > 0) bind_rows(all_flags)      else data.frame(country = character(), iso = character(), measure = character(), row_type = character(), year = integer(), flag = character()),
      notes      = if (length(all_notes)      > 0) bind_rows(all_notes)      else data.frame(country = character(), iso = character(), measure = character(), note = character()),
      no_updates = if (length(all_no_updates) > 0) bind_rows(all_no_updates) else data.frame(country = character(), iso = character(), measure = character(), no_update = logical()),
      responses  = if (length(all_responses)  > 0) bind_rows(all_responses)  else data.frame(country = character(), iso = character(), measure = character(), question_index = integer(), response = character()),
      tu1       = if (length(all_tu1)       > 0) bind_rows(all_tu1)       else data.frame(country = character(), iso = character(), row = integer(), col = character(), value = character()),
      tu2       = if (length(all_tu2)       > 0) bind_rows(all_tu2)       else data.frame(country = character(), iso = character(), row = integer(), col = character(), value = character()),
      feedback  = if (nrow(fb_df) > 0) fb_df else data.frame(country = character(), iso = character(), timestamp = character(), message = character())
    )
  })

  # Filtered view based on country + table selection
  admin_filtered <- reactive({
    req(admin_auth())
    all_data  <- admin_all_data()
    tbl_name  <- input$admin_table_select %||% "entries"
    country_f <- input$admin_country_filter %||% "ALL"

    df <- all_data[[tbl_name]]
    if (is.null(df) || nrow(df) == 0) return(df)
    if (country_f != "ALL") df <- df[df$iso == country_f, , drop = FALSE]
    df
  })

  output$admin_data_table <- DT::renderDataTable({
    df <- admin_filtered()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No submissions found."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    # Drop the iso column for display
    display_df <- df[, setdiff(names(df), "iso"), drop = FALSE]
    DT::datatable(display_df, rownames = FALSE, filter = "top",
                  options = list(pageLength = 25, scrollX = TRUE))
  })

  output$admin_download_csv <- downloadHandler(
    filename = function() {
      tbl  <- input$admin_table_select %||% "entries"
      iso  <- input$admin_country_filter %||% "ALL"
      paste0("submissions_", tbl, "_", iso, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- admin_filtered()
      if (is.null(df)) df <- data.frame()
      write.csv(df, file, row.names = FALSE)
    }
  )

  # ── Admin reset: two-click confirmation ────────────────────────────────────
  admin_reset_confirm <- reactiveVal(FALSE)

  observeEvent(input$admin_reset_btn, {
    if (!admin_reset_confirm()) {
      # First click: ask for confirmation
      admin_reset_confirm(TRUE)
      output$admin_reset_feedback <- renderUI(
        tags$div(
          style = "display:flex;align-items:center;gap:10px;",
          actionButton("admin_reset_confirm_btn", "Yes, delete everything",
                       style = "background:#E63312;color:white;border:none;font-weight:600;font-size:12px;"),
          actionButton("admin_reset_cancel_btn", "Cancel",
                       style = "font-size:12px;"),
          tags$span(style = "color:#E63312;font-size:12px;font-weight:600;",
                    "Are you sure? This cannot be undone.")
        )
      )
    }
  })

  observeEvent(input$admin_reset_cancel_btn, {
    admin_reset_confirm(FALSE)
    output$admin_reset_feedback <- renderUI(NULL)
  })

  observeEvent(input$admin_reset_confirm_btn, {
    req(admin_auth())
    # Delete all country session files
    rds_files <- list.files("sessions", pattern = "^[A-Z]{3}\\.rds$", full.names = TRUE)
    n_deleted <- 0
    for (f in rds_files) {
      tryCatch({ file.remove(f); n_deleted <- n_deleted + 1 },
               error = function(e) NULL)
    }
    admin_reset_confirm(FALSE)
    output$admin_reset_feedback <- renderUI(
      tags$p(style = "color:#1F7A4D;font-size:12px;font-weight:600;",
             paste0("\u2713 Deleted ", n_deleted, " session file(s) at ",
                    format(Sys.time(), "%H:%M:%S"), "."))
    )
  })

  # ── Feedback handler ─────────────────────────────────────────────────────────
  observeEvent(input$send_feedback_btn, {
    fb_text <- input$feedback_text
    if (is.null(fb_text) || !nzchar(trimws(fb_text))) {
      output$feedback_status <- renderUI(
        tags$span(style = "font-size:11px;color:#E63312;", "Please enter some feedback first."))
      return()
    }
    dir.create("sessions", showWarnings = FALSE)
    fb_file <- file.path("sessions", "feedback.rds")
    existing <- if (file.exists(fb_file)) tryCatch(readRDS(fb_file), error = function(e) list()) else list()
    existing[[length(existing) + 1]] <- list(
      country   = credentials$country %||% "unknown",
      timestamp = Sys.time(),
      message   = fb_text
    )
    saveRDS(existing, fb_file)
    # Clear the textarea
    runjs("document.getElementById('feedback_text').value = '';")
    output$feedback_status <- renderUI(
      tags$span(style = "font-size:11px;color:#1F7A4D;font-weight:600;",
                paste0("\u2713 Thank you! Feedback received at ", format(Sys.time(), "%H:%M:%S"), ".")))
  })

}

shinyApp(ui = ui, server = server)
