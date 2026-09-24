source("./global.R")

# ── Shared head & JS ─────────────────────────────────────────────────────────
shared_head <- tagList(
  useShinyjs(),
  tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
  tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
  tags$script(src = "https://cdn.jsdelivr.net/npm/echarts@5/dist/echarts.min.js"),
  tags$script(HTML(paste0(
    "window.__validRanges = ", jsonlite::toJSON(validation_ranges, auto_unbox = TRUE), ";\n",
    "window.__rowValidRanges = ", jsonlite::toJSON(row_validation_overrides, auto_unbox = TRUE), ";"
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

    // Validate .year-input values in a panel against measure/row ranges.
    // Most rows share the measure's default range, but a few rows (e.g. the
    // Deprivation row within all_rows_dep_vert measures) use a different
    // range regardless of measure - those take priority when present. Sets
    // red borders and an error status message on failure. Returns true if
    // all values are valid (or no range applies to this measure/row), false
    // otherwise. Shared by submitMeasure() and saveAndContinue() so both
    // enforce the same limits.
    function validateMeasureRanges(container, measure, safe_id) {
      var inputs = container.querySelectorAll('.year-input');
      var range = window.__validRanges && window.__validRanges[measure];
      var rowRanges = window.__rowValidRanges || {};
      if (!range && Object.keys(rowRanges).length === 0) return true;

      // Every offending box turns red, but the written explanation is
      // aggregated: one sentence per distinct expected range rather than one
      // per field. A second sentence therefore appears only when values breach
      // two genuinely different ranges (i.e. two different units).
      var nBad = 0;
      var groups = {};       // 'min-max' -> {min, max, n}
      var groupOrder = [];   // first-seen order, so the message is stable
      inputs.forEach(function(inp) {
        if (inp.value === '') return;
        var v = parseFloat(inp.value);
        if (isNaN(v)) return;
        var rowKey = inp.dataset.row || 'country_avg';
        var r = rowRanges[rowKey] || range;
        if (!r) { inp.style.border = '1px solid #ccc'; return; }
        if (v < r.min || v > r.max) {
          var key = r.min + '-' + r.max;
          if (!groups[key]) {
            groups[key] = { min: r.min, max: r.max, n: 0 };
            groupOrder.push(key);
          }
          groups[key].n++;
          nBad++;
          inp.style.border = '2px solid #E63312';
        } else {
          inp.style.border = '1px solid #ccc';
        }
      });
      if (nBad > 0) {
        var statusEl = document.getElementById('status_' + safe_id);
        if (statusEl) {
          var parts = groupOrder.map(function(k) {
            var g = groups[k];
            return g.n + (g.n === 1 ? ' value is' : ' values are') +
                   ' outside the expected range of ' + g.min + ' to ' + g.max;
          });
          statusEl.style.color = '#E63312';
          statusEl.innerText = parts.join('; ') +
            '. The affected fields are highlighted in red - please correct them before submitting.';
        }
        return false;
      }
      return true;
    }

    // Clear every value and flag in a panel's Enter Data grid. Deliberately
    // does NOT save: the clearance is only recorded once the user presses
    // Submit data (the server treats a blank cell as an explicit deletion).
    function clearAllInputs(safe_id) {
      var container = document.getElementById('inputs_' + safe_id);
      if (!container) return;
      var inputs = container.querySelectorAll('.year-input');
      var filled = 0;
      inputs.forEach(function(inp) { if (inp.value !== '') filled++; });
      if (filled === 0) return;
      if (!confirm('Clear all ' + filled + ' value(s) entered for this indicator?')) return;
      inputs.forEach(function(inp) {
        inp.value = '';
        inp.style.border = '1px solid #ccc';
      });
      container.querySelectorAll('.flag-select').forEach(function(sel) { sel.value = ''; });
      var st = document.getElementById('status_' + safe_id);
      if (st) {
        st.style.color = '#888';
        st.innerText = filled + ' value(s) cleared from the form. Nothing is saved yet - press Submit data to record the removal.';
      }
    }

    // Revert every value/flag in a panel's Enter Data grid back to the
    // existing OECD data (published, falling back to non-used) that was on
    // record before the country made any entries - i.e. each field's
    // data-default/data-default-flag, embedded when the panel was rendered.
    // Like Clear all, this is purely client-side: nothing is saved until
    // Submit data is pressed.
    function revertToDefaults(safe_id) {
      var container = document.getElementById('inputs_' + safe_id);
      if (!container) return;
      if (!confirm('Revert this indicator to the existing OECD data on record, discarding any values you have entered here?')) return;
      container.querySelectorAll('.year-input').forEach(function(inp) {
        var def = inp.dataset.default;
        inp.value = (def === undefined) ? '' : def;
        inp.style.border = '1px solid #ccc';
      });
      container.querySelectorAll('.flag-select').forEach(function(sel) {
        sel.value = sel.dataset.defaultFlag || '';
      });
      var st = document.getElementById('status_' + safe_id);
      if (st) {
        st.style.color = '#888';
        st.innerText = 'Reverted to the existing OECD data. Nothing is saved yet - press Submit data to record the change.';
      }
    }

    function submitMeasure(safe_id, measure) {      var container = document.getElementById('inputs_' + safe_id);
      if (!validateMeasureRanges(container, measure, safe_id)) return;
      if (!checkBreakdownConsistency(container, measure, safe_id)) return;
      var inputs = container.querySelectorAll('.year-input');

      var values = {};
      inputs.forEach(function(inp) {
        var row = inp.dataset.row || 'country_avg';
        var yr  = inp.dataset.year;
        if (!values[row]) values[row] = {};
        // Send an explicit empty string (not null) for a blank cell so the
        // server can tell explicitly-cleared apart from never-touched;
        // otherwise a cleared field falls back to the published value.
        values[row][yr] = inp.value === '' ? '' : parseFloat(String(inp.value).replace(',', '.'));
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
      // The 'Age groups differ?' note is free text, so it is sent on its own
      // rather than through values, which is parsed as numbers.
      var ageEl = container.querySelector('.age-note-input');
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
          age_note: ageEl ? ageEl.value : null,
          timestamp: new Date().toISOString() },
        {priority: 'event'});
      // Optimistic feedback: the server rebuild takes a moment, so acknowledge
      // the click immediately rather than leaving the button looking dead.
      var st = document.getElementById('status_' + safe_id);
      if (st) { st.style.color = '#009EDB'; st.innerText = '\u23F3 Saving\u2026'; }
      markBusy(true);
    }

    // Dim the heatmap while the server rebuilds it. Always self-clears via a
    // timeout so a server-side error can never leave the UI stuck grey.
    var __busyTimer = null;
    function markBusy(on) {
      ['heatmap_submissions', 'heatmap_coverage'].forEach(function(id) {
        var el = document.getElementById(id);
        if (!el) return;
        el.style.transition = 'opacity 0.15s ease';
        el.style.opacity = on ? '0.55' : '1';
      });
      document.body.style.cursor = on ? 'progress' : '';
      if (__busyTimer) { clearTimeout(__busyTimer); __busyTimer = null; }
      if (on) __busyTimer = setTimeout(function() { markBusy(false); }, 4000);
    }

    $(document).on('shiny:idle shiny:error shiny:disconnected', function() {
      markBusy(false);
    });

  ")),
  # Separate <script> tag, for the same 10000-character reason noted below.
  tags$script(HTML("
    // Non-blocking plausibility check run by submitMeasure() after the
    // blocking range validation. Warns when a demographic breakdown value is
    // wildly different from the country average for the same year (gap above
    // 40% of the measure's allowed range span). The vert/dep rows are
    // different concepts from the country average, so they are skipped.
    // The user can always dismiss the warning and submit anyway.
    function checkBreakdownConsistency(container, measure, safe_id) {
      var range = window.__validRanges && window.__validRanges[measure];
      if (!range) return true;
      var threshold = 0.4 * (range.max - range.min);
      var avg = {};
      container.querySelectorAll('.year-input').forEach(function(inp) {
        if ((inp.dataset.row || 'country_avg') !== 'country_avg') return;
        var v = parseFloat(String(inp.value).replace(',', '.'));
        if (!isNaN(v)) avg[inp.dataset.year] = v;
      });
      var offenders = [];
      container.querySelectorAll('.year-input').forEach(function(inp) {
        var row = inp.dataset.row || 'country_avg';
        if (row === 'country_avg' || row === 'vert' || row === 'dep') return;
        var a = avg[inp.dataset.year];
        if (a === undefined) return;
        var v = parseFloat(String(inp.value).replace(',', '.'));
        if (isNaN(v)) return;
        if (Math.abs(v - a) > threshold) offenders.push(inp);
      });
      if (offenders.length === 0) return true;
      offenders.forEach(function(inp) { inp.style.border = '2px solid #F89C1C'; });
      var st = document.getElementById('status_' + safe_id);
      if (st) {
        st.style.color = '#B26A00';
        st.innerText = offenders.length + ' value' +
          (offenders.length === 1 ? ' differs' : 's differ') +
          ' substantially from the country average for the same year (highlighted in orange). Please double-check them.';
      }
      return confirm('Some breakdown values look very different from the country average for the same year - are you sure they are correct? Press OK to submit anyway, or Cancel to go back and review the highlighted fields.');
    }
  ")),
  # Continued in a new <script> tag, for the same 10000-character reason
  # noted below.
  tags$script(HTML("
    // Collect the current state of a measure panel without client-side range
    // validation. Used by the Save and continue button so work-in-progress
    // values can be stored (and the heatmap refreshed) without the stricter
    // checks submitMeasure() applies.
    function collectMeasure(safe_id, measure) {
      var container = document.getElementById('inputs_' + safe_id);
      if (!container) return null;
      var values = {};
      container.querySelectorAll('.year-input').forEach(function(inp) {
        var row = inp.dataset.row || 'country_avg';
        var yr  = inp.dataset.year;
        if (!values[row]) values[row] = {};
        // Send an explicit empty string (not null) for a blank cell so the
        // server can tell explicitly-cleared apart from never-touched;
        // otherwise a cleared field falls back to the published value.
        values[row][yr] = inp.value === '' ? '' : parseFloat(String(inp.value).replace(',', '.'));
      });
      var flags = {};
      container.querySelectorAll('.flag-select').forEach(function(sel) {
        var row = sel.dataset.row || 'country_avg';
        var yr  = sel.dataset.year;
        if (!flags[row]) flags[row] = {};
        if (sel.value !== '') flags[row][yr] = sel.value;
      });
      var ageEl = container.querySelector('.age-note-input');
      var panel = document.getElementById('panel_' + safe_id);
      var responses = {};
      if (panel) {
        panel.querySelectorAll('.resp-input').forEach(function(inp) {
          responses[inp.dataset.idx] = inp.value;
        });
      }
      return { measure: measure, safe_id: safe_id, values: values,
               flags: flags, responses: responses,
               age_note: ageEl ? ageEl.value : null,
               timestamp: new Date().toISOString() };
    }

    // Save and continue: user-triggered draft save. Conserves the panel's
    // current values and refreshes the heatmap exactly like Submit, but
    // does not mark the indicator complete. Replaces the old auto-save.
    // Enforces the same range validation as Submit - an out-of-range value
    // should never be silently drafted.
    function saveAndContinue(safe_id, measure) {
      var container = document.getElementById('inputs_' + safe_id);
      if (!container) return;
      if (!validateMeasureRanges(container, measure, safe_id)) return;
      var payload = collectMeasure(safe_id, measure);
      if (!payload) return;
      Shiny.setInputValue('saved_draft_data', payload, {priority: 'event'});
      var el = document.getElementById('status_' + safe_id);
      if (el) { el.style.color = '#888'; el.innerText = 'Saving\u2026'; }
    }
  ")),
  # Split into a second <script> tag: a single R string literal containing
  # \\uXXXX escapes is capped at 10000 characters, and the block above is
  # already close to that limit.
  tags$script(HTML("
    function collectTable(table_id) {
      var container = document.getElementById(table_id);
      if (!container) return null;
      var rows = container.querySelectorAll('tr[data-row]');
      var data = {};
      rows.forEach(function(row) {
        var r = row.dataset.row; data[r] = {};
        row.querySelectorAll('input').forEach(function(inp) {
          data[r]['c' + inp.dataset.col] = inp.value;
        });
      });
      return data;
    }

    // Draft save for a single time use table. Stores progress without
    // marking the time use submission as complete.
    function saveTable(table_id) {
      Shiny.setInputValue('saved_table',
        { table: table_id, data: collectTable(table_id),
          timestamp: new Date().toISOString() },
        {priority: 'event'});
    }

    // Final submission of step 3: survey details plus both tables. The
    // survey name and year are required, so they are checked here for
    // immediate feedback and again on the server.
    function submitTimeUseTables() {
      var nm = document.getElementById('tu_survey_name');
      var yr = document.getElementById('tu_survey_year');
      var st = document.getElementById('tu_submit_all_status');
      var missing = [];
      if (!nm || nm.value.trim() === '') missing.push('survey name');
      if (!yr || yr.value.toString().trim() === '') missing.push('latest survey year');
      if (missing.length > 0) {
        if (st) {
          st.style.color = '#E63312';
          st.innerText = '\u26A0 Please fill in the ' + missing.join(' and ') + ' above before submitting.';
        }
        [nm, yr].forEach(function(el) {
          if (el && el.value.toString().trim() === '') el.style.border = '2px solid #E63312';
        });
        if (nm && nm.value.trim() === '') nm.focus();
        else if (yr) yr.focus();
        return;
      }
      [nm, yr].forEach(function(el) { if (el) el.style.border = ''; });
      if (st) { st.style.color = '#009EDB'; st.innerText = '\u231B Submitting\u2026'; }
      Shiny.setInputValue('tu_submit_all',
        { t1: collectTable('tu_table1'), t2: collectTable('tu_table2'),
          timestamp: new Date().toISOString() },
        {priority: 'event'});
    }

  ")),
  # Third <script> tag, for the same 10000-character reason as above.
  tags$script(HTML("
    // Jump to the Well-being Data Coverage tab; used by the landing-hero link.
    function goToCoverageTab() {
      var el = document.querySelector('#main_navbar a[data-value=\"Well-being Data Coverage\"]');
      if (el) el.click();
      return false;
    }

    function submitNote(safe_id, measure) {
      var el = document.getElementById('note_' + safe_id);
      Shiny.setInputValue('submitted_note',
        { measure: measure, safe_id: safe_id, note: el ? el.value : '', timestamp: new Date().toISOString() },
        {priority: 'event'});
      // A saved, non-empty note satisfies the revision prompt.
      if (el && el.value.trim() !== '') clearRevisionNote(safe_id);
    }

    // Prompt the user to explain changed figures in the note box. Shown only
    // after a Submit that actually altered one or more values.
    function flagRevisionNote(safe_id, n) {
      var box = document.getElementById('revnote_' + safe_id);
      var ta  = document.getElementById('note_' + safe_id);
      if (box) {
        box.style.display = 'block';
        box.innerHTML = '<strong>&#9888; ' + n + ' figure' + (n === 1 ? '' : 's') +
          ' changed.</strong> Please briefly explain the revision in the box below ' +
          '(e.g. break in series, methodological change, revised source).';
      }
      if (ta) {
        ta.style.border = '2px solid #F89C1C';
        if (ta.value.trim() === '') {
          ta.placeholder = 'Please explain what changed and why\u2026';
        }
      }
    }

    function clearRevisionNote(safe_id) {
      var box = document.getElementById('revnote_' + safe_id);
      var ta  = document.getElementById('note_' + safe_id);
      if (box) box.style.display = 'none';
      if (ta) ta.style.border = '1px solid #ccc';
    }

    function declareNoUpdate(safe_id, measure) {
      var btn = document.getElementById('noupdate_' + safe_id);
      var isActive = btn.classList.toggle('active');
      Shiny.setInputValue('no_update_declared',
        { measure: measure, safe_id: safe_id, active: isActive,
          timestamp: new Date().toISOString() },
        {priority: 'event'});
      var st = document.getElementById('status_' + safe_id);
      if (st && isActive) { st.style.color = '#009EDB'; st.innerText = '\u23F3 Saving\u2026'; }
      markBusy(true);
    }

    function declareTUNoUpdate() {
      var btn = document.getElementById('tu_no_update_btn');
      var isActive = btn.classList.toggle('active');
      Shiny.setInputValue('tu_no_update_declared',
        { active: isActive, timestamp: new Date().toISOString() },
        {priority: 'event'});
    }

    function declareTUTableNoUpdate(tableNum) {
      var btn = document.getElementById('tu_table' + tableNum + '_no_update_btn');
      var isActive = btn.classList.toggle('active');
      Shiny.setInputValue('tu_table_no_update_declared',
        { table: tableNum, active: isActive, timestamp: new Date().toISOString() },
        {priority: 'event'});
    }
  ")),
  tags$script(HTML("
    // ── Paste-from-spreadsheet modal for time use tables ──────────
    var __pasteTarget = null;
    var __pasteTextCols = 0;
    var __pasteTotalCols = 0;

    function openPasteModal(tableId, nTextCols, nTotalCols) {
      __pasteTarget = tableId;
      __pasteTextCols = nTextCols;
      __pasteTotalCols = nTotalCols;
      var nNumCols = nTotalCols - nTextCols;
      document.getElementById('paste_modal_textarea').value = '';
      document.getElementById('paste_modal_status').innerText = '';
      document.getElementById('paste_modal_hint').innerText =
        'Paste ' + nNumCols + ' numeric column(s) of data, one row per line, values separated by tabs. ' +
        'Rows should match the table order (skip header/divider rows).';
      document.getElementById('paste_modal_overlay').style.display = 'flex';
    }

    function closePasteModal() {
      document.getElementById('paste_modal_overlay').style.display = 'none';
      __pasteTarget = null;
    }

    function applyPastedData() {
      var text = document.getElementById('paste_modal_textarea').value.trim();
      if (!text || !__pasteTarget) return;

      var table = document.getElementById(__pasteTarget);
      if (!table) return;

      var lines = text.split(/\\r?\\n/).filter(function(l) { return l.trim() !== ''; });
      var dataRows = table.querySelectorAll('tr[data-row]');
      // Filter to non-divider rows (those with input elements)
      var editableRows = [];
      dataRows.forEach(function(row) {
        if (row.querySelector('input')) editableRows.push(row);
      });

      var nFilled = 0;
      var nNumCols = __pasteTotalCols - __pasteTextCols;

      for (var i = 0; i < lines.length && i < editableRows.length; i++) {
        var cells = lines[i].split('\t');
        // User might paste all columns (incl. text) or just numeric columns
        var numValues;
        if (cells.length >= __pasteTotalCols) {
          // Pasted full row including text columns - take only numeric part
          numValues = cells.slice(__pasteTextCols, __pasteTotalCols);
        } else {
          // Assume only numeric columns were pasted
          numValues = cells.slice(0, nNumCols);
        }

        var inputs = editableRows[i].querySelectorAll('input:not([readonly])');
        for (var j = 0; j < numValues.length && j < inputs.length; j++) {
          var val = numValues[j].trim();
          if (val !== '') {
            inputs[j].value = val;
            inputs[j].dispatchEvent(new Event('input', {bubbles: true}));
            nFilled++;
          }
        }
      }

      var statusEl = document.getElementById('paste_modal_status');
      statusEl.style.color = '#1F7A4D';
      statusEl.innerText = '\u2713 Populated ' + nFilled + ' cells across ' +
        Math.min(lines.length, editableRows.length) + ' rows.';
      setTimeout(closePasteModal, 1200);
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

    // Yes/No toggle: set hidden value and highlight choice.
    function setToggle(inputId, btn, val) {
      var input = document.getElementById(inputId);
      if (input) input.value = val;
      var group = btn.parentNode;
      group.querySelectorAll('.toggle-btn').forEach(function(b) { b.classList.remove('active'); });
      btn.classList.add('active');
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

    // ── Position fixed tooltips on hover ──────────────────────────
    document.addEventListener('mouseenter', function(e) {
      var trigger = e.target.closest('.info-tooltip');
      if (!trigger) return;
      var tip = trigger.querySelector('.tooltip-text');
      if (!tip) return;
      var rect = trigger.getBoundingClientRect();
      tip.style.left = Math.max(8, rect.left - 140) + 'px';
      tip.style.top  = (rect.top - tip.offsetHeight - 8) + 'px';
    }, true);
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
        cell.textContent = isNaN(val) ? '-' : parseFloat(val.toFixed(2)).toString();
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
    tags$span(style = "display:inline-block;width:11px;height:11px;background:#009EDB;border-radius:2px;margin-right:5px;vertical-align:middle;"),
    "Revision of existing data"
  ),
  tags$span(
    tags$span(style = "display:inline-block;width:11px;height:11px;background:#B4530A;border-radius:2px;margin-right:5px;vertical-align:middle;"),
    "Submitted this session, then revised"
  ),
  tags$span(
    tags$span(style = "display:inline-block;width:11px;height:11px;background:#C4B5D4;border-radius:2px;margin-right:5px;vertical-align:middle;"),
    "Previously submitted (not used)"
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
.oecd_choices <- setNames(oecd_countries, oecd_names)
.partner_choices <- setNames(partner_countries, partner_names)
login_country_choices <- list(
  "Select your country" = "",
  "OECD countries" = as.list(.oecd_choices[order(names(.oecd_choices))]),
  "Accession countries" = as.list(.partner_choices[order(names(.partner_choices))])
)

# ── Related-resource card ─────────────────────────────────────────────────────
# The whole card is the link, so the entire box is clickable.
resource_card <- function(href, title, desc, star = FALSE) {
  tags$a(
    class = paste("wb-resource-card", if (star) "wb-resource-card-featured"),
    href = href, target = "_blank",
    tags$span(class = "wb-resource-card-title",
              paste0(if (star) "\u2B50 " else "\U0001F517 ", title)),
    tags$p(desc)
  )
}

# Deadline reminder banner shown at the top of both submission tabs.
deadline_banner <- tags$div(
  style = paste0(
    "background:#FEF5E7;border:1px solid #F89C1C;border-left:5px solid #F89C1C;",
    "border-radius:8px;padding:12px 18px;margin:0 0 18px;",
    "font-size:13px;color:#1F2B3A;"
  ),
  tags$b("\u23F0 Reminder:"),
  " please complete and submit your data by ",
  tags$b("Friday 16 October 2026."), 
  " Once your submission is final, please notify ",
  tags$a(href = "mailto:kate.chalmers@oecd.org", "kate.chalmers@oecd.org"),
  " so that your submission can be frozen - this protects your finalized data from any further changes."
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- tagList(
  shared_head,

  # ── Paste-from-spreadsheet modal overlay ─────────────────────────────────
  tags$div(
    id = "paste_modal_overlay",
    style = paste0(
      "display:none;position:fixed;top:0;left:0;width:100%;height:100%;z-index:10000;",
      "background:rgba(0,0,0,0.45);align-items:center;justify-content:center;"
    ),
    tags$div(
      style = paste0(
        "background:#fff;border-radius:10px;padding:28px 32px;width:560px;max-width:92vw;",
        "box-shadow:0 8px 32px rgba(0,0,0,0.2);"
      ),
      tags$div(
        style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;",
        tags$h4(style = "margin:0;font-size:15px;color:#1F2B3A;", "Paste data from spreadsheet"),
        tags$button(onclick = "closePasteModal()",
          style = "background:none;border:none;font-size:20px;cursor:pointer;color:#888;padding:0;line-height:1;",
          "\u00D7")
      ),
      tags$p(id = "paste_modal_hint",
        style = "font-size:11px;color:#666;margin:0 0 10px;line-height:1.5;"),
      tags$textarea(id = "paste_modal_textarea",
        style = paste0(
          "width:100%;height:200px;font-size:11px;font-family:monospace;",
          "border:1px solid #ccc;border-radius:6px;padding:10px;box-sizing:border-box;",
          "resize:vertical;"
        ),
        placeholder = "Select your data in Excel, copy (Ctrl+C / Cmd+C), then paste here (Ctrl+V / Cmd+V)..."
      ),
      tags$div(
        style = "display:flex;align-items:center;gap:12px;margin-top:12px;",
        tags$button(onclick = "applyPastedData()",
          style = paste0(
            "background:#009EDB;color:white;border:none;padding:7px 20px;border-radius:5px;",
            "cursor:pointer;font-size:12px;font-weight:600;"
          ),
          "\u2713 Apply to table"),
        tags$button(onclick = "closePasteModal()",
          style = "background:#f5f5f5;color:#555;border:1px solid #ccc;padding:7px 16px;border-radius:5px;cursor:pointer;font-size:12px;",
          "Cancel"),
        tags$span(id = "paste_modal_status",
          style = "font-size:11px;font-weight:600;")
      )
    )
  ),

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
        style = "text-align:center;margin-top:16px;position:relative;z-index:10;",
        tags$a(href = "#", onclick = "toggleAdminLogin(); return false;",
               style = "font-size:11px;color:#888;text-decoration:none;",
               "Admin access \u2192")
      ),
      tags$div(
        id = "admin_back_link",
        style = "text-align:center;margin-top:16px;display:none;position:relative;z-index:10;",
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
#        actionLink("change_pw_modal_btn", label = NULL, icon = icon("gear"),
#                   title = "Change password"),
        uiOutput("save_status_ui", inline = TRUE),
        actionLink("logout_btn", label = NULL, icon = icon("right-from-bracket"),
                   title = "Log out")
      ),
      uiOutput("freeze_banner"),
      navbarPage(
        title = uiOutput("nav_title", inline = TRUE),
        id    = "main_navbar",

        # ── Tab 1: Data Submissions ────────────────────────────────────────
        tabPanel("Well-being Data Submissions",
          fluidPage(
            tags$div(class = "wb-page",

              deadline_banner,

              # ── Header: who's logged in + purpose, folded into one card ──
              tags$div(class = "landing-hero",
                tags$div(class = "landing-logo-row",
                  img(src = "wise_logo.png", height = 60),
                  img(src = "OECD_logo.svg", height = 36)
                ),
                uiOutput("country_status"),
                tags$p(style = "margin-bottom: 15px", HTML(
                  "This questionnaire gathers national data on well-being in OECD Member and accession countries for the OECD Well-being Database and
                  associated products, including the How's Life? publication series, the Well-being Data Monitor,
                  and the annual well-being country profiles.
                  <br><br>
                  The OECD Well-being Database includes over 80 indicators, most sourced from other OECD and external data collections. The full set may be viewed on the <a href='#' onclick='return goToCoverageTab();'>Well-being Data Coverage</a> tab.
                  <b>To streamline the data collection process, this questionnaire covers only a subset of these indicators unique to the OECD Well-being Database, plus relevant
                  Time Use information, and that are not managed through other OECD or external data collection activities.</b> 
                  All official surveys are welcome as sources, including but not limited to household, health, general social, time-use, and ad hoc surveys."
                )),
                tags$p(class = "wb-resource-heading", "Reference material"),
                tags$div(class = "wb-resource-grid",
                  resource_card("https://www.oecd.org/content/dam/oecd/en/topics/policy-sub-issues/measuring-well-being-and-progress/oecd-well-being-database-definitions.pdf",
                                "OECD Well-being Metadata",
                                "Metadata and indicators definitions for every indicator in the OECD Well-being Database.",
                                star = TRUE),
                  resource_card("https://www.oecd.org/wise/measuring-well-being-and-progress.htm",
                                "The OECD's Well-being Framework",
                                "Background on how the OECD defines and measures well-being.")
                ),
                tags$p(class = "wb-resource-heading", "Where the data is featured"),
                tags$div(class = "wb-resource-grid",
                  resource_card("http://data-explorer.oecd.org/s/fu",
                                "Well-being Database",
                                "Browse and download every published well-being indicator in the OECD Data Explorer."),
                  resource_card("http://data-explorer.oecd.org/s/177",
                                "Time Use Database",
                                "Browse and download published time-use indicators in the OECD Data Explorer."),
                  resource_card("https://www.oecd.org/en/publications/serials/how-s-life_g1g317ee.html",
                                "How's Life?",
                                "OECD flagship publication series reporting on well-being across countries."),
                  resource_card("https://www.oecd.org/en/data/tools/well-being-data-monitor.html",
                                "Well-being Data Monitor",
                                "Interactive tool for exploring well-being data across countries and indicators."),
                  resource_card("https://github.com/wise-oecd/data_monitor/tree/main/country%20profiles",
                                "Well-being country profiles",
                                "Annually updated profiles summarising well-being outcomes by country.")

              )
              ),
              # ── Step 1: quick guide ───────────────────────────────────
              tags$div(class = "wb-card",
                tags$p(class = "wb-card-title",
                       tags$span(class = "wb-step-num", "1"), "How it works"),
                tags$ul(class = "wb-guide-list",
                  tags$li("Each indicator is a row in the heatmap. Click a row to expand its panel and enter values. Data are entered manually, or in bulk with the optional Excel template below (quickest if you have many values). See the legend for what each cell colour means."),
                  tags$li(tags$b("Save and continue"), "stores a draft without marking the indicator complete; drafts are restored on your next login.",
                          tags$b("\u2713 Submit"), "saves the values and updates the heatmap. Only Submit changes the heatmap."),
                  tags$li("Fields are pre-filled with previous submissions. You can overwrite or re-submit as often as needed before the deadline."),
                  tags$li("Indicators marked", tags$span(style = "font-size:9px;background:#F89C1C;color:white;border-radius:3px;padding:1px 4px;", "\u26A0 Awaiting data input"),
                          "still need your input; those marked",
                          tags$span(style = "font-size:9px;background:#009EDB;color:white;border-radius:3px;padding:1px 4px;", "\u231B Awaiting Time Use submission"),
                          "turn complete once the", tags$b("Time Use"), "tab is submitted."),
                  tags$li("Please enter the data values themselves in the portal. Links to a national database or publication are welcome as a source in the",
                          tags$b("Other useful information"), "box, but a link on its own cannot be processed."),
                  tags$li("The portal cannot accept attachments. Email supporting documents (methodological notes, questionnaires, publications) to",
                          tags$a(href = "mailto:kate.chalmers@oecd.org", "kate.chalmers@oecd.org"),
                          "and mention them in the relevant comments box so we can match them to your submission."),
                  tags$li("The portal collects annual figures only. If", tags$b("quarterly or monthly data"),
                          "exists for an indicator, please flag this in the", tags$b("Other useful information"),
                          "box and attach the data to your final submission email."),
                  tags$li("When flagging data, choose the", tags$b("single most important flag"),
                          "and note any others in the", tags$b("Other useful information"), "box. See the",
                          tags$a(href = "https://sdmx.org/wp-content/uploads/CL_OBS_STATUS_v2_3-for-publication.docx",
                                 target = "_blank", "SDMX observation status guidelines"), "for reference."),
                  tags$li("Provide metadata (survey names, question wording) in English where possible, or the official name in the original language if no translation exists.")
                )
              ),
              # ── Step 2: bulk upload / download ────────────────────────
              tags$div(class = "wb-card", style = "text-align:center;",
                # Clickable header (always visible)
                tags$div(
                  onclick = "var body=document.getElementById('template_body'); var arrow=document.getElementById('template_arrow'); if(body.style.display==='none'){body.style.display='block';arrow.textContent='\\u25B2';}else{body.style.display='none';arrow.textContent='\\u25BC';}",
                  style = "cursor:pointer;display:flex;align-items:center;justify-content:center;gap:8px;",
                  tags$p(class = "wb-card-title", style = "margin:0;",
                         tags$span(class = "wb-step-num", "2"), "Bulk data entry (optional)"),
                  tags$span(id = "template_arrow",
                    style = "font-size:10px;color:#8a9bae;",
                    "\u25BC"
                  )
                ),
                # Collapsible body (hidden by default)
                tags$div(
                  id = "template_body",
                  style = "display:none;padding-top:14px;",
                  tags$p(
                    style = "font-size:11px;color:#55606B;margin:0 0 6px;line-height:1.5;",
                    "This template is meant to help speed up data entry for this portal. It is not an alternative to filling out the portal, and there is no need to send it to us separately.", br(),
                    "It is particularly useful when you have many values to enter, wish to populate the portal programmatically, or copy directly from existing tables. You can also use it to keep a record of the data you submit."
                  ),
                  tags$div(
                    style = "text-align:left;font-size:11px;color:#55606B;margin:0 0 12px;line-height:1.6;max-width:760px;margin-left:auto;margin-right:auto;",
                    tags$p(style = "font-weight:600;margin:0 0 3px;color:#1F2B3A;font-size:11px;", "How to use:"),
                    tags$ol(style = "margin:0;padding-left:18px;",
                      tags$li("Click ", tags$b("Download template"), " to get an Excel file tailored to your country."),
                      tags$li("Each sheet corresponds to one indicator. Enter numeric values in the year columns (2004 onwards). Leave cells blank where no data is available."),
                      tags$li("Do not modify the ", tags$code("breakdown_key"), " column (column A). This is used to match your data to the correct breakdown rows in the portal."),
                      tags$li("Save the file and click ", tags$b("Upload completed template"), " to auto-fill the portal fields."),
                      tags$li("You can review and adjust any values in the portal after uploading. Data flags can also be set in the portal.")
                    )
                  ),
                  tags$div(
                    style = "display:flex;align-items:center;justify-content:center;gap:16px;",
                    downloadButton("dl_wb_template", "Download template",
                      style = paste0(
                        "background:#8a9bae;color:white;border:none;padding:8px 20px;border-radius:5px;",
                        "font-size:12px;font-weight:600;cursor:pointer;white-space:nowrap;",
                        "line-height:1;height:34px;box-sizing:border-box;vertical-align:middle;"
                      )),
                    tags$div(
                      class = "upload-btn-wrap",
                      fileInput("upload_wb_file", label = NULL, accept = ".xlsx",
                                buttonLabel = tagList(icon("upload"), "Upload completed template"),
                                placeholder = "No file selected",
                                width = "auto")
                    )
                  ),
                  # Upload feedback sits on its own line beneath the buttons
                  tags$div(
                    id = "upload_wb_status",
                    style = paste0(
                      "display:none;margin:12px auto 0;max-width:780px;text-align:left;",
                      "font-size:11px;font-weight:600;line-height:1.5;word-break:break-word;",
                      "padding:8px 12px;border-radius:6px;background:#f2f7f4;border:1px solid #d7e5dd;"
                    )
                  )
                )
              ),

              # ── Step 3: the data itself ────────────────────────────────
              tags$div(class = "wb-card wb-card-wide",
                tags$p(class = "wb-card-title",
                       tags$span(class = "wb-step-num", "3"), "Enter your data"),
                tags$div(class = "heatmap-content", style = "text-align:center;",
                  heatmap_legend,
                  shinycssloaders::withSpinner(uiOutput("heatmap_submissions"))
                )
              )
            )
          )
        ),

        # ── Tab 3: Time Use ────────────────────────────────────────────────
        tabPanel("Time Use Data Submissions",
          fluidPage(
            tags$div(class = "wb-page",

              # ── Header: who's logged in + purpose, folded into one card ──
              tags$div(class = "landing-hero",
                tags$div(class = "landing-logo-row",
                  img(src = "wise_logo.png", height = 60),
                  img(src = "OECD_logo.svg", height = 36)
                ),
                uiOutput("tu_country_status"),
                tags$p(HTML(
                  "This section collects detailed time use data from national time use surveys, for the OECD Time Use
                  Database and related well-being products. Table 1 gathers time spent on daily activities
                  (in minutes per day); Table 2 asks you to map your national activity codes to the OECD classification."
                )),
                tags$p(class = "wb-resource-heading", "Reference material and databases"),
                tags$div(class = "wb-resource-grid",
                  resource_card("http://data-explorer.oecd.org/s/177",
                                "Time Use Database",
                                "Browse and download published time-use indicators in the OECD Data Explorer."),
                  resource_card("https://www.oecd.org/en/data/datasets/time-use-database.html",
                                "Time Use Database (dataset page)",
                                "Download a more detailed breakdown of daily activities."),
                  resource_card("http://data-explorer.oecd.org/s/fu",
                                "Well-being Database",
                                "Time use indicators also feed into the OECD Well-being Database and How's Life? reporting.")
                )
              ),

              # ── Step 1: quick guide ───────────────────────────────────
              tags$div(class = "wb-card",
                tags$p(class = "wb-card-title",
                       tags$span(class = "wb-step-num", "1"), "How it works"),
                tags$ul(class = "wb-guide-list",
                  tags$li("Table 1 collects time spent on daily activities in",
                          tags$b("minutes per day"), "for the total population (15\u201364), men, and women."),
                  tags$li("Group subtotals and the grand total are",
                          tags$b("calculated automatically"), "from the values you enter."),
                  tags$li("Subtotal rows (e.g. 2.3 Care for household members) are",
                          tags$b("auto-summed"), "from their sub-categories (2.3.1 + 2.3.2)."),
                  tags$li("The daily total should sum to",
                          tags$b("1440 minutes"), "(24 hours). If it differs, you will be asked to provide a brief explanation."),
                  tags$li("Table 2 asks you to map your national activity codes to each OECD activity category."),
                  tags$li("Survey details, Table 1 and Table 2 are all part of", tags$b("step 3"),
                          "and are submitted together. Use", tags$b("Save and continue"),
                          "under each table to store progress as you work; nothing is submitted until you press",
                          tags$b("\u2713 Submit time use tables"), "at the end of step 3."),
                  tags$li("The survey name and latest survey year are", tags$b("required"),
                          "before the tables can be submitted."),
                  tags$li("Provide metadata such as survey names or question wording in English where possible, or the official name in the original language if no translation exists."),
                  tags$li("The portal cannot accept file attachments. Any supporting documents (e.g. survey questionnaire, activity coding list) can be emailed to",
                          tags$a(href = "mailto:kate.chalmers@oecd.org", "kate.chalmers@oecd.org"),
                          "- please mention them in the notes box below so we can match them to your submission."),
                  tags$li("Values can be revised any time before the submission deadline, including after submitting; just remember to press",
                          tags$b("\u2713 Submit time use tables"), "again to save the revision."),
                  tags$li("Data can be entered manually or uploaded using the Paste from spreadsheet feature.")
                )
              ),

              # ── Step 2: latest survey on record ────────────────────────
              tags$div(class = "wb-card",
                tags$p(class = "wb-card-title",
                       tags$span(class = "wb-step-num", "2"), "Your time use survey on record"),
                uiOutput("tu_last_survey_box")
              ),

              # ── Step 3: survey details + both tables, submitted together ──
              tags$div(class = "wb-card wb-card-wide",
                tags$p(class = "wb-card-title",
                       tags$span(class = "wb-step-num", "3"),
                       "Your survey details and time use tables"),
                tags$p(style = "font-size:12px;color:#55606B;margin:0 0 14px;line-height:1.5;",
                       "The survey details and both tables below form a single submission. ",
                       "Use ", tags$b("Save and continue"), " under each table to store progress, then press ",
                       tags$b("\u2713 Submit time use tables"), " at the bottom to submit everything together."),

                # ── 3a. Survey details ──────────────────────────────────
                tags$p(class = "tu-subhead", "Survey details"),
                # Everything here is auto-saved as it is typed; the final
                # submit button below requires the name and year.
                fluidRow(
                  column(6,
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
                tags$label("Additional information or notes on your time use survey",
                           style = "font-size:13px;font-weight:600;display:block;margin-bottom:2px;"),
                tags$p(style = "font-size:11px;color:#888;margin:0 0 6px;line-height:1.5;",
                       "Optional. Anything that helps us interpret your figures, for example ",
                       "changes in methodology since the last survey, population coverage, sample ",
                       "size, how your national activity categories map onto the OECD ones, or why ",
                       "particular rows are left blank. Supporting documents can be emailed to ",
                       tags$a(href = "mailto:kate.chalmers@oecd.org", "kate.chalmers@oecd.org"),
                       " - please note here if you are sending any."),
                textAreaInput("tu_notes", label = NULL, value = "", width = "100%",
                              rows = 4,
                              placeholder = "Notes on methodology, coverage, definitions\u2026"),
                tags$div(
                  style = "display:flex;align-items:center;gap:8px;margin-top:-6px;",
                  tags$span(style = "font-size:11px;color:#8a9bae;",
                            "Saved automatically as you type and restored next time you log in."),
                  tags$span(style = "font-size:11px;color:#1F7A4D;font-weight:600;",
                            textOutput("tu_meta_status", inline = TRUE))
                ),

                # ── 3b. Table 1 ─────────────────────────────────────────
                tags$div(
                  style = "display:flex;align-items:center;justify-content:space-between;gap:12px;flex-wrap:wrap;margin:22px 0 2px;",
                  tags$p(class = "tu-subhead", style = "margin:0;",
                         HTML("Table 1. Time spent on daily activities (<u>minutes per day</u>)")),
                  tags$button(
                    onclick = "openPasteModal('tu_table1', 2, 5)",
                    style = "background:#f5f5f5;color:#555;border:1px solid #ccc;padding:5px 12px;border-radius:4px;cursor:pointer;font-size:11px;white-space:nowrap;",
                    "\U0001F4CB Paste from spreadsheet"
                  )
                ),
                tags$p(style = "font-size:11px;color:#888;margin:0 0 8px;",
                       "Blue-highlighted rows are calculated automatically. Enter values in the white rows only."),
                uiOutput("time_use_table1_ui"),
                tags$div(
                  id = "tu1_1440_warning",
                  style = paste0(
                    "display:none;background:#FFF8E1;border:1px solid #F5C518;border-radius:8px;",
                    "padding:14px 18px;margin-top:12px;margin-bottom:0;"
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

                # ── 3c. Table 2 ─────────────────────────────────────────
                tags$div(
                  style = "display:flex;align-items:center;justify-content:space-between;gap:12px;flex-wrap:wrap;margin:26px 0 8px;",
                  tags$p(class = "tu-subhead", style = "margin:0;",
                         "Table 2. Considering the activity coding list in the national time-use survey, please indicate which activity codes are grouped under each activity (e.g. 1.1. paid work)."),
                  tags$button(
                    onclick = "openPasteModal('tu_table2', 2, 3)",
                    style = "background:#f5f5f5;color:#555;border:1px solid #ccc;padding:5px 12px;border-radius:4px;cursor:pointer;font-size:11px;white-space:nowrap;",
                    "\U0001F4CB Paste from spreadsheet"
                  )
                ),
                uiOutput("time_use_table2_ui"),

                # ── 3d. Final submission of the whole of step 3 ─────────
                tags$div(
                  style = paste0("margin-top:22px;padding-top:16px;border-top:1px solid #e6e9ee;",
                                 "display:flex;align-items:center;gap:12px;flex-wrap:wrap;"),
                  tags$button(
                    id = "tu_submit_all_btn",
                    onclick = "submitTimeUseTables()",
                    style = paste0("background:#003189;color:white;border:none;padding:9px 20px;",
                                   "border-radius:5px;cursor:pointer;font-size:13px;font-weight:700;"),
                    "\u2713 Submit time use tables"
                  ),
                  tags$span(id = "tu_submit_all_status",
                            style = "font-size:12px;font-weight:600;color:#1F7A4D;"),
                  tags$span(style = "font-size:11px;color:#8a9bae;",
                            "Submits the survey details and both tables together.")
                )
              )
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

      # ── Final submit bar (hidden until all items complete) ───────────
      tags$div(
        id = "final_submit_bar",
        style = paste0(
          "display:none;position:fixed;bottom:0;left:0;width:100%;z-index:9000;",
          "background:#003189;box-shadow:0 -4px 16px rgba(0,0,0,0.15);",
          "padding:14px 0;text-align:center;"
        ),
        tags$div(
          style = "display:flex;align-items:center;justify-content:center;gap:20px;",
          tags$span(
            style = "color:rgba(255,255,255,0.9);font-size:13px;font-weight:500;",
            "\u2713 All data items are complete."
          ),
          actionButton("final_submit_btn", "Submit all responses",
            icon = icon("paper-plane"),
            style = paste0(
              "background:#1F7A4D;color:white;border:none;padding:10px 28px;border-radius:6px;",
              "font-size:14px;font-weight:700;cursor:pointer;"
            )
          )
        )
      ),
      # ── Post-submission bar: record copy stays available ────────────
      uiOutput("finalized_bar"),

      # ── Final submit confirmation overlay ──────────────────────────
      tags$div(
        id = "final_submit_confirm",
        style = paste0(
          "display:none;position:fixed;top:0;left:0;width:100%;height:100%;z-index:9500;",
          "background:rgba(0,0,0,0.5);align-items:center;justify-content:center;"
        ),
        tags$div(
          style = paste0(
            "background:white;border-radius:12px;padding:36px 40px;max-width:480px;",
            "text-align:center;box-shadow:0 12px 40px rgba(0,0,0,0.25);"
          ),
          tags$div(style = "font-size:40px;margin-bottom:12px;", "\u2713"),
          tags$h3(style = "margin:0 0 8px;color:#1F2B3A;font-size:18px;font-weight:700;",
                  "Submission complete"),
          tags$p(id = "final_submit_msg",
                 style = "font-size:13px;color:#55606B;line-height:1.6;margin:0 0 20px;",
                 "All your data and responses have been submitted successfully. Thank you for your contribution."),
          tags$div(
            style = "margin-bottom:18px;",
            downloadButton("dl_submission_copy", "Download a copy for your records",
              icon = icon("download"),
              style = paste0(
                "background:#1F7A4D;color:white;border:none;padding:9px 22px;border-radius:5px;",
                "font-size:13px;font-weight:600;"
              )),
            tags$p(style = "font-size:11px;color:#8a9bae;margin:8px 0 0;line-height:1.5;",
                   "An Excel workbook containing every value, flag, note and questionnaire ",
                   "response you submitted. You can download it again at any time from the ",
                   "bar at the bottom of the page.")
          ),
          tags$button(
            onclick = "document.getElementById('final_submit_confirm').style.display='none';",
            style = paste0(
              "background:#009EDB;color:white;border:none;padding:8px 24px;border-radius:5px;",
              "font-size:13px;font-weight:600;cursor:pointer;"
            ),
            "Close"
          )
        )
      ),

      # ── Page bottom padding ──────────────────────────────────────────
      tags$div(style = "height:80px;"),

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
                  "For any technical issues or questions about data submissions:<br>",
                  "<a href='mailto:kate.chalmers@oecd.org' style='color:#009EDB;'>kate.chalmers@oecd.org</a><br>",
                  "<a href='mailto:lara.fleischer@oecd.org' style='color:#009EDB;'>lara.fleischer@oecd.org</a><br>",
                  "<a href='mailto:wellbeing@oecd.org' style='color:#009EDB;'>wellbeing@oecd.org</a>"
                ))
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
                  "OECD Well-being Portal: Admin"),
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
                            choices = c("Completion status" = "completion",
                                        "Data entries" = "entries",
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
            # ── Per-country freeze / last-activity controls ────────────
            uiOutput("admin_country_controls"),
            DT::dataTableOutput("admin_data_table"),
            tags$hr(style = "margin:30px 0 20px;border-color:#eee;"),
            # ── Bulk backup / restore of every country session ──────────
            tags$div(
              style = paste0(
                "background:#f0f6ff;border:1px solid #c5d7ee;border-radius:8px;",
                "padding:16px 20px;margin-bottom:20px;"
              ),
              tags$p(style = "font-weight:700;margin:0 0 6px;font-size:13px;",
                     "Backup all country sessions"),
              tags$p(
                style = "font-size:11px;color:#55606B;margin:0 0 10px;line-height:1.5;",
                "Downloads every country's saved session as a single archive. ",
                "Take a backup immediately before redeploying the app, then restore ",
                "it afterwards to return all in-progress submissions. ",
                tags$b("Restoring overwrites existing session files with the same country code.")
              ),
              tags$div(
                style = "display:flex;align-items:center;gap:16px;flex-wrap:wrap;",
                downloadButton("admin_backup_all", "Download all sessions",
                               style = "background:#009EDB;color:white;border:none;font-weight:600;"),
                tags$div(
                  class = "upload-btn-wrap",
                  fileInput("admin_restore_all", label = NULL, accept = ".zip",
                            buttonLabel = tagList(icon("upload"), "Restore all sessions"),
                            placeholder = "No file selected", width = "auto")
                ),
                uiOutput("admin_restore_feedback", inline = TRUE)
              )
            ),
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

  # ── Breakdown/flag lookup constants ─────────────────────────────────────────
  # Shared between the panel/heatmap builder below and store_measure() (used
  # to detect when a submission reverts a cell back to its existing/published
  # value), so both stay in sync with a single definition.
  breakdown_filters <- list(
    country_avg = list(sex = "_T", age = "_T", edu = "_T"),
    male        = list(sex = "M",  age = "_T", edu = "_T"),
    female      = list(sex = "F",  age = "_T", edu = "_T"),
    young       = list(sex = "_T", age = "YOUNG", edu = "_T"),
    middle_aged = list(sex = "_T", age = "MID",   edu = "_T"),
    old         = list(sex = "_T", age = "OLD",   edu = "_T"),
    primary     = list(sex = "_T", age = "_T", edu = "ISCED11_1"),
    secondary   = list(sex = "_T", age = "_T", edu = "ISCED11_2_3"),
    tertiary    = list(sex = "_T", age = "_T", edu = "ISCED11_5T8")
  )
  # vert/dep use separate measures with _VER/_DEP suffix
  dep_vert_keys  <- c("vert", "dep")
  # obs_status -> flag mapping (A = normal, W = not a standard flag)
  status_to_flag <- c(B = "B", D = "D", E = "E", P = "P", U = "U")

  # Existing OECD figure for one breakdown/year of a measure: published data,
  # falling back to non-used data, exactly as shown by "Revert to default" and
  # by the input panels themselves. Returns list(value = num/NA, flag = chr).
  lookup_default_obs <- function(measure, bk, yr, d = NULL, iso = NULL) {
    none <- list(value = NA_real_, flag = "")
    if (is.null(d)) d <- isolate(dat_rv())
    if (is.null(d)) return(none)
    if (bk %in% dep_vert_keys) {
      meas <- paste0(measure, if (bk == "vert") "_VER" else "_DEP")
      sx <- "_T"; ag <- "_T"; ed <- "_T"
    } else if (!is.null(breakdown_filters[[bk]])) {
      bf <- breakdown_filters[[bk]]
      meas <- measure; sx <- bf$sex; ag <- bf$age; ed <- bf$edu
    } else return(none)
    yr_num <- suppressWarnings(as.numeric(yr))

    pub <- d[d$measure == meas & d$sex == sx & d$age == ag &
             d$education_lev == ed & as.numeric(d$time_period) == yr_num, ]
    if (nrow(pub) > 0 && !is.na(pub$obs_value[1])) {
      flag <- if ("obs_status" %in% names(pub) && !is.na(pub$obs_status[1]) &&
                  pub$obs_status[1] %in% names(status_to_flag)) {
        unname(status_to_flag[pub$obs_status[1]])
      } else ""
      return(list(value = pub$obs_value[1], flag = flag))
    }
    if (is.null(iso)) iso <- isolate(credentials$country)
    nu <- nonused_dat[nonused_dat$ref_area == iso & nonused_dat$measure == meas &
                       nonused_dat$sex == sx & nonused_dat$age == ag &
                       nonused_dat$education_lev == ed &
                       as.numeric(nonused_dat$time_period) == yr_num, ]
    if (nrow(nu) > 0 && !is.na(nu$obs_value[1])) {
      return(list(value = nu$obs_value[1], flag = ""))
    }
    none
  }

  # ── Authentication ──────────────────────────────────────────────────────────
  credentials <- reactiveValues(authenticated = FALSE, country = NULL, country_name = NULL)
  # Snapshot of entries as of the last explicit Submit, "Save and continue",
  # or template upload - all three refresh this immediately so the heatmap
  # and input panels (which depend on this rather than on
  # session_data$entries directly) stay in sync with the latest values.
  committed_entries   <- reactiveVal(list())
  committed_revisions <- reactiveVal(list())
  # Plain environment (deliberately non-reactive) caching the per-measure
  # echarts HTML, which is expensive to build and depends only on the data.
  chart_cache <- new.env(parent = emptyenv())
  chart_cache$key <- NULL
  # Bumped by explicit user actions that should refresh the heatmap/panels:
  # Submit, no-update toggle, template upload, backup restore. Nothing else
  # invalidates this expensive rebuild - in particular "Save and continue"
  # deliberately does not, so drafting never moves the heatmap.
  ui_refresh <- reactiveVal(0)
  bump_ui <- function() ui_refresh(isolate(ui_refresh()) + 1)
  dat_rv      <- reactiveVal(NULL)

  # Prominent header on the submissions tab: which country is logged in, the
  # most recent request round they responded to, and what that means for the
  # indicators below (pre-populated vs starting empty).
  output$country_status <- renderUI({
    req(credentials$authenticated)
    iso <- credentials$country
    yr  <- if (iso %in% names(latest_request)) latest_request[[iso]] else NULL
    responded <- !is.null(yr) && !is.na(yr)

    tags$div(class = "wb-status-row",
      tags$div(
        style = "display:flex;align-items:baseline;gap:12px;flex-wrap:wrap;",
        tags$span(style = "font-size:22px;font-weight:700;color:#1F2B3A;",
                  paste0("Welcome, ", credentials$country_name)),
        if (responded) {
          tags$span(
            style = paste0("font-size:11px;font-weight:600;background:#e8f1fb;color:#003189;",
                           "border:1px solid #c5d7ee;border-radius:12px;padding:2px 10px;"),
            paste0("Last responded: ", yr, " data request")
          )
        } else {
          tags$span(
            style = paste0("font-size:11px;font-weight:600;background:#FFF8E1;color:#8a6d1a;",
                           "border:1px solid #F5C518;border-radius:12px;padding:2px 10px;"),
            "No previous response on record"
          )
        }
      ),
      tags$p(
        style = "font-size:12.5px;color:#55606B;margin:8px 0 0;line-height:1.5;",
        if (responded) {
          paste0("The indicators and metadata below are pre-populated with the values ", credentials$country_name,
                 " provided in the ", yr, " data request for this questionnaire. Existing figures can be ",
                 "overwritten where you have newer or revised data.")
        } else {
          paste0("We have no previous response from ", credentials$country_name,
                 " on record, so the indicators below start unpopulated. Please enter ",
                 "values wherever data are available.")
        }
      )
    )
  })

  output$nav_title <- renderUI({
    if (!credentials$authenticated) return(tags$span("OECD Well-being and Time Use Questionnaire Portal"))
    tags$span(
      style = "color:#ffffff !important;",
      "OECD Well-being and Time Use Questionnaire Portal"
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
      # Populate country filter with every known country, not just those that
      # have logged in before, so a submission can be frozen pre-emptively.
      isos <- unname(country_name_vector)
      names(isos) <- names(country_name_vector)
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
    pw_store  <- session_read("passwords") %||% list()
    valid_pw  <- pw_store[[input$login_country]] %||% "oecd2026"
    if (input$login_password != valid_pw) {
      output$login_error <- renderUI(
        tags$p(style = "color:#E63312;font-size:12px;margin:0;", "Incorrect password. Please try again.")
      )
      return()
    }
    # Successful login: wipe any state left over from a previous country
    # before this country's own session is loaded.
    reset_country_state()

    iso   <- input$login_country
    cname <-  country_name_vector[country_name_vector == iso] %>% names()
    credentials$authenticated <- TRUE
    credentials$country       <- iso
    credentials$country_name  <- cname

    # Filter dataset to this country
    dat_rv(dat %>% filter(ref_area == iso))

    # Auto-load country session if exists
    loaded <- session_read(iso)
    if (!is.null(loaded)) {
        session_data$entries    <- loaded$entries    %||% list()
        session_data$notes      <- loaded$notes      %||% list()
        session_data$responses  <- loaded$responses  %||% list()
        session_data$no_updates <- loaded$no_updates %||% list()
        session_data$flags      <- loaded$flags      %||% list()
        session_data$age_notes  <- loaded$age_notes  %||% list()
        session_data$revisions  <- loaded$revisions  %||% list()
        # explicit_submit tracks which measures were actually confirmed via
        # the Submit button (as opposed to merely uploaded or draft-saved).
        # For sessions saved before this field existed, fall back to treating
        # any measure with a filled entry as previously submitted so past
        # work isn't demoted to "incomplete".
        if (!is.null(loaded$explicit_submit)) {
          session_data$explicit_submit <- loaded$explicit_submit
        } else {
          is_filled_legacy <- function(v) !is.null(v) && !is.na(v) && v != ""
          legacy_measures <- names(session_data$entries)[vapply(session_data$entries, function(m) {
            is.list(m) && any(vapply(m, function(row) {
              is.list(row) && any(vapply(row, is_filled_legacy, logical(1)))
            }, logical(1)))
          }, logical(1))]
          session_data$explicit_submit <- setNames(as.list(rep(TRUE, length(legacy_measures))), legacy_measures)
        }
        # Seed the committed snapshot so restored work renders on the heatmap.
        committed_entries(session_data$entries)
        committed_revisions(session_data$revisions)
        bump_ui()
        session_data$time_use_1   <- loaded$time_use_1
        session_data$time_use_2   <- loaded$time_use_2
        session_data$tu_draft_1   <- loaded$tu_draft_1 %||% loaded$time_use_1
        session_data$tu_draft_2   <- loaded$tu_draft_2 %||% loaded$time_use_2
        session_data$tu_no_update <- loaded$tu_no_update %||% FALSE
        # Restoring this keeps the record-copy bar (and hides the submit bar)
        # for a country that has already finalised.
        session_data$finalized    <- loaded$finalized
        session_data$frozen       <- isTRUE(loaded$frozen)
        session_data$frozen_at    <- loaded$frozen_at
        session_data$last_edited  <- loaded$last_edited
        # Survey metadata: prefer the tu_meta list, falling back to the older
        # top-level fields for sessions saved before tu_meta existed.
        meta <- loaded$tu_meta
        if (is.null(meta) || length(meta) == 0) {
          meta <- list(survey_name = loaded$tu_survey_name %||% "",
                       survey_year = loaded$tu_survey_year,
                       notes       = loaded$tu_notes %||% "")
        }
        session_data$tu_meta <- meta
        updateTextInput(session, "tu_survey_name", value = meta$survey_name %||% "")
        if (!is.null(meta$survey_year))
          updateNumericInput(session, "tu_survey_year", value = meta$survey_year)
        updateTextAreaInput(session, "tu_notes", value = meta$notes %||% "")
        if (!is.null(loaded$tu1_explanation) && nzchar(loaded$tu1_explanation))
          updateTextAreaInput(session, "tu1_explanation", value = loaded$tu1_explanation)
    }

    # Freeze state: the shared "frozen" pin (written by the Admin panel) is
    # authoritative when it exists; older sessions stored the flag inside the
    # session pin itself and were restored above.
    fl <- frozen_map_read()
    if (!is.null(fl)) {
      session_data$frozen    <- !is.null(fl[[iso]])
      session_data$frozen_at <- fl[[iso]]
    }

    # The block above just repopulated entries/notes/.../tu_meta from disk,
    # which will fire the auto-save observer once; don't let that look like
    # a fresh edit.
    suppress_edit_stamp(TRUE)

    shinyjs::hide("login_screen")
    shinyjs::show("main_app")

    # tu_no_update button state is now restored via the renderUI in
    # tu_last_survey_box, so no manual JS restore needed here.
  })

  # ── Logout ────────────────────────────────────────────────────────────────────
  observeEvent(input$logout_btn, {
    # Push any unsaved changes before the country state is wiped.
    flush_save_sync()
    credentials$authenticated <- FALSE
    credentials$country       <- NULL
    credentials$country_name  <- NULL
    dat_rv(NULL)

    # Wipe the whole country instance (data, drafts, uploads, client-side text)
    reset_country_state()

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
      title = paste0("Change password: ", credentials$country_name),
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

    pw_store <- session_read("passwords") %||% list()
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

    pw_store[[iso]] <- input$pw_new
    session_write(pw_store, "passwords")
    output$change_pw_msg <- renderUI(
      tags$p(style = "color:#1F7A4D;font-size:11px;margin:4px 0 0;", "\u2713 Password updated successfully."))
    Sys.sleep(1.5)
    removeModal()
  })

  # ── Session data ─────────────────────────────────────────────────────────────
  session_data <- reactiveValues(
    entries      = list(),    notes        = list(),
    responses    = list(),
    no_updates   = list(),
    flags        = list(),
    # age_notes[[measure]] = free-text answer to "Age groups differ?"
    age_notes    = list(),
    # revisions[[measure]][[breakdown_key]][[year]] = list of
    # list(from, to, at) records, appended each time a non-empty value is
    # overwritten with a different (or blank) value during a session.
    revisions    = list(),
    # explicit_submit[[measure]] = TRUE once the user has pressed "Submit
    # data" for that measure this session (or a prior session). Uploading a
    # template or using "Save and continue" populates entries/revisions but
    # deliberately does NOT set this, so the indicator is not marked
    # complete until the user reviews and submits it.
    explicit_submit = list(),
    time_use_1   = NULL,
    time_use_2   = NULL,
    # Drafts written by "Save and continue" on each time use table. Kept
    # apart from time_use_1/2 so that unfinished work is restored on the
    # next login without counting as a submission.
    tu_draft_1   = NULL,
    tu_draft_2   = NULL,
    tu_no_update   = FALSE,
    tu_no_update_1 = FALSE,
    tu_no_update_2 = FALSE,
    # Time use survey metadata: name, year and free-text notes. Held in
    # session_data (rather than read off the inputs only at save time) so that
    # editing it triggers the auto-save like any other piece of data.
    tu_meta        = list(),
    finalized      = NULL,
    # Set by an admin from the Admin panel once a country has told us its
    # submission is complete. While TRUE, every mutating observer below
    # refuses to record further changes and the UI is shown read-only.
    frozen         = FALSE,
    frozen_at      = NULL,
    # Timestamp of the most recent change to entries/notes/responses/flags/
    # time-use tables/survey metadata, refreshed by the auto-save observer.
    # Purely informational (shown to admins); not used to gate anything.
    last_edited    = NULL
  )

  # Skips the *next* auto-save stamp of last_edited. Set right after a
  # country's data is restored at login, so simply logging in (which touches
  # every tracked field once as it is populated) is never mistaken for the
  # country having just edited something.
  suppress_edit_stamp <- reactiveVal(FALSE)

  # ── Wipe every trace of the current country's instance ──────────────────────
  # Called on logout AND immediately before a new country's session is loaded,
  # so nothing uploaded, downloaded or typed under one country can ever leak
  # into another country's view within the same browser session.
  reset_country_state <- function() {
    session_data$entries         <- list()
    session_data$notes           <- list()
    session_data$responses       <- list()
    session_data$no_updates      <- list()
    session_data$flags           <- list()
    session_data$age_notes       <- list()
    session_data$revisions       <- list()
    session_data$explicit_submit <- list()
    session_data$time_use_1      <- NULL
    session_data$time_use_2      <- NULL
    session_data$tu_draft_1      <- NULL
    session_data$tu_draft_2      <- NULL
    session_data$tu_no_update    <- FALSE
    session_data$tu_no_update_1  <- FALSE
    session_data$tu_no_update_2  <- FALSE
    session_data$tu_meta         <- list()
    session_data$finalized       <- NULL
    session_data$frozen          <- FALSE
    session_data$frozen_at       <- NULL
    session_data$last_edited     <- NULL

    committed_entries(list())
    committed_revisions(list())

    # Charts are country-specific; force a rebuild on next render.
    chart_cache$key <- NULL
    chart_cache$sub <- NULL
    chart_cache$cov <- NULL

    # Free-text Time Use inputs
    updateTextInput(session,     "tu_survey_name",  value = "")
    updateNumericInput(session,  "tu_survey_year",  value = NA)
    updateTextAreaInput(session, "tu_notes",        value = "")
    updateTextAreaInput(session, "tu1_explanation", value = "")
    tu_meta_saved_at(NULL)
    tu_meta_synced(FALSE)

    # Clear the uploaded-file widget and any client-side status text left
    # behind by the previous country (upload/submit/note confirmations).
    shinyjs::reset("upload_wb_file")
    runjs(paste0(
      "(function(){",
      "var u=document.getElementById('upload_wb_status');",
      "if(u){u.innerText='';u.style.display='none';}",
      "document.querySelectorAll(\"[id^='status_'],[id^='note_status_'],",
      "[id^='tu_no_update_status'],[id^='table_status_']\")",
      ".forEach(function(el){el.innerText='';});",
      "document.querySelectorAll('.collapsible-panel')",
      ".forEach(function(el){el.style.display='none';});",
      "})();"
    ))

    bump_ui()
  }

  # ── Save manager: debounced, coalesced, async ───────────────────────────────
  # A Drive write takes several seconds, so persisting on *every* change made
  # buttons look broken. Instead, edits only mark the session dirty; the
  # actual write happens (a) at most once per debounce window, and (b) on a
  # mirai daemon when available, so the UI never blocks on the round-trip.
  # If a write is already in flight when new changes arrive, exactly one
  # follow-up write with the latest snapshot runs when it returns (never a
  # queue of stale payloads). Last-write-wins semantics are unchanged.
  save_epoch  <- reactiveVal(0)        # bumped on every tracked change
  save_status <- reactiveVal("idle")   # idle | pending | saving | saved | error
  .saved_epoch     <- 0                # epoch covered by the last started write
  .write_in_flight <- FALSE
  .write_pending   <- FALSE

  build_save_payload <- function() {
    c(reactiveValuesToList(session_data),
      list(tu_survey_name  = isolate(input$tu_survey_name),
           tu_survey_year  = isolate(input$tu_survey_year),
           tu1_explanation = isolate(input$tu1_explanation)))
  }

  notify_save_failed <- function() {
    save_status("error")
    showNotification(
      "Your latest changes could not be saved to storage. Please try again shortly.",
      type = "error", duration = 10, session = session
    )
  }

  do_save <- function() {
    iso <- isolate(credentials$country)
    if (is.null(iso)) return(invisible(NULL))
    payload <- isolate(build_save_payload())
    .saved_epoch <<- isolate(save_epoch())
    save_status("saving")
    if (!async_board_enabled) {
      if (session_write(payload, iso)) save_status("saved") else notify_save_failed()
      return(invisible(NULL))
    }
    .write_in_flight <<- TRUE
    promises::then(
      session_write_async(payload, iso),
      onFulfilled = function(ok) {
        .write_in_flight <<- FALSE
        if (.write_pending) {
          .write_pending <<- FALSE
          do_save()
        } else if (isTRUE(ok)) {
          save_status("saved")
        } else {
          # Daemon-side failure: one synchronous retry so a broken daemon
          # never silently drops data.
          if (session_write(payload, iso)) save_status("saved") else notify_save_failed()
        }
      },
      onRejected = function(e) {
        .write_in_flight <<- FALSE
        if (session_write(payload, iso)) save_status("saved") else notify_save_failed()
      }
    )
    invisible(NULL)
  }

  # Write the latest snapshot now (async); coalesce if one is already running.
  request_save <- function() {
    if (.write_in_flight) .write_pending <<- TRUE else do_save()
  }

  # Synchronous flush of any unsaved changes. Used at logout and session end,
  # where blocking is harmless and an async write might not get to finish.
  flush_save_sync <- function() {
    iso <- isolate(credentials$country)
    if (is.null(iso)) return(invisible(NULL))
    if (isolate(save_epoch()) > .saved_epoch || .write_pending) {
      .write_pending <<- FALSE
      .saved_epoch   <<- isolate(save_epoch())
      session_write(isolate(build_save_payload()), iso)
    }
    invisible(NULL)
  }

  # Mark the session dirty whenever any tracked field changes; the debounced
  # flush below performs the actual write once the user pauses.
  observe({
    req(credentials$authenticated, credentials$country)
    # Touch all fields to create reactive dependencies
    list(session_data$entries, session_data$notes, session_data$responses,
         session_data$no_updates, session_data$flags, session_data$revisions,
         session_data$age_notes,
         session_data$time_use_1, session_data$time_use_2,
         session_data$tu_draft_1, session_data$tu_draft_2,
         session_data$tu_no_update, session_data$tu_meta)
    # Skipped once right after login, when this observer fires purely because
    # the country's saved data was just restored into these fields - nothing
    # new to write, and it should not stamp last_edited.
    if (isolate(suppress_edit_stamp())) {
      suppress_edit_stamp(FALSE)
      return()
    }
    session_data$last_edited <- Sys.time()
    save_epoch(isolate(save_epoch()) + 1)
    save_status("pending")
  })

  save_flush_trigger <- debounce(reactive(save_epoch()), 5000)
  observeEvent(save_flush_trigger(), {
    req(credentials$authenticated, credentials$country)
    if (isolate(save_epoch()) > .saved_epoch) request_save()
  }, ignoreInit = TRUE)

  # A closed tab never gets another debounce tick: flush any pending changes
  # before the session is torn down.
  session$onSessionEnded(function() {
    flush_save_sync()
  })

  # Small persistent indicator so a background save in progress is visible,
  # rather than the app just looking unresponsive.
  output$save_status_ui <- renderUI({
    req(credentials$authenticated)
    st <- save_status()
    if (st == "idle") return(NULL)
    txt <- switch(st,
      pending = "Unsaved changes\u2026",
      saving  = "Saving\u2026",
      saved   = "\u2713 All changes saved",
      error   = "\u26A0 Save failed - retrying on your next change")
    col <- switch(st, saved = "#1F7A4D", error = "#E63312", "#55606B")
    tags$span(style = paste0("font-size:11px;font-weight:600;color:", col,
                             ";margin-right:12px;"),
              txt)
  })

  # ── Helper: null coalescing ──────────────────────────────────────────────────
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # ── Freeze / block collection ───────────────────────────────────────────────
  # An admin freezes a country from the Admin panel, which writes the tiny
  # shared "frozen" pin (admin and country sessions are separate R processes
  # with their own session_data, so there is no shared reactive value to
  # flip). While a country is logged in, this poll re-reads that pin
  # periodically so a freeze/unfreeze applied mid-session is picked up
  # without requiring the user to log out and back in. The pin is a few
  # hundred bytes (vs. the full session it used to re-download), and the read
  # happens on a mirai daemon when available so the poll never blocks anyone.
  apply_frozen_map <- function(fl) {
    iso <- isolate(credentials$country)
    # NULL pin = not migrated yet; keep whatever the login restored (legacy)
    if (is.null(fl) || is.null(iso)) return(invisible(NULL))
    new_frozen <- !is.null(fl[[iso]])
    if (!identical(new_frozen, isolate(session_data$frozen))) {
      session_data$frozen    <- new_frozen
      session_data$frozen_at <- fl[[iso]]
    }
    invisible(NULL)
  }
  observe({
    req(credentials$authenticated, credentials$country)
    invalidateLater(30000)
    if (async_board_enabled) {
      promises::then(session_read_async("frozen"),
                     onFulfilled = apply_frozen_map,
                     onRejected  = function(e) NULL)
    } else {
      apply_frozen_map(frozen_map_read())
    }
  })

  # Toggles a CSS class on <body> so inputs/buttons inside the two submission
  # tabs become inert (see .country-frozen rules in stylesheet.css) whenever
  # this country is frozen.
  observe({
    req(credentials$authenticated)
    runjs(sprintf("document.body.classList.toggle('country-frozen', %s);",
                  tolower(isTRUE(session_data$frozen))))
  })

  # Read-only helper for server-side guards below; TRUE once the country is
  # frozen, whether that was loaded at login or picked up by the poll above.
  country_is_frozen <- function() isTRUE(isolate(session_data$frozen))

  # Persistent banner shown on every tab once a country is frozen.
  output$freeze_banner <- renderUI({
    req(credentials$authenticated)
    if (!isTRUE(session_data$frozen)) return(NULL)
    since <- if (!is.null(session_data$frozen_at)) {
      paste0(" on ", format(session_data$frozen_at, "%d %B %Y at %H:%M"))
    } else ""
    tags$div(
      style = paste0(
        "background:#fdecea;border-bottom:2px solid #E63312;padding:10px 24px;",
        "display:flex;align-items:center;gap:10px;font-size:13px;color:#7a251c;"
      ),
      tags$span(style = "font-size:16px;", "\u2744"),
      tags$span(
        tags$b("This submission has been frozen by the OECD Secretariat"), since, ". ",
        "No further edits can be recorded. Contact us if you need to make a change."
      )
    )
  })

  # ── Well-being Excel template download ─────────────────────────────────────
  output$dl_wb_template <- downloadHandler(
    filename = function() {
      iso <- credentials$country %||% "unknown"
      paste0("wellbeing_template_", iso, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(credentials$authenticated, credentials$country)

      iso <- credentials$country
      is_eu_silc <- iso %in% eu_silc_countries
      measures <- if (is_eu_silc) setdiff(xlsx_measures, eu_silc_measures) else xlsx_measures

      d <- dat_rv()
      if (is.null(d)) d <- data.frame(measure = character(), sex = character(),
                                       age = character(), education_lev = character(),
                                       time_period = numeric(), obs_value = numeric())
      years <- 2004:2026

      # Row definitions come from global.R so the template, the record copy of
      # a submission and the on-screen panels never drift apart.
      row_defs_fn <- dl_row_defs

      # Full data for pre-filling all breakdowns
      d_full_dl <- d %>%
        select(measure, sex, age, education_lev, time_period, obs_value) %>%
        mutate(time_period = as.numeric(time_period))

      # Non-used responses for this country (fallback after published)
      d_nonused_dl <- nonused_dat %>%
        filter(ref_area == iso) %>%
        select(measure, sex, age, education_lev, time_period, obs_value) %>%
        mutate(time_period = as.numeric(time_period))

      # Breakdown key -> filter conditions
      bf_map <- breakdown_filter_map

      sheets <- list()
      for (m in measures) {
        rows <- row_defs_fn(m)
        lbl <- dict %>% filter(measure == m) %>% pull(label) %>% first()
        lbl <- if (is.na(lbl) || is.null(lbl)) m else lbl

        # Build data frame: breakdown_key, breakdown_label, then year columns
        df <- data.frame(
          breakdown_key   = sapply(rows, `[[`, "key"),
          breakdown_label = sapply(rows, `[[`, "label"),
          stringsAsFactors = FALSE
        )
        # Add year columns with existing data pre-filled
        for (yr in years) {
          vals <- sapply(rows, function(r) {
            # Check session data first - "" is an explicit deletion marker
            saved <- session_data$entries[[m]]
            if (!is.null(saved) && !is.null(saved[[r$key]]) &&
                !is.null(saved[[r$key]][[as.character(yr)]])) {
              sv <- saved[[r$key]][[as.character(yr)]]
              if (identical(sv, "")) return(NA_real_)
              return(as.numeric(sv))
            }
            # Fall back to published data, then non-used data
            if (r$key %in% c("vert", "dep")) {
              suffix <- if (r$key == "vert") "_VER" else "_DEP"
              meas <- paste0(m, suffix)
              sx <- "_T"; ag <- "_T"; ed <- "_T"
            } else if (!is.null(bf_map[[r$key]])) {
              bf <- bf_map[[r$key]]
              meas <- m; sx <- bf$sex; ag <- bf$age; ed <- bf$edu
            } else {
              return(NA_real_)
            }
            existing <- d_full_dl %>%
              filter(measure == meas, sex == sx, age == ag, education_lev == ed,
                     time_period == yr)
            if (nrow(existing) > 0 && !is.na(existing$obs_value[1]))
              return(existing$obs_value[1])
            # Fall back to non-used data
            nonused <- d_nonused_dl %>%
              filter(measure == meas, sex == sx, age == ag, education_lev == ed,
                     time_period == yr)
            if (nrow(nonused) > 0 && !is.na(nonused$obs_value[1]))
              return(nonused$obs_value[1])
            return(NA_real_)
          })
          df[[as.character(yr)]] <- vals
        }

        # Sanitise sheet name (max 31 chars, no special chars)
        sheet_name <- paste0(m, " - ", substr(lbl, 1, 25))
        sheet_name <- gsub("[\\[\\]:*?/\\\\]", "", sheet_name)
        sheet_name <- substr(sheet_name, 1, 31)
        sheets[[sheet_name]] <- df
      }

      # Add a README sheet
      readme <- data.frame(
        Instructions = c(
          paste0("Well-being data template for ", names(country_name_vector)[country_name_vector == iso]),
          "",
          "Each sheet corresponds to one indicator.",
          "Column A (breakdown_key) identifies the breakdown; do NOT modify this column.",
          "Column B (breakdown_label) is a human-readable label for reference.",
          "Columns C onward are years (2004–2026).",
          "Enter numeric values in the year columns. Leave cells blank if no data.",
          "Existing data has been pre-filled where available.",
          "",
          "When finished, save this file and upload it using the 'Upload completed template' button.",
          "",
          "Data flags (B, E, P, D, U) can be set in the app after upload.",
          paste0("Generated: ", Sys.Date())
        ),
        stringsAsFactors = FALSE
      )
      sheets <- c(list(README = readme), sheets)

      writexl::write_xlsx(sheets, file)
    }
  )

  # Download buttons are outputs; Shiny suspends them while hidden (the template
  # panel is collapsed by default), which renders the link greyed-out/unclickable.
  outputOptions(output, "dl_wb_template", suspendWhenHidden = FALSE)

  # ── Record copy of a country's submission ──────────────────────────────────
  # Offered once the final submission is made so countries keep an archive of
  # exactly what they sent us. This reads session_data only: it is a record of
  # what was SUBMITTED, not of what is published in the OECD database.
  build_submission_sheets <- function() {
    iso     <- credentials$country
    cname   <- names(country_name_vector)[country_name_vector == iso]
    entries <- session_data$entries
    flags   <- session_data$flags
    notes   <- session_data$notes
    resps   <- session_data$responses
    no_upd  <- session_data$no_updates
    fin     <- session_data$finalized
    meta    <- session_data$tu_meta %||% list()

    is_filled <- function(v) {
      !is.null(v) && length(v) == 1 && !is.na(v) && !identical(as.character(v), "")
    }
    label_for <- function(m) {
      lbl <- dict$label[dict$measure == m]
      if (length(lbl) == 0 || is.na(lbl[1])) m else lbl[1]
    }

    # ── Submitted values, long format ──
    data_rows <- list()
    for (m in names(entries)) {
      rows    <- dl_row_defs(m)
      row_lbl <- setNames(vapply(rows, `[[`, character(1), "label"),
                          vapply(rows, `[[`, character(1), "key"))
      for (bk in names(entries[[m]])) {
        vals <- entries[[m]][[bk]]
        if (!is.list(vals)) next
        for (yr in names(vals)) {
          if (!is_filled(vals[[yr]])) next
          fl <- tryCatch(flags[[m]][[bk]][[yr]], error = function(e) NULL)
          data_rows[[length(data_rows) + 1]] <- data.frame(
            measure       = m,
            indicator     = label_for(m),
            breakdown_key = bk,
            breakdown     = if (bk %in% names(row_lbl)) unname(row_lbl[[bk]]) else bk,
            year          = suppressWarnings(as.numeric(yr)),
            value         = suppressWarnings(as.numeric(vals[[yr]])),
            flag          = if (is_filled(fl)) as.character(fl) else "",
            stringsAsFactors = FALSE
          )
        }
      }
    }
    data_df <- if (length(data_rows) > 0) {
      bind_rows(data_rows) %>% arrange(measure, breakdown_key, year)
    } else {
      data.frame(measure = character(), indicator = character(),
                 breakdown_key = character(), breakdown = character(),
                 year = numeric(), value = numeric(), flag = character(),
                 stringsAsFactors = FALSE)
    }

    # ── "Other useful information" notes ──
    kept_notes <- Filter(is_filled, notes)
    # The "Age groups differ?" answers travel with the notes sheet so the
    # record copy keeps them too.
    kept_ages  <- Filter(is_filled, session_data$age_notes %||% list())
    note_ms    <- union(names(kept_notes), names(kept_ages))
    notes_df <- if (length(note_ms) > 0) {
      data.frame(measure   = note_ms,
                 indicator = vapply(note_ms, label_for, character(1)),
                 note      = vapply(note_ms, function(m) as.character(kept_notes[[m]] %||% ""), character(1)),
                 age_groups_differ = vapply(note_ms, function(m) as.character(kept_ages[[m]] %||% ""), character(1)),
                 stringsAsFactors = FALSE, row.names = NULL)
    } else {
      data.frame(measure = character(), indicator = character(), note = character(),
                 age_groups_differ = character(), stringsAsFactors = FALSE)
    }

    # ── Country question-format responses ──
    fmt_indics <- vapply(xlsx_response_format, function(x) as.character(x$indic)[1], character(1))
    resp_rows  <- list()
    for (m in names(resps)) {
      hit  <- which(fmt_indics == m)
      labs <- if (length(hit) > 0) xlsx_response_format[[hit[1]]]$response$label else character(0)
      for (idx in names(resps[[m]])) {
        val <- resps[[m]][[idx]]
        if (!is_filled(val)) next
        i <- suppressWarnings(as.integer(idx))
        resp_rows[[length(resp_rows) + 1]] <- data.frame(
          measure   = m,
          indicator = label_for(m),
          question  = if (!is.na(i) && i <= length(labs)) labs[i] else paste0("Question ", idx),
          response  = as.character(val),
          stringsAsFactors = FALSE
        )
      }
    }
    resp_df <- if (length(resp_rows) > 0) bind_rows(resp_rows) else {
      data.frame(measure = character(), indicator = character(),
                 question = character(), response = character(),
                 stringsAsFactors = FALSE)
    }

    # ── Time use tables: nested list -> data frame ──
    tu_sheet <- function(saved, row_text, col_names) {
      if (is.null(saved) || length(saved) == 0) return(NULL)
      n_text <- ncol(row_text)
      out    <- row_text
      names(out) <- ifelse(nzchar(col_names[seq_len(n_text)]),
                           col_names[seq_len(n_text)],
                           paste0("Column ", seq_len(n_text)))
      for (cc in seq(n_text + 1, length(col_names))) {
        nm <- if (nzchar(col_names[cc])) gsub("\n", " ", col_names[cc]) else "Value"
        out[[nm]] <- vapply(seq_len(nrow(row_text)), function(r) {
          v <- tryCatch(saved[[as.character(r)]][[paste0("c", cc)]], error = function(e) NULL)
          if (is.null(v)) "" else as.character(v)
        }, character(1))
      }
      out
    }
    tu1 <- tu_sheet(session_data$time_use_1, time_use_row_text_1, time_use_col_names_1)
    tu2 <- tu_sheet(session_data$time_use_2, time_use_row_text_2, time_use_col_names_2)

    declared <- names(no_upd)[vapply(no_upd, isTRUE, logical(1))]

    summary_df <- data.frame(
      Item = c("Country", "ISO code", "Submission finalised",
               "Indicators with values submitted", "Values submitted",
               "Indicators declared 'no data update'",
               "Time use survey name", "Latest survey year", "Time use notes",
               "Time use table 1", "Time use table 2",
               "Time use: no update declared", "File generated"),
      Value = c(
        cname, iso,
        if (is.null(fin)) "Not yet finalised" else format(fin, "%Y-%m-%d %H:%M"),
        as.character(length(unique(data_df$measure))),
        as.character(nrow(data_df)),
        if (length(declared) > 0) paste(declared, collapse = ", ") else "None",
        if (is_filled(meta$survey_name)) as.character(meta$survey_name) else "Not provided",
        if (is_filled(meta$survey_year)) as.character(meta$survey_year) else "Not provided",
        if (is_filled(meta$notes))       as.character(meta$notes)       else "None",
        if (is.null(session_data$time_use_1)) "Not submitted" else "Submitted",
        if (is.null(session_data$time_use_2)) "Not submitted" else "Submitted",
        if (isTRUE(session_data$tu_no_update)) "Yes" else "No",
        format(Sys.time(), "%Y-%m-%d %H:%M")
      ),
      stringsAsFactors = FALSE
    )

    sheets <- list(
      Summary       = summary_df,
      `Data`        = data_df,
      `Notes`       = notes_df,
      `Questionnaire` = resp_df
    )
    if (!is.null(tu1)) sheets[["Time use table 1"]] <- tu1
    if (!is.null(tu2)) sheets[["Time use table 2"]] <- tu2
    sheets
  }

  submission_copy_filename <- function() {
    iso <- credentials$country %||% "unknown"
    paste0("wellbeing_submission_", iso, "_", Sys.Date(), ".xlsx")
  }
  write_submission_copy <- function(file) {
    req(credentials$authenticated, credentials$country)
    writexl::write_xlsx(build_submission_sheets(), file)
  }

  # Two buttons share one handler: one inside the confirmation overlay, one on
  # the persistent bar shown after finalisation.
  output$dl_submission_copy <- downloadHandler(
    filename = submission_copy_filename, content = write_submission_copy)
  output$dl_submission_copy_bar <- downloadHandler(
    filename = submission_copy_filename, content = write_submission_copy)
  outputOptions(output, "dl_submission_copy",     suspendWhenHidden = FALSE)
  outputOptions(output, "dl_submission_copy_bar", suspendWhenHidden = FALSE)

  # ── Well-being Excel template upload ───────────────────────────────────────
  observeEvent(input$upload_wb_file, {
    req(credentials$authenticated)
    if (country_is_frozen()) return()
    file_info <- input$upload_wb_file
    req(file_info)

    tryCatch({
      sheet_names <- readxl::excel_sheets(file_info$datapath)
      # Skip the README sheet
      data_sheets <- setdiff(sheet_names, "README")

      n_measures <- 0
      n_values   <- 0
      n_deleted  <- 0
      warnings   <- character()

      for (sn in data_sheets) {
        df <- readxl::read_excel(file_info$datapath, sheet = sn, col_types = "text")

        if (!"breakdown_key" %in% names(df)) {
          warnings <- c(warnings, paste0("Sheet '", sn, "': missing breakdown_key column, skipped."))
          next
        }

        # Extract measure code from sheet name (before " - ")
        m <- trimws(sub(" - .*$", "", sn))
        if (!m %in% xlsx_measures) {
          warnings <- c(warnings, paste0("Sheet '", sn, "': measure '", m, "' not recognised, skipped."))
          next
        }

        # Year columns are those that look like 4-digit years
        year_cols <- grep("^\\d{4}$", names(df), value = TRUE)
        if (length(year_cols) == 0) {
          warnings <- c(warnings, paste0("Sheet '", sn, "': no year columns found, skipped."))
          next
        }

        # Populate session_data$entries with uploaded values. Routed through
        # store_measure() so a blank cell that clears a previously non-empty
        # value is recorded as a revision, exactly as if the user had cleared
        # the field manually. This does NOT count as a submission - the
        # indicator is not marked complete until the user reviews and clicks
        # Submit (explicit_submit is left untouched here).
        entry <- session_data$entries[[m]] %||% list()
        for (i in seq_len(nrow(df))) {
          bk <- df$breakdown_key[i]
          if (is.na(bk) || !nzchar(bk)) next
          if (is.null(entry[[bk]])) entry[[bk]] <- list()
          for (yr in year_cols) {
            val <- df[[yr]][i]
            if (!is.na(val) && nzchar(val)) {
              # Accept commas as decimal delimiters in uploaded workbooks
              num_val <- suppressWarnings(as.numeric(gsub(",", ".", val, fixed = TRUE)))
              if (!is.na(num_val)) {
                entry[[bk]][[yr]] <- num_val
                n_values <- n_values + 1
              }
            } else {
              # Blank cell: store "" to override any published data fallback
              # so the input field shows blank after upload.
              entry[[bk]][[yr]] <- ""
              if (!is.null(session_data$entries[[m]][[bk]][[yr]]) &&
                  session_data$entries[[m]][[bk]][[yr]] != "") {
                n_deleted <- n_deleted + 1
              }
            }
          }
        }
        store_measure(list(measure = m, values = entry), record_revisions = TRUE)
        n_measures <- n_measures + 1
      }

      # Publish to the committed snapshot immediately so the heatmap and the
      # input panels reflect the uploaded (and cleared) values right away.
      # This is a preview, not a submission: explicit_submit is untouched, so
      # the indicator still shows "Awaiting data input" until the user
      # reviews it and presses Submit.
      committed_entries(session_data$entries)
      committed_revisions(session_data$revisions)
      bump_ui()

      msg <- paste0("\u2713 Populated data for ", n_measures, " indicator(s) (",
                     n_values, " values loaded",
                     if (n_deleted > 0) paste0(", ", n_deleted, " removed") else "",
                     "). Review each indicator and click Submit to confirm.")
      if (length(warnings) > 0) {
        msg <- paste0(msg, " Warnings: ", paste(warnings, collapse = " "))
      }
      runjs(paste0(
        "var el = document.getElementById('upload_wb_status');",
        "if(el){ el.style.display='block'; el.style.color='#1F7A4D';",
        "el.style.background='#f2f7f4'; el.style.borderColor='#d7e5dd';",
        "el.innerText='", gsub("'", "\\\\'", msg), "'; }"
      ))
    }, error = function(e) {
      runjs(paste0(
        "var el = document.getElementById('upload_wb_status');",
        "if(el){ el.style.display='block'; el.style.color='#E63312';",
        "el.style.background='#fdf1f0'; el.style.borderColor='#f3c9c4';",
        "el.innerText='Error reading file: ",
        gsub("'", "\\\\'", conditionMessage(e)), "'; }"
      ))
    })
  })

  # ── Time Use table builder ───────────────────────────────────────────────────
  make_time_use_table <- function(n_rows, col_names, n_text_cols, table_id,
                                   row_text = NULL, saved = NULL,
                                   show_sums = FALSE, computed_codes = character(0),
                                   table_num = 1, no_update_active = FALSE,
                                   numeric_only = TRUE, submitted = FALSE) {
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
                   "border:1px solid #0f2843;background:#003189;text-align:center;min-width:60px;'>-</td>")
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
              if (numeric_only) {
                paste0("<td style='padding:2px;'>",
                       "<input type='text' inputmode='decimal' class='tu-num year-input' ",
                       "data-row='", r, "' data-col='", c, "' value='", saved_val, "' ",
                       "oninput=\"this.value=this.value.replace(/,/g,'.').replace(/[^0-9.\\-]/g,'')\" ",
                       "style='width:100%;min-width:60px;font-size:11px;border:1px solid #ccc;",
                       "border-radius:3px;padding:2px 4px;text-align:center;'/>",
                       "</td>")
              } else {
                paste0("<td style='padding:2px;'>",
                       "<input type='text' class='tu-num year-input' ",
                       "data-row='", r, "' data-col='", c, "' value='", htmltools::htmlEscape(saved_val), "' ",
                       "style='width:100%;min-width:60px;font-size:11px;border:1px solid #ccc;",
                       "border-radius:3px;padding:2px 4px;text-align:center;'/>",
                       "</td>")
              }
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
      "<div style='margin-top:10px;display:flex;align-items:center;gap:10px;'>",
      "<button onclick=\"saveTable('", table_id, "')\" ",
      "style='background:#f5f5f5;color:#333;border:1px solid #ccc;padding:6px 16px;border-radius:5px;cursor:pointer;font-size:12px;font-weight:600;'>",
      "Save and continue</button>",
      "<span id='status_", table_id, "' style='font-size:11px;color:#1F7A4D;font-weight:600;'>",
      if (isTRUE(submitted)) "&#10003; Submitted" else "", "</span>",
      "<span style='font-size:11px;color:#8a9bae;'>Stores your progress; submit both tables at the end of step 3.</span>",
      "</div></div>"
    )
  }

  # ── Last time use survey on record at the OECD (read-only, informational) ────
  # Sits directly inside the "Your time use survey on record" wb-card, so this
  # no longer draws its own bordered box around the content (that produced a
  # box-within-a-box look); the survey name/year and submission round are
  # emphasised with size/weight/colour instead.
  output$tu_last_survey_box <- renderUI({
    req(credentials$authenticated, credentials$country)
    rec <- last_tu_survey[[credentials$country]]
    detail <- if (is.null(rec) || nrow(rec) == 0) {
      tags$span(style = "font-size:12px;color:#8a9bae;font-style:italic;",
                "No previous time use survey on record.")
    } else {
      nm <- as.character(rec$survey_name[1])
      # Survey year is free text ("2020-21", "2019-2010", ...) so it is shown
      # verbatim; year_used is the data request round it was submitted in.
      yr <- if ("survey_year" %in% names(rec)) as.character(rec$survey_year[1]) else NA_character_
      yu <- if ("year_used"   %in% names(rec)) as.character(rec$year_used[1])   else NA_character_
      tags$div(
        style = "margin:8px 0 14px;padding:10px 14px;background:#f7f9fc;border-left:3px solid #003189;border-radius:0 6px 6px 0;",
        tags$div(
          style = "font-size:16px;font-weight:700;color:#003189;line-height:1.3;",
          paste0(nm, if (!is.na(yr) && nzchar(yr)) paste0(" (", yr, ")") else "")
        ),
        if (!is.na(yu) && nzchar(yu)) tags$div(
          style = "font-size:12.5px;font-weight:600;color:#1F2B3A;margin-top:4px;",
          paste0("Submitted as part of the ", yu, " data request.")
        )
      )
    }
    is_active <- isTRUE(session_data$tu_no_update)
    btn_class <- paste0("no-update-btn", if (is_active) " active" else "")
    status_text <- if (is_active) "\u2713 Marked as no update" else ""

    tags$div(
      tags$p(style = "font-size:11px;color:#55606B;margin:0 0 4px;line-height:1.5;",
             "This is the survey you previously submitted to us, shown here for your information. ",
             "If this is still your latest time use survey, you do not need to fill in the tables below; ",
             "just click ", tags$b("No time use data update to declare"), " to finish this page."),
      detail,
      tags$div(
        style = "display:flex;align-items:center;gap:10px;",
        tags$button(
          id = "tu_no_update_btn",
          onclick = "declareTUNoUpdate()",
          class = btn_class,
          style = "background:#f5f5f5;color:#555;border:1px solid #ccc;padding:6px 14px;border-radius:4px;cursor:pointer;font-size:12px;",
          "No time use data update to declare"
        ),
        tags$span(id = "tu_no_update_status",
                  style = "font-size:11px;color:#1F7A4D;font-weight:600;",
                  status_text)
      )
    )
  })

  # Compact identity banner for the Time Use hero, mirroring country_status
  # on the Well-being Submissions tab: country name + a status badge showing
  # whether a previous time use survey is on record.
  output$tu_country_status <- renderUI({
    req(credentials$authenticated)
    rec <- last_tu_survey[[credentials$country]]
    has_rec <- !is.null(rec) && nrow(rec) > 0
    yr <- if (has_rec && "survey_year" %in% names(rec)) as.character(rec$survey_year[1]) else NA_character_

    tags$div(class = "wb-status-row",
      tags$div(
        style = "display:flex;align-items:baseline;gap:12px;flex-wrap:wrap;",
        tags$span(style = "font-size:22px;font-weight:700;color:#1F2B3A;",
                  paste0("Welcome, ", credentials$country_name)),
        if (has_rec) {
          tags$span(
            style = paste0("font-size:11px;font-weight:600;background:#e8f1fb;color:#003189;",
                           "border:1px solid #c5d7ee;border-radius:12px;padding:2px 10px;"),
            paste0("Latest survey on record", if (!is.na(yr) && nzchar(yr)) paste0(": ", yr) else "")
          )
        } else {
          tags$span(
            style = paste0("font-size:11px;font-weight:600;background:#FFF8E1;color:#8a6d1a;",
                           "border:1px solid #F5C518;border-radius:12px;padding:2px 10px;"),
            "No previous time use survey on record"
          )
        }
      ),
      tags$p(
        style = "font-size:12.5px;color:#55606B;margin:8px 0 0;line-height:1.5;",
        if (has_rec) {
          paste0("Details of the latest Time Use survey that ", credentials$country_name, " shared are shown below. ",
                 "Update them if you have newer data, or confirm there is no change.")
        } else {
          paste0("We have no previous time use survey on record for ", credentials$country_name,
                 ". Please complete the survey details and tables below.")
        }
      )
    )
  })

  # ── Time use survey details: name, year and notes ───────────────────────────
  # Held in session_data$tu_meta so that editing them triggers the same
  # auto-save as any other data. A debounced observer persists typing on its
  # own, so there is nothing for the user to press.
  tu_meta_saved_at <- reactiveVal(NULL)
  # FALSE until the browser has echoed back the values restored at login.
  # Until then a blank read is treated as "not loaded yet" rather than as the
  # user having cleared the fields; afterwards, clearing them does persist.
  tu_meta_synced   <- reactiveVal(FALSE)

  tu_meta_inputs <- debounce(reactive({
    list(survey_name = input$tu_survey_name %||% "",
         survey_year = input$tu_survey_year,
         notes       = input$tu_notes %||% "")
  }), 1200)

  tu_meta_is_blank <- function(m) {
    blank <- function(v) {
      is.null(v) || length(v) != 1 || is.na(v) || !nzchar(trimws(as.character(v)))
    }
    blank(m$survey_name) && blank(m$survey_year) && blank(m$notes)
  }

  # Compares meta values after normalising type/whitespace quirks introduced
  # by the browser round-trip (e.g. a restored numeric year can come back as
  # a slightly different numeric type than what was stored). Without this,
  # every fresh login re-triggers a "Saved at <login time>" on an unchanged
  # survey record, which looks like another country's save leaking through.
  tu_meta_normalize <- function(m) {
    yr <- suppressWarnings(as.numeric(m$survey_year %||% NA))
    list(
      survey_name = trimws(as.character(m$survey_name %||% "")),
      survey_year = if (length(yr) != 1 || is.na(yr)) NA_real_ else yr,
      notes       = trimws(as.character(m$notes %||% ""))
    )
  }

  observe({
    req(credentials$authenticated)
    m   <- tu_meta_inputs()
    cur <- isolate(session_data$tu_meta)
    if (!isolate(tu_meta_synced())) {
      if (tu_meta_is_blank(m) && length(cur) > 0 && !tu_meta_is_blank(cur)) return()
      tu_meta_synced(TRUE)
    }
    if (identical(tu_meta_normalize(m), tu_meta_normalize(cur))) return()
    if (country_is_frozen()) return()
    session_data$tu_meta <- m
    tu_meta_saved_at(Sys.time())
  })

  output$tu_meta_status <- renderText({
    ts <- tu_meta_saved_at()
    if (is.null(ts)) "" else paste0("\u2713 Saved at ", format(ts, "%H:%M"))
  })

  # ── Time Use table outputs ───────────────────────────────────────────────────
  output$time_use_table1_ui <- renderUI({
    HTML(make_time_use_table(31, time_use_col_names_1, 2, "tu_table1",
                              row_text = time_use_row_text_1,
                              saved    = session_data$tu_draft_1 %||% session_data$time_use_1,
                              show_sums = TRUE,
                              computed_codes = c("T"),
                              table_num = 1,
                              no_update_active = isTRUE(session_data$tu_no_update_1),
                              submitted = !is.null(session_data$time_use_1)))
  })
  output$time_use_table2_ui <- renderUI({
    HTML(make_time_use_table(30, time_use_col_names_2, 2, "tu_table2",
                              row_text = time_use_row_text_2,
                              saved    = session_data$tu_draft_2 %||% session_data$time_use_2,
                              table_num = 2,
                              no_update_active = isTRUE(session_data$tu_no_update_2),
                              numeric_only = FALSE,
                              submitted = !is.null(session_data$time_use_2)))
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
             "<td style='font-size:11px;padding:3px 0;color:#333;'>", if (is.na(val)) "-" else linkify(val), "</td>",
             "</tr>")
    }, resp$label$label, resp$label$response, SIMPLIFY = TRUE), collapse = "")
    oecd_html <- paste0("<table style='width:100%;border-collapse:collapse;'>", oecd_rows, "</table>")

    safe_indic <- gsub("\\.", "_", resp$indic)

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

      # Escape HTML entities for safe embedding in textarea content
      safe_pre_val <- gsub("&", "&amp;", pre_val, fixed = TRUE)
      safe_pre_val <- gsub("<", "&lt;", safe_pre_val, fixed = TRUE)
      safe_pre_val <- gsub(">", "&gt;", safe_pre_val, fixed = TRUE)

      if (is.na(q_val) && is_yesno) {
        yes_active <- if (identical(tolower(pre_val), "yes")) " active" else ""
        no_active  <- if (identical(tolower(pre_val), "no"))  " active" else ""
        parts[i] <- paste0(
          "<div style='margin-bottom:8px;'>",
          "<p style='font-size:11px;font-weight:600;color:#444;margin:0 0 4px;'>", q_lbl, "</p>",
          "<input type='hidden' id='", inp_id, "' class='resp-input' data-idx='", i, "' value='", pre_val, "'/>",
          "<div class='toggle-group'>",
          "<button type='button' class='toggle-btn", yes_active, "' onclick=\"setToggle('", inp_id, "', this, 'Yes')\">Yes</button>",
          "<button type='button' class='toggle-btn", no_active, "' onclick=\"setToggle('", inp_id, "', this, 'No')\">No</button>",
          "</div></div>"
        )
      } else if (is.na(q_val)) {
        parts[i] <- paste0(
          "<div style='margin-bottom:8px;'>",
          "<p style='font-size:11px;font-weight:600;color:#444;margin:0 0 3px;'>", q_lbl, "</p>",
          "<textarea id='", inp_id, "' class='resp-input resp-textarea' data-idx='", i, "' ",
          "placeholder='Enter response\u2026' rows='1' ",
          "style='width:100%;font-size:11px;border:1px solid #ccc;border-radius:4px;padding:6px 8px;",
          "box-sizing:border-box;resize:vertical;overflow:hidden;line-height:1.5;font-family:inherit;",
          "min-height:32px;transition:min-height 0.15s ease;'>",
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
    # The ONLY reactive triggers for this (expensive) rebuild are an explicit
    # user action and the underlying data. Everything else is isolated.
    ui_refresh()
    d <- dat_rv()
    # Countries with no published data at all (e.g. UKR) legitimately give a
    # zero-row frame: build the empty grid rather than req()-ing out, which
    # would leave the heatmap spinner running forever.
    req(!is.null(d))

    # Keep full data for pre-filling all breakdowns in the input panels
    d_full <- d %>%
      select(measure, sex, age, education_lev, time_period, obs_value, obs_status) %>%
      mutate(time_period = as.numeric(time_period))

    # Non-used responses for this country (submitted previously but not incorporated)
    d_nonused <- nonused_dat %>%
      filter(ref_area == credentials$country) %>%
      select(measure, sex, age, education_lev, time_period, obs_value) %>%
      mutate(time_period = as.numeric(time_period))

    # Heatmap only shows _T aggregates and excludes _DEP/_VER measures
    d <- d %>% filter(sex == "_T", age == "_T", education_lev == "_T",
                      !grepl("_DEP$|_VER$", measure))
    heatmap_measures <- unique(measure_list$measure[!grepl("_DEP$|_VER$", measure_list$measure)])

    entries <- isolate(committed_entries())
    years   <- 2004:2026

    # val_lookup: country averages only (for heatmap cells and charts)
    val_lookup <- d %>%
      select(measure, time_period, obs_value) %>%
      mutate(time_period = as.numeric(time_period))

    # full_val_lookup: all breakdowns (for pre-filling input rows)
    # Maps breakdown_key -> list(measure_code, sex, age, education_lev)
    breakdown_filters <- list(
      country_avg = list(sex = "_T", age = "_T", edu = "_T"),
      male        = list(sex = "M",  age = "_T", edu = "_T"),
      female      = list(sex = "F",  age = "_T", edu = "_T"),
      young       = list(sex = "_T", age = "YOUNG", edu = "_T"),
      middle_aged = list(sex = "_T", age = "MID",   edu = "_T"),
      old         = list(sex = "_T", age = "OLD",   edu = "_T"),
      primary     = list(sex = "_T", age = "_T", edu = "ISCED11_1"),
      secondary   = list(sex = "_T", age = "_T", edu = "ISCED11_2_3"),
      tertiary    = list(sex = "_T", age = "_T", edu = "ISCED11_5T8")
    )
    # vert/dep use separate measures with _VER/_DEP suffix
    dep_vert_keys <- c("vert", "dep")

    # Unit lookup from dictionary (for display in enter-data panels)
    unit_lookup <- dict %>%
      select(measure, unit) %>%
      distinct() %>%
      { setNames(.$unit, .$measure) }

    # obs_status -> flag mapping (A = normal, W = not a standard flag)
    status_to_flag <- c(B = "B", D = "D", E = "E", P = "P", U = "U")

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
        lapply(heatmap_measures, function(m) {
          if (m %in% time_use_measures) return("")
          make_year_chart(m, prefix)
        }),
        heatmap_measures
      )
    }
    # The charts depend only on the underlying data, never on submissions, so
    # cache them across rebuilds instead of regenerating on every Submit.
    if (is.null(chart_cache$key) || !identical(chart_cache$key, credentials$country)) {
      chart_cache$sub <- make_charts_lookup("sub_")
      chart_cache$cov <- make_charts_lookup("cov_")
      chart_cache$key <- credentials$country
    }
    sub_charts_lookup <- chart_cache$sub
    cov_charts_lookup <- chart_cache$cov

    # dat_tidy: full grid of measures × years
    dat_tidy <- d %>%
      select(measure, time_period, obs_value) %>%
      complete(measure = heatmap_measures, time_period = years) %>%
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
      paste0("<div style='flex:1 1 0;min-width:0;text-align:center;font-size:9px;color:#888;'>", lbl, "</div>")
    }), collapse = "")
    axis_row <- paste0(
      "<div style='display:flex;flex-direction:row;align-items:center;width:100%;margin-bottom:4px;'>",
      "<div style='flex:0 0 20%;'></div>",
      "<div style='flex:1 1 0;min-width:0;display:flex;flex-direction:row;'>", year_axis_cells, "</div>",
      "<div style='flex:0 0 200px;'></div></div>"
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
      if (m %in% no_country_average) {
        list(
          list(key="male",        label="Men",                  bold=FALSE),
          list(key="female",      label="Women",                bold=FALSE),
          list(key="young",       label=al$young,                bold=FALSE),
          list(key="middle_aged", label=al$middle_aged,          bold=FALSE),
          list(key="old",         label=al$old,                  bold=FALSE),
          list(key="age_flag",    label="",                      bold=FALSE, is_age_flag=TRUE),
          list(key="primary",     label="Primary (ISCED levels 0-2)",   bold=FALSE),
          list(key="secondary",   label="Secondary (ISCED levels 3-4)", bold=FALSE),
          list(key="tertiary",    label="Tertiary (ISCED levels 5-8)",  bold=FALSE)
        )
      } else if (m %in% all_rows) {
        list(
          list(key="country_avg", label="Country average",       bold=TRUE),
          list(key="male",        label="Men",                  bold=FALSE),
          list(key="female",      label="Women",                bold=FALSE),
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
          list(key="vert",        label="Vertical inequality",   bold=FALSE,
               tooltip="To calculate vertical inequality data (bottom 20% and top 20%), sort the data you have from the lowest score given to the highest level of the indicator declared, and (after weighting) you divide the results in five equal parts. Then, calculate the average of the group with the highest 20% and lowest 20% of scores."),
          list(key="dep",         label="Deprivation",           bold=FALSE,
               tooltip="Share of people reporting a score equal to 4 or below"),
          list(key="male",        label="Men",                  bold=FALSE),
          list(key="female",      label="Women",                bold=FALSE),
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
          list(key="male",        label="Men",            bold=FALSE),
          list(key="female",      label="Women",          bold=FALSE)
        )
      } else {
        list(list(key="country_avg", label="Country average", bold=TRUE))
      }
    }

    make_year_inputs <- function(m) {
      # Read from session_data$entries (live data) so that uploaded-but-not-yet-
      # submitted values appear in the input fields for user review.
      saved <- isolate(session_data$entries[[m]])
      saved_flags <- isolate(session_data$flags[[m]])
      rows  <- row_defs(m)
      label_w <- "180px"
      yr_header <- paste(sapply(years, function(yr) {
        paste0("<div style='flex:1;text-align:center;font-size:8px;color:#888;min-width:32px;'>", yr, "</div>")
      }), collapse = "")
      header_html <- paste0(
        "<div style='display:flex;align-items:center;margin-bottom:2px;'>",
        "<div style='flex:0 0 ", label_w, ";'></div>",
        "<div style='flex:1;display:flex;'>", yr_header, "</div></div>"
      )

      # Unit hint per row key
      base_unit <- unit_lookup[m]
      if (is.na(base_unit)) base_unit <- ""
      row_unit <- function(key) {
        if (key == "dep") return("(%)")
        if (key == "vert") return("(ratio)")
        # Shorten base unit for display
        u <- base_unit
        if (grepl("ercent|hare", u, ignore.case = TRUE)) return("(%)")
        if (grepl("ours per day", u, ignore.case = TRUE)) return("(hrs/day)")
        if (grepl("ours per week", u, ignore.case = TRUE)) return("(hrs/week)")
        if (grepl("inutes per day", u, ignore.case = TRUE)) return("(min/day)")
        if (grepl("ean.*satisf|ean.*trust|ean.*score|ean.*life", u, ignore.case = TRUE)) return("(0-10 scale)")
        if (nzchar(u)) return(paste0("(", substr(u, 1, 20), ")"))
        ""
      }

      # The "Age groups differ?" note is free text, so it lives in its own
      # store. Sessions saved before that store existed kept it among the
      # numeric values, so fall back to there.
      saved_age_note <- isolate(session_data$age_notes[[m]])
      if (is.null(saved_age_note)) saved_age_note <- saved[["age_flag"]][["note"]]
      saved_age_note <- if (length(saved_age_note) == 1 && is.character(saved_age_note)) {
        saved_age_note
      } else ""

      # Flag options
      flag_codes <- c("", "B", "E", "P", "D", "U")
      flag_labels <- c("-", "B", "E", "P", "D", "U")

      # Helper: look up existing data row from d_full, falling back to non-used
      # Returns list(row = data.frame, source = "published"|"nonused"|"none")
      lookup_existing <- function(r_key, yr) {
        if (r_key %in% dep_vert_keys) {
          suffix <- if (r_key == "vert") "_VER" else "_DEP"
          meas <- paste0(m, suffix)
          sx <- "_T"; ag <- "_T"; ed <- "_T"
        } else if (!is.null(breakdown_filters[[r_key]])) {
          bf <- breakdown_filters[[r_key]]
          meas <- m; sx <- bf$sex; ag <- bf$age; ed <- bf$edu
        } else {
          return(list(row = data.frame(), source = "none"))
        }
        published <- d_full %>%
          filter(measure == meas, sex == sx, age == ag, education_lev == ed,
                 time_period == yr)
        if (nrow(published) > 0) return(list(row = published, source = "published"))
        nonused <- d_nonused %>%
          filter(measure == meas, sex == sx, age == ag, education_lev == ed,
                 time_period == yr)
        if (nrow(nonused) > 0) return(list(row = nonused, source = "nonused"))
        list(row = data.frame(), source = "none")
      }

      row_htmls <- sapply(rows, function(r) {
        # Special row: age group difference flag (text input spanning full width)
        if (!is.null(r$is_age_flag) && isTRUE(r$is_age_flag)) {
          flag_val <- saved_age_note
          return(paste0(
            "<div style='display:flex;align-items:center;margin-bottom:4px;margin-top:2px;'>",
            "<div style='flex:0 0 ", label_w, ";font-size:10px;color:#888;padding-right:6px;text-align:right;font-style:italic;'>",
            "Age groups differ?</div>",
            "<div style='flex:1;'>",
            "<input type='text' class='age-note-input' data-row='age_flag' data-year='note' ",
            "value='", htmltools::htmlEscape(flag_val, attribute = TRUE), "' ",
            "placeholder='If your age groups differ from the above, describe here' ",
            "style='width:100%;padding:3px 6px;border:1px solid #dde1e6;border-radius:4px;",
            "font-size:10px;color:#555;'/>",
            "</div></div>"
          ))
        }
        # Unit hint for this row
        unit_hint <- row_unit(r$key)

        # Combined value + flag cells (stacked within each year column)
        cells <- sapply(years, function(yr) {
          # Look up existing data once per cell (published, then non-used)
          lookup    <- lookup_existing(r$key, yr)
          existing_row  <- lookup$row
          has_existing   <- nrow(existing_row) > 0

          # Value: session save > published/non-used data
          # The original, un-edited OECD figure for this cell (published data,
          # falling back to non-used data), independent of anything the
          # country has since entered or saved. Embedded as data-default so
          # "Revert to default" can restore it client-side without a round
          # trip, mirroring how Clear all works.
          default_val  <- if (has_existing && !is.na(existing_row$obs_value[1])) existing_row$obs_value[1] else NA
          default_flag <- if (has_existing && lookup$source == "published" &&
                               "obs_status" %in% names(existing_row) &&
                               !is.na(existing_row$obs_status[1]) &&
                               existing_row$obs_status[1] %in% names(status_to_flag)) {
            unname(status_to_flag[existing_row$obs_status[1]])
          } else ""
          default_attr <- if (!is.na(default_val)) paste0("data-default='", default_val, "'") else "data-default=''"

          # Highlight cells whose value was previously submitted but not
          # published (non-used data), matching the purple legend colour.
          is_nonused <- identical(lookup$source, "nonused")
          input_bg <- if (is_nonused) "background:#C4B5D4;border-color:#b3a1c7;" else ""

          v <- if (!is.null(saved) && !is.null(saved[[r$key]]) &&
                   !is.null(saved[[r$key]][[as.character(yr)]])) {
            saved[[r$key]][[as.character(yr)]]
          } else if (!is.na(default_val)) {
            default_val
          } else NA
          has_val          <- !is.na(v)
          value_attr       <- if (has_val) paste0("value='", v, "'") else ""
          placeholder_attr <- if (!has_val) "placeholder='\u00b7'" else ""

          # Flag: session save > obs_status from published data > empty
          # (non-used data has no obs_status column)
          saved_f <- if (!is.null(saved_flags) && !is.null(saved_flags[[r$key]]) &&
                         !is.null(saved_flags[[r$key]][[as.character(yr)]])) {
            saved_flags[[r$key]][[as.character(yr)]]
          } else default_flag
          opts_html <- paste(mapply(function(code, lbl) {
            sel <- if (identical(code, saved_f)) " selected" else ""
            paste0("<option value='", code, "'", sel, ">", lbl, "</option>")
          }, flag_codes, flag_labels, SIMPLIFY = TRUE), collapse = "")

          paste0(
            "<div style='flex:1;min-width:32px;padding:0 1px;display:flex;flex-direction:column;'>",
            "<input type='text' inputmode='decimal' class='year-input' ",
            "data-row='", r$key, "' data-year='", yr, "' ",
            value_attr, " ", placeholder_attr, " ", default_attr,
            " oninput=\"this.value=this.value.replace(/,/g,'.').replace(/[^0-9.\\-]/g,'')\"",
            " style='width:100%;padding:2px 1px;border:1px solid #dde1e6;border-radius:4px 4px 0 0;",
            "font-size:10px;text-align:center;border-bottom:none;margin:0;box-sizing:border-box;",
            input_bg, "'/>",
            "<select class='flag-select' data-row='", r$key, "' data-year='", yr, "' ",
            "data-default-flag='", default_flag, "' ",
            "style='width:100%;padding:0;border:1px solid #dde1e6;border-radius:0 0 4px 4px;",
            "font-size:7px;text-align:center;color:#999;background:#fafbfc;cursor:pointer;",
            "line-height:1;height:14px;-webkit-appearance:none;appearance:none;margin:0;box-sizing:border-box;'>",
            opts_html, "</select>",
            "</div>"
          )
        })
        tip_html <- if (!is.null(r$tooltip)) {
          paste0("<span class='info-tooltip'>\u2139\uFE0E",
                 "<span class='tooltip-text'>", htmltools::htmlEscape(r$tooltip), "</span></span>")
        } else ""
        # Unit hint shown in lighter text after the label
        unit_span <- if (nzchar(unit_hint)) {
          paste0(" <span style='font-size:9px;color:#999;font-weight:400;'>", unit_hint, "</span>")
        } else ""
        paste0(
          "<div style='display:flex;align-items:center;margin-bottom:3px;'>",
          "<div style='flex:0 0 ", label_w, ";font-size:11px;color:#444;padding-right:6px;text-align:right;",
          if (r$bold) "font-weight:600;" else "", "'>", r$label, unit_span, tip_html, "</div>",
          "<div style='flex:1;display:flex;'>", paste(cells, collapse=""), "</div></div>"
        )
      })
      # Flag legend. Includes a slot (filled in per-panel, once the safe_id
      # is known) so the "Clear all" button sits next to the flag key rather
      # than next to the "Enter Data" title above.
      flag_legend <- paste0(
        "<div style='display:flex;align-items:center;justify-content:space-between;",
        "flex-wrap:wrap;gap:10px;margin-top:6px;margin-left:", label_w, ";'>",
        "<div style='font-size:9px;color:#999;padding:4px 8px;",
        "background:#f8f9fa;border-radius:4px;display:inline-block;'>",
        "<strong style='color:#666;'>Flags:</strong>",
        " B = Break in series &nbsp;&middot;&nbsp; ",
        "E = Estimate &nbsp;&middot;&nbsp; ",
        "P = Provisional &nbsp;&middot;&nbsp; ",
        "D = Definition differs &nbsp;&middot;&nbsp; ",
        "U = Low reliability",
        # Info button: the icon itself opens the SDMX guidelines; the hover
        # tooltip explains where the link goes.
        "<a class='info-tooltip' target='_blank' style='text-decoration:none;' ",
        "href='https://sdmx.org/wp-content/uploads/CL_OBS_STATUS_v2_3-for-publication.docx'>&#8505;&#65038;",
        "<span class='tooltip-text'>Flag codes follow the SDMX observation status standard. ",
        "Click to open the full SDMX guidelines (Word document).</span></a>",
        "</div>",
        "__CLEAR_ALL_SLOT__",
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
          saved_resp   = isolate(session_data$responses[[m]]),
          prefill_resp = if (!is.null(prefill_for_country)) prefill_for_country[[m]] else NULL
        )
      }),
      unique(dat_tidy$measure)
    )

    revisions <- isolate(committed_revisions())

    # Only an explicit Submit may colour the heatmap. Values conserved by
    # "Save and continue" (or loaded by a template upload) live in
    # session_data/committed_entries so the input panels keep them, but they
    # are filtered out here so the heatmap stays exactly as it was.
    explicit_submit_now <- isolate(session_data$explicit_submit)
    submitted_measures <- names(explicit_submit_now)[
      vapply(explicit_submit_now, isTRUE, logical(1))
    ]
    entries <- entries[names(entries) %in% submitted_measures]

    submitted_df <- if (length(entries) > 0) {
      is_filled <- function(v) !is.null(v) && !is.na(v) && v != ""
      bind_rows(lapply(names(entries), function(m) {
        breakdowns <- entries[[m]]
        if (!is.list(breakdowns)) return(data.frame())
        breakdowns <- breakdowns[vapply(breakdowns, is.list, logical(1))]
        rev_m <- revisions[[m]] %||% list()

        # A year counts as submitted/revised if ANY breakdown (country average,
        # sex, age, education) has a value / a recorded revision for that year.
        yrs <- unique(unlist(lapply(breakdowns, names)))
        if (length(yrs) == 0) return(data.frame())

        rev_yrs <- unique(unlist(lapply(rev_m, names)))

        data.frame(
          measure     = m,
          time_period = as.numeric(yrs),
          submitted   = vapply(yrs, function(yr) {
            any(vapply(breakdowns, function(row_data) is_filled(row_data[[yr]]),
                       logical(1)))
          }, logical(1)),
          # A year is a revision of existing data if ANY breakdown's submitted
          # value differs from the published figure for that same breakdown.
          # (Must cover all breakdowns: some measures, e.g. 8_2 voter turnout,
          # have no country-average row at all.)
          differs     = vapply(yrs, function(yr) {
            any(vapply(names(breakdowns), function(bk) {
              v <- suppressWarnings(as.numeric(breakdowns[[bk]][[yr]]))
              if (length(v) != 1 || is.na(v)) return(FALSE)
              if (bk %in% dep_vert_keys) {
                meas <- paste0(m, if (bk == "vert") "_VER" else "_DEP")
                sx <- "_T"; ag <- "_T"; ed <- "_T"
              } else if (!is.null(breakdown_filters[[bk]])) {
                bf <- breakdown_filters[[bk]]
                meas <- m; sx <- bf$sex; ag <- bf$age; ed <- bf$edu
              } else return(FALSE)
              o <- d_full$obs_value[d_full$measure == meas & d_full$sex == sx &
                                    d_full$age == ag & d_full$education_lev == ed &
                                    d_full$time_period == as.numeric(yr)]
              # Previously submitted (not used) figures count as existing data
              # too: overwriting one with a different value is a revision of
              # existing data, not a fresh submission.
              if (length(o) == 0 || is.na(o[1])) {
                o <- d_nonused$obs_value[d_nonused$measure == meas & d_nonused$sex == sx &
                                         d_nonused$age == ag & d_nonused$education_lev == ed &
                                         d_nonused$time_period == as.numeric(yr)]
              }
              length(o) > 0 && !is.na(o[1]) && abs(v - o[1]) > 1e-8
            }, logical(1)))
          }, logical(1)),
          revised     = yrs %in% rev_yrs,
          stringsAsFactors = FALSE
        )
      }))
    } else {
      data.frame(measure = character(), time_period = numeric(),
                 submitted = logical(), differs = logical(), revised = logical())
    }

    # Measures marked "no data update to declare"
    no_updates_now <- isolate(session_data$no_updates)
    no_update_measures <- names(no_updates_now)[
      vapply(no_updates_now, isTRUE, logical(1))
    ]

    # Combined: measures that are "done" (either submitted or no-update)
    done_measures <- union(submitted_measures, no_update_measures)

    # Time-use indicators (4_1, 4_2, 4_3, 7_2) need BOTH their own submission
    # (values entered, or "no data update to declare") AND a completed Time Use
    # tab. Three states:
    #   own indicator not submitted            -> awaiting data input
    #   own indicator submitted, Time Use open -> awaiting Time Use submission
    #   own indicator submitted + Time Use in  -> complete
    tu1_done <- !is.null(isolate(session_data$time_use_1))
    tu2_done <- !is.null(isolate(session_data$time_use_2))
    tu_complete <- isTRUE(isolate(session_data$tu_no_update)) || (tu1_done && tu2_done)

    # Non-used data (for heatmap cell coloring, year-specific). Measures with
    # no country-average row (e.g. 8_2 voter turnout) are matched on their
    # population-group rows instead of the _T/_T/_T aggregate.
    nonused_heatmap <- d_nonused %>%
      filter(!grepl("_DEP$|_VER$", measure), !is.na(obs_value)) %>%
      filter(if_else(measure %in% no_country_average,
                     !(sex == "_T" & age == "_T" & education_lev == "_T"),
                     sex == "_T" & age == "_T" & education_lev == "_T")) %>%
      select(measure, time_period) %>%
      distinct() %>%
      mutate(has_nonused = TRUE)

    # Published population-group data for measures collected by breakdown only.
    # On the submissions heatmap these drive the "already published" (green)
    # cells; the coverage heatmap keeps using country-level figures.
    group_only_published <- d_full %>%
      filter(measure %in% no_country_average,
             !(sex == "_T" & age == "_T" & education_lev == "_T"),
             !is.na(obs_value)) %>%
      distinct(measure, time_period) %>%
      mutate(group_published = TRUE)

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
        { if (!coverage_mode) filter(., measure %in% submission_measures)
          else filter(., !measure %in% coverage_hidden) } %>%
        mutate(time_period = as.numeric(time_period)) %>%
        left_join(submitted_df, by = c("measure", "time_period")) %>%
        left_join(nonused_heatmap, by = c("measure", "time_period")) %>%
        { if (coverage_mode)
            left_join(., coverage_counts, by = c("measure", "time_period"))
          else
            mutate(., n_countries = NA_integer_)
        } %>%
        # Breakdown-only measures (8_2 voter turnout): on the submissions
        # heatmap a year counts as published when population-group data exist,
        # regardless of whether a country-level figure is available.
        left_join(group_only_published, by = c("measure", "time_period")) %>%
        mutate(
          group_published = replace_na(group_published, FALSE),
          obs_value = if (coverage_mode) obs_value else if_else(
            measure %in% no_country_average,
            if_else(group_published, 1, NA_real_),
            obs_value
          )
        ) %>%
        mutate(
          submitted = replace_na(submitted, FALSE),
          revised = replace_na(revised, FALSE),
          differs = replace_na(differs, FALSE),
          # Revision of existing data: submitted value differs from the
          # published figure in at least one breakdown.
          revised_existing = !coverage_mode & submitted & differs,
          # Submitted this session and then overwritten with a different value.
          revised_session  = !coverage_mode & submitted & revised & is.na(obs_value),
          # A revision was recorded for this year (e.g. a blank cell uploaded
          # over a previously-filled value) but nothing is currently filled -
          # i.e. the value was cleared/removed rather than replaced.
          revised_cleared  = !coverage_mode & revised & !submitted,
          has_nonused = replace_na(has_nonused, FALSE),
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
            revised_existing                                ~ "#009EDB",
            revised_cleared                                 ~ "#009EDB",
            revised_session                                 ~ "#B4530A",
            !is.na(obs_value)                               ~ "#1F7A4D",
            # Existing data (green, above) and previously submitted-but-not-used
            # (purple, here) both outrank "submitted this session": resubmitting
            # such a cell unchanged must leave its colour alone. Only a value
            # that actually differs flips it, via revised_existing above.
            !coverage_mode & has_nonused                    ~ "#C4B5D4",
            submitted                                       ~ "#F89C1C",
            coverage_mode & is_no_concern & n_countries > 0 ~ "#FCE4B8",
            coverage_mode & n_countries > 0                 ~ gap_color,
            TRUE                                            ~ "#D9DDE3"
          ),
          tooltip = case_when(
            revised_existing                   ~ "Revision of existing data",
            revised_cleared                    ~ "Revision: previously reported value removed",
            revised_session                    ~ "Submitted this session, then revised",
            !is.na(obs_value)                  ~ "",
            !coverage_mode & has_nonused       ~ "Previously submitted (not used)",
            coverage_mode & n_countries > 0    ~ paste0(n_countries, " of ", n_total_countries, " countries have data"),
            TRUE                               ~ ""
          )
        ) %>%
        select(measure, time_period, color, tooltip, cat, group) %>%
        group_by(measure) %>%
        mutate(
          boxes = paste0(
            "<div style='flex:1 1 0;min-width:0;height:15px;background:", color,
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

          # Completion state must be computed BEFORE the highlight styles so a
          # finished indicator can drop its amber/blue highlighting.
          own_done = measure %in% done_measures,
          # Time-use indicators additionally require the Time Use tab to be
          # submitted before they count as complete.
          is_done     = if_else(is_time_use, own_done & tu_complete, own_done),
          awaiting_tu = is_time_use & own_done & !tu_complete,

          row_border = case_when(
            is_done     ~ "",
            needs_input ~ "border-left:3px solid #F89C1C;background:#fffbf4;",
            is_time_use ~ "border-left:3px solid #009EDB;background:#f0faff;",
            TRUE        ~ ""
          ),
          row_hover = case_when(
            is_done     ~ "",
            needs_input ~ "#fffbf4",
            is_time_use ~ "#f0faff",
            TRUE        ~ ""
          ),
          badge_html = case_when(
            coverage_mode ~ "",
            !needs_input  ~ "",
            is_done       ~ "<span style='font-size:9px;background:#1F7A4D;color:white;border-radius:3px;padding:1px 4px;white-space:nowrap;'>&#10003; Complete</span>",
            awaiting_tu   ~ "<span title='Submit the Time Use tab to finish this indicator' style='font-size:9px;background:#009EDB;color:white;border-radius:3px;padding:1px 4px;white-space:nowrap;'>&#8987; Awaiting Time Use submission</span>",
            TRUE          ~ "<span title='New data required' style='font-size:9px;background:#F89C1C;color:white;border-radius:3px;padding:1px 4px;white-space:nowrap;'>&#9888; Awaiting data input</span>"
          ),
          panel_border = case_when(
            is_done     ~ "border-left:3px solid #1F7A4D;",
            needs_input ~ "border-left:3px solid #F89C1C;",
            is_time_use ~ "border-left:3px solid #009EDB;",
            TRUE        ~ "border-left:3px solid #D4D9DF;"
          ),

          is_no_update = measure %in% no_update_measures,

          note_html = sapply(measure, function(m) {
            nt <- measure_notes[[m]]
            if (is.null(nt) || !nzchar(nt)) return("")
            paste0(
              "<div style='background:#f0f6ff;border:1px solid #c5d7ee;border-radius:6px;padding:10px 14px;margin-bottom:12px;'>",
              "<strong style='font-size:12px;color:#003189;'>Please note</strong>",
              "<p style='font-size:11px;color:#444;margin:4px 0 0;line-height:1.5;'>",
              htmltools::htmlEscape(nt), "</p></div>"
            )
          }),

          comment_html = sapply(measure, function(m) {
            country_comments <- oecd_comments[[country_iso]]
            cmt <- if (!is.null(country_comments) && m %in% names(country_comments)) country_comments[[m]] else NULL
            if (is.null(cmt) || !nzchar(cmt)) return("")
            # Optional second part: the specific reason the figure could not be
            # used, where the comments file supplies one.
            country_reasons <- oecd_reasons[[country_iso]]
            rsn <- if (!is.null(country_reasons) && m %in% names(country_reasons)) country_reasons[[m]] else NULL
            rsn_html <- if (!is.null(rsn) && nzchar(rsn)) {
              paste0(
                "<p style='font-size:11px;color:#444;margin:8px 0 0;line-height:1.5;'>",
                "<strong>Reason not included:</strong> ",
                htmltools::htmlEscape(rsn), "</p>"
              )
            } else ""
            paste0(
              "<div style='background:#FFF8E1;border:1px solid #F5C518;border-radius:6px;padding:10px 14px;margin-bottom:12px;'>",
              "<strong style='font-size:12px;color:#8a6d1a;'>OECD Comment</strong>",
              "<p style='font-size:11px;color:#444;margin:4px 0 0;line-height:1.5;'>",
              htmltools::htmlEscape(cmt), "</p>",
              rsn_html, "</div>"
            )
          }),

          panel_body = mapply(function(ni, itu, sid, mn, yi, yc, q, oqh, cqh, def, tech, unt, lbl, is_nu, nth, cth) {
            if (ni) {
              nu_active <- if (is_nu) " active" else ""
              # "Clear all" is injected into the flag legend at the bottom of
              # the input grid (see flag_legend's __CLEAR_ALL_SLOT__), so it
              # sits next to the flags rather than next to the title above.
              clear_btn_html <- paste0(
                # Grouped in one flex container (rather than left as loose
                # siblings) so both buttons sit together at the right edge of
                # the space-between row above, instead of being spread out
                # evenly across it.
                "<div style='display:flex;align-items:center;gap:8px;margin-left:auto;'>",
                "<button onclick=\"clearAllInputs('", sid, "')\" ",
                "title='Clear every value and flag in the grid below' ",
                "style='background:none;border:none;padding:0;color:#8a9bae;font-size:10px;",
                "text-decoration:underline;cursor:pointer;font-weight:500;white-space:nowrap;'>",
                "Clear all</button>",
                "<span style='color:#ccc;font-size:10px;'>|</span>",
                "<button onclick=\"revertToDefaults('", sid, "')\" ",
                "title='Revert every value and flag below to the existing OECD data, before any entries were made' ",
                "style='background:none;border:none;padding:0;color:#8a9bae;font-size:10px;",
                "text-decoration:underline;cursor:pointer;font-weight:500;white-space:nowrap;'>",
                "Revert to default</button>",
                "</div>"
              )
              yi_final <- sub("__CLEAR_ALL_SLOT__", clear_btn_html, yi, fixed = TRUE)
              paste0(
                "<div style='display:flex;flex-direction:row;gap:16px;text-align:left;'>",
                "<div style='flex:1;overflow:auto;'><strong style='font-size:13px;'>OECD Question Format</strong>",
                "<div style='margin-top:6px;'>", oqh, "</div></div>",
                "<div style='flex:1;overflow:auto;'><strong style='font-size:13px;'>Country Question Format</strong>",
                "<div style='margin-top:6px;'>", cqh, "</div></div>",
                "</div>",
                "<hr style='margin:0;border:none;border-top:1px solid #ddd;'/>",
                nth,
                cth,
                "<div style='width:100%;'>",
                "<div style='text-align:center;'>",
                "<strong style='font-size:13px;'>Enter Data</strong>",
                "<div style='font-size:11px;color:#888;margin:2px 0 0;'>Please add any comments on values to the <i>Other useful information</i> box above and any comments on the breaks to the <i>Are there breaks in the series?</i> box above.</div>",
                "</div>",
                "<div id='inputs_", sid, "' data-safeid='", sid, "' data-measure='", mn, "' style='display:flex;flex-direction:row;flex-wrap:wrap;margin-top:8px;text-align:left;'>", yi_final, "</div>",
                # Status/validation messages sit on their own full-width line
                # above the buttons: the message can still wrap, and inside the
                # button row it would shift the buttons.
                "<div id='status_", sid, "' style='margin-top:10px;font-size:11px;color:green;",
                "font-weight:600;line-height:1.45;word-break:break-word;'></div>",
                "<div style='margin-top:10px;display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:8px;'>",
                "<button onclick=\"saveAndContinue('", sid, "','", mn, "')\" ",
                "style='background:#003189;color:#fff;border:none;padding:9px 22px;border-radius:5px;cursor:pointer;font-size:13px;font-weight:700;",
                "box-shadow:0 1px 3px rgba(0,49,137,0.35);'>",
                "Save and continue</button>",
                "<div style='display:flex;align-items:center;flex-wrap:wrap;gap:8px;'>",
                "<button onclick=\"submitMeasure('", sid, "','", mn, "')\" ",
                "style='background:#f5f5f5;color:#555;border:1px solid #ccc;padding:6px 14px;border-radius:4px;cursor:pointer;font-size:11px;font-weight:600;'>",
                "&#10003; Submit data</button>",
                "<span style='font-size:11px;color:#888;'>or</span>",
                "<button id='noupdate_", sid, "' onclick=\"declareNoUpdate('", sid, "','", mn, "')\" ",
                "class='no-update-btn", nu_active, "' ",
                "style='background:#f5f5f5;color:#555;border:1px solid #ccc;padding:6px 14px;border-radius:4px;cursor:pointer;font-size:11px;font-weight:600;'>",
                "No data update to declare</button>",
                "</div>",
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
                "<strong style='font-size:13px;'>Any comments or questions?</strong>",
                "<p style='font-size:11px;color:#888;margin:4px 0 6px;'>Add any relevant context, caveats, source notes, or technical observations about your country's data not included in the above definition. ",
                "Supporting documents can be emailed to <a href='mailto:kate.chalmers@oecd.org'>kate.chalmers@oecd.org</a> - please note here if you are sending any.</p>",
                "<div id='revnote_", sid, "' style='display:none;font-size:11px;color:#8a6d1a;",
                "background:#FFF8E1;border:1px solid #F5C518;border-radius:6px;",
                "padding:8px 10px;margin:0 0 6px;line-height:1.5;'></div>",
                "<textarea id='note_", sid, "' rows='3' ",
                "style='width:100%;font-size:12px;border:1px solid #ccc;border-radius:4px;padding:6px;box-sizing:border-box;resize:vertical;'>",
                htmltools::htmlEscape(isolate(session_data$notes[[mn]]) %||% ""),
                "</textarea>",
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
          note_html, comment_html,
          SIMPLIFY = TRUE, USE.NAMES = FALSE),

          row_html = paste0(
            "<div onclick=\"togglePanel('", safe_id, "')\" ",
            "style='cursor:pointer;display:flex;flex-direction:row;align-items:center;margin-bottom:1px;width:100%;padding:2px;border-radius:3px;",
            row_border,
            "' onmouseover=\"this.style.background='#f0f0f0'\" onmouseout=\"this.style.background='", row_hover, "'\">",
            "<div style='flex:0 0 20%;font-size:12px;padding-right:2px;text-align:right;'>", label, "</div>",
            "<div style='flex:1 1 0;min-width:0;display:flex;flex-direction:row;'>", boxes, "</div>",
            "<div style='flex:0 0 200px;text-align:left;padding-left:6px;overflow:hidden;'>", badge_html, "</div>",
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
  # Store a measure's values, recording any overwrites (or clearing) of
  # existing non-empty values as revisions. Shared by the Submit button, the
  # "Save and continue" listener, and the Excel template upload handler.
  store_measure <- function(d, record_revisions = TRUE) {
    n_revised <- 0L
    prev <- session_data$entries[[d$measure]]
    if (!is.null(prev) && record_revisions) {
      is_filled <- function(v) !is.null(v) && !is.na(v) && v != ""
      rev_m <- session_data$revisions[[d$measure]] %||% list()
      # Fetched once and reused for every cell below (rather than per-cell)
      # since a measure can have dozens of breakdown/year combinations.
      dat_lookup <- isolate(dat_rv())
      iso_lookup <- isolate(credentials$country)
      for (bk in names(prev)) {
        old_row <- prev[[bk]]
        new_row <- d$values[[bk]]
        if (!is.list(old_row) || !is.list(new_row)) next
        for (yr in names(old_row)) {
          old_v <- old_row[[yr]]
          new_v <- new_row[[yr]]
          old_filled <- is_filled(old_v)
          new_filled <- is_filled(new_v)
          # A revision is either a filled value changing to a different
          # filled value, OR a previously filled value being cleared (blank).
          changed <- old_filled &&
            ((new_filled && !identical(as.character(old_v), as.character(new_v))) ||
             !new_filled)

          # If what's being submitted now matches the existing OECD data
          # (published, falling back to non-used) exactly - e.g. after
          # "Revert to default" - any revision history recorded earlier for
          # this cell no longer reflects a real difference from what is on
          # record, so it is cleared rather than extended. Without this, a
          # revert-then-submit still left the heatmap marked "revised".
          default_obs <- lookup_default_obs(d$measure, bk, yr, d = dat_lookup, iso = iso_lookup)
          matches_default <- if (new_filled) {
            !is.na(default_obs$value) &&
              isTRUE(all.equal(suppressWarnings(as.numeric(new_v)), default_obs$value))
          } else {
            is.na(default_obs$value)
          }

          if (matches_default) {
            if (!is.null(rev_m[[bk]]) && !is.null(rev_m[[bk]][[yr]])) {
              rev_m[[bk]][[yr]] <- NULL
            }
          } else if (changed) {
            rev_m[[bk]] <- rev_m[[bk]] %||% list()
            rev_m[[bk]][[yr]] <- c(
              rev_m[[bk]][[yr]] %||% list(),
              list(list(from = as.character(old_v),
                        to   = if (new_filled) as.character(new_v) else "",
                        at   = Sys.time()))
            )
            n_revised <- n_revised + 1L
          }
        }
      }
      if (length(rev_m) > 0) session_data$revisions[[d$measure]] <- rev_m
    }

    session_data$entries[[d$measure]] <- d$values
    if (!is.null(d$flags))     session_data$flags[[d$measure]]     <- d$flags
    if (!is.null(d$responses)) session_data$responses[[d$measure]] <- d$responses
    # Free-text note on differing age groups; stored per measure so it is
    # never treated as a data value.
    if (!is.null(d$age_note))  session_data$age_notes[[d$measure]] <- d$age_note
    invisible(n_revised)
  }

  # Persist a measure's values via store_measure() AND publish the change to
  # the committed snapshot so the input panels keep them. "Save and continue"
  # and "Submit data" conserve values identically - both call this - but only
  # Submit marks the measure complete (session_data$explicit_submit) and only
  # Submit rebuilds the heatmap (refresh_ui).
  commit_measure <- function(d, record_revisions = TRUE, refresh_ui = TRUE) {
    n_revised <- store_measure(d, record_revisions = record_revisions)
    committed_entries(session_data$entries)
    committed_revisions(session_data$revisions)
    if (refresh_ui) bump_ui()
    invisible(n_revised)
  }

  # "Save and continue": user-triggered draft save. Conserves values but must
  # NEVER touch the heatmap - no rebuild here, and the rebuild triggered by
  # any later action ignores measures that were never explicitly submitted.
  observeEvent(input$saved_draft_data, {
    req(credentials$authenticated)
    if (country_is_frozen()) return()
    d <- input$saved_draft_data
    commit_measure(d, record_revisions = TRUE, refresh_ui = FALSE)
    runjs(paste0("
      var el = document.getElementById('status_", d$safe_id, "');
      if(el) { el.style.color = '#888'; el.innerText = 'Draft saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })

  observeEvent(input$submitted_data, {
    req(credentials$authenticated)
    if (country_is_frozen()) return()
    d <- input$submitted_data

    # Only an explicit Submit click marks the measure as officially
    # submitted / complete (uploads and "Save and continue" never do). Set
    # this before commit_measure() so the heatmap rebuild it triggers
    # already sees the updated flag.
    session_data$explicit_submit[[d$measure]] <- TRUE
    n_session_rev <- commit_measure(d, record_revisions = TRUE)

    # Also count values that differ from the already-published figures, since
    # that is the kind of revision reviewers most want explained. Wrapped
    # defensively: a failure here must never abort the submission.
    n_published_rev <- tryCatch({
      n <- 0L
      dd <- isolate(dat_rv())
      if (!is.null(dd) && nrow(dd) > 0 && !is.null(d$values[["country_avg"]])) {
        pub <- dd %>%
          filter(measure == d$measure, sex == "_T", age == "_T", education_lev == "_T") %>%
          mutate(time_period = as.character(time_period)) %>%
          select(time_period, obs_value)
        sub_row <- d$values[["country_avg"]]
        for (yr in names(sub_row)) {
          v <- suppressWarnings(as.numeric(sub_row[[yr]]))
          if (length(v) != 1 || is.na(v)) next
          o <- pub$obs_value[match(yr, pub$time_period)]
          if (length(o) == 1 && !is.na(o) && abs(v - o) > 1e-8) n <- n + 1L
        }
      }
      n
    }, error = function(e) 0L)

    n_rev <- max(n_session_rev, n_published_rev)

    runjs(paste0("
      var el = document.getElementById('status_", d$safe_id, "');
      if(el) { el.style.color = '#1F7A4D'; el.innerText = '\\u2713 Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
    # When figures have changed, nudge the user to explain why in the note box.
    if (n_rev > 0) {
      runjs(paste0("flagRevisionNote('", d$safe_id, "', ", n_rev, ");"))
    } else {
      runjs(paste0("clearRevisionNote('", d$safe_id, "');"))
    }
  })

  observeEvent(input$submitted_note, {
    req(credentials$authenticated)
    if (country_is_frozen()) return()
    d <- input$submitted_note
    session_data$notes[[d$measure]] <- d$note
    runjs(paste0("
      var el = document.getElementById('note_status_", d$safe_id, "');
      if(el) { el.innerText = '\\u2713 Saved at ", format(Sys.time(), "%H:%M:%S"), "'; }
    "))
  })

  observeEvent(input$no_update_declared, {
    req(credentials$authenticated)
    if (country_is_frozen()) return()
    d <- input$no_update_declared
    session_data$no_updates[[d$measure]] <- d$active
    bump_ui()
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

  # "Save and continue" on a single time use table: keeps a draft only. The
  # time use submission is not treated as complete until both tables are
  # submitted together via the button at the end of step 3.
  observeEvent(input$saved_table, {
    req(credentials$authenticated)
    if (country_is_frozen()) return()
    d <- input$saved_table
    if (identical(d$table, "tu_table1")) session_data$tu_draft_1 <- d$data
    else                                 session_data$tu_draft_2 <- d$data
    runjs(paste0("
      var el = document.getElementById('status_", d$table, "');
      if(el) { el.style.color = '#1F7A4D';
               el.innerText = '\\u2713 Progress saved at ", format(Sys.time(), "%H:%M:%S"), " (not yet submitted)'; }
    "))
  })

  # Final submission of step 3: survey details plus both tables together.
  observeEvent(input$tu_submit_all, {
    req(credentials$authenticated)
    if (country_is_frozen()) return()
    d <- input$tu_submit_all
    blank <- function(v) {
      is.null(v) || length(v) != 1 || is.na(v) || !nzchar(trimws(as.character(v)))
    }
    missing <- c(if (blank(input$tu_survey_name)) "survey name",
                 if (blank(input$tu_survey_year)) "latest survey year")
    if (length(missing) > 0) {
      runjs(paste0("
        var el = document.getElementById('tu_submit_all_status');
        if(el) { el.style.color = '#E63312';
                 el.innerText = '\\u26A0 Please fill in the ", paste(missing, collapse = " and "),
                 " above before submitting.'; }
      "))
      return()
    }

    session_data$tu_draft_1 <- d$t1
    session_data$tu_draft_2 <- d$t2
    session_data$time_use_1 <- d$t1
    session_data$time_use_2 <- d$t2
    # Time-use indicators on the submissions tab depend on this, so refresh.
    bump_ui()
    runjs(paste0("
      var el = document.getElementById('tu_submit_all_status');
      if(el) { el.style.color = '#1F7A4D';
               el.innerText = '\\u2713 Time use tables submitted at ", format(Sys.time(), "%H:%M:%S"), "'; }
      ['tu_table1','tu_table2'].forEach(function(t) {
        var s = document.getElementById('status_' + t);
        if(s) { s.style.color = '#1F7A4D'; s.innerText = '\\u2713 Submitted'; }
      });
    "))
  })

  # ── Time-use no-update observer ────────────────────────────────────────────
  observeEvent(input$tu_no_update_declared, {
    req(credentials$authenticated)
    if (country_is_frozen()) return()
    d <- input$tu_no_update_declared
    session_data$tu_no_update <- d$active
    bump_ui()
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

  # Per-table "no data update" declarations on the Time Use tab. Without this
  # observer the JS message was sent but never stored, so time-use indicators
  # never progressed past "Awaiting data input".
  observeEvent(input$tu_table_no_update_declared, {
    req(credentials$authenticated)
    if (country_is_frozen()) return()
    d <- input$tu_table_no_update_declared
    if (identical(as.character(d$table), "1")) {
      session_data$tu_no_update_1 <- isTRUE(d$active)
    } else {
      session_data$tu_no_update_2 <- isTRUE(d$active)
    }
    bump_ui()
    runjs(paste0("
      var el = document.getElementById('status_tu_table", d$table, "');
      if(el) {
        if(", tolower(isTRUE(d$active)), ") {
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

    no_updates <- session_data$no_updates

    # Well-being: count remaining indicators. Only an explicit Submit counts as
    # done - "Save and continue" and template uploads conserve values but leave
    # the indicator incomplete, so having values is NOT sufficient here. This is
    # the same source of truth as the per-indicator completion badge.
    explicit_submit <- session_data$explicit_submit
    submitted_measures <- names(explicit_submit)[
      vapply(explicit_submit, isTRUE, logical(1))
    ]
    no_update_measures <- names(no_updates)[vapply(no_updates, isTRUE, logical(1))]

    is_eu_silc <- credentials$country %in% eu_silc_countries
    sub_measures <- if (is_eu_silc) setdiff(xlsx_measures, eu_silc_measures) else xlsx_measures
    done_wb <- union(submitted_measures, no_update_measures)
    remaining_wb <- length(setdiff(sub_measures, done_wb))

    # Time use: done if both tables submitted OR no-update declared
    tu_done <- ((!is.null(session_data$time_use_1) && !is.null(session_data$time_use_2)) ||
                  isTRUE(session_data$tu_no_update))

    wb_badge_js <- if (remaining_wb > 0) paste0("'", remaining_wb, "'") else "null"
    tu_badge_js <- if (!tu_done) "'!'" else "null"

    # Once finalised the submit bar gives way to the record-copy bar.
    all_complete <- remaining_wb == 0 && tu_done && is.null(session_data$finalized)
    final_bar_js <- if (all_complete) {
      "document.getElementById('final_submit_bar').style.display='block';"
    } else {
      "document.getElementById('final_submit_bar').style.display='none';"
    }

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
        %s
      }, 100);
    ", wb_badge_js, tu_badge_js, final_bar_js))
  })

  # ── Final submit ──────────────────────────────────────────────────────────
  observeEvent(input$final_submit_btn, {
    req(credentials$authenticated, credentials$country)
    if (country_is_frozen()) return()
    # Mark as finalized with timestamp
    session_data$finalized <- Sys.time()
    # Persist right away, without blocking the confirmation overlay. finalized
    # is not a tracked field, so bump the epoch by hand; a write that is still
    # pending when the user leaves is flushed at logout / session end.
    save_epoch(isolate(save_epoch()) + 1)
    request_save()
    # Show confirmation overlay
    runjs("document.getElementById('final_submit_confirm').style.display='flex';")
    runjs("document.getElementById('final_submit_bar').style.display='none';")
  })

  # Persistent bar shown after finalisation: confirms the submission and keeps
  # the record copy downloadable on later visits.
  output$finalized_bar <- renderUI({
    req(credentials$authenticated)
    fin <- session_data$finalized
    if (is.null(fin)) return(NULL)
    tags$div(
      style = paste0(
        "position:fixed;bottom:0;left:0;width:100%;z-index:8900;",
        "background:#1F7A4D;box-shadow:0 -4px 16px rgba(0,0,0,0.15);",
        "padding:12px 0;text-align:center;"
      ),
      tags$div(
        style = "display:flex;align-items:center;justify-content:center;gap:20px;flex-wrap:wrap;",
        tags$span(
          style = "color:rgba(255,255,255,0.95);font-size:13px;font-weight:500;",
          paste0("\u2713 Submitted on ", format(fin, "%d %B %Y at %H:%M"), ".")
        ),
        downloadButton("dl_submission_copy_bar", "Download a copy for your records",
          icon = icon("download"),
          style = paste0(
            "background:rgba(255,255,255,0.15);color:white;border:1px solid rgba(255,255,255,0.6);",
            "padding:7px 18px;border-radius:5px;font-size:12px;font-weight:600;"
          ))
      )
    )
  })

  # ── Admin ───────────────────────────────────────────────────────────────────
  admin_auth <- reactiveVal(FALSE)
  # Bumped after a freeze/unfreeze toggle so the completion table and the
  # per-country controls panel re-read the session file immediately, without
  # waiting for a country filter/table change.
  admin_refresh <- reactiveVal(0)

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

  # ── Per-country freeze / last-activity panel ────────────────────────────────
  # Shown once a specific country (not "All countries") is picked in the
  # filter above. Freezing sets a flag in that country's session file; the
  # country's own live session polls this flag every few seconds and, on
  # every mutating observer, refuses further changes while it is TRUE.
  output$admin_country_controls <- renderUI({
    req(admin_auth())
    admin_refresh()
    iso <- input$admin_country_filter %||% "ALL"
    if (!nzchar(iso) || iso == "ALL") {
      return(tags$p(style = "font-size:12px;color:#888;margin:4px 0 16px;",
                     "Select a single country above to view its last activity or freeze its submission."))
    }
    s  <- session_read(iso)
    fl <- frozen_map_read()
    if (!is.null(fl)) {
      is_frozen   <- !is.null(fl[[iso]])
      frozen_time <- fl[[iso]]
    } else {
      # Legacy: sessions saved before the shared "frozen" pin existed
      is_frozen   <- !is.null(s) && isTRUE(s$frozen)
      frozen_time <- if (is_frozen) s$frozen_at else NULL
    }
    cname       <- {n <- names(country_name_vector)[country_name_vector == iso]; if (length(n)) n[1] else iso}
    last_edited <- if (!is.null(s) && !is.null(s$last_edited)) {
      format(s$last_edited, "%d %b %Y at %H:%M")
    } else "No edits recorded yet"
    frozen_at <- if (is_frozen && !is.null(frozen_time)) format(frozen_time, "%d %b %Y at %H:%M") else NULL

    tags$div(
      style = paste0(
        "background:", if (is_frozen) "#fdecea" else "#f0f6ff", ";",
        "border:1px solid ", if (is_frozen) "#f3b4ab" else "#c5d7ee", ";",
        "border-radius:8px;padding:14px 18px;margin:4px 0 20px;",
        "display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:12px;"
      ),
      tags$div(
        tags$p(style = "font-weight:700;margin:0 0 4px;font-size:13px;color:#1F2B3A;",
               paste0(cname, " (", iso, ")")),
        tags$p(style = "font-size:12px;color:#55606B;margin:0;",
               paste0("Last edited: ", last_edited)),
        if (is_frozen) tags$p(
          style = "font-size:12px;color:#a83a2c;font-weight:600;margin:4px 0 0;",
          paste0("\u2744 Frozen", if (!is.null(frozen_at)) paste0(" since ", frozen_at) else "",
                 " \u2014 the country can no longer edit this submission.")
        )
      ),
      if (is_frozen) {
        actionButton("admin_unfreeze_btn", "Unfreeze submission", icon = icon("unlock"),
                     style = "background:#1F7A4D;color:white;border:none;font-weight:600;")
      } else {
        actionButton("admin_freeze_btn", "Freeze submission", icon = icon("lock"),
                     style = "background:#E63312;color:white;border:none;font-weight:600;")
      }
    )
  })

  # Applies a freeze/unfreeze via the tiny shared "frozen" pin. This happens
  # from a separate R session (the admin's), so it cannot write to that
  # country's live session_data; the country's session instead polls the pin
  # periodically and picks the change up within ~30 seconds. Writing the
  # shared pin (rather than the country's session pin) also means a freeze
  # can never overwrite edits the country is saving at the same moment.
  set_country_frozen <- function(iso, frozen) {
    frozen_map_set(iso, frozen)
  }

  observeEvent(input$admin_freeze_btn, {
    req(admin_auth())
    iso <- input$admin_country_filter
    req(iso, iso != "ALL")
    set_country_frozen(iso, TRUE)
    admin_refresh(admin_refresh() + 1)
  })

  observeEvent(input$admin_unfreeze_btn, {
    req(admin_auth())
    iso <- input$admin_country_filter
    req(iso, iso != "ALL")
    set_country_frozen(iso, FALSE)
    admin_refresh(admin_refresh() + 1)
  })

  # Read all session files and assemble into tidy tables
  admin_all_data <- reactive({
    req(admin_auth())
    # Re-read every time the reactive fires (invalidated by table selection /
    # filter, or by a freeze/unfreeze toggle).
    input$admin_country_filter
    input$admin_table_select
    admin_refresh()

    isos <- session_list_countries()
    # Read every saved session once; reused by the tidy-table loop below and
    # the completion-status table (avoids repeated round-trips to the board).
    saved_sessions <- setNames(lapply(isos, session_read), isos)
    empty <- list(entries = data.frame(), flags = data.frame(),
                  notes = data.frame(), no_updates = data.frame(),
                  responses = data.frame(), tu1 = data.frame(),
                  tu2 = data.frame(), feedback = data.frame())
    if (length(isos) == 0) return(empty)

    all_entries    <- list()
    all_flags      <- list()
    all_notes      <- list()
    all_no_updates <- list()
    all_responses  <- list()
    all_tu1        <- list()
    all_tu2        <- list()

    for (iso in isos) {
      s <- saved_sessions[[iso]]
      if (is.null(s)) next
      cname <-  country_name_vector[country_name_vector == iso] %>% names()

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
    fb_list <- session_read("feedback") %||% list()
    fb_df <- if (length(fb_list) > 0) {
        bind_rows(lapply(fb_list, function(fb) {
          cname <-  country_name_vector[country_name_vector == fb$country] %>% names()
          data.frame(
            country   = if (!is.na(cname)) cname else fb$country,
            iso       = fb$country,
            timestamp = format(fb$timestamp, "%Y-%m-%d %H:%M:%S"),
            message   = fb$message,
            stringsAsFactors = FALSE
          )
        }))
    } else data.frame()

    # ── Build completion status table ──────────────────────────────────────
    # One row per country, columns for each WB indicator + TU tables + finalized
    # Always use the full xlsx_measures set; mark EU-SILC-excluded indicators as N/A
    all_countries <- c(oecd_countries, partner_countries)
    all_country_names <- c(oecd_names, partner_names)

    # Column labels from dictionary for readability
    measure_labels <- setNames(
      sapply(xlsx_measures, function(m) {
        lbl <- dict %>% filter(measure == m) %>% pull(label) %>% first()
        if (is.na(lbl) || is.null(lbl)) m else paste0(m, " ", substr(lbl, 1, 20))
      }),
      xlsx_measures
    )

    # Read the shared freeze pin once for the whole table; NULL means it has
    # not been created yet, in which case the legacy per-session flag applies.
    frozen_map <- frozen_map_read()
    completion_rows <- list()
    for (i in seq_along(all_countries)) {
      iso   <- all_countries[i]
      cname <- all_country_names[i]
      s <- saved_sessions[[iso]]

      is_eu_silc <- iso %in% eu_silc_countries

      # Per-indicator status (always all xlsx_measures)
      indicator_status <- sapply(xlsx_measures, function(m) {
        # EU-SILC countries don't need to submit EU-SILC measures
        if (is_eu_silc && m %in% eu_silc_measures) return("")
        if (!is.null(s) && isTRUE(s$no_updates[[m]])) return("No update")
        if (!is.null(s) && !is.null(s$entries[[m]])) {
          has_val <- any(vapply(s$entries[[m]], function(row_data) {
            is.list(row_data) &&
              any(vapply(row_data, function(v) !is.null(v) && !is.na(v) && v != "", logical(1)))
          }, logical(1)))
          if (has_val) return("Submitted")
        }
        "Pending"
      })
      names(indicator_status) <- measure_labels

      # Time use status
      tu1_status <- if (!is.null(s) && isTRUE(s$tu_no_update)) {
        "No update"
      } else if (!is.null(s) && !is.null(s$time_use_1)) {
        "Submitted"
      } else "Pending"

      tu2_status <- if (!is.null(s) && isTRUE(s$tu_no_update)) {
        "No update"
      } else if (!is.null(s) && !is.null(s$time_use_2)) {
        "Submitted"
      } else "Pending"

      finalized <- if (!is.null(s) && !is.null(s$finalized)) {
        format(s$finalized, "%Y-%m-%d %H:%M")
      } else ""

      last_edited_str <- if (!is.null(s) && !is.null(s$last_edited)) {
        format(s$last_edited, "%Y-%m-%d %H:%M")
      } else ""
      frozen_str <- if (!is.null(frozen_map)) {
        if (!is.null(frozen_map[[iso]])) "Frozen" else ""
      } else if (!is.null(s) && isTRUE(s$frozen)) "Frozen" else ""

      row <- c(country = cname, iso = iso, indicator_status,
               "TU Table 1" = tu1_status, "TU Table 2" = tu2_status,
               "Last edited" = last_edited_str, "Status" = frozen_str,
               Finalized = finalized)
      completion_rows[[length(completion_rows) + 1]] <- row
    }
    completion_df <- as.data.frame(do.call(rbind, completion_rows), stringsAsFactors = FALSE)

    list(
      completion = completion_df,
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
    tbl_name <- input$admin_table_select %||% "entries"
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No submissions found."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    # Drop the iso column for display
    display_df <- df[, setdiff(names(df), "iso"), drop = FALSE]

    if (tbl_name == "completion") {
      # Color-coded completion table
      dt <- DT::datatable(display_df, rownames = FALSE, filter = "top",
                    options = list(pageLength = 50, scrollX = TRUE,
                                  columnDefs = list(list(className = "dt-center",
                                                         targets = seq(1, ncol(display_df) - 1)))))
      # Style status columns (indicator/TU columns only; "Last edited",
      # "Status" and "Finalized" get their own treatment below)
      status_cols <- setdiff(names(display_df), c("country", "Last edited", "Status", "Finalized"))
      for (col in status_cols) {
        dt <- dt %>%
          DT::formatStyle(col,
            backgroundColor = DT::styleEqual(
              c("Submitted", "No update", "Pending", ""),
              c("#d4edda", "#fff3cd", "#f8d7da", "#f0f0f0")
            ),
            color = DT::styleEqual(
              c("Submitted", "No update", "Pending", ""),
              c("#155724", "#856404", "#721c24", "#ccc")
            ),
            fontWeight = "600",
            fontSize = "11px"
          )
      }
      if ("Status" %in% names(display_df)) {
        dt <- dt %>%
          DT::formatStyle("Status",
            backgroundColor = DT::styleEqual(c("Frozen", ""), c("#fdecea", "transparent")),
            color = DT::styleEqual(c("Frozen", ""), c("#a83a2c", "#ccc")),
            fontWeight = "700",
            fontSize = "11px"
          )
      }
      dt
    } else {
      DT::datatable(display_df, rownames = FALSE, filter = "top",
                    options = list(pageLength = 25, scrollX = TRUE))
    }
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
  # ── Admin: bulk backup / restore of all country sessions ───────────────────
  # Intended for redeploys: the sessions/ directory is the single source of
  # truth for in-progress country submissions, so archiving it preserves
  # everything across a clean push of the app.
  output$admin_backup_all <- downloadHandler(
    filename = function() {
      paste0("portal_sessions_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip")
    },
    content = function(file) {
      # Materialise each pin back into a flat .rds file so the archive keeps
      # the same layout as before (and stays restorable via the upload below).
      tmpdir <- file.path(tempdir(), paste0("backup_", as.integer(Sys.time())))
      dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
      files <- character(0)
      for (nm in session_list_all()) {
        obj <- session_read(nm)
        if (is.null(obj)) next
        f <- file.path(tmpdir, paste0(nm, ".rds"))
        saveRDS(obj, f)
        files <- c(files, f)
      }
      if (length(files) == 0) {
        # Still produce a valid (empty) archive rather than a broken download.
        tmp <- file.path(tempdir(), "EMPTY.txt")
        writeLines("No session files present at backup time.", tmp)
        files <- tmp
      }
      utils::zip(zipfile = file, files = files, flags = "-j -q")
    }
  )
  outputOptions(output, "admin_backup_all", suspendWhenHidden = FALSE)

  observeEvent(input$admin_restore_all, {
    req(admin_auth())
    fi <- input$admin_restore_all
    req(fi)

    msg <- function(text, colour) {
      output$admin_restore_feedback <- renderUI(
        tags$span(style = paste0("font-size:12px;font-weight:600;color:", colour, ";"), text)
      )
    }

    result <- tryCatch({
      exdir <- file.path(tempdir(), paste0("restore_", as.integer(Sys.time())))
      dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
      utils::unzip(fi$datapath, exdir = exdir)

      found <- list.files(exdir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
      # Only accept country session files and the password/feedback stores.
      keep <- found[grepl("^([A-Z]{3}|passwords|feedback)\\.rds$", basename(found))]
      if (length(keep) == 0) stop("No valid session files found in the archive.")

      # Verify each file is readable before overwriting anything.
      bad <- keep[vapply(keep, function(f) {
        is.null(tryCatch(readRDS(f), error = function(e) NULL))
      }, logical(1))]
      if (length(bad) > 0) {
        stop("Unreadable file(s) in archive: ", paste(basename(bad), collapse = ", "))
      }

      ok <- vapply(keep, function(f) {
        session_write(readRDS(f), sub("\\.rds$", "", basename(f)))
      }, logical(1))
      sum(ok)
    }, error = function(e) e)

    if (inherits(result, "error")) {
      msg(paste0("Restore failed: ", conditionMessage(result)), "#E63312")
    } else {
      msg(paste0("\u2713 Restored ", result, " session file(s). ",
                 "Countries will see their data on next login."), "#1F7A4D")
    }
  })

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
    # Delete all country session pins (passwords and feedback are kept)
    isos <- session_list_countries()
    n_deleted <- if (length(isos) > 0) {
      sum(vapply(isos, session_delete, logical(1)))
    } else 0
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
    existing <- session_read("feedback") %||% list()
    existing[[length(existing) + 1]] <- list(
      country   = credentials$country %||% "unknown",
      timestamp = Sys.time(),
      message   = fb_text
    )
    session_write(existing, "feedback")
    # Clear the textarea
    runjs("document.getElementById('feedback_text').value = '';")
    output$feedback_status <- renderUI(
      tags$span(style = "font-size:11px;color:#1F7A4D;font-weight:600;",
                paste0("\u2713 Thank you! Feedback received at ", format(Sys.time(), "%H:%M:%S"), ".")))
  })

}

shinyApp(ui = ui, server = server)
