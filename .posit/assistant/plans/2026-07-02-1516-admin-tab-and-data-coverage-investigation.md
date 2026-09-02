# Plan: Admin Tab for Viewing Country Submissions

## Goal
Add a password-protected admin tab to the Shiny app that lets the portal owner view all submitted data across countries and export it as CSV.

## Changes

### 1. UI: Add an "Admin" tab to the navbar (`app.R`)
- Add a new `tabPanel("Admin", ...)` after the "Time Use" tab
- Content: a password gate (password input + button), hidden behind which is:
  - A **country selector** (dropdown) to pick a country or "All"
  - A **DT data table** showing submitted data entries (measure, row, year, value)
  - A **second DT table** for notes/responses
  - A **download button** for CSV export of the currently displayed data

### 2. Server: Admin authentication and data loading (`app.R`)
- Add a `reactiveVal` for admin authentication (`admin_authenticated`)
- `observeEvent` on the admin password button: check against password `admin2026`
- On successful admin login, show the admin content panel
- Build a reactive that scans `sessions/*.rds` files, reads each, and assembles:
  - **Entries table**: country | measure | row_type | year | value
  - **Notes table**: country | measure | note
  - **Responses table**: country | measure | question_index | response
  - **Time-use tables**: country | table | row | col | value
- Render these as DT tables, filterable by country dropdown
- Add `downloadHandler` that writes the current view to CSV

### 3. No changes to `global.R`
All admin logic lives in `app.R`.

## Password
- Admin password: `admin2026`
- The admin tab is visible to everyone in the navbar, but content is hidden behind the password gate (same pattern as the main login).
