library(rsconnect)

app_dir <- "."
app_dir <- normalizePath(app_dir, mustWork = TRUE)

rsconnect::deployApp(
  appDir = app_dir,
  appFiles = c(
    "AHR app.R",
    "AHR_combined_data.xlsx",
    "AHR 30th/data/hm81.xlsx",
    "AHR 30th/data/sf4.xlsx"
  ),
  appPrimaryDoc = "AHR app.R",
  appMode = "shiny",
  appName = "annual_highway_report_dashboard",
  account = "reason",
  server = "shinyapps.io",
  forceUpdate = TRUE
)
