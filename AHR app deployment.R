library(rsconnect)

app_dir <- "."
app_dir <- normalizePath(app_dir, mustWork = TRUE)

rsconnect::deployApp(
  appDir = app_dir,
  appFiles = c(
    "AHR app.R",
    "AHR_combined_data.xlsx",
    "hm81_2023.xlsx",
    "sf4_2023.xlsx"
  ),
  appPrimaryDoc = "AHR app.R",
  appName = "annual_highway_report_dashboard",
  account = "reason",
  server = "shinyapps.io",
  forceUpdate = TRUE
)
