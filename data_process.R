rm(list = ls())
library(rio)
library(dplyr)

# find all top‐level folders named "AHR 27th", "AHR 28th", etc.
all_dirs <- list.dirs(path = ".", recursive = FALSE)
ahr_dirs <- all_dirs[ grepl("^\\.\\/AHR \\d+(st|nd|rd|th)$", all_dirs) ]

# extract the ordinal (e.g. "27th") and build each file path
ordinals <- sub("^.*AHR (\\d+(st|nd|rd|th))$", "\\1", ahr_dirs)
files <- file.path(ahr_dirs, paste0("AHR_data ", ordinals, " dashboard.xlsx"))

# define the sheets to pull
sheets <- c("Individual Scores & Rankings",
            "State Mileage",
            "All Rankings")

# for each sheet, import from every file and row‐bind
combined <- lapply(sheets, function(sheet_name) {
  df_list <- lapply(files, function(f) {
    import(f, sheet = sheet_name)
  })
  bind_rows(df_list)
})

# give them nice names
names(combined) <- sheets

# get three data frames for inspection:
Individual_Scores_Rankings <- combined[["Individual Scores & Rankings"]]
State_Mileage               <- combined[["State Mileage"]]
All_Rankings                <- combined[["All Rankings"]]

# export to back to Excel for Shiny app
rio::export(combined, "AHR_combined_data.xlsx")