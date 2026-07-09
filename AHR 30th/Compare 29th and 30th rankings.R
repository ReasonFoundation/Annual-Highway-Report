library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

# Compare state score/rank changes between the 29th and 30th AHR output files.
# Run from either the project root or the "AHR 30th" folder.

get_project_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  if (basename(wd) == "AHR 30th") {
    return(dirname(wd))
  }

  wd
}

first_existing_path <- function(paths) {
  existing_paths <- paths[file.exists(paths)]

  if (length(existing_paths) == 0) {
    stop(
      "None of these files exist:\n",
      paste(paths, collapse = "\n"),
      call. = FALSE
    )
  }

  existing_paths[[1]]
}

project_root <- get_project_root()

ahr_29_path <- file.path(project_root, "AHR 29th", "output", "AHR_data 29th.xlsx")

# The requested 30th path is checked first. The current repo also has this file
# in the root-level output folder, so use that as a fallback.
ahr_30_path <- first_existing_path(c(
  file.path(project_root, "AHR 30th", "output", "AHR_data 30th.xlsx"),
  file.path(project_root, "output", "AHR_data 30th.xlsx")
))

output_dir <- file.path(project_root, "AHR 30th", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_scores <- function(path, report_year) {
  read_excel(path, sheet = "Scores & Rankings") %>%
    clean_names() %>%
    mutate(
      report_year = report_year,
      state = str_squish(state)
    )
}

scores_29 <- read_scores(ahr_29_path, "29th")
scores_30 <- read_scores(ahr_30_path, "30th")

rank_cols <- intersect(
  names(scores_29)[str_detect(names(scores_29), "_rank$")],
  names(scores_30)[str_detect(names(scores_30), "_rank$")]
)

score_cols <- intersect(
  names(scores_29)[str_detect(names(scores_29), "_score$")],
  names(scores_30)[str_detect(names(scores_30), "_score$")]
)

rank_changes <- scores_29 %>%
  select(state, all_of(rank_cols)) %>%
  pivot_longer(-state, names_to = "metric", values_to = "rank_29th") %>%
  left_join(
    scores_30 %>%
      select(state, all_of(rank_cols)) %>%
      pivot_longer(-state, names_to = "metric", values_to = "rank_30th"),
    by = c("state", "metric")
  ) %>%
  mutate(
    metric = str_remove(metric, "_rank$"),
    rank_change = rank_30th - rank_29th,
    movement = case_when(
      is.na(rank_29th) | is.na(rank_30th) ~ "missing",
      rank_change < 0 ~ "moved up",
      rank_change > 0 ~ "moved down",
      TRUE ~ "no change"
    )
  ) %>%
  arrange(metric, rank_change)

score_changes <- scores_29 %>%
  select(state, all_of(score_cols)) %>%
  pivot_longer(-state, names_to = "metric", values_to = "score_29th") %>%
  left_join(
    scores_30 %>%
      select(state, all_of(score_cols)) %>%
      pivot_longer(-state, names_to = "metric", values_to = "score_30th"),
    by = c("state", "metric")
  ) %>%
  mutate(
    metric = str_remove(metric, "_score$"),
    score_change = score_30th - score_29th
  ) %>%
  arrange(metric, desc(abs(score_change)))

overall_rank_changes <- rank_changes %>%
  filter(metric == "overall_score") %>%
  left_join(
    score_changes %>%
      filter(metric == "overall") %>%
      select(state, score_29th, score_30th, score_change),
    by = "state"
  ) %>%
  arrange(rank_change, state)

output_xlsx <- file.path(output_dir, "AHR ranking changes 29th to 30th.xlsx")
output_overall_csv <- file.path(output_dir, "AHR overall ranking changes 29th to 30th.csv")
output_rank_csv <- file.path(output_dir, "AHR all ranking changes 29th to 30th.csv")
output_score_csv <- file.path(output_dir, "AHR all score changes 29th to 30th.csv")

write_xlsx(
  list(
    "Overall Rank Changes" = overall_rank_changes,
    "All Rank Changes" = rank_changes,
    "All Score Changes" = score_changes
  ),
  output_xlsx
)

write_csv(overall_rank_changes, output_overall_csv)
write_csv(rank_changes, output_rank_csv)
write_csv(score_changes, output_score_csv)

cat("Compared files:\n")
cat("29th:", ahr_29_path, "\n")
cat("30th:", ahr_30_path, "\n\n")

cat("Wrote:\n")
cat(output_xlsx, "\n")
cat(output_overall_csv, "\n")
cat(output_rank_csv, "\n")
cat(output_score_csv, "\n\n")

cat("Overall ranking changes preview:\n")
print(overall_rank_changes, n = 50)
