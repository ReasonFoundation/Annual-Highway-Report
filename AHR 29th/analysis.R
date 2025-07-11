library(readxl)

ahr_29 <- readxl::read_excel("AHR_data 29th.xlsx", sheet = 2)

ahr_28 <- read_excel("data/AHR_data 28th_summing_components_columns.xlsx", sheet = 2)

setdiff(ahr_28 %>% colnames(),
ahr_29 %>% colnames())


compare_ahr <- function(ahr_28, ahr_29) {
  common_cols <- intersect(colnames(ahr_28), colnames(ahr_29))
  common_cols <- setdiff(common_cols, "state")
  
  joined <- ahr_28 %>%
    rename_with(~ paste0(.x, "_28"), all_of(common_cols)) %>%
    inner_join(
      ahr_29 %>% rename_with(~ paste0(.x, "_29"), all_of(common_cols)),
      by = "state"
    )
  
  for (col in common_cols) {
    joined[[paste0(col, "_diff")]] <- joined[[paste0(col, "_29")]] - joined[[paste0(col, "_28")]]
  }
  
  return(joined)
}

compare_ahr(ahr_28, ahr_29) %>% 
  select(state, contains("overall_score_rank")) %>% 
  write.csv("output/change_in_overall_rank.csv")

 
