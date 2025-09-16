# Calculate scores and rankings for new dashboard
scores2 <- AHR_data %>% 
  pivot_longer(cols = rural_interstate_poor_percent:other_fatalities_per_100m_VMT, 
               names_to = "key_metrics", 
               values_to = "value") %>% 
  arrange(key_metrics) %>% 
  select(state, key_metrics, value) %>% 
  group_by(key_metrics) %>% 
  mutate(exp_value = value[state == "United States"]) %>% 
  ungroup() %>% 
  bind_rows(disbursement_data) %>% 
  mutate(relative_score = value / exp_value,
         key_metrics = paste0(key_metrics, "_score")) %>% 
  filter(state != "United States") %>% 
  group_by(key_metrics) %>% 
  mutate(rank = min_rank(relative_score)) %>% 
  ungroup() %>% 
  mutate(year = 2023, .before = everything()) %>% 
  split(.$key_metrics)

capital_disbursement_perlm_score <- scores2$capital_disbursement_perlm_score %>% 
  left_join(AHR_data %>% select(state, capital_disbursement, state_tot_lane_miles)) %>% 
  select(year:state, capital_disbursement, state_tot_lane_miles, everything()) %>% 
  rename(numerator = capital_disbursement, denominator = state_tot_lane_miles)

maintenance_disbursement_perlm_score <- scores2$maintenance_disbursement_perlm_score %>% 
  left_join(AHR_data %>% select(state, maintenance_disbursement, state_tot_lane_miles)) %>% 
  select(year:state, maintenance_disbursement, state_tot_lane_miles, everything()) %>% 
  rename(numerator = maintenance_disbursement, denominator = state_tot_lane_miles)

admin_disbursement_perlm_score <- scores2$admin_disbursement_perlm_score %>% 
  left_join(AHR_data %>% select(state, admin_disbursement, state_tot_lane_miles)) %>% 
  select(year:state, admin_disbursement, state_tot_lane_miles, everything()) %>% 
  rename(numerator = admin_disbursement, denominator = state_tot_lane_miles)

other_disbursement_perlm_score <- scores2$other_disbursement_perlm_score %>% 
  left_join(AHR_data %>% select(state, other_disbursement, state_tot_lane_miles)) %>% 
  select(year:state, other_disbursement, state_tot_lane_miles, everything()) %>% 
  rename(numerator = other_disbursement, denominator = state_tot_lane_miles)

rural_interstate_poor_percent_score <- scores2$rural_interstate_poor_percent_score %>% 
  left_join(AHR_data %>% select(state, rural_interstate_above_170, rural_interstate_total)) %>% 
  select(year:state, rural_interstate_above_170, rural_interstate_total, everything()) %>% 
  rename(numerator = rural_interstate_above_170, denominator = rural_interstate_total)

urban_interstate_poor_percent_score <- scores2$urban_interstate_poor_percent_score %>% 
  left_join(AHR_data %>% select(state, urban_interstate_above_170, urban_interstate_total)) %>% 
  select(year:state, urban_interstate_above_170, urban_interstate_total, everything()) %>% 
  rename(numerator = urban_interstate_above_170, denominator = urban_interstate_total)

rural_OPA_poor_percent_score <- scores2$rural_OPA_poor_percent_score %>%
  left_join(AHR_data %>% select(state, rural_OPA_above_220, rural_OPA_total)) %>% 
  select(year:state, rural_OPA_above_220, rural_OPA_total, everything()) %>% 
  rename(numerator = rural_OPA_above_220, denominator = rural_OPA_total)

urban_OPA_poor_percent_score <- scores2$urban_OPA_poor_percent_score %>%
  left_join(AHR_data %>% select(state, urban_OPA_above_220, urban_OPA_total)) %>% 
  select(year:state, urban_OPA_above_220, urban_OPA_total, everything()) %>% 
  rename(numerator = urban_OPA_above_220, denominator = urban_OPA_total)

state_avg_congestion_hours_score <- scores2$state_avg_congestion_hours_score %>%
  left_join(AHR_data %>% select(state, state_tot_congestion_hours, state_tot_commuters)) %>% 
  select(year:state, state_tot_congestion_hours, state_tot_commuters, everything()) %>% 
  rename(numerator = state_tot_congestion_hours, denominator = state_tot_commuters)

poor_bridges_percent_score <- scores2$poor_bridges_percent_score %>%
  left_join(AHR_data %>% select(state, total_poor_bridges, total_bridges)) %>% 
  select(year:state, total_poor_bridges, total_bridges, everything()) %>% 
  rename(numerator = total_poor_bridges, denominator = total_bridges)

rural_fatalities_per_100m_VMT_score <- scores2$rural_fatalities_per_100m_VMT_score %>%
  left_join(AHR_data %>% select(state, rural_fatality_interstate_OFE_OPA, rural_VMT_interstate_OFE_OPA)) %>% 
  select(year:state, rural_fatality_interstate_OFE_OPA, rural_VMT_interstate_OFE_OPA, everything()) %>% 
  rename(numerator = rural_fatality_interstate_OFE_OPA, denominator = rural_VMT_interstate_OFE_OPA)

urban_fatalities_per_100m_VMT_score <- scores2$urban_fatalities_per_100m_VMT_score %>%
  left_join(AHR_data %>% select(state, urban_fatality_interstate_OFE_OPA, urban_VMT_interstate_OFE_OPA)) %>% 
  select(year:state, urban_fatality_interstate_OFE_OPA, urban_VMT_interstate_OFE_OPA, everything()) %>% 
  rename(numerator = urban_fatality_interstate_OFE_OPA, denominator = urban_VMT_interstate_OFE_OPA)

other_fatalities_per_100m_VMT_score <- scores2$other_fatalities_per_100m_VMT_score %>%
  left_join(AHR_data %>% select(state, other_fatality, other_VMT)) %>% 
  select(year:state, other_fatality, other_VMT, everything()) %>% 
  rename(numerator = other_fatality, denominator = other_VMT)

scores2_1 <- bind_rows(
  capital_disbursement_perlm_score,
  maintenance_disbursement_perlm_score,
  admin_disbursement_perlm_score,
  other_disbursement_perlm_score,
  rural_interstate_poor_percent_score,
  urban_interstate_poor_percent_score,
  rural_OPA_poor_percent_score,
  urban_OPA_poor_percent_score,
  state_avg_congestion_hours_score,
  poor_bridges_percent_score,
  rural_fatalities_per_100m_VMT_score,
  urban_fatalities_per_100m_VMT_score,
  other_fatalities_per_100m_VMT_score
)

check <- scores2 %>% 
  mutate(value_check = numerator / denominator,
         value_diff = value - value_check)


check2 <- check %>% 
  filter(value_diff != 0) %>% 
  mutate(value_check = value_check * 100,
         value_diff = value - value_check) %>% 
  filter(!str_detect(key_metrics, "percent|100m"))


spec <- tibble::tribble(
  ~score,                                  ~num,                                     ~denom,
  "capital_disbursement_perlm_score",       "capital_disbursement",                   "state_tot_lane_miles",
  "maintenance_disbursement_perlm_score",   "maintenance_disbursement",               "state_tot_lane_miles",
  "admin_disbursement_perlm_score",         "admin_disbursement",                     "state_tot_lane_miles",
  "other_disbursement_perlm_score",         "other_disbursement",                     "state_tot_lane_miles",
  "rural_interstate_poor_percent_score",    "rural_interstate_above_170",             "rural_interstate_total",
  "urban_interstate_poor_percent_score",    "urban_interstate_above_170",             "urban_interstate_total",
  "rural_OPA_poor_percent_score",           "rural_OPA_above_220",                    "rural_OPA_total",
  "urban_OPA_poor_percent_score",           "urban_OPA_above_220",                    "urban_OPA_total",
  "state_avg_congestion_hours_score",       "state_tot_congestion_hours",             "state_tot_commuters",
  "poor_bridges_percent_score",             "total_poor_bridges",                     "total_bridges",
  "rural_fatalities_per_100m_VMT_score",    "rural_fatality_interstate_OFE_OPA",      "rural_VMT_interstate_OFE_OPA",
  "urban_fatalities_per_100m_VMT_score",    "urban_fatality_interstate_OFE_OPA",      "urban_VMT_interstate_OFE_OPA",
  "other_fatalities_per_100m_VMT_score",    "other_fatality",                         "other_VMT"
)


make_score <- function(score_df, num, denom, AHR_data) {
  # num, denom are character scalars like "capital_disbursement"
  score_df %>%
    left_join(
      select(AHR_data, state, all_of(num), all_of(denom)),
      by = "state"
    ) %>%
    select(year:state, all_of(num), all_of(denom), everything()) %>%
    rename(
      numerator  = !!sym(num),
      denominator = !!sym(denom)
    )
}


scores_built <- pmap(
  spec,
  function(score, num, denom) {
    make_score(scores2[[score]], num, denom, AHR_data)
  }
) %>%  
  set_names(spec$score) %>% 
  bind_rows()


