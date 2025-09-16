rm(list = ls())
library(tidyverse)
library(janitor)
library(readxl)
library(modelr)
library(rio)
library(broom)
library(plotly)

#Cost of living index for later adjustment to spending numbers
# coli <- read.csv("coli_meric.csv") %>% 
#   clean_names() %>% 
#   select(state, coli_index) %>% 
#   mutate(coli_index = coli_index/100)


#HM-10: Public road length, miles by ownership
HM_10 <- read_excel("data/hm10.xls", sheet = "A") %>% 
  remove_empty() %>% 
  select(1, 2, 5, 8, 11) %>% 
  slice(-(1:6)) %>% 
  rename(state = 1,
         rural_SHA = 2,
         rural_other = 3,
         urban_SHA = 4,
         urban_other = 5) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(rural_SHA:urban_other, as.numeric),
         SHA_miles = rural_SHA + urban_SHA) %>% 
  select(state, SHA_miles)


#HM-81: State highway agency-owned public roads; rural and urban miles, estimated lane-miles and daily travel

#Note: In California Policy Center report, the table shows "total lane miles" BUT it probably refers to dvmt infact. 
#https://californiapolicycenter.org/californias-transportation-future-part-four-the-common-road/

HM_81 <- read_excel("data/hm81.xls", sheet  = "A") %>% 
  remove_empty() %>% 
  rename(state = 1,
         state_urban_lane_miles = 10,
         #state_urban_dvmt = 11,
         #state_tot_dvmt = 18
         state_tot_lane_miles = 17,
  ) %>% 
  select(state, state_urban_lane_miles, state_tot_lane_miles) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(state_urban_lane_miles:state_tot_lane_miles, as.numeric)) %>% 
  
  # calculate percentage
  mutate(pct_urban_lane_miles = state_urban_lane_miles/state_tot_lane_miles,
         
         # national 
         US_pct_urban_lane_miles = sum(state_urban_lane_miles)/sum(state_tot_lane_miles), 
         
         # index - relative to national level
         urban_lm_index = (pct_urban_lane_miles/US_pct_urban_lane_miles)) %>% 
  select(state, state_tot_lane_miles, pct_urban_lane_miles, urban_lm_index)

  

#SF-4: Disbursements for state-administered highways (thousands of dollars)
#Note: the 2020 numbers for some reason exactly match the 2019 numbers? 
SF_4 <- read_excel("data/sf4.xlsx", sheet = "A") %>% 
  remove_empty() %>% 
  select(1:7) %>% 
  rename(state = 1,
         capital_disbursement = 2,
         maintenance_disbursement = 3,
         admin_disbursement = 4) %>%
  slice(-(1:6)) %>% 
  
  mutate(state = str_replace_all(state, "[:punct:]", ""),    #remove all special characters
         state = str_replace_all(state, "[:digit:]", ""),    #remove all numbers
         state = str_trim(state),
         across(2:7, as.numeric)) %>% 
  
  # unit in original file is thousand dollars  
  mutate(other_disbursement = rowSums(across(5:7)),
         capital_disbursement = capital_disbursement*1000, 
         maintenance_disbursement = maintenance_disbursement * 1000, 
         admin_disbursement = admin_disbursement * 1000,
         other_disbursement = other_disbursement * 1000) %>% 
  filter(state %in% state.name) %>% select(-c(5:7))

# Adjusting 4 categories of disbursement for urban_lm_index 
# SF_4_adjusted <- SF_4 %>% 
#   left_join(HM_81) %>% 
#   mutate(across(2:5, ~.x/urban_lm_index, .names = "{.col}_adjusted")) %>% 
#   select(state, capital_disbursement_adjusted:other_disbursement_adjusted) 

# write_csv(SF_4_adjusted, "SF_4_adjusted_coliIndex_urbanDvmtIndex.csv")
  

#HM-64: Miles by measured pavement roughness
HM64_rural_interstate <- read_excel("data/hm64.xls", sheet = "A") %>% 
  remove_empty() %>% 
  select(1, 8:11) %>% 
  rename(state = 1,
         rural_interstate_171_194 = 2,
         rural_interstate_195_220 = 3,
         rural_interstate_above_220 = 4,
         rural_interstate_total = 5) %>% 
  slice(-(1:7)) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(rural_interstate_171_194:rural_interstate_total, as.numeric),
         rural_interstate_above_170 = rural_interstate_171_194 + rural_interstate_195_220 + rural_interstate_above_220)
  

HM64_rural_OPA <- read_excel("data/hm64.xls", sheet = "B") %>% 
  remove_empty() %>% 
  select(1, 20:21) %>% 
  rename(state = 1,
         rural_OPA_above_220 = 2,
         rural_OPA_total = 3) %>% 
  slice(-(1:7)) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(rural_OPA_above_220:rural_OPA_total, as.numeric))
  
  
HM64_urban_interstate <- read_excel("data/hm64.xls", sheet = "C") %>% 
  remove_empty() %>% 
  select(1, 8:11) %>% 
  rename(state = 1,
         urban_interstate_171_194 = 2,
         urban_interstate_195_220 = 3, 
         urban_interstate_above_220 = 4,
         urban_interstate_total = 5) %>% 
  slice(-(1:7)) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(urban_interstate_171_194:urban_interstate_total, as.numeric),
         urban_interstate_above_170 = urban_interstate_171_194 + urban_interstate_195_220 + urban_interstate_above_220)


HM64_urban_OPA <- read_excel("data/hm64.xls", sheet = "D") %>% 
  remove_empty() %>% 
  select(1, 20:21) %>% 
  rename(state = 1,
         urban_OPA_above_220 = 2,
         urban_OPA_total = 3) %>% 
  slice(-(1:7)) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(urban_OPA_above_220:urban_OPA_total, as.numeric))

# Combined HM-64: Miles by measured pavement roughness
HM64 <- HM64_rural_interstate %>% 
  left_join(HM64_rural_OPA) %>% 
  left_join(HM64_urban_interstate) %>% 
  left_join(HM64_urban_OPA) %>% 
  select(state,
         rural_interstate_above_170,
         rural_interstate_total,
         rural_OPA_above_220,
         rural_OPA_total,
         urban_interstate_above_170,
         urban_interstate_total,
         urban_OPA_above_220,
         urban_OPA_total)


#FI-20: Persons fatally injured in motor vehicle crashes
FI_20 <- read_excel("data/fi20.xls", sheet = "A", skip = 13) %>% 
  rename(state = 1) %>% 
  
  # 3 groups of fatality
  mutate(rural_fatality_interstate_OFE_OPA = (`...2` + `EXPRESSWAYS...3` + ARTERIAL...4),
         urban_fatality_interstate_OFE_OPA = (`...10` + `EXPRESSWAYS...11` + ARTERIAL...12),
         other_fatality = (ARTERIAL...5 + COLLECTOR...6 + COLLECTOR...7 + `...8` + ARTERIAL...13 + COLLECTOR...14 + COLLECTOR...15 + `...16`)) %>% 
  select(1, 20:22) %>% 
  filter(state %in% state.name)

#VM-2: Annual vehicle miles
VM_2 <- read_excel("data/vm2.xls", sheet = "A", skip = 13) %>% 
  rename(state = 1) %>% 
  mutate(rural_VMT_interstate_OFE_OPA = (`...2` + EXPRESSWAYS...3 + ARTERIAL...4),
         urban_VMT_interstate_OFE_OPA = (...10 + EXPRESSWAYS...11 + ARTERIAL...12),
         other_VMT = (ARTERIAL...5 + COLLECTOR...6 + COLLECTOR...7 + ...8 + ARTERIAL...13 + COLLECTOR...14 + COLLECTOR...15 + ...16)) %>% 
  select(1, 19:21) %>% 
  mutate(across(2:4)) %>%   
  filter(state %in% state.name)

#Bridge data
bridge_raw <- read_excel("data/fccount21.xlsx", sheet = "2021")  

bridge_total <- bridge_raw[1:58,] %>% 
  rename(state = 1) %>% 
  mutate(state = str_to_title(state)) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(2:13, as.numeric)) %>% 
  mutate(total_bridges = rowSums(across(where(is.numeric)))) %>% 
  select(state, total_bridges)

bridge_poor <- bridge_raw[180:nrow(bridge_raw),] %>% 
  rename(state = 1) %>% 
  mutate(state = str_to_title(state)) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(2:13, as.numeric)) %>% 
  mutate(total_poor_bridges = rowSums(across(where(is.numeric)))) %>% 
  select(state, total_poor_bridges)

bridge_data <- bridge_total %>% 
  left_join(bridge_poor)


#Congestion data 
source("Congestion data process umr.R")


#Bring everything together 
AHR_states <- list(HM_10, 
                   HM_81, 
                   SF_4, 
                   HM64, 
                   FI_20, 
                   VM_2, 
                   bridge_data, 
                   congestion_data_summary) %>% 
  reduce(left_join, by = "state") 

#Calculate national metrics
AHR_national <- AHR_states %>%
  summarise(across(2:ncol(AHR_states), sum)) %>% 
  mutate(state = "United States")

AHR_data <- bind_rows(AHR_states, AHR_national) %>% 
  
  #Rename congestion columns for comparability with the next report
  rename(
    state_tot_congestion_hours = state_delay_hours,
    state_tot_commuters = state_auto_commuters
  ) %>% 

  #Add SHA ratio
  mutate(SHA_ratio = state_tot_lane_miles / SHA_miles, .after = state_tot_lane_miles) %>% 


  #Calculate key metrics
  mutate(
    
    #Disbursement
    capital_disbursement_perlm = capital_disbursement / state_tot_lane_miles,
    maintenance_disbursement_perlm = maintenance_disbursement /  state_tot_lane_miles,
    admin_disbursement_perlm = admin_disbursement /  state_tot_lane_miles,
    other_disbursement_perlm = other_disbursement / state_tot_lane_miles,
    
    #Pavement roughness (4 indicators)
    rural_interstate_poor_percent = rural_interstate_above_170 / rural_interstate_total * 100,
    urban_interstate_poor_percent = urban_interstate_above_170 / urban_interstate_total * 100,
    rural_OPA_poor_percent = rural_OPA_above_220 / rural_OPA_total * 100,
    urban_OPA_poor_percent = urban_OPA_above_220 / urban_OPA_total * 100,
   
    #Congestion Hours
    state_avg_congestion_hours = state_tot_congestion_hours / state_tot_commuters,
    
    #Bridge
    poor_bridges_percent = total_poor_bridges / total_bridges * 100,
    
    #Fatality
    rural_fatalities_per_100m_VMT = rural_fatality_interstate_OFE_OPA / rural_VMT_interstate_OFE_OPA * 100,
    urban_fatalities_per_100m_VMT = urban_fatality_interstate_OFE_OPA / urban_VMT_interstate_OFE_OPA * 100,
    other_fatalities_per_100m_VMT = other_fatality / other_VMT * 100
  )


#Calculate disbursement scores adjusted by urban miles percentage using linear regression
disbursement_data <- AHR_data %>% 
  select(state, pct_urban_lane_miles, capital_disbursement_perlm, maintenance_disbursement_perlm,
         admin_disbursement_perlm, other_disbursement_perlm) %>% 
  filter(state != "United States") %>%   #exclude the US numbers from the data
  pivot_longer(cols = 3:6, names_to = "key_metrics", values_to = "value") %>% 
  arrange(key_metrics)

 #Check linear model
disbursement_model_lm <- disbursement_data %>% 
  nest(data = -key_metrics) %>% 
  mutate(fit = map(data, ~ lm(value ~ pct_urban_lane_miles, data = .x)),
         tidied = map(fit, tidy)) %>% 
  unnest(tidied)

# test_data <- disbursement_data %>%
#   filter(key_metrics == "other_disbursement_perlm")
# 
# model_lm <- lm(value ~ pct_urban_lane_miles, data = test_data)
# summary(model_lm)
# model_lm$residuals
# sigma(model_lm)
# 
# ggplot(test_data, aes(x = pct_urban_lane_miles, y = value)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   geom_point(data = test_data %>% filter(state == "Maine"), col = "red")

 #Comment: there's a problem with the linear model for the "other_disbursement" data: some predicted values are negative

 #Check local regression model
# model_loess <- loess(value ~ pct_urban_lane_miles, data = test_data)
# summary(model_loess)
# residuals(model_loess)
# model_loess$s
# 
# 
# ggplot(test_data, aes(x = pct_urban_lane_miles, y = value)) +
#   geom_point() +
#   geom_smooth(method = "loess") +
#   geom_point(data = test_data %>% filter(state == "Maine"), col = "red")


 #Get fitted values (expected spending per lane mile given some level of urban roads)
disbursement_data <- disbursement_data %>% 
  group_by(key_metrics) %>% 
  do(data.frame(., fitted_linear = fitted(lm(value ~ pct_urban_lane_miles, data = .)))) %>% 
  do(data.frame(., fitted_loess = fitted(loess(value ~ pct_urban_lane_miles, data = .)))) %>%    #note that we use the default span of 0.75
  ungroup()

 #The loess model seems better for our purposes as it doesn't produce negative predicted values
disbursement_data <- disbursement_data %>% 
  select(state, key_metrics, value, fitted_loess) %>% 
  rename(exp_value = fitted_loess)


#Calculate scores and rankings (original)
scores <- AHR_data %>% 
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
  select(state, key_metrics, relative_score) %>% 
  pivot_wider(names_from = key_metrics,
              values_from = relative_score) %>% 
  rowwise() %>% 
  mutate(overall_score = mean(c_across(other_fatalities_per_100m_VMT_score:other_disbursement_perlm_score), na.rm = T)) %>% 
  ungroup() %>% 
  filter(state != "United States") %>% 
  mutate(across(c(2:15), min_rank, .names = "{.col}_rank"),
         # rank_26th_report = c(28, 48, 29, 17, 45, 37, 31, 44, 41, 14, 47, #hawaii
         #                      8, 40, 32, 22, 7, 4, 35, 33, 38, #Maryland
         #                      43, 34, 18, 15, 3, 11, 21, 20, 19, 50, #New Jersey
         #                      27, 46, 5, 1, 24, 36, 25, 39, 49, 23, #South Carolina
         #                      9, 10, 16, 6, 13, 2, 30, 29, 26, 12 
         # )
         ) 

# Calculate scores and rankings for new dashboard
scores_process <- AHR_data %>% 
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
  mutate(year = 2020, .before = everything())

## Spec lookup
spec <- tribble(
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

## Build a long view of just the needed AHR columns
needed_cols <- unique(c(spec$num, spec$denom))

AHR_long <- AHR_data %>%
  select(state, all_of(needed_cols)) %>%
  pivot_longer(-state, names_to = "col", values_to = "val")

AHR_num   <- AHR_long %>% rename(num   = col, numerator   = val)
AHR_denom <- AHR_long %>% rename(denom = col, denominator = val)

## Assemble: map metric -> (num, denom), bring both values in
scores2 <- scores_process %>%
  left_join(spec, by = c("key_metrics" = "score")) %>%
  left_join(AHR_num,   by = c("state", "num")) %>%
  left_join(AHR_denom, by = c("state", "denom")) %>%
  select(year, state, numerator, denominator, everything()) %>%
  select(-num, -denom)

## Arrange key_metrics to match spec order
order_map <- tibble(
  key_metrics = spec$score,
  key_order   = seq_along(spec$score)
)

scores2 <- scores2 %>%
  left_join(order_map, by = "key_metrics") %>%
  arrange(key_order, state) %>%
  select(-key_order)


# Get State Controlled Highway Miles table
state_controlled_mileage_table <- AHR_data %>% 
  select(state, SHA_miles, state_tot_lane_miles, SHA_ratio) %>% 
  mutate(year = 2020, .before = everything())

# Get only rankings in all categories
all_rankings <- scores %>% 
  select(state,
         overall_score_rank,
         capital_disbursement_perlm_score_rank,
         maintenance_disbursement_perlm_score_rank,
         admin_disbursement_perlm_score_rank,
         other_disbursement_perlm_score_rank,
         rural_interstate_poor_percent_score_rank,
         urban_interstate_poor_percent_score_rank,
         rural_OPA_poor_percent_score_rank,
         urban_OPA_poor_percent_score_rank,
         state_avg_congestion_hours_score_rank,
         poor_bridges_percent_score_rank,
         rural_fatalities_per_100m_VMT_score_rank,
         urban_fatalities_per_100m_VMT_score_rank,
         other_fatalities_per_100m_VMT_score_rank) %>% 
  rename_with(~ str_remove(., "_score")) %>% 
  mutate(year = 2020, .before = everything())



#List of data frames to be exported
data_list <- list("AHR Data" = AHR_data,
                  "Scores & Rankings" = scores,
                  "Disbursement Data" = disbursement_data)


data_list_new <- list("Individual Scores & Rankings" = scores2,
                      "State Mileage" = state_controlled_mileage_table,
                      "All Rankings" = all_rankings)


rio::export(data_list, "AHR_data 27th.xlsx")

rio::export(data_list_new, "AHR_data 27th dashboard.xlsx")






