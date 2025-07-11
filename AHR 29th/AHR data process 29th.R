rm(list = ls())
library(tidyverse)
library(janitor)
library(readxl)
library(modelr)
library(rio)
library(broom)
library(plotly)

#Congestion data 
source("Congestion data process.R")
#Source: https://www.fhwa.dot.gov/policyinformation/statistics/2023/

state_name_df = data.frame(state.name, state.abb)

#HM-10: Public road length, miles by ownership
HM_10 <- read_excel("data/hm10.xlsx", sheet = "A") %>% 
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


#HM-81: State highway agency-owned public roads; 
#rural and urban miles, estimated lane-miles and daily travel

#Note: In California Policy Center report, the table shows "total lane miles" BUT it probably refers to dvmt infact. 
#https://californiapolicycenter.org/californias-transportation-future-part-four-the-common-road/

HM_81 <- read_excel("data/hm81.xlsx", sheet  = "A") %>% 
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
HM64_rural_interstate <- read_excel("data/hm64.xlsx", sheet = "A") %>% 
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


HM64_rural_OPA <- read_excel("data/hm64.xlsx", sheet = "B") %>% 
  remove_empty() %>% 
  select(1, 20:21) %>% 
  rename(state = 1,
         rural_OPA_above_220 = 2,
         rural_OPA_total = 3) %>% 
  slice(-(1:7)) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(rural_OPA_above_220:rural_OPA_total, as.numeric))


HM64_urban_interstate <- read_excel("data/hm64.xlsx", sheet = "C") %>% 
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


HM64_urban_OPA <- read_excel("data/hm64.xlsx", sheet = "D") %>% 
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
VM_2 <- read_excel("data/vm2m.xls", sheet = "A", skip = 13) %>% 
  rename(state = 1) %>% 
  mutate(rural_VMT_interstate_OFE_OPA = (`...2` + EXPRESSWAYS...3 + ARTERIAL...4),
         urban_VMT_interstate_OFE_OPA = (...10 + EXPRESSWAYS...11 + ARTERIAL...12),
         other_VMT = (ARTERIAL...5 + COLLECTOR...6 + COLLECTOR...7 + ...8 + ARTERIAL...13 + COLLECTOR...14 + COLLECTOR...15 + ...16)) %>% 
  select(1, 19:21) %>% 
  mutate(across(2:4)) %>%   
  filter(state %in% state.name)

####Bridge data####
bridge_raw <- read_excel("data/fccount23.xlsx", sheet = "Sheet1", skip = 10)  

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



####Bring everything together#### 
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


#Calculate scores and rankings
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
  mutate(across(c(2:15), min_rank, .names = "{.col}_rank")) 


#List of data frames to be exported
data_list <- list("AHR Data" = AHR_data,
                  "Scores & Rankings" = scores,
                  "Disbursement Data" = disbursement_data)


rio::export(data_list, "AHR_data 29th.xlsx")



