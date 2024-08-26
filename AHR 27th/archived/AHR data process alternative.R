library(tidyverse)
library(janitor)
library(readxl)
library(modelr)
library(rio)

#Cost of living index for later adjustment to spending numbers
coli <- read.csv("coli_meric.csv") %>% 
  clean_names() %>% 
  select(state, coli_index) %>% 
  mutate(coli_index = coli_index/100)


#HM-10: Public road length, miles by ownership
HM_10 <- read_excel("hm10.xls", sheet = "A") %>% 
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
HM_81 <- read_excel("hm81.xls", sheet  = "A") %>% 
  remove_empty() %>% 
  select(1, 17) %>% 
  rename(state = 1,
         state_controlled_lane_miles = 2) %>% 
  slice(-(1:7)) %>% 
  filter(state %in% state.name) %>% 
  mutate(state_controlled_lane_miles = as.numeric(state_controlled_lane_miles))


#SF-4: Disbursements for state-administered highways (thousands of dollars)
#Note: the 2020 numbers for some reason exactly match the 2019 numbers? 
SF_4 <- read_excel("sf4.xlsx", sheet = "A") %>% 
  remove_empty() %>% 
  select(1:4, 8) %>% 
  rename(state = 1,
         capital_disbursement = 2,
         maintenance_disbursement = 3,
         admin_disbursement = 4,
         total_disbursement = 5) %>% 
  slice(-(1:6)) %>% 
  mutate(state = str_replace_all(state, "[:punct:]", ""),    #remove all special characters
         state = str_replace_all(state, "[:digit:]", ""),    #remove all numbers
         state = str_trim(state),
         across(capital_disbursement:total_disbursement, as.numeric)) %>% 
  filter(state %in% state.name)

#Apply cost of living adjustment to spending numbers
SF_4_adjusted <- SF_4 %>% 
  left_join(coli) %>% 
  mutate(across(2:5, ~ .x/coli_index)) %>% 
  select(-coli_index)

#HM-64: Miles by measured pavement roughness
HM64_rural_interstate <- read_excel("hm64.xls", sheet = "A") %>% 
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
  

HM64_rural_OPA <- read_excel("hm64.xls", sheet = "B") %>% 
  remove_empty() %>% 
  select(1, 20:21) %>% 
  rename(state = 1,
         rural_OPA_above_220 = 2,
         rural_OPA_total = 3) %>% 
  slice(-(1:7)) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(rural_OPA_above_220:rural_OPA_total, as.numeric))
  
  
HM64_urban_interstate <- read_excel("hm64.xls", sheet = "C") %>% 
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


HM64_urban_OPA <- read_excel("hm64.xls", sheet = "D") %>% 
  remove_empty() %>% 
  select(1, 20:21) %>% 
  rename(state = 1,
         urban_OPA_above_220 = 2,
         urban_OPA_total = 3) %>% 
  slice(-(1:7)) %>% 
  filter(state %in% state.name) %>% 
  mutate(across(urban_OPA_above_220:urban_OPA_total, as.numeric))
  
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
FI_20 <- read_excel("fi20.xls", sheet = "A") %>% 
  remove_empty() %>% 
  # select(1:4, 10:12, 19) %>% 
  select(1, 9, 17) %>% 
  rename(
    state = 1,
    rural_total_fatalities = 2,
    urban_total_fatalities = 3
    # rural_interstate_fatalities = 2,
    # rural_OFE_fatalities = 3,
    # rural_OPA_fatalities = 4,
    # urban_interstate_fatalities = 5,
    # urban_OFE_fatalities = 6,
    # urban_OPA_fatalities = 7,
    # total_fatalities = 8
  ) %>% 
  filter(state %in% state.name) %>% 
  mutate(
    # across(rural_interstate_fatalities:total_fatalities, as.numeric),
    across(2:3, as.numeric)
    # rural_I_OFE_OPA_fatalities = rural_interstate_fatalities + rural_OFE_fatalities + rural_OPA_fatalities,
    # urban_I_OFE_OPA_fatalities = urban_interstate_fatalities + urban_OFE_fatalities + urban_OPA_fatalities
  ) 
  # select(state, total_fatalities, rural_I_OFE_OPA_fatalities, urban_I_OFE_OPA_fatalities)


#VM-2: Annual vehicle miles
VM_2 <- read_excel("vm2.xls", sheet = "A") %>%
  # select(1:4, 10:12, 18) %>% 
  select(1,9,17,18) %>% 
  rename(
    state = 1,
    rural_total_VMT = 2,
    urban_total_VMT = 3,
    # rural_interstate_VMT = 2,
    # rural_OFE_VMT = 3,
    # rural_OPA_VMT = 4,
    # urban_interstate_VMT = 5,
    # urban_OFE_VMT = 6,
    # urban_OPA_VMT = 7,
    total_VMT = 4
    ) %>% 
  filter(state %in% state.name) %>% 
  mutate(
    across(2:4, as.numeric)
    # rural_VMT = rural_interstate_VMT + rural_OFE_VMT + rural_OPA_VMT,
    # urban_VMT = urban_interstate_VMT + urban_OFE_VMT + urban_OPA_VMT
  )
  # select(state, total_VMT, rural_VMT, urban_VMT)

#Bridge data
bridge_raw <- read_excel("fccount21.xlsx", sheet = "2021")  

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
state_names <- data.frame(state.abb, state.name)     #need this to get state full names for congestion data
  
congestion_data_summary <- congestion_data_summary %>% 
  left_join(state_names, by = c("state" = "state.abb")) %>% 
  select(-state) %>% 
  rename(state = state.name) %>% 
  filter(!is.na(state))
  
#Bring everything together
AHR_data <- list(HM_10, 
                 HM_81, 
                 SF_4_adjusted, 
                 HM64, 
                 FI_20, 
                 VM_2, 
                 bridge_data, 
                 congestion_data_summary) %>% 
  reduce(left_join, by = "state")

#Calculate national metrics
AHR_data_national <- AHR_data %>%
  summarise(across(2:ncol(AHR_data), sum)) %>% 
  mutate(state = "United States")

AHR_data <- bind_rows(AHR_data, AHR_data_national)

#Add SHA ratio
AHR_data <- AHR_data %>% 
  mutate(SHA_ratio = state_controlled_lane_miles / SHA_miles, .after = state_controlled_lane_miles)


#Calculate key metrics
AHR_data <- AHR_data %>% 
  mutate(
    capital_disbursement_per_vmt = capital_disbursement / total_VMT * 1000,
    
    maintenance_disbursement_per_vmt = maintenance_disbursement / total_VMT * 1000,
    
    admin_disbursement_per_vmt = admin_disbursement / total_VMT * 1000,
    
    # total_disbursement_per_lm = total_disbursement / state_controlled_lane_miles * 1000,
    
    rural_interstate_poor_percent = rural_interstate_above_170 / rural_interstate_total * 100,
    
    urban_interstate_poor_percent = urban_interstate_above_170 / urban_interstate_total * 100,
    
    rural_OPA_poor_percent = rural_OPA_above_220 / rural_OPA_total * 100,
    
    # urban_OPA_poor_percent = urban_OPA_above_220 / urban_OPA_total * 100,
    
    poor_bridges_percent = total_poor_bridges / total_bridges * 100,
    
    # total_fatalities_per_100m_VMT = total_fatalities / total_VMT * 100,
    
    rural_fatalities_per_100m_VMT = rural_total_fatalities / rural_total_VMT * 100,
    
    urban_fatalities_per_100m_VMT = urban_total_fatalities / urban_total_VMT * 100,
    
    state_avg_delay_hours = state_peak_delay/state_auto_commuters + state_remain_delay/state_population
  )


#Calculate overall scores
AHR_data_composite <- AHR_data %>% 
  pivot_longer(cols = capital_disbursement_per_vmt:state_avg_delay_hours, 
               names_to = "key_metrics", 
               values_to = "value") %>% 
  arrange(key_metrics) %>% 
  select(state, key_metrics, value) %>% 
  group_by(key_metrics) %>% 
  mutate(relative_score = value / value[state == "United States"]) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  summarise(overall_score = mean(relative_score, na.rm = T)) %>% 
  ungroup()
  
#Add overall scores to AHR_data
AHR_data <- AHR_data %>% 
  left_join(AHR_data_composite)

#Rank states according to the calculated metrics
AHR_data_rank <- AHR_data %>% 
  filter(state != "United States") %>% 
  mutate(
    across(capital_disbursement_per_vmt:overall_score, min_rank, .names = "{.col}_rank")
  )

AHR_data <- AHR_data %>% 
  left_join(AHR_data_rank)

export(AHR_data, "AHR_data 2022 alternative final 2.xlsx")

#Visualize AHR raw scores






