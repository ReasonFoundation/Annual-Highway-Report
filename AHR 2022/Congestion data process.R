library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(modelr)

inrix <- read_csv("inrix data 2021.csv")
commuter_data <- read_csv("ACS Commuter Data 2019.csv")
vehicle_miles_data <- read_excel("HM-74 Daily Vehicle Miles Travelled.xlsx", sheet = "Sheet1")

#Clean data
commuter_data_clean <- commuter_data %>% 
  clean_names() %>% 
  select(name, s0802_c02_001e, s0802_c03_001e) %>% 
  rename(area = name,
         auto_commuters_alone = s0802_c02_001e,
         auto_commuters_carpooled = s0802_c03_001e) %>% 
  mutate(auto_commuters_alone = as.numeric(auto_commuters_alone),
         auto_commuters_carpooled = as.numeric(auto_commuters_carpooled),
         auto_commuters_combined = auto_commuters_alone + auto_commuters_carpooled/2.2) %>%    #2.2 is the average carpool occupancy. 
  filter(!is.na(auto_commuters_alone)) %>% 
  mutate(city = str_replace(area, ",.*", ""),       #remove state name and area label (metro vs micro)
         city = str_replace(city, " City", ""),     #remove "City" in city names 
         first_city = str_split(city, "-", simplify = T)[,1],
         # city = str_replace(city, "-.*", ""),   #retain the first city name only
         second_city = str_split(city, "-", simplify = T)[,2],
         third_city = str_split(city, "-", simplify = T)[,3],
         
         first_state = str_extract(area, ", .."),   
         first_state = str_replace(first_state, ", ", ""),  
         urban = ifelse(grepl("Metro Area", area), "Metro Area", "Micro Area")
         ) %>%
  filter(urban == "Metro Area") %>%   #retain metro areas only. 
  relocate(area, city, first_city, second_city, third_city, first_state, urban)


vehicle_miles_data_clean <- vehicle_miles_data %>% 
  clean_names() %>% 
  mutate(first_city = str_replace(area, ",.*",""),
         first_city = str_replace(first_city, "-.*",""),
         first_city = str_replace(first_city, " City", ""),
         first_state = str_extract(area, ", .."),
         first_state = str_replace(first_state, ", ", ""),
         total_dmvt = interstate_total + ofe_total + opa_total + ma_total   #exclude "unreported" dvm based on the documentation and past results. Can change this later if we decide to include this number. 
         ) %>% 
  group_by(area) %>% 
  mutate(dmvt_pct = total_dmvt/sum(total_dmvt)) %>%
  ungroup() %>% 
  select(area, first_city,  first_state, state, dmvt_pct)

#Combine commuter data (ACS) and Inrix data
congestion_data <- commuter_data_clean %>% 
  left_join(inrix, by = c("first_state" = "state", "first_city" = "city")) %>% 
  left_join(inrix, by = c("first_state" = "state", "second_city" = "city")) %>% 
  left_join(inrix, by = c("first_state" = "state", "third_city" = "city")) %>% 
  rowwise() %>% 
  mutate(congestion_hours = mean(c(hours_lost_in_congestion.x, hours_lost_in_congestion.y, hours_lost_in_congestion), na.rm = T)) %>% 
  ungroup()

  
# ggplot(congestion_data, aes(x = auto_commuters_combined, y = congestion_hours)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   geom_smooth(method = "loess")

#Run a linear regression model to find the relationship between congestion hours (inrix data) and the number of auto commuters
congestion_data_inrix <- congestion_data %>% 
  filter(!is.na(congestion_hours))

model <- lm(congestion_hours ~ auto_commuters_combined, data = congestion_data_inrix)  

#Check model
summary(model)

#Use the model to predict the congestion hours for areas not included in the inrix data set
congestion_data_non_inrix <- congestion_data %>% 
  filter(is.na(congestion_hours)) %>% 
  add_predictions(model) %>% 
  mutate(congestion_hours = pred) %>% 
  select(-pred)

# pred <- predict(model, congestion_data_non_inrix)

#Combine inrix and non-inrix congestion data and add the daily vehicle miles traveled data to allocate the commuter number for multi-state areas
congestion_data_final <- bind_rows(congestion_data_inrix, congestion_data_non_inrix) %>% 
  arrange(area) %>% 
  left_join(vehicle_miles_data_clean, by = c("first_city", "first_state")) %>%
  mutate(dmvt_pct = ifelse(is.na(dmvt_pct), 1, dmvt_pct),
         state = ifelse(is.na(state), first_state, state),
         total_congestion_hours = congestion_hours * auto_commuters_combined * dmvt_pct,
         adjusted_auto_commuters_combined = auto_commuters_combined * dmvt_pct)

#Calculate congestion hours per commuter by state
congestion_data_summary <- congestion_data_final %>% 
  group_by(state) %>% 
  summarise(state_tot_congestion_hours = sum(total_congestion_hours),
            state_tot_commuters = sum(adjusted_auto_commuters_combined)) %>% 
  ungroup()
  # mutate(state_avg_congestion_hours = state_tot_congestion_hours/state_tot_commuters)



