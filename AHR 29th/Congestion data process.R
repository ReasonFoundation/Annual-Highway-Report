# library(tidyverse)
# library(readxl)
# library(janitor)
# library(stringr)
# library(modelr)
# library(dplyr)

inrix <- read_csv("data/inrix_delay_hour_2024.csv") %>% 
  clean_names() %>% 
  rename(delay_2024 = 2) %>% 
  filter(delay_2024 != "Hours Lost") %>% 
  filter(str_detect(urban_area, " [A-Z]{2}$")) %>% 

  mutate(change_from_2023 = str_remove(change_from_2023, "%"),
         change_from_2023 = str_squish(change_from_2023),
         delay_2024 = as.numeric(delay_2024),
         change_frac = as.numeric(change_from_2023)/ 100 ) %>% 
mutate(
  # Calculate 2023 value
  delay_2023 = round(delay_2024 / (1 + change_frac),1)
) %>% 
  select(urban_area, delay_2023)
   

#https://data.census.gov/table?g=010XX00US$31000M1
#All metropolitan Statistical Areas within United States and PR,
#S0802 Means of transportation to Work by selected characteristics
commuter_data <- read_csv("data/ACSST1Y2023.S0802-Data.csv")

# vehicle_miles_data <- import("data/hm74.xls")
state_name_df <- data.frame(state.abb, state.name)

#Clean data
inrix_clean <- inrix %>% 
  mutate(state = str_extract(urban_area, ".{2}$"),
         city = str_replace(urban_area, ".{2}$", ""),
         city = str_remove_all(city, "(City)|(Township)"),
         city = str_trim(city)) %>% select(-urban_area) %>% 
  rename(hours_lost_in_congestion = delay_2023) 


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

#Import and process HM74 data (vehicle miles data)
## Function to process each sheet in the HM74 data
process_hm74 <- function(sheet_name) {
  read_excel("data/hm74.xlsx", sheet = sheet_name) %>%
    slice(-(1:8)) %>% 
    select(1:27) %>% 
    rename(area = 1,
           state = 2, 
           unreported = 3) %>% 
    mutate(across(3:27, as.numeric)) %>% 
    mutate(interstate_total = rowSums(.[,4:8]),
           ofe_total = rowSums(.[,10:14]),
           opa_total =rowSums(.[,16:20]),
           ma_total =rowSums(.[,22:26])) %>% 
    select(-c(4:27)) %>% 
    mutate(across(unreported:ma_total, as.numeric)) %>% 
    filter(!str_detect(area, "footnote|Total|NULL"))
}

## List of sheets to process
excel_sheets("data/hm74.xlsx")
sheets <- 2:9

## Process each sheet and combine them into one data frame
vehicle_miles_data <- map_df(sheets, process_hm74)


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
  left_join(inrix_clean, by = c("first_state" = "state", "first_city" = "city")) %>% 
  left_join(inrix_clean, by = c("first_state" = "state", "second_city" = "city")) %>% 
  left_join(inrix_clean, by = c("first_state" = "state", "third_city" = "city")) %>% 
  rowwise() %>% 
  mutate(congestion_hours = mean(c(hours_lost_in_congestion.x, hours_lost_in_congestion.y, 
                                   hours_lost_in_congestion), na.rm = T)) %>% 
  ungroup()


#Run a linear regression model to find the relationship between congestion hours (inrix data) and the number of auto commuters
congestion_data_inrix <- congestion_data %>% 
  filter(!is.na(congestion_hours))

model <- lm(congestion_hours ~ auto_commuters_combined, data = congestion_data_inrix)  


#Check model
summary(model)

#plot the model, with log transformation
ggplot(congestion_data_inrix, aes(x = auto_commuters_combined, y = congestion_hours)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(method = "loess") +
  scale_x_log10()

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
  ungroup() %>% 
  left_join(state_name_df, by = c("state" = "state.abb")) %>% 
  select(-state) %>% 
  rename(state = state.name) %>% 
  filter(state %in% state.name)
  
  # mutate(state_avg_congestion_hours = state_tot_congestion_hours/state_tot_commuters)



