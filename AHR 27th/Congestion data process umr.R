# library(tidyverse)
# library(readxl)
# library(janitor)
# library(stringr)
# library(modelr)
# library(rio)

umr_2020 <- import("AHR 27th/complete-data-2021-umr-by-tti.xlsx", sheet = "urban areas")
vehicle_miles_data <- read_excel("AHR 27th/HM-74 Daily Vehicle Miles Travelled.xlsx", sheet = "Sheet1")


#Function to solve for "peak period delay" and "remaining delay" (see umr report's methodology)
# solve_delay <- function(commuters, pop, delay_per_commuter, total_delay){
#   a <- rbind(c(1/commuters, 1/pop),
#              c(1, 1))
#   b <- c(delay_per_commuter, total_delay)
#   
#   x <- solve(a,b)
#   return(x)
# }

#Clean umr data and calculate peak period delay and remaining delay for each area
state_list <- paste0(state.abb, collapse = "|")   #use this to remove state abbreviations later

umr_2020_clean <- umr_2020 %>% 
  filter(Year == "2020") %>% 
  rename(
    population = Population...6,
    auto_commuters = Auto, # original column:  Auto Commuter (000)
    total_hours_delay = `Annual Hours of Delay`,
    hours_delay_perAuto_commuter = `...24`, # original column: per Auto Commuter
    first_state = Primary, 
    urban_area = `Urban Area`,
  ) %>% 
  select(urban_area, first_state, population, auto_commuters, total_hours_delay, hours_delay_perAuto_commuter) %>% 
  mutate(across(population:hours_delay_perAuto_commuter, as.numeric),
         auto_commuters = auto_commuters * 1000,
         population = population * 1000) %>% 
  # rowwise() %>% 
  # mutate(delay_numbers = list(solve_delay(commuters = auto_commuters,
  #                                         pop = population, 
  #                                         delay_per_commuter = hours_delay_perAuto_commuter, 
  #                                         total_delay = total_hours_delay)),
  #        peak_delay = delay_numbers[1],
  #        remain_delay = delay_numbers[2]) %>% 
  # ungroup() %>% 
  
  # clean city name 
  mutate(urban_area = str_remove_all(urban_area, " City"),
         city = str_remove_all(urban_area, state_list),    #remove state abbreviations
         city = str_remove(city, "-+$"),
         city = str_squish(city),
         
         # split first-second-third city ; first-second state--> each will have proportion of daily vehicle mile traveled corresponding to each state. 
         first_city = str_split(city, "-", simplify = T)[,1],
         second_city = str_split(city, "-", simplify = T)[,2],
         third_city = str_split(city, "-", simplify = T)[,3]
         )
  # select(first_city, first_state, auto_commuters, population, peak_delay, remain_delay) 
  
  

# commuter_data_clean <- commuter_data %>% 
#   clean_names() %>% 
#   select(name, s0802_c02_001e, s0802_c03_001e) %>% 
#   rename(area = name,
#          auto_commuters_alone = s0802_c02_001e,
#          auto_commuters_carpooled = s0802_c03_001e) %>% 
#   mutate(auto_commuters_alone = as.numeric(auto_commuters_alone),
#          auto_commuters_carpooled = as.numeric(auto_commuters_carpooled),
#          auto_commuters_combined = auto_commuters_alone + auto_commuters_carpooled/2.2) %>%    #2.2 is the average carpool occupancy. 
#   filter(!is.na(auto_commuters_alone)) %>% 
#   mutate(city = str_replace(area, ",.*", ""),       #remove state name and area label (metro vs micro)
#          city = str_replace(city, " City", ""),     #remove "City" in city names 
#          first_city = str_split(city, "-", simplify = T)[,1],
#          # city = str_replace(city, "-.*", ""),   #retain the first city name only
#          second_city = str_split(city, "-", simplify = T)[,2],
#          third_city = str_split(city, "-", simplify = T)[,3],
#          
#          first_state = str_extract(area, ", .."),   
#          first_state = str_replace(first_state, ", ", ""),  
#          urban = ifelse(grepl("Metro Area", area), "Metro Area", "Micro Area")
#          ) %>%
#   filter(urban == "Metro Area") %>%   #retain metro areas only. 
#   relocate(area, city, first_city, second_city, third_city, first_state, urban)


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
  select(area, first_city,  first_state, state, dmvt_pct) %>% 
  #fix some city names to match the umr data
  mutate(first_city = replace(first_city, first_city == "Urban Honolulu", "Honolulu"),
         first_city = replace(first_city, first_city == "Louisville/Jefferson County", "Louisville"),
         first_city = replace(first_city, first_city == "East Stroudsburg", "East Stoudsburg"),
         first_city = replace(first_city, first_city == "El Paso de Robles (Paso Robles)", "El Paso de Robles"),
         first_city = replace(first_city, first_city == "Homosassa Springs", "Homosassa Spr"),
         first_city = replace(first_city, first_city == "Round Lake Beach", "Round Lake Bch"),
         first_state = replace(first_state, first_city == "Round Lake Bch", "WI"))

#Combine commuter data (ACS) and Inrix data
# congestion_data <- commuter_data_clean %>% 
#   left_join(inrix, by = c("first_state" = "state", "first_city" = "city")) %>% 
#   left_join(inrix, by = c("first_state" = "state", "second_city" = "city")) %>% 
#   left_join(inrix, by = c("first_state" = "state", "third_city" = "city")) %>% 
#   rowwise() %>% 
#   mutate(congestion_hours = mean(c(hours_lost_in_congestion.x, hours_lost_in_congestion.y, hours_lost_in_congestion), na.rm = T)) %>% 
#   ungroup()

  
# ggplot(congestion_data, aes(x = auto_commuters_combined, y = congestion_hours)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   geom_smooth(method = "loess")

#Run a linear regression model to find the relationship between congestion hours (inrix data) and the number of auto commuters
# congestion_data_inrix <- congestion_data %>% 
#   filter(!is.na(congestion_hours))
# 
# model <- lm(congestion_hours ~ auto_commuters_combined, data = congestion_data_inrix)  

#Check model
# summary(model)

#Use the model to predict the congestion hours for areas not included in the inrix data set
# congestion_data_non_inrix <- congestion_data %>% 
#   filter(is.na(congestion_hours)) %>% 
#   add_predictions(model) %>% 
#   mutate(congestion_hours = pred) %>% 
#   select(-pred)

# pred <- predict(model, congestion_data_non_inrix)




#Combine inrix and non-inrix congestion data and add the daily vehicle miles traveled data to allocate the commuter number for multi-state areas
state_names <- data.frame(state.abb, state.name)     #to get state full names 

congestion_data_final <- umr_2020_clean %>% 
  left_join(vehicle_miles_data_clean, by = c("first_city", "first_state")) %>%
  mutate(dmvt_pct = ifelse(is.na(dmvt_pct), 1, dmvt_pct),
         state = ifelse(is.na(state), first_state, state),
         ad_auto_commuters = auto_commuters * dmvt_pct,
         ad_delay_hours = ad_auto_commuters * hours_delay_perAuto_commuter
         # ad_population = population * dmvt_pct,
         # ad_peak_delay = peak_delay * dmvt_pct,
         # ad_remain_delay = remain_delay * dmvt_pct
         )

#Calculate delay hours per commuter by state
congestion_data_summary <- congestion_data_final %>% 
  group_by(state) %>% 
  summarise(state_auto_commuters = sum(ad_auto_commuters),
            state_delay_hours = sum(ad_delay_hours)
            # state_population = sum(ad_population),
            # state_peak_delay = sum(ad_peak_delay),
            # state_remain_delay = sum(ad_remain_delay)
            ) %>% 
  ungroup() %>% 
  left_join(state_names, by = c("state" = "state.abb")) %>% 
  select(-state) %>% 
  rename(state = state.name) %>% 
  filter(!is.na(state))
  # mutate(state_avg_delay_hours = state_delay_hours/state_auto_commuters)
  
# mutate(state_avg_delay_hours = state_peak_delay/state_auto_commuters + state_remain_delay/state_population)



