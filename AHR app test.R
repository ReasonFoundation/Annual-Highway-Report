# Load required libraries
library(shiny)
library(reactable)
library(tidyverse)
library(readxl)
library(rio)
library(scales)
library(leaflet)
library(sf)
library(tigris)
library(plotly)
library(janitor)
library(bslib)

# Load disbursement illustration data (2023)
HM_81 <- read_excel("hm81_2023.xlsx", sheet = "A") %>%
  remove_empty() %>%
  rename(state = 1,
         state_urban_lane_miles = 10,
         state_tot_lane_miles = 17) %>%
  filter(state %in% state.name) %>%
  mutate(state_urban_lane_miles = as.numeric(state_urban_lane_miles),
         state_tot_lane_miles = as.numeric(state_tot_lane_miles),
         pct_urban_lane_miles = state_urban_lane_miles / state_tot_lane_miles) %>%
  select(state, pct_urban_lane_miles, state_tot_lane_miles)

SF_4 <- read_excel("sf4_2023.xlsx", sheet = "A") %>%
  remove_empty() %>%
  select(1:7) %>%
  rename(state = 1,
         capital_disbursement = 2,
         maintenance_disbursement = 3,
         admin_disbursement = 4) %>%
  slice(-(1:6)) %>%
  mutate(state = str_replace_all(state, "[:punct:]|[:digit:]", ""),
         state = str_trim(state),
         across(2:7, as.numeric)) %>%
  mutate(other_disbursement = rowSums(across(5:7)),
         across(c(capital_disbursement, maintenance_disbursement, admin_disbursement, other_disbursement), ~ . * 1000)) %>%
  filter(state %in% state.name) %>% select(-c(5:7))

AHR_states <- SF_4 %>%
  left_join(HM_81, by = "state")

disbursement_data <- AHR_states %>%
  mutate(across(c(capital_disbursement, maintenance_disbursement, admin_disbursement, other_disbursement), ~ . / state_tot_lane_miles, .names = "{col}_perlm")) %>%
  select(state, pct_urban_lane_miles, state_tot_lane_miles, capital_disbursement_perlm, maintenance_disbursement_perlm, admin_disbursement_perlm, other_disbursement_perlm) %>%
  pivot_longer(cols = ends_with("perlm"), names_to = "key_metrics", values_to = "value") %>%
  group_by(key_metrics) %>%
  mutate(fitted_loess = fitted(loess(value ~ pct_urban_lane_miles)),
         national_avg = sum(value * state_tot_lane_miles) / sum(state_tot_lane_miles),
         relative_loess = value / fitted_loess,
         relative_avg = value / national_avg,
         rank_loess = min_rank(relative_loess),
         rank_avg = min_rank(relative_avg)) %>% 
  ungroup()

# Load main data
data_list <- import_list("AHR_combined_data.xlsx")
individual <- as_tibble(data_list[[1]])
mileage <- as_tibble(data_list[[2]])
rankings <- as_tibble(data_list[[3]])

# Prepare map data
states_sf_map <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AS", "GU", "MP", "PR", "VI")) %>%
  shift_geometry() %>%
  st_transform(crs = 4326)

# Category mapping
category_choices <- c(
  "All Rankings" = "all_rankings",
  "State-Controlled Mileage" = "state_mileage",
  "Capital & Bridge Disbursements" = "capital_disbursement_perlm_score",
  "Maintenance Disbursements" = "maintenance_disbursement_perlm_score",
  "Admin Disbursements" = "admin_disbursement_perlm_score",
  "Other Disbursements" = "other_disbursement_perlm_score",
  "Rural Interstate Pavement Condition" = "rural_interstate_poor_percent_score",
  "Urban Interstate Pavement Condition" = "urban_interstate_poor_percent_score",
  "Rural Arterial Pavement Condition" = "rural_OPA_poor_percent_score",
  "Urban Arterial Pavement Condition" = "urban_OPA_poor_percent_score",
  "Urbanized Area Congestion" = "state_avg_congestion_hours_score",
  "Structurally Deficient Bridge" = "poor_bridges_percent_score",
  "Rural Fatality Rate" = "rural_fatalities_per_100m_VMT_score",
  "Urban Fatality Rate" = "urban_fatalities_per_100m_VMT_score",
  "Other Fatality Rate" = "other_fatalities_per_100m_VMT_score"
)

# Value column names mapping
value_col_names <- list(
  "capital_disbursement_perlm_score" = "Capital Disbursement Per Lane-Mile",
  "maintenance_disbursement_perlm_score" = "Maintenance Disbursement Per Lane-Mile",
  "admin_disbursement_perlm_score" = "Admin Disbursement Per Lane-Mile",
  "other_disbursement_perlm_score" = "Other Disbursement Per Lane-Mile",
  "rural_interstate_poor_percent_score" = "Percent Rural Interstate Mileage in Poor Condition",
  "urban_interstate_poor_percent_score" = "Percent Urban Interstate Mileage in Poor Condition",
  "rural_OPA_poor_percent_score" = "Percent Rural Other Principal Arterial Mileage in Poor Condition",
  "urban_OPA_poor_percent_score" = "Percent Urban Other Principal Arterial Mileage in Poor Condition",
  "state_avg_congestion_hours_score" = "Peak Hours Spent in Congestion per Auto Commuter",
  "poor_bridges_percent_score" = "Percent Structurally Deficient Bridges",
  "rural_fatalities_per_100m_VMT_score" = "Rural Fatality Rate Per 100 Million Vehicles-Miles",
  "urban_fatalities_per_100m_VMT_score" = "Urban Fatality Rate Per 100 Million Vehicles-Miles",
  "other_fatalities_per_100m_VMT_score" = "Other Fatality Rate Per 100 Million Vehicles-Miles"
)

# Color palette (green for best (low rank/score), red for worst (high rank/score))
rank_color_palette <- c("#1A9850", "#FEE08B", "#D73027")  # green-yellow-red

# Style function for ranks (low good green, high bad red)
rank_style <- function(value) {
  if (is.na(value)) return(NULL)
  normalized <- (value - 1) / (50 - 1)
  color <- colorNumeric(rank_color_palette, domain = c(0, 1))(normalized)
  text_color <- ifelse(normalized > 0.35 & normalized < 0.65, "black", "white")
  list(background = color, color = text_color)
}

# Style function for relative scores (low good green, high bad red)
score_style <- function(value, min_val, max_val) {
  if (is.na(value)) return(NULL)
  if (min_val == max_val) {
    normalized <- 0.5
  } else {
    normalized <- (value - min_val) / (max_val - min_val)
  }
  color <- colorNumeric(rank_color_palette, domain = c(0, 1))(normalized)
  text_color <- ifelse(normalized > 0.35 & normalized < 0.65, "black", "white")
  list(background = color, color = text_color)
}

# Bar cell function with label always visible
bar_cell <- function(label, value, min_val, max_val) {
  if (is.na(value)) return(label)
  if (min_val == max_val) {
    normalized <- 0.5
  } else {
    normalized <- (value - min_val) / (max_val - min_val)
  }
  bar_color <- colorNumeric(rank_color_palette, domain = c(0, 1))(normalized)
  bar_width <- paste0(normalized * 100, "%")
  text_color <- ifelse(normalized > 0.5, "white", "black")  # Better contrast
  htmltools::div(
    style = "position: relative; background: #e1e1e1; border-radius: 3px; height: 20px;",
    htmltools::div(
      style = paste0(
        "position: absolute; left: 0; top: 0; bottom: 0; width: ", bar_width, ";",
        "background: ", bar_color, "; border-radius: 3px;"
      )
    ),
    htmltools::div(
      style = paste0("position: absolute; left: 5px; top: 0; bottom: 0; display: flex; align-items: center; color: ", text_color, ";")
      , label
    )
  )
}

# Disbursement category choices
disp_category_choices <- c(
  "Capital & Bridge Disbursements" = "capital_disbursement_perlm",
  "Maintenance Disbursements" = "maintenance_disbursement_perlm",
  "Admin Disbursements" = "admin_disbursement_perlm",
  "Other Disbursements" = "other_disbursement_perlm"
)

# UI
ui <- page_fluid(
  theme = bs_theme(
    version = 5,  # Upgrade to Bootstrap 5
    bootswatch = "flatly",  # Modern theme; alternatives: "cosmo", "litera", "minty"
    base_font = font_google("Open Sans"),  # Keeps your existing font; bslib handles Google Fonts import
    heading_font = font_google("Open Sans"),
    font_scale = 0.85
  ),
  tags$style(HTML("
    body { font-family: 'Open Sans', sans-serif; }
    h2 { 
      font-size: 1.5rem; /* Reduced from ~2rem (Bootstrap default) */
      font-weight: 700; 
    }
    h3 { 
      font-size: 1.2rem; /* Reduced from ~1.75rem (Bootstrap default) */
    }
    .leaflet-container { background: #FFFFFF; }
    .small-legend { 
      font-size: 10px;              /* text smaller   */
      line-height: 12px;
      padding: 2px 4px;
    }
    .small-legend .legend-title {
      font-size: 11px;
      margin-bottom: 2px;
    }
    .small-legend i {               /* color boxes */
      width: 10px !important;
      height: 10px !important;
    }
    .reactable { font-size: 12px; } /* Adjust reactable table font size */
    .plotly .main-svg { font-size: 12px !important; } /* Adjust plotly text */
    .leaflet-tooltip { font-size: 10px !important; } /* Adjust map tooltip text */
  ")),  # Your custom CSS can stay; no need for separate Google Fonts link
  fluidRow(
    column(10, h2("Annual Highway Report Dashboard")),
    column(2)
  ),
  hr(),
  mainPanel(
    width = 12,
    tabsetPanel(
      tabPanel("State Scores & Map",
               fluidRow(
                 column(4,
                        h4("Map & Table Controls"),
                        selectInput("year_select_map", "Select Year:",
                                    choices = sort(unique(individual$year)),
                                    selected = max(unique(individual$year))),
                        radioButtons("category_select_map", "Select Category:",
                                     choices = category_choices),
                        selectInput("state_select_map", "Select a State:",
                                    choices = c("All States", sort(unique(individual$state)))),
                        actionButton("show_all_map_button", "Show All States", width = "100%")
                 ),
                 column(8,
                        leafletOutput("state_map", height = "500px")
                 )
               ),
               fluidRow(
                 column(12,
                        hr(),
                        uiOutput("map_table_header"),
                        reactableOutput("map_table"),
                        downloadButton("download_table", "Download Table")
                 )
               )
      ),
      tabPanel("Disbursement Illustration",
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("disp_category", "Disbursement Category:",
                                choices = disp_category_choices),
                   selectInput("disp_state_select", "Select a State:",
                               choices = c("All States", sort(unique(disbursement_data$state)))),
                   actionButton("disp_show_all_button", "Show All States", width = "100%")
                 ),
                 mainPanel(
                   plotlyOutput("disp_plot"),
                   reactableOutput("disp_table")
                 )
               )
      ),
      tabPanel("Methodology",
               fluidRow(
                 column(10, offset = 1,
                        h2("Methodology", style = "text-align:center; border-bottom: 1px solid #ddd; padding-bottom:10px; margin-top:20px;"),
                        h3("I. Overview"),
                        p("This dashboard evaluates U.S. state highway performance using key metrics. Each state's value is compared to a benchmark—either the national average or an expected value adjusted for state-specific factors like urbanization. The relative score is the state's value divided by the benchmark. Lower scores are better for cost, condition, and safety metrics. States are ranked 1 (best) to 50 (worst) based on relative scores. The overall rank is the average of all relative scores."),
                        h3("II. Data Sources"),
                        p("Data is primarily from the Federal Highway Administration (FHWA), with congestion from INRIX and ACS:"),
                        tags$ul(
                          tags$li("Road ownership and length (HM-10, HM-81)"),
                          tags$li("Disbursements (SF-4)"),
                          tags$li("Pavement roughness (HM-64)"),
                          tags$li("Fatal crashes (FI-20)"),
                          tags$li("Vehicle miles (VM-2)"),
                          tags$li("Bridges (National Bridge Inventory)"),
                          tags$li("Congestion hours (INRIX), commuters (ACS S0802)")
                        ),
                        hr(),
                        h3("III. Metric Calculations"),
                        h4("1. Disbursements Per Lane-Mile (Capital & Bridge, Maintenance, Admin, Other)"),
                        p("These metrics show how much money a state spends in each category per lane-mile of road it has. We calculate it by taking the total spending in a category (like capital or maintenance) and dividing it by the state's total number of lane-miles."),
                        p("However, not all states are the same. Roads in urban (city) areas generally cost more to build, maintain, or administer than roads in rural (country) areas because of things like higher labor costs, more traffic, and complex infrastructure. To make comparisons fair, we adjust the spending numbers based on how urban each state is."),
                        p("We measure 'urban-ness' as the percentage of a state's lane-miles that are in urban areas (% urban lane-miles = urban lane-miles / total lane-miles)."),
                        p("To adjust, we use a statistical method called LOESS (Locally Estimated Scatterplot Smoothing). Imagine you have a graph where the x-axis is % urban lane-miles for each state, and the y-axis is spending per lane-mile. Each state is a point on this graph. LOESS draws a smooth curve that follows the general pattern of these points, showing how spending tends to change as urbanization increases."),
                        p("LOESS works by looking at groups of nearby points on the graph. For each point (state), it considers 75% of the closest points (the default 'span') and calculates a local trend. This creates a flexible curve that captures real patterns without assuming a straight line or simple shape."),
                        p("Steps in detail:"),
                        tags$ul(
                          tags$li("For each disbursement category, collect spending per lane-mile and % urban for all 50 states."),
                          tags$li("Create the graph of spending vs. % urban."),
                          tags$li("Apply LOESS to draw the smooth trend curve through the points."),
                          tags$li("For each state, find the y-value on this curve at its % urban x-value. This is the 'expected' spending—the typical amount for a state with that level of urbanization, based on national patterns."),
                          tags$li("The relative score is the state's actual spending divided by this expected spending. A score below 1 means spending less than expected (more efficient), above 1 means more (less efficient).")
                        ),
                        p("Formula: Expected Spending = value on LOESS curve at state's % urban lane-miles."),
                        p("This adjustment prevents penalizing states with many urban roads, as their higher costs are expected. It levels the playing field for comparison."),
                        h4("2. Pavement Condition (Rural/Urban Interstate/Arterial)"),
                        p("% mileage in 'poor' condition (IRI >170 for interstates, >220 for arterials; higher IRI = rougher road)."),
                        p("Relative score = state % / national %."),
                        h4("3. Urbanized Area Congestion"),
                        p("Average peak hours delayed per auto commuter."),
                        p("For 100+ major areas: Use INRIX 'hours lost'."),
                        p("For others: Estimate using linear regression on INRIX data."),
                        p("Model: Hours Lost = a + b × Number of Auto Commuters (a and b are numbers estimated from the data to best match the pattern)."),
                        p("This equation predicts hours for smaller areas based on commuter count."),
                        p("Aggregate to state:"),
                        tags$ul(
                          tags$li("For areas spanning multiple states, split based on % of daily vehicle miles traveled (VMT) in each state and number of commuters."),
                          tags$li("State total delay hours = sum over areas (hours × commuters × state VMT %)."),
                          tags$li("Average = total delay hours / total commuters.")
                        ),
                        p("Relative score = state average / national average."),
                        h4("4. Structurally Deficient Bridges"),
                        p("% bridges poor/deficient."),
                        p("Relative score = state % / national %."),
                        h4("5. Fatality Rates (Rural, Urban, Other)"),
                        p("Fatalities / 100 million VMT, by road type (interstate/OFE/OPA for rural/urban; 'other' for remaining)."),
                        p("Relative score = state rate / national rate."),
                        h4("6. State-Controlled Mileage"),
                        p("Avg number of lanes = Total lane-miles / Centerline Mileage.")
                 )
               )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Global min/max for scores per category/year
  global_stats <- reactive({
    year_val <- input$year_select_map
    cat_val <- input$category_select_map
    
    if (cat_val %in% category_choices[3:length(category_choices)]) {
      df_all <- individual %>%
        filter(year == year_val, key_metrics == cat_val)
      overall_min <- min(c(df_all$value, df_all$exp_value), na.rm = TRUE)
      overall_max <- max(c(df_all$value, df_all$exp_value), na.rm = TRUE)
      list(
        score_min = min(df_all$relative_score, na.rm = TRUE),
        score_max = max(df_all$relative_score, na.rm = TRUE),
        overall_min = overall_min,
        overall_max = overall_max
      )
    } else if (cat_val == "state_mileage") {
      df_all <- mileage %>%
        filter(year == year_val, state != "United States")
      overall_min <- min(c(df_all$SHA_miles, df_all$state_tot_lane_miles), na.rm = TRUE)
      overall_max <- max(c(df_all$SHA_miles, df_all$state_tot_lane_miles), na.rm = TRUE)
      list(
        overall_min = overall_min,
        overall_max = overall_max,
        ratio_min = min(df_all$SHA_ratio, na.rm = TRUE),
        ratio_max = max(df_all$SHA_ratio, na.rm = TRUE)
      )
    } else {
      list()
    }
  })
  
  # Map data reactive
  map_plot_data_r <- reactive({
    req(input$year_select_map, input$category_select_map)
    year_val <- input$year_select_map
    cat_val <- input$category_select_map
    
    if (cat_val %in% category_choices[3:length(category_choices)]) {
      # Individual metrics
      scores_df <- individual %>%
        filter(year == year_val, key_metrics == cat_val) %>%
        select(State = state, ScoreToDisplay = rank)
    } else if (cat_val == "all_rankings") {
      # All Rankings - use overall_rank
      scores_df <- rankings %>%
        filter(year == year_val) %>%
        select(State = state, ScoreToDisplay = overall_rank)
    } else if (cat_val == "state_mileage") {
      # State Mileage - rank SHA_ratio
      scores_df <- mileage %>%
        filter(year == year_val, state != "United States") %>%
        mutate(ScoreToDisplay = min_rank(SHA_ratio)) %>%
        select(State = state, ScoreToDisplay)
    }
    
    states_sf_map %>%
      inner_join(scores_df, by = c("NAME" = "State"))
  })
  
  # Render map
  output$state_map <- renderLeaflet({
    req(map_plot_data_r())
    pal <- colorNumeric(palette = rank_color_palette, domain = c(1, 50))
    
    leaflet(data = map_plot_data_r(),
            options = leafletOptions(
              zoomControl = FALSE, scrollWheelZoom = FALSE, doubleClickZoom = FALSE,
              dragging = FALSE, touchZoom = FALSE
            )) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal(ScoreToDisplay),
        weight = 1, opacity = 1, color = "white", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
        label = ~sprintf("<strong>%s (%s)</strong><br/>%s: %.0f",
                         NAME, input$year_select_map, names(category_choices)[category_choices == input$category_select_map], ScoreToDisplay) %>% lapply(htmltools::HTML),
        layerId = ~NAME
      )
  })
  
  # Update map on input change
  observeEvent(c(input$year_select_map, input$category_select_map), {
    req(map_plot_data_r())
    pal <- colorNumeric(palette = rank_color_palette, domain = c(1, 50))
    
    leafletProxy("state_map", data = map_plot_data_r()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(ScoreToDisplay),
        weight = 1, opacity = 1, color = "white", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
        label = ~sprintf("<strong>%s (%s)</strong><br/>%s: %.0f",
                         NAME, input$year_select_map, names(category_choices)[category_choices == input$category_select_map], ScoreToDisplay) %>% lapply(htmltools::HTML),
        layerId = ~NAME
      )
  }, ignoreInit = TRUE)
  
  # Click on map updates state select
  observeEvent(input$state_map_shape_click, {
    updateSelectInput(session, "state_select_map", selected = input$state_map_shape_click$id)
  })
  
  # Button resets to All States
  observeEvent(input$show_all_map_button, {
    updateSelectInput(session, "state_select_map", selected = "All States")
  })
  
  # Table data reactive
  table_data_r <- reactive({
    year_val <- input$year_select_map
    cat_val <- input$category_select_map
    state_val <- input$state_select_map
    
    if (cat_val %in% category_choices[3:length(category_choices)]) {
      # Individual metrics
      value_col_name <- value_col_names[[cat_val]]
      df <- individual %>%
        filter(year == year_val, key_metrics == cat_val) %>%
        select(State = state, Year = year, Value = value, `Average/Expected Value` = exp_value,
               `Relative Score` = relative_score, Rank = rank) %>%
        rename(!!value_col_name := Value)
    } else if (cat_val == "all_rankings") {
      # All Rankings
      df <- rankings %>%
        filter(year == year_val) %>%
        rename(State = state, Year = year, `Overall Rank` = overall_rank,
               `Capital & Bridge Disbursements` = capital_disbursement_perlm_rank,
               `Maintenance Disbursements` = maintenance_disbursement_perlm_rank,
               `Admin Disbursements` = admin_disbursement_perlm_rank,
               `Other Disbursements` = other_disbursement_perlm_rank,
               `Rural Interstate Pavement Condition` = rural_interstate_poor_percent_rank,
               `Urban Interstate Pavement Condition` = urban_interstate_poor_percent_rank,
               `Rural Arterial Pavement Condition` = rural_OPA_poor_percent_rank,
               `Urban Arterial Pavement Condition` = urban_OPA_poor_percent_rank,
               `Urbanized Area Congestion` = state_avg_congestion_hours_rank,
               `Structurally Deficient Bridge` = poor_bridges_percent_rank,
               `Rural Fatality Rate` = rural_fatalities_per_100m_VMT_rank,
               `Urban Fatality Rate` = urban_fatalities_per_100m_VMT_rank,
               `Other Fatality Rate` = other_fatalities_per_100m_VMT_rank)
    } else if (cat_val == "state_mileage") {
      # State Mileage
      df <- mileage %>%
        filter(year == year_val) %>%
        select(State = state, Year = year, `Centerline Mileage` = SHA_miles,
               `Total Lane Miles` = state_tot_lane_miles, `Avg number of lanes` = SHA_ratio)
    }
    
    if (state_val != "All States") {
      df <- df %>% filter(State == state_val)
    }
    
    df
  })
  
  # Dynamic header
  output$map_table_header <- renderUI({
    req(input$year_select_map, input$state_select_map, input$category_select_map)
    cat_name <- names(category_choices)[category_choices == input$category_select_map]
    header_text <- if (input$state_select_map == "All States") {
      paste("State-Level Data for", cat_name, "in", input$year_select_map)
    } else {
      paste("State Details for", input$state_select_map, "in", input$year_select_map, "(", cat_name, ")")
    }
    h3(header_text)
  })
  
  # Render table
  output$map_table <- renderReactable({
    req(table_data_r())
    df <- table_data_r()
    cat_val <- input$category_select_map
    stats <- global_stats()
    cat_name <- names(category_choices)[category_choices == cat_val]
    
    if (cat_val %in% c("capital_disbursement_perlm_score", "maintenance_disbursement_perlm_score",
                       "admin_disbursement_perlm_score", "other_disbursement_perlm_score")) {
      value_col_name <- value_col_names[[cat_val]]
      
      columns_list <- list()
      columns_list[[value_col_name]] <- colDef(
        format = colFormat(prefix = "$", separators = TRUE, digits = 0),
        cell = function(value) {
          label <- scales::dollar(value, accuracy = 1, big.mark = ",")
          bar_cell(label, value, stats$overall_min, stats$overall_max)
        }
      )
      columns_list[["Average/Expected Value"]] <- colDef(
        format = colFormat(prefix = "$", separators = TRUE, digits = 0),
        cell = function(value) {
          label <- scales::dollar(value, accuracy = 1, big.mark = ",")
          bar_cell(label, value, stats$overall_min, stats$overall_max)
        }
      )
      columns_list[["Relative Score"]] <- colDef(
        format = colFormat(digits = 2),
        style = function(value) score_style(value, stats$score_min, stats$score_max)
      )
      columns_list[["Rank"]] <- colDef(
        style = rank_style
      )
      
      reactable(df, filterable = TRUE, highlight = TRUE, defaultPageSize = 10,
                columns = columns_list)
    } else if (cat_val %in% c("rural_interstate_poor_percent_score", "urban_interstate_poor_percent_score",
                              "rural_OPA_poor_percent_score", "urban_OPA_poor_percent_score")) {
      value_col_name <- value_col_names[[cat_val]]
      
      columns_list <- list()
      columns_list[[value_col_name]] <- colDef(
        format = colFormat(suffix = "%", digits = 2),
        cell = function(value) {
          label <- paste0(formatC(value, digits = 2, format = "f"), "%")
          bar_cell(label, value, stats$overall_min, stats$overall_max)
        }
      )
      columns_list[["Average/Expected Value"]] <- colDef(
        format = colFormat(suffix = "%", digits = 2),
        cell = function(value) {
          label <- paste0(formatC(value, digits = 2, format = "f"), "%")
          bar_cell(label, value, stats$overall_min, stats$overall_max)
        }
      )
      columns_list[["Relative Score"]] <- colDef(
        format = colFormat(digits = 2),
        style = function(value) score_style(value, stats$score_min, stats$score_max)
      )
      columns_list[["Rank"]] <- colDef(
        style = rank_style
      )
      
      reactable(df, filterable = TRUE, highlight = TRUE, defaultPageSize = 10,
                columns = columns_list)
    } else if (cat_val == "state_avg_congestion_hours_score") {
      value_col_name <- value_col_names[[cat_val]]
      
      columns_list <- list()
      columns_list[[value_col_name]] <- colDef(
        format = colFormat(digits = 1),
        cell = function(value) {
          label <- formatC(value, digits = 1, format = "f")
          bar_cell(label, value, stats$overall_min, stats$overall_max)
        }
      )
      columns_list[["Average/Expected Value"]] <- colDef(
        format = colFormat(digits = 1),
        cell = function(value) {
          label <- formatC(value, digits = 1, format = "f")
          bar_cell(label, value, stats$overall_min, stats$overall_max)
        }
      )
      columns_list[["Relative Score"]] <- colDef(
        format = colFormat(digits = 2),
        style = function(value) score_style(value, stats$score_min, stats$score_max)
      )
      columns_list[["Rank"]] <- colDef(
        style = rank_style
      )
      
      reactable(df, filterable = TRUE, highlight = TRUE, defaultPageSize = 10,
                columns = columns_list)
    } else if (cat_val == "poor_bridges_percent_score") {
      value_col_name <- value_col_names[[cat_val]]
      
      columns_list <- list()
      columns_list[[value_col_name]] <- colDef(
        format = colFormat(suffix = "%", digits = 2),
        cell = function(value) {
          label <- paste0(formatC(value, digits = 2, format = "f"), "%")
          bar_cell(label, value, stats$overall_min, stats$overall_max)
        }
      )
      columns_list[["Average/Expected Value"]] <- colDef(
        format = colFormat(suffix = "%", digits = 2),
        cell = function(value) {
          label <- paste0(formatC(value, digits = 2, format = "f"), "%")
          bar_cell(label, value, stats$overall_min, stats$overall_max)
        }
      )
      columns_list[["Relative Score"]] <- colDef(
        format = colFormat(digits = 2),
        style = function(value) score_style(value, stats$score_min, stats$score_max)
      )
      columns_list[["Rank"]] <- colDef(
        style = rank_style
      )
      
      reactable(df, filterable = TRUE, highlight = TRUE, defaultPageSize = 10,
                columns = columns_list)
    } else if (cat_val %in% c("rural_fatalities_per_100m_VMT_score", "urban_fatalities_per_100m_VMT_score",
                              "other_fatalities_per_100m_VMT_score")) {
      value_col_name <- value_col_names[[cat_val]]
      
      columns_list <- list()
      columns_list[[value_col_name]] <- colDef(
        format = colFormat(digits = 2),
        cell = function(value) {
          label <- formatC(value, digits = 2, format = "f")
          bar_cell(label, value, stats$overall_min, stats$overall_max)
        }
      )
      columns_list[["Average/Expected Value"]] <- colDef(
        format = colFormat(digits = 2),
        cell = function(value) {
          label <- formatC(value, digits = 2, format = "f")
          bar_cell(label, value, stats$overall_min, stats$overall_max)
        }
      )
      columns_list[["Relative Score"]] <- colDef(
        format = colFormat(digits = 2),
        style = function(value) score_style(value, stats$score_min, stats$score_max)
      )
      columns_list[["Rank"]] <- colDef(
        style = rank_style
      )
      
      reactable(df, filterable = TRUE, highlight = TRUE, defaultPageSize = 10,
                columns = columns_list)
    } else if (cat_val == "all_rankings") {
      # All Rankings - color all rank columns
      col_defs <- list()
      rank_cols <- setdiff(names(df), c("State", "Year"))
      for (col in rank_cols) {
        col_defs[[col]] <- colDef(style = rank_style)
      }
      
      reactable(df, filterable = TRUE, highlight = TRUE, defaultPageSize = 10,
                columns = col_defs)
    } else if (cat_val == "state_mileage") {
      # State Mileage
      reactable(df, filterable = TRUE, highlight = TRUE, defaultPageSize = 10,
                columns = list(
                  `Centerline Mileage` = colDef(
                    format = colFormat(separators = TRUE, digits = 0),
                    cell = function(value, index) {
                      if (df$State[index] == "United States") {
                        scales::comma(value)
                      } else {
                        label <- scales::comma(value)
                        bar_cell(label, value, stats$overall_min, stats$overall_max)
                      }
                    }
                  ),
                  `Total Lane Miles` = colDef(
                    format = colFormat(separators = TRUE, digits = 0),
                    cell = function(value, index) {
                      if (df$State[index] == "United States") {
                        scales::comma(value)
                      } else {
                        label <- scales::comma(value)
                        bar_cell(label, value, stats$overall_min, stats$overall_max)
                      }
                    }
                  ),
                  `Avg number of lanes` = colDef(
                    format = colFormat(digits = 2),
                    cell = function(value, index) {
                      if (df$State[index] == "United States") {
                        formatC(value, digits = 2, format = "f")
                      } else {
                        label <- formatC(value, digits = 2, format = "f")
                        bar_cell(label, value, stats$ratio_min, stats$ratio_max)
                      }
                    }
                  )
                ))
    }
  })
  
  # Download handler
  output$download_table <- downloadHandler(
    filename = function() {
      cat_name <- names(category_choices)[category_choices == input$category_select_map]
      state_name <- ifelse(input$state_select_map == "All States", "All_States", input$state_select_map)
      paste0("AHR_Table_", cat_name, "_", input$year_select_map, "_", state_name, ".csv")
    },
    content = function(file) {
      write.csv(table_data_r(), file, row.names = FALSE)
    }
  )
  
  # Disbursement illustration reactive
  disp_df_r <- reactive({
    selected <- input$disp_category
    state_val <- input$disp_state_select
    df <- disbursement_data %>%
      filter(key_metrics == selected) %>%
      select(State = state, `Disbursement Per Lane-Mile` = value, `Expected Value` = fitted_loess, `National Average Value` = national_avg,
             `Relative Score Based on Expected Value` = relative_loess, `Relative Score Based on National Avg` = relative_avg,
             `Rank Based on Expected Value` = rank_loess, `Rank Based on National Avg` = rank_avg)
    if (state_val != "All States") {
      df <- df %>% filter(State == state_val)
    }
    df
  })
  
  # Disbursement plot data
  disp_plot_df_r <- reactive({
    selected <- input$disp_category
    state_val <- input$disp_state_select
    df <- disbursement_data %>%
      filter(key_metrics == selected) %>%
      arrange(pct_urban_lane_miles)  # Sort for consistent indexing
    df$selected <- ifelse(df$state == state_val & state_val != "All States", "Selected", "Other")
    df
  })
  
  # Disbursement plot
  output$disp_plot <- renderPlotly({
    df <- disp_plot_df_r()
    national_avg <- unique(df$national_avg)
    
    plot_ly(source = "disp_plot_source") %>%
      add_trace(data = df, x = ~pct_urban_lane_miles, y = ~value, type = 'scatter', mode = 'markers',
                marker = list(color = ~ifelse(selected == "Selected", "red", "black"),
                              size = ~ifelse(selected == "Selected", 15, 10)),
                text = ~paste("State: ", state, "<br>Urban %: ", paste0(round(pct_urban_lane_miles * 100), "%"), "<br>Spending: ", scales::dollar(value),
                              "<br>LOESS Score: ", round(relative_loess, 2), "<br>Avg Score: ", round(relative_avg, 2)),
                hoverinfo = 'text', showlegend = FALSE) %>%
      add_trace(data = df, x = ~pct_urban_lane_miles, y = ~fitted_loess, type = 'scatter', mode = 'lines',
                line = list(color = "blue"), name = "Expected",
                hovertemplate = "Urban %: %{x:.0%}<br>Expected Spending: %{y:$,.0f}") %>%
      add_trace(data = df, x = ~pct_urban_lane_miles, y = national_avg, type = 'scatter', mode = 'lines',
                line = list(color = "red"), name = "National Average",
                hovertemplate = "Urban %: %{x:.0%}<br>National Average: %{y:$,.0f}") %>%
      layout(title = paste0(names(disp_category_choices)[disp_category_choices == input$disp_category], " (2023)"),
             xaxis = list(title = "Percent Urban Lane Miles"),
             yaxis = list(title = "Disbursement Per Lane Mile"),
             showlegend = TRUE) %>%
      event_register('plotly_click')
  })
  
  # Update state select on plot click
  observeEvent(event_data("plotly_click", source = "disp_plot_source"), {
    click_data <- event_data("plotly_click", source = "disp_plot_source")
    if (!is.null(click_data)) {
      # Since curveNumber 0 is points, 1 is LOESS, 2 is avg
      if (click_data$curveNumber == 0) {
        selected_state <- disp_plot_df_r()$state[click_data$pointNumber + 1]
        updateSelectInput(session, "disp_state_select", selected = selected_state)
      }
    }
  })
  
  # Show all button for disbursement
  observeEvent(input$disp_show_all_button, {
    updateSelectInput(session, "disp_state_select", selected = "All States")
  })
  
  # Disbursement table
  output$disp_table <- renderReactable({
    df <- disp_df_r()
    selected_category <- input$disp_category
    full_df <- disbursement_data %>% filter(key_metrics == selected_category)
    overall_min <- min(c(full_df$value, full_df$fitted_loess, full_df$national_avg), na.rm = TRUE)
    overall_max <- max(c(full_df$value, full_df$fitted_loess, full_df$national_avg), na.rm = TRUE)
    score_min <- min(c(full_df$relative_loess, full_df$relative_avg), na.rm = TRUE)
    score_max <- max(c(full_df$relative_loess, full_df$relative_avg), na.rm = TRUE)
    
    reactable(df, filterable = TRUE, highlight = TRUE, defaultPageSize = 10,
              columns = list(
                `Disbursement Per Lane-Mile` = colDef(
                  format = colFormat(prefix = "$", separators = TRUE, digits = 0),
                  cell = function(value) {
                    label <- scales::dollar(value, accuracy = 1, big.mark = ",")
                    bar_cell(label, value, overall_min, overall_max)
                  }
                ),
                `Expected Value` = colDef(
                  format = colFormat(prefix = "$", separators = TRUE, digits = 0),
                  cell = function(value) {
                    label <- scales::dollar(value, accuracy = 1, big.mark = ",")
                    bar_cell(label, value, overall_min, overall_max)
                  }
                ),
                `National Average Value` = colDef(
                  format = colFormat(prefix = "$", separators = TRUE, digits = 0),
                  cell = function(value) {
                    label <- scales::dollar(value, accuracy = 1, big.mark = ",")
                    bar_cell(label, value, overall_min, overall_max)
                  }
                ),
                `Relative Score Based on Expected Value` = colDef(
                  format = colFormat(digits = 2),
                  style = function(value) score_style(value, score_min, score_max)
                ),
                `Relative Score Based on National Avg` = colDef(
                  format = colFormat(digits = 2),
                  style = function(value) score_style(value, score_min, score_max)
                ),
                `Rank Based on Expected Value` = colDef(
                  style = rank_style
                ),
                `Rank Based on National Avg` = colDef(
                  style = rank_style
                )
              ))
  })
}

# Run app
shinyApp(ui = ui, server = server)