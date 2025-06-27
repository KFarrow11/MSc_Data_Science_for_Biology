library(tidyverse)
library(plotly)
library(readr)
library(readxl)
library(DT)
library(maps)
library(sf)
library(htmlwidgets)
library(leaflet)
library(scales)
library(RColorBrewer)
library(crosstalk)

# USA DATA ----
usa_cases <- read_xlsx("data/measles_usa/new-cases-of-measles-in-the-us-1985-2025.xlsx") # bar/line chart
usa_age_24 <- read_xlsx("data/measles_usa/cases_by_age_2024.xlsx") # bar chart
usa_age_25 <- read_xlsx("data/measles_usa/cases_by_age_2025.xlsx") # bar chart

# USA States latitude and longitude coordinates
# Source: [Geodatos](https://www.geodatos.net/en/coordinates/united-states), [LatLong.net](https://www.latlong.net/category/states-236-14.html), [Where Am I](https://where-am-i.org/district-of-columbia-latitude.php)
state_coords <- tibble(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
            "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
            "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
            "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
            "New Hampshire", "New Jersey", "New Mexico", "New York", "New York City", "North Carolina",
            "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
            "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  lat = c(32.3182, 66.1605, 34.0489, 34.7999, 36.7783, 39.1130, 41.5999, 39.0000, 38.8950, 27.9944,
          33.2479, 19.7418, 44.0682, 40.0000, 40.2735, 42.0329, 38.5000, 37.8393, 30.3918, 45.3676,
          39.0458, 42.4072, 44.1822, 46.3924, 33.0000, 38.5739, 46.9653, 41.5000, 39.8760, 44.0000,
          39.8339, 34.3071, 43.0000, 40.7143, 35.7822, 47.6506, 40.3675, 36.0846, 44.0000, 41.2033,
          41.7423, 33.8361, 44.5000, 35.8601, 31.0000, 39.4192, 44.0000, 37.9269, 47.7511, 39.0000,
          44.5000, 43.0759),
  lon = c(-86.9023, -153.3691, -111.0937, -92.1999, -119.4179, -105.3589, -72.6999, -75.5000, -77.03667,
          -81.7603, -83.4412, -155.8444, -114.7420, -89.0000, -86.1269, -93.5815, -98.0000, -84.2700,
          -92.3291, -68.9722, -76.6413, -71.3824, -84.5068, -94.6362, -90.0000, -92.6038, -109.5337,
          -100.0000, -117.2241, -71.5000, -74.8718, -106.0181, -75.0000, -74.0060, -80.7935, -100.4370,
          -82.9962, -96.9214, -120.5000, -77.1945, -71.7423, -81.1637, -100.0000, -86.6602, -100.0000,
          -111.9507, -72.6999, -78.0249, -120.7401, -80.5000, -89.5000, -107.2903))

usa_state_24 <- read_xlsx("data/measles_usa/cases_state_2024.xlsx") %>%
  left_join(state_coords, by = "state")
# map + table of top 10

usa_state_25 <- read_xlsx("data/measles_usa/cases_state_2025.xlsx") %>%
  left_join(state_coords, by = "state")
# map + table of top 10

usa_vac_20_25 <- read_xlsx("data/measles_usa/vac_20_25.xlsx") %>%
  mutate(cases = as.integer(cases))  # Convert cases to whole numbers
# stacked bar chart

usa_autism <- read_xlsx("data/measles_usa/us-adults-who-believed-vaccines-cause-autism-2015-2024.xlsx") 
# show this as a pie chart/ stacked bar chart = toggle between

# Calculate key metrics for USA dashboard
total_usa_cases <- sum(usa_cases$cases, na.rm = TRUE)

# Year-over-year change (2025 vs 2024)
cases_2025 <- usa_cases$cases[usa_cases$year == "2025"]
cases_2024 <- usa_cases$cases[usa_cases$year == "2024"]
cases_pct_change <- round(((cases_2025 - cases_2024) / cases_2024) * 100, 1)

# Find peak year
peak_year_data <- usa_cases[which.max(usa_cases$cases), ]

# Find highest state in 2025
highest_state_2025 <- usa_state_25[which.max(usa_state_25$cases), ]

# Most affected age group (combining 2024 and 2025)
combined_age_data <- bind_rows(
  usa_age_24 %>% select(age_group = `Age Group`, cases = Count),
  usa_age_25 %>% select(age_group, cases)
)

age_totals <- combined_age_data %>%
  group_by(age_group) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  arrange(desc(total_cases))
highest_age_group <- age_totals[1, ]

# Children under 5 percentage (from recent age data)
children_under_5_cases <- combined_age_data %>%
  filter(str_detect(age_group, "Under 5|0-|less than 1|1 to 4")) %>%
  summarise(total = sum(cases, na.rm = TRUE)) %>%
  pull(total)

total_age_cases <- sum(combined_age_data$cases, na.rm = TRUE)
children_under_5_pct <- round((children_under_5_cases / total_age_cases) * 100)

# Average annual cases
avg_annual_cases <- round(total_usa_cases / nrow(usa_cases))

# Unvaccinated percentage in 2025
vac_2025 <- usa_vac_20_25 %>% filter(year == "2025")
total_vac_2025 <- sum(vac_2025$cases, na.rm = TRUE)
unvaccinated_2025 <- vac_2025 %>%
  filter(str_detect(vaccinine_status, "Unvaccinated")) %>%
  summarise(total = sum(cases, na.rm = TRUE)) %>%
  pull(total)
unvaccinated_pct <- round((unvaccinated_2025 / total_vac_2025) * 100)

## USA ROW 2 - STATES {height="1000px"}
### USA cases by Geographic Distribution

# First, create state abbreviation lookup and add to data
state_abbrev <- tibble(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
            "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
            "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
            "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
            "New Hampshire", "New Jersey", "New Mexico", "New York", "New York City", "North Carolina",
            "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
            "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL",
                   "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV",
                   "NH", "NJ", "NM", "NY", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
                   "UT", "VT", "VA", "WA", "WV", "WI", "WY")
) %>%
  rename(state_code = abbreviation)

usa_state_24 <- read_xlsx("data/measles_usa/cases_state_2024.xlsx") %>% 
  left_join(state_abbrev, by = "state") %>%
  mutate(year = 2024)

usa_state_25 <- read_xlsx("data/measles_usa/cases_state_2025.xlsx") %>% 
  left_join(state_abbrev, by = "state") %>%
  mutate(year = 2025)

# Combine datasets
combined_data <- bind_rows(usa_state_24, usa_state_25)

# Create color palettes for each year
usa_2024 <- "#0000cd"
usa_2025 <- "#ff4500"

## bar and line charts ----
# Create bar chart with dropdown toggle - Enhanced version
usa_24_25_states_bar_chart <- plot_ly() %>%
  add_bars(
    data = usa_state_24,
    x = ~reorder(state, -cases),
    y = ~cases,
    marker = list(color = usa_2024),
    name = "2024",
    visible = TRUE,
    hovertemplate = paste(
      "<b>%{x}</b><br>",
      "Cases: %{y}<br>",
      "Year: 2024",
      "<extra></extra>"
    )
  ) %>%
  add_bars(
    data = usa_state_25,
    x = ~reorder(state, -cases),
    y = ~cases,
    marker = list(color = usa_2025),
    name = "2025",
    visible = FALSE,
    hovertemplate = paste(
      "<b>%{x}</b><br>",
      "Cases: %{y}<br>",
      "Year: 2025",
      "<extra></extra>"
    )
  ) %>%
  layout(
    xaxis = list(
      title = "State",
      tickangle = -45,
      tickfont = list(size = 14)
    ),
    yaxis = list(
      title = "Number of Cases",
      tickfont = list(size = 14)
    ),
    updatemenus = list(
      list(
        type = "dropdown",
        direction = "down",
        showactive = TRUE,
        x = 0,
        y = 1.1,
        xanchor = "left",
        yanchor = "top",
        bgcolor = "white",
        bordercolor = "gray",
        borderwidth = 1,
        font = list(size = 18),
        buttons = list(
          list(
            label = "2024",
            method = "update",
            args = list(
              list(visible = c(TRUE, FALSE)),
              list()
            )
          ),
          list(
            label = "2025",
            method = "update",
            args = list(
              list(visible = c(FALSE, TRUE)),
              list()
            )
          )
        )
      )
    ),
    margin = list(l = 80, r = 50, t = 120, b = 120),
    showlegend = FALSE
  ) %>%
  config(
    displayModeBar = FALSE
  )

# Display the bar chart
usa_24_25_states_bar_chart


# USA Maps ----
state_abbrev <- tibble(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
            "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
            "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
            "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
            "New Hampshire", "New Jersey", "New Mexico", "New York", "New York City", "North Carolina",
            "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
            "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL",
                   "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV",
                   "NH", "NJ", "NM", "NY", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
                   "UT", "VT", "VA", "WA", "WV", "WI", "WY")
) %>%
  rename(state_code = abbreviation)

# Add state codes to your data
usa_state_24 <- read_xlsx("data/measles_usa/cases_state_2024.xlsx") %>% 
  left_join(state_abbrev, by = "state") %>%
  left_join(state_coords, by = "state")

usa_state_25 <- read_xlsx("data/measles_usa/cases_state_2025.xlsx") %>% 
  left_join(state_abbrev, by = "state") %>%
  left_join(state_coords, by = "state")

# Custom Color Gradient for US Measles Cases
us_cases_colors <- list(
  c(0, "#f7fcf5"),        # Pale green (almost white-green)
  c(0.25, "#c7e9c0"),     # Light natural green
  c(0.5, "#74c476"),      # Medium natural green
  c(0.75, "#31a354"),     # Darker natural green
  c(1.0, "#006d2c")       # Deep forest green
)

# Create 2024 Map
us_map_2024 <- plot_geo(usa_state_24, locationmode = "USA-states") %>%
  add_trace(
    locations = ~state_code,  # Keep state codes for correct mapping
    z = ~cases,
    zmin = 0,
    zmax = 100,
    color = ~cases,
    colorscale = us_cases_colors,
    hoverinfo = "text",
    hovertemplate = paste(
      "<b>%{text}</b><br>",
      "Cases: %{z}<br>",
      "Year: %{customdata}",
      "<extra></extra>"
    ),
    text = ~state,  # Ensure state names appear in hover
    customdata = ~year,  # Dynamically show the year
    marker = list(
      line = list(color = "black", width = 0.8)
    ),
    colorbar = list(
      title = list(text = "Measles Cases", side = "bottom"),
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1,
      len = 0.8,
      thickness = 15
    )
  ) %>%
  layout(
    geo = list(
      scope = "usa",
      showframe = FALSE,
      showcoastlines = TRUE,
      showland = TRUE,
      landcolor = "white",
      countrycolor = "lightgray",
      projection = list(type = "albers usa"),
      bgcolor = "rgba(0,0,0,0)"
    ),
    margin = list(l = 20, r = 20, t = 20, b = 60)
  ) %>%
  config(displayModeBar = FALSE)

# Create 2025 Map
us_map_2025 <- plot_geo(usa_state_25, locationmode = "USA-states") %>%
  add_trace(
    locations = ~state_code,  
    z = ~cases,
    zmin = 0,
    zmax = 110,
    color = ~cases,
    colorscale = us_cases_colors,
    hoverinfo = "text",
    hovertemplate = paste(
      "<b>%{text}</b><br>",
      "Cases: %{z}<br>",
      "Year: %{customdata}",
      "<extra></extra>"
    ),
    text = ~state,  # Show state names when hovering
    customdata = ~year,
    marker = list(
      line = list(color = "black", width = 1)
    ),
    colorbar = list(
      title = list(text = "Measles Cases", side = "bottom"),
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1,
      len = 0.8,
      thickness = 15,
      tick0 = 0,
      dtick = 20,
      tickvals = c(0, 20, 40, 60, 80, 100),
      ticktext = c("0", "20", "40", "60", "80", "100+")
    )
  ) %>%
  layout(
    geo = list(
      scope = "usa",
      showframe = FALSE,
      showcoastlines = TRUE,
      showland = TRUE,
      landcolor = "white",
      countrycolor = "lightgray",
      projection = list(type = "albers usa"),
      bgcolor = "rgba(0,0,0,0)"
    ),
    margin = list(l = 20, r = 20, t = 20, b = 60)
  ) %>%
  config(displayModeBar = FALSE)

us_map_2024
us_map_2025
           
## USA ROW 3 - Vac boxes {height="250px"}
usa_vac_20_25 <- read_xlsx("data/measles_usa/vac_20_25.xlsx") %>%
  mutate(cases = as.integer(cases))  # Convert cases to whole numbers
usa_autism <- read_xlsx("data/measles_usa/us-adults-who-believed-vaccines-cause-autism-2015-2024.xlsx") 
# Vaccination effectiveness metrics
total_unvaccinated <- sum(usa_vac_20_25$cases[usa_vac_20_25$vaccinine_status == "Unvaccinated or Unknown"], na.rm = TRUE)
total_vac_cases <- sum(usa_vac_20_25$cases, na.rm = TRUE)
unvaccinated_pct <- (total_unvaccinated / total_vac_cases) * 100
# Two-dose vaccination cases
total_two_dose <- sum(usa_vac_20_25$cases[usa_vac_20_25$vaccinine_status == "Two MMR doses"], na.rm = TRUE)
two_dose_pct <- (total_two_dose / total_vac_cases) * 100
vaccine_effectiveness <- 100 - two_dose_pct
# Autism belief trends
autism_2015 <- usa_autism %>% filter(year == 2015)
autism_2024 <- usa_autism %>% filter(year == 2024)
# Calculate belief percentages
yes_cause_2015 <- autism_2015$distribution[autism_2015$autism == "Yes, a cause"] / sum(autism_2015$distribution) * 100
yes_cause_2024 <- autism_2024$distribution[autism_2024$autism == "Yes, a cause"] / sum(autism_2024$distribution) * 100
belief_change <- yes_cause_2024 - yes_cause_2015
# Unsure percentage in 2024
unsure_2024 <- autism_2024$distribution[autism_2024$autism == "Unsure"] / sum(autism_2024$distribution) * 100

## USA Row 4 - Vaccination donuts {height="1000px"}

#### USA Measles Cases by Vaccination Status 2020
# Calculate percentages
usa_vac_percentage <- usa_vac_20_25 %>%
  group_by(year) %>%
  mutate(total_cases = sum(cases),
         percentage = round((cases / total_cases) * 100, 0)) %>%
  ungroup()

# Define colors for consistency
colors <- c("Unvaccinated or Unknown" = "#738",
 "One MMR dose" = "#0A7029", 
 "Two MMR doses" = "#52688F")

# Modified Donut Chart Function
create_donut_chart <- function(year_data, year_title) {
  # Filter out zero values for cleaner visualization
  year_data_filtered <- year_data %>% filter(cases > 0)
  
  if (nrow(year_data_filtered) == 0) {
    return(NULL)
  }
  
  plot_ly(
    data = year_data_filtered,
    labels = ~vaccinine_status,  # Use actual column name from dataset
    values = ~cases,
    type = 'pie',
    hole = 0.5,
    textinfo = 'value+percent',  # Show both cases and percentages
    textposition = 'inside',  # Place text inside the donut segments
    textfont = list(size = 16, color = "white"),  # White text for better contrast
    texttemplate = '%{value}<br>(%{percent})',  # Format: cases on top, percentage below in parentheses
    hovertemplate = paste(
      '<b>%{label}</b><br>',
      'Cases: %{value}<br>',
      'Percentage: %{percent}<br>',
      '<extra></extra>'
    ),
    marker = list(
      colors = colors[year_data_filtered$vaccinine_status],
      line = list(color = 'white', width = 1)
    ),
    showlegend = TRUE,
    domain = list(x = c(0, 1), y = c(0, 0.85))
  ) %>%
    layout(
      font = list(family = "Arial, sans-serif", size = 12),
      margin = list(t = 100, b = 40, l = 40, r = 40),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1,
        xanchor = "center",
        yanchor = "bottom",
        font = list(size = 16) # Change this to 16
      ),
      annotations = list(
        list(
          text = paste0("<b>", year_title, "</b>"),
          x = 0.5, y = 0.5,
          font = list(size = 20, color = "black"),
          showarrow = FALSE
        ),
        list(
          text = paste0("Total Cases<br><b>", sum(year_data$cases), "</b>"),
          x = 0.5, y = 0.4,
          font = list(size = 20, color = "black"),
          showarrow = FALSE))
    ) %>%
    config(displayModeBar = FALSE)
}

# Alternative function with better text positioning for small segments
create_donut_chart_smart <- function(year_data, year_title) {
  year_data_filtered <- year_data %>% filter(cases > 0)
  
  if (nrow(year_data_filtered) == 0) {
    return(NULL)
  }
  
  # Determine text position based on percentage size
  year_data_filtered <- year_data_filtered %>%
    mutate(
      text_position = ifelse(percentage >= 15, "inside", "outside"),
      text_info = ifelse(percentage >= 15, "value+percent", "label+value+percent")
    )
  
  plot_ly() %>%
    add_pie(
      data = year_data_filtered,
      labels = ~vaccinine_status,
      values = ~cases,
      hole = 0.4,
      textinfo = 'value+percent',
      textposition = ~text_position,
      texttemplate = '%{value}<br>(%{percent})',  # Format: cases on top, percentage in parentheses
      textfont = list(size = 14),
      hovertemplate = paste(
        '<b>%{label}</b><br>',
        'Cases: %{value}<br>',
        'Percentage: %{percent}<br>',
        '<extra></extra>'
      ),
      marker = list(
        colors = colors[year_data_filtered$vaccinine_status],
        line = list(color = 'white', width = 1)
      ),
      showlegend = TRUE,
      domain = list(x = c(0, 1), y = c(0, 0.85))
    ) %>%
    layout(
      font = list(family = "Arial, sans-serif", size = 12),
      margin = list(t = 100, b = 40, l = 40, r = 40),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1,
        xanchor = "center",
        yanchor = "bottom",
        font = list(size = 16) # Change this to 16
      ),
      annotations = list(
        list(
          text = paste0("<b>", year_title, "</b>"),
          x = 0.5, y = 0.5,
          font = list(size = 20, color = "black"),
          showarrow = FALSE
        ),
        list(
          text = paste0("Total Cases<br><b>", sum(year_data$cases), "</b>"),
          x = 0.5, y = 0.4,
          font = list(size = 20, color = "black"),
          showarrow = FALSE
        )
      )
    ) %>%
    config(displayModeBar = FALSE)
}

# Create individual donut charts for each year
years <- unique(usa_vac_percentage$year)
donut_plots <- list()

for (year in years) {
  year_data <- usa_vac_percentage %>% filter(year == !!year)
  donut_plots[[as.character(year)]] <- create_donut_chart_smart(year_data, year)
}

donut_plots[["2020"]]

#### 2021
donut_plots[["2021"]]

#### 2022
donut_plots[["2022"]]

#### 2023
donut_plots[["2023"]]

#### 2024
donut_plots[["2024"]]

#### 2025
donut_plots[["2025"]]


### USA Beliefs About Vaccines Causing Autism {.tabset}
#### USA Beliefs About Vaccines Causing Autism 2015

# Define colors for beliefs
autism_colors <- c("Yes, a cause" = "#d62728",  # Red
        "No, not a cause" = "#2ca02c",  # Green
        "Unsure" = "#ff7f0e")  # Orange

# Function to create donut chart
create_autism_donut_chart <- function(year_data, year_title) {
  # Filter out zero values for cleaner visualization
  year_data_filtered <- year_data %>% filter(distribution > 0)
  
  if (nrow(year_data_filtered) == 0) {
    return(NULL)
  }
  
  plot_ly(
    data = year_data_filtered,
    labels = ~autism,
    values = ~distribution,
    type = 'pie',
    hole = 0.5,
    textinfo = 'value+percent',
    textposition = 'inside',
    textfont = list(size = 16, color = "white"),
    texttemplate = '%{value}<br>(%{percent})',
    hovertemplate = paste(
      '<b>%{label}</b><br>',
      'Responses: %{value}<br>',
      'Percentage: %{percent}<br>',
      '<extra></extra>'
    ),
    marker = list(
      colors = autism_colors[year_data_filtered$autism],
      line = list(color = 'white', width = 1)
    ),
    showlegend = TRUE,
    domain = list(x = c(0, 1), y = c(0, 0.85))
  ) %>%
    layout(
      font = list(family = "Arial, sans-serif", size = 12),
      margin = list(t = 100, b = 40, l = 40, r = 40),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1,
        xanchor = "center",
        font = list(size = 16)
      ),
      annotations = list(
        list(
          text = paste0("<b>", year_title, "</b>"),
          x = 0.5, y = 0.5,
          font = list(size = 20, color = "black"),
          showarrow = FALSE
        ),
        list(
          text = paste0("Total Responses<br><b>", sum(year_data$distribution), "</b>"),
          x = 0.5, y = 0.4,
          font = list(size = 20, color = "black"),
          showarrow = FALSE
        )
      )
    ) %>%
    config(displayModeBar = FALSE)
}

# Generate donut charts for each year
autism_years <- unique(usa_autism$year)
donut_plots_autism <- list()

for (year in autism_years) {
  year_data <- usa_autism %>% filter(year == !!year)
  donut_plots_autism[[as.character(year)]] <- create_autism_donut_chart(year_data, year)
}

donut_plots_autism[["2015"]]

#### 2019
donut_plots_autism[["2019"]]

#### 2024
donut_plots_autism[["2024"]]


## USA Row 5 - Vaccination Summary {height="800px"}
### usa final row col 1
timeline_data <- usa_vac_20_25 %>%
  mutate(year_num = as.numeric(year)) %>%
  arrange(year_num, vaccinine_status)

timeline_bar <- timeline_data %>%
  plot_ly(
    x = ~vaccinine_status,
    y = ~cases,
    color = ~vaccinine_status,
    colors = colors,
    type = 'bar',
    frame = ~year,
    text = ~cases,
    textposition = 'outside',
    hovertemplate = paste(
      '<b>%{x}</b><br>',
      'Year: %{frame}<br>',
      'Cases: %{y}<br>',
      '<extra></extra>'
    )
  ) %>%
  layout(
    xaxis = list(
      title = "<b>Vaccination Status</b>",
      font = list(size = 16),
      tickfont = list(size = 14)  # Increase axis value text size
    ),
    yaxis = list(
      title = "<b>Number of Cases</b>",
      range = c(0, max(timeline_data$cases) + 15),
      font = list(size = 16),
      tickfont = list(size = 14)  # Increase axis value text size
    ),
    showlegend = FALSE
  ) %>%
  animation_opts(
    frame = 400,
    transition = 300,
    easing = "cubic-in-out",
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Year: ",
      font = list(size = 16, color = "#333")
    ),
    len = 0.8,
    x = 0.1
  ) %>%
  animation_button(
    x = 0.1, y = 0,
    bgcolor = "white",
    bordercolor = "black",
    font = list(color = "black")
  ) %>%
  config(
    displayModeBar = FALSE,  # Completely hides the mode bar
    staticPlot = TRUE        # Disables all interactive features
  )

timeline_bar


### usa final row col 2 {width="40%"}
summary_year <- usa_vac_20_25%>%
  rename(Year = year, 
         `Vaccination Status` = vaccinine_status, 
         Cases = cases)

us_cases_colors2 <- list("#f7fcf5", # Pale green (almost white-green) 
                         "#c7e9c0", # Light natural green 
                         "#74c476" # Medium natural green 
)

# Define breakpoints for the color intervals
breaks <- quantile(summary_year$Cases, probs = seq(0, 1, length.out = 3), na.rm = TRUE)

# Create the interactive table with updated column names and color styling
final_table <- datatable(summary_year, 
                         rownames = FALSE,
                         class = "compact stripe hover order-column",
                         options = list(
                           pageLength = 18,
                           lengthMenu = list(c(9, 12, 16, 18), c("9", "12", "16", "All")),
                           autoWidth = FALSE,
                           columnDefs = list(
                             list(targets = "_all", className = "dt-left")
                           )
                         ),
                         filter = "top"
) %>%
  formatStyle(
    names(summary_year),
    fontFamily = "Arial",
    fontSize = "14px",
    textAlign = "left"
  ) %>%
  formatStyle(
    "Cases",
    backgroundColor = styleInterval(breaks[-1], us_cases_colors2),
    fontWeight = "bold"
  )

# Display the updated table
final_table


## USA ROW 6 - SOURCES {height="300px"}
### 🇺🇸 United States Data Sources

#**USA State Codes**
  
#-   **Source**: US State Abbreviations

#-   **URL**: https://www.50states.com/abbreviations.htm

#-   **Usage**: Standardised state coding for mapping and data processing

#**USA Historical Cases Trend**

#-   **File**: `new-cases-of-measles-in-the-us-1985-2025.xlsx`

#-   **Source**: [New cases of measles in the US since 1950](https://www.statista.com/statistics/186678/new-cases-of-measles-in-the-us-since-1950/)

#-   **Institution**: Statista (compiled from CDC data)

#-   **Coverage**: Long-term trend analysis from 1985-2025

#-   **Purpose**: Historical context and outbreak pattern analysis

#**USA Cases by Age Group (2024)**
  
#-   **File**: `cases_by_age_2024.xlsx`

#-   **Source**: [Number of measles cases by age U.S. 2020-2024](https://www.statista.com/statistics/1469710/number-measles-cases-in-the-us-by-age/)

#-   **Institution**: Statista (CDC surveillance data)

#-   **Coverage**: Age-stratified case distribution for 2024

#-   **Purpose**: Identifying vulnerable age groups

#**USA Cases by Age Group (2025)**
  
#-   **File**: `cases_by_age_2025.xlsx`

#-   **Source**: [Number of measles cases by age U.S. 2024-2025](https://www.statista.com/statistics/1560807/number-measles-cases-by-age/)

#-   **Institution**: Statista (CDC surveillance data)

#-   **Coverage**: Current age-stratified case distribution

#-   **Purpose**: Real-time epidemiological monitoring

#**USA Cases by State (2024)**/
  
#-   **File**: `cases_state_2024.xlsx`

#-   **Source**: Number of measles by state U.S. 2024

#-   **Institution**: Statista (CDC surveillance data)

#-   **Coverage**: State-level case distribution for 2024

#-   **Purpose**: Geographic outbreak mapping

#**USA Cases by State (2025)**
  
#-   **File**: `cases_state_2025.xlsx`

#-   **Source**: Number of measles by state U.S. 2025

#-   **Institution**: Statista (CDC surveillance data)

#-   **Coverage**: Current state-level case distribution

#-   **Purpose**: Real-time outbreak monitoring

#**USA Vaccination Status Analysis**
  
#-   **Files**:
  
#-   `vac_2024.xlsx` - Number of measles cases by vaccination status U.S. 2020-2024

#-   `vac_2025.xlsx` - Distribution of measles cases by vaccination status U.S. 2024-2025

#-   `vac_20_25.xlsx` - Combined vaccination status data 2020-2025

#-   **Institution**: Statista (CDC surveillance data)

#-   **Coverage**: Vaccination status of confirmed measles cases

#-   **Purpose**: Vaccine effectiveness monitoring and breakthrough case analysis

#**US Public Perception Survey**
  
# -   **File**: `us-adults-who-believed-vaccines-cause-autism-2015-2024.xlsx`

#-   **Source**: Opinions on whether vaccines cause autism U.S. 2024

#-   **Institution**: Statista (survey research)

#-   **Coverage**: Public beliefs about MMR-autism connection (2015-2024)

#-   **Purpose**: Understanding vaccine hesitancy drivers
                                 
