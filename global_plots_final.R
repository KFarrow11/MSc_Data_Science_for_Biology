library(tidyverse)
library(plotly)
library(readr)
library(readxl)
library(DT)
library(maps)
library(sf)
library(htmlwidgets)
library(leaflet)

## GLOBAL DATA ----
### Country coordinates from the maps package ----
country_coords <- world.cities %>%
  group_by(country.etc) %>%
  summarise(
    lat = mean(lat, na.rm = TRUE),
    lng = mean(long, na.rm = TRUE)
  ) %>%
  rename(country = country.etc)

### Global cases and deaths ----
# 🚀 Load & Process Global Measles Data (1980–2021)
global_cases_deaths_summary <- read_csv("data/measles_global/global_cases_deaths.csv") %>%
  select(measure, location, year, val) %>%  # ✅ Keep only relevant columns
  filter(measure %in% c("Incidence", "Deaths")) %>%  # ✅ Focus only on cases & deaths
  group_by(location, year, measure) %>%
  summarise(Value = round(sum(val, na.rm = TRUE), 0), .groups = "drop") %>%  # ✅ Round values for clean display
  pivot_wider(names_from = measure, values_from = Value, values_fill = 0) %>%  # ✅ Separate cases & deaths
  rename(cases = Incidence, deaths = Deaths) %>%  # ✅ Rename for clarity
  left_join(country_coords, by = c("location" = "country")) %>%  # ✅ Merge with country coordinates
  filter(year >= 1980 & year <= 2021) %>%  # ✅ Ensure only years within 1980-2021 are included
  group_by(location, year) %>%
  summarise(
    total_cases = sum(cases, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE),
    .groups = "drop")

### Global cases by sex and age ----
global_sex_age_grouped <- read_csv("data/measles_global/global_cases_sex_age.csv") %>%
  select(location, sex, age, year, val) %>%  # ✅ Removed 'measure' column
  filter(sex %in% c("Male", "Female")) %>%
  mutate(
    age_group = case_when(
      age == "<1 year" ~ "<1",
      age == "2-4 years" ~ "2-4",
      age %in% c("5-14 years", "15-19 years") ~ "5-19",
      age %in% c("20-24 years", "25-29 years") ~ "20-29",
      age %in% c("30-34 years", "35-39 years") ~ "30-39",
      age %in% c("40-44 years", "45-49 years") ~ "40-49",
      age %in% c("50-54 years", "55-59 years") ~ "50-59",
      age %in% c("60-64 years", "65-69 years") ~ "60-69",
      age == "70+ years" ~ "70+",
      TRUE ~ "no_data"  # ✅ Explicitly marking missing age group as "no_data"
    )
  ) %>%
  group_by(location, year, sex, age_group) %>%
  summarise(cases = round(sum(val, na.rm = TRUE), 0), .groups = "drop") %>%
  left_join(country_coords, by = c("location" = "country")) %>%
  filter(year >= 1980 & year <= 2021) %>%
  mutate(
    lat = replace_na(lat, 0),
    lng = replace_na(lng, 0)
  )

# CREATE CONTINENT MAPPING AND VARIABLES NEEDED FOR VALUE BOXES ----
# Define color palette for continents
colors_vector <- c(
  "Africa" = "#65463E",   # Carafe, representing the rich wildlife and savannah
  "Asia" = "#0A7029",     # Green, symbolizing lush forests and growth
  "Europe" = "#2535D9",   # Royal Blue, echoing tradition and stability
  "Americas" = "#B22222",  # Firebrick Red, conveying energy and history
  "Oceania" = "#C08D2C"   # Desert Sun, reflecting arid landscapes and warmth
)

# Updated continent mapping
continent_mapping <- data.frame(
  location = c("Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", "Australia", 
               "Austria", "Azerbaijan", "Bahrain", "Bangladesh", "Belarus", "Belgium", "Benin", 
               "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Bulgaria", "Burkina Faso",
               "Burundi", "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", 
               "Chile", "China", "Colombia", "Comoros", "Congo", "Costa Rica", "Croatia", "Cuba",
               "Cyprus", "Czech Republic", "Democratic Republic of Congo", "Denmark", "Djibouti",
               "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", 
               "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Finland", "France", "Gabon", 
               "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Guatemala", "Guinea", 
               "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", 
               "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", 
               "Jordan", "Kazakhstan", "Kenya", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", 
               "Lebanon", "Lesotho", "Liberia", "Libya", "Lithuania", "Luxembourg", "Madagascar", 
               "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Mauritania", "Mauritius", 
               "Mexico", "Moldova", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", 
               "Namibia", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", 
               "North Korea", "North Macedonia", "Norway", "Oman", "Pakistan", "Panama", 
               "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", 
               "Qatar", "Romania", "Russia", "Rwanda", "Saudi Arabia", "Senegal", "Serbia", 
               "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Somalia", "South Africa", 
               "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", 
               "Switzerland", "Syria", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", 
               "Togo", "Tunisia", "Turkey", "Turkmenistan", "Uganda", "Ukraine", "United Arab Emirates", 
               "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Venezuela", "Vietnam", 
               "Yemen", "Zambia", "Zimbabwe"),
  
  continent = c("Asia", "Europe", "Africa", "Africa", "Americas", "Asia", "Oceania",
                "Europe", "Asia", "Asia", "Asia", "Europe", "Europe", "Africa",
                "Americas", "Europe", "Africa", "Americas", "Europe", "Africa",
                "Africa", "Asia", "Africa", "Americas", "Africa", "Africa",
                "Americas", "Asia", "Americas", "Africa", "Africa", "Americas", "Europe", "Americas",
                "Europe", "Europe", "Africa", "Europe", "Africa",
                "Americas", "Americas", "Africa", "Americas", "Africa",
                "Africa", "Europe", "Africa", "Africa", "Europe", "Europe", "Africa",
                "Africa", "Asia", "Europe", "Africa", "Europe", "Americas", "Africa",
                "Africa", "Americas", "Africa", "Americas", "Europe", "Europe", "Asia",
                "Asia", "Asia", "Asia", "Europe", "Asia", "Europe", "Americas", "Asia",
                "Asia", "Asia", "Africa", "Asia", "Asia", "Asia", "Europe",
                "Asia", "Africa", "Africa", "Africa", "Europe", "Europe", "Africa",
                "Africa", "Asia", "Asia", "Africa", "Europe", "Africa", "Africa",
                "Americas", "Europe", "Asia", "Europe", "Africa", "Africa", "Asia",
                "Africa", "Asia", "Europe", "Oceania", "Americas", "Africa", "Africa",
                "Asia", "Europe", "Europe", "Asia", "Asia", "Americas",
                "Oceania", "Americas", "Americas", "Asia", "Europe", "Europe",
                "Asia", "Europe", "Europe", "Africa", "Europe", "Asia", "Africa",
                "Africa", "Asia", "Europe", "Europe", "Africa", "Africa",
                "Asia", "Africa", "Europe", "Asia", "Africa", "Americas", "Europe",
                "Europe", "Asia", "Asia", "Africa", "Asia", "Asia",
                "Africa", "Africa", "Asia", "Asia", "Africa", "Europe", "Asia",
                "Europe", "Americas", "Americas", "Asia", "Asia", "Asia",
                "Europe", "Africa", "Africa")
)

# Prepare country-level data for cases
country_cases_data <- global_cases_deaths_summary %>%
  filter(!location %in% c("Africa", "Asia", "Europe", "Americas", "Oceania", "World")) %>%
  filter(total_cases > 0) %>%
  left_join(continent_mapping, by = "location") %>%
  filter(!is.na(continent))

# Get available years and continents
years_available <- sort(unique(country_cases_data$year))
continents_available <- sort(unique(country_cases_data$continent))

# CREATE CONTINENTAL DATA FOR CHARTS
continent_cases <- global_cases_deaths_summary %>%
  left_join(continent_mapping, by = "location") %>%
  filter(!is.na(continent)) %>%
  group_by(continent, year) %>%
  summarise(
    total_cases = sum(total_cases, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cases_millions = total_cases / 1000000,
    exact_cases = total_cases
  )

continent_deaths <- global_cases_deaths_summary %>%
  left_join(continent_mapping, by = "location") %>%
  filter(!is.na(continent)) %>%
  group_by(continent, year) %>%
  summarise(
    total_deaths = sum(total_deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    deaths_thousands = total_deaths / 1000,
    exact_deaths = total_deaths
  )

# Define continent colors
continent_colors <- colors_vector

# Read vaccination data
vac_1dose <- read_csv("data/measles_global/1dose_share-of-children-vaccinated-against-measles.csv") %>%
  rename(`1_dose` = `Share of one-year-olds who have received their first dose of measles-containing vaccine (MCV1)`,
         `location` = `Entity`, `year` = `Year`) %>%
  left_join(country_coords, by = c("location" = "country"))

vac_2dose <- read_csv("data/measles_global/2_doses_ share-of-children-vaccinated-with-mcv.csv") %>%
  rename(`2_dose` = `Share of children who have received two doses of measles-containing vaccine as per the national schedule (MCV2)`,
         `location` = `Entity`, `year` = `Year`) %>%
  left_join(country_coords, by = c("location" = "country"))

start_year_1dose <- min(vac_1dose$year, na.rm = TRUE)
start_year_2dose <- min(vac_2dose$year, na.rm = TRUE)
later_start_year <- max(start_year_1dose, start_year_2dose)

vac_1dose_filtered <- vac_1dose %>% filter(year >= later_start_year)
vac_2dose_filtered <- vac_2dose %>% filter(year >= later_start_year)

vac_combined <- vac_1dose_filtered %>%
  left_join(vac_2dose_filtered %>% select(location, year, `2_dose`), by = c("location", "year")) %>%
  rename(dose_1 = `1_dose`, dose_2 = `2_dose`)


## GLOBAL ROW 1 - Global boxes {height="200px"}


#| include: false
#| lable: global_boxes
#| message: false

# Filter data based on selected continents
filtered_data <- country_cases_data %>%
  filter(continent %in% continents_available)

# Create a data frame for all years
stats_data <- map_dfr(years_available, function(yr) {
  year_data <- filtered_data %>% filter(year == yr)
  # Total cases for the year
  total_cases_year <- sum(year_data$total_cases, na.rm = TRUE)
  # Country with highest cases
  highest_country <- year_data %>%
    arrange(desc(total_cases)) %>%
    slice_head(n = 1)
  # Total deaths for the year
  total_deaths_year <- sum(year_data$total_deaths, na.rm = TRUE)
  # Country with highest deaths
  highest_deaths_country <- year_data %>%
    arrange(desc(total_deaths)) %>%
    slice_head(n = 1)
  data.frame(
    year = yr,
    total_cases = total_cases_year,
    highest_cases_country = if(nrow(highest_country) > 0) highest_country$location[1] else NA,
    highest_cases_continent = if(nrow(highest_country) > 0) highest_country$continent[1] else NA,
    highest_cases_value = if(nrow(highest_country) > 0) highest_country$total_cases[1] else 0,
    total_deaths = total_deaths_year,
    highest_deaths_country = if(nrow(highest_deaths_country) > 0) highest_deaths_country$location[1] else NA,
    highest_deaths_continent = if(nrow(highest_deaths_country) > 0) highest_deaths_country$continent[1] else NA,
    highest_deaths_value = if(nrow(highest_deaths_country) > 0) highest_deaths_country$total_deaths[1] else 0
  )
})

# Find the earliest and latest year
min_year <- min(stats_data$year, na.rm = TRUE)
max_year <- max(stats_data$year, na.rm = TRUE)

# Extract stats for both years
min_year_stats <- stats_data %>% filter(year == min_year) %>%
  select(highest_cases_country, highest_deaths_country, total_cases, total_deaths)
max_year_stats <- stats_data %>% filter(year == max_year) %>%
  select(highest_cases_country, highest_deaths_country, total_cases, total_deaths)

# Calculate percentage change
cases_pct_change <- ((max_year_stats$total_cases - min_year_stats$total_cases) / min_year_stats$total_cases) * 100
deaths_pct_change <- ((max_year_stats$total_deaths - min_year_stats$total_deaths) / min_year_stats$total_deaths) * 100

# Create a summary dataframe
percentage_change_summary <- data.frame(
  metric = c("Total Cases", "Total Deaths"),
  min_year = c(min_year_stats$total_cases, min_year_stats$total_deaths),
  max_year = c(max_year_stats$total_cases, max_year_stats$total_deaths),
  percentage_change = c(cases_pct_change, deaths_pct_change)
)

# 1) Find the country with the highest measles cases and deaths overall

# Country with highest total cases across all years
highest_cases_country_overall <- filtered_data %>%
  group_by(location, continent) %>%
  summarise(max_cases = max(total_cases, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(max_cases)) %>%
  slice_head(n = 1)

# Country with highest total deaths across all years
highest_deaths_country_overall <- filtered_data %>%
  group_by(location, continent) %>%
  summarise(max_deaths = max(total_deaths, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(max_deaths)) %>%
  slice_head(n = 1)

# 2) Find percentage difference between highest and lowest cases for the highest-case country

# Get the country name with highest cases
top_country_name <- highest_cases_country_overall$location[1]

# Get all data for this country across all years
top_country_data <- filtered_data %>%
  filter(location == top_country_name) %>%
  arrange(year)

# Find min and max values for this country
country_stats <- top_country_data %>%
  summarise(
    min_cases = min(total_cases, na.rm = TRUE),
    max_cases = max(total_cases, na.rm = TRUE),
    min_deaths = min(total_deaths, na.rm = TRUE),
    max_deaths = max(total_deaths, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate percentage differences
cases_pct_diff <- ((country_stats$max_cases - country_stats$min_cases) / country_stats$min_cases) * 100
deaths_pct_diff <- ((country_stats$max_deaths - country_stats$min_deaths) / country_stats$min_deaths) * 100

# Create summary for the top country
top_country_summary <- data.frame(
  country = top_country_name,
  continent = highest_cases_country_overall$continent[1],
  metric = c("Cases", "Deaths"),
  min_value = c(country_stats$min_cases, country_stats$min_deaths),
  max_value = c(country_stats$max_cases, country_stats$max_deaths),
  percentage_difference = c(cases_pct_diff, deaths_pct_diff)
)


# Optional: Show year-by-year data for the top country
top_country_data %>% select(year, total_cases, total_deaths)

# Additional Analysis: Top 5 countries by cases and deaths
top_5_cases <- filtered_data %>%
  group_by(location, continent) %>%
  summarise(max_cases = max(total_cases, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(max_cases)) %>%
  slice_head(n = 5)


top_5_deaths <- filtered_data %>%
  group_by(location, continent) %>%
  summarise(max_deaths = max(total_deaths, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(max_deaths)) %>%
  slice_head(n = 5)


# Analysis by continent
continent_summary <- filtered_data %>%
  group_by(continent) %>%
  summarise(
    total_cases = sum(total_cases, na.rm = TRUE),
    total_deaths = sum(total_deaths, na.rm = TRUE),
    avg_cases = mean(total_cases, na.rm = TRUE),
    avg_deaths = mean(total_deaths, na.rm = TRUE),
    countries_count = n_distinct(location),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_cases))

# Summary statistics across all data
overall_stats <- filtered_data %>%
  summarise(
    total_countries = n_distinct(location),
    total_years = n_distinct(year),
    total_records = n(),
    sum_all_cases = sum(total_cases, na.rm = TRUE),
    sum_all_deaths = sum(total_deaths, na.rm = TRUE),
    avg_cases_per_record = mean(total_cases, na.rm = TRUE),
    avg_deaths_per_record = mean(total_deaths, na.rm = TRUE),
    median_cases = median(total_cases, na.rm = TRUE),
    median_deaths = median(total_deaths, na.rm = TRUE)
  )



### Total Measles Cases (1990-2021)


#| content: valuebox
#| title: "Total Measles Cases"
#| icon: virus
#| color: danger
#| fig-height: 200
list(
  value = paste0(format(max_year_stats$total_cases, big.mark = ","), " (", round(cases_pct_change, 1), "% change)")
)


### Highest Recorded Cases


#| content: valuebox
#| title: "Highest Recorded Cases"
#| icon: hospital
#| color: danger
#| fig-height: 200
highest_cases_year <- top_country_data %>% 
  filter(total_cases == max(total_cases, na.rm = TRUE)) %>% 
  pull(year)
highest_cases_pct <- top_country_summary %>% 
  filter(metric == "Cases") %>% 
  pull(percentage_difference)
highest_cases_value <- top_country_summary %>% 
  filter(metric == "Cases") %>% 
  pull(max_value)
list(
  value = paste0(top_country_name, " - ", format(highest_cases_value, big.mark = ","), " (", highest_cases_year, ")")
)


### Total Measles Deaths (1990-2021)


#| content: valuebox
#| title: "Total Measles Deaths"
#| icon: virus
#| color: info
#| fig-height: 200
list(
  value = paste0(format(max_year_stats$total_deaths, big.mark = ","), " (", round(deaths_pct_change, 1), "% change)")
)


### Highest Recorded Deaths


#| content: valuebox
#| title: "Highest Recorded Deaths"
#| icon: hospital
#| color: info
#| fig-height: 200
highest_deaths_year <- filtered_data %>% 
  filter(location == highest_deaths_country_overall$location[1]) %>% 
  filter(total_deaths == max(total_deaths, na.rm = TRUE)) %>% 
  pull(year)
highest_deaths_pct <- top_country_summary %>% 
  filter(metric == "Deaths") %>% 
  pull(percentage_difference)
list(
  value = paste0(highest_deaths_country_overall$location[1], " - ", format(highest_deaths_country_overall$max_deaths, big.mark = ","), " (", highest_deaths_year, ")")
)


## Global ROW 2 - Measles {height="1000px"}

### Global Measles maps {.tabset}


#| title: Global Measles Cases

# 🚀 **Filtering the dataset to remove Antarctica & blank regions**
filtered_global_cases <- global_cases_deaths_summary %>%
  filter(location != "Antarctica" & total_cases > 0)  # ✅ Remove Antarctica & exclude empty data

# 🌍 Custom Color Gradient for Measles Cases
global_cases_colors <- list(
  c(0, "grey"),       # 0 cases
  c(0.1, "lightorange"),  
  c(0.3, "orange"),
  c(0.5, "red"),
  c(0.7, "darkred"),
  c(0.9, "maroon"),
  c(1, "black")  # Extreme cases (10M+)
)

# 🚀 Apply the corrected colorscale inside `plot_geo()`
global_map_cases <- plot_geo(filtered_global_cases, locationmode = "country names", frame = ~year) %>%
  add_trace(
    locations = ~location,
    z = ~total_cases,
    zmin = 0,
    zmax = 10000000,  # ✅ Set max value for color scale
    color = ~total_cases,
    colorscale = global_cases_colors,  # ✅ Now mapped directly to case numbers
    hoverinfo = "text",
    text = ~paste(location, "<br>Total Cases:", format(total_cases, big.mark = ",")), # CHANGED: Added format with comma separator
    marker = list(
      line = list(color = "black", width = 0.8)),
    colorbar = list(
      title = "Cases (Millions)",
      titleside = "bottom",
      titlefont = list(size = 16),
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 0.05,
      yanchor = "top",
      tickmode = "array",
      tickvals = c(0, 2000000, 4000000, 6000000, 8000000, 10000000),
      ticktext = c("0", "2", "4", "6", "8", "10"),
      len = 0.8,
      thickness = 15
    )) %>%
  layout(
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,     # ✅ Enables coastlines
      showcountries = TRUE,      # ✅ Displays country borders
      showland = TRUE,           # ✅ Highlights land areas distinctly
      landcolor = "white",       # ✅ Keeps land visible while ensuring borders stand out
      countrycolor = "black",    # ✅ Ensures country borders appear in black
      projection = list(type = "mercator"),
      lataxis = list(range = c(-40, 75)),
      dragmode = FALSE,          # ✅ Disables dragging
      scrollZoom = FALSE         # ✅ Disables scroll zoom on geo plots
    ),
    margin = list(l = 30, r = 30, t = 50, b = 30)  # Standard margins
  ) %>%
  config(
    displayModeBar = FALSE,      # ✅ Removes the entire toolbar
    scrollZoom = FALSE,          # ✅ Disables scroll zooming
    doubleClick = FALSE,         # ✅ Disables double-click zoom
    staticPlot = FALSE           # ✅ Keeps interactivity but removes zoom
  )

global_map_cases



#| title: Deaths

# Filter global data for deaths
filtered_global_deaths <- global_cases_deaths_summary %>%
  filter(location != "Antarctica" & total_cases > 0)  # ✅ Remove Antarctica & exclude empty data

# 🌍 Custom Color Gradient for Measles Deaths
global_deaths_colors <- list(
  c(0, "grey"),        # 0 deaths
  c(0.1, "lightyellow"),
  c(0.3, "lightorange"),  
  c(0.5, "orange"),
  c(0.7, "red"),
  c(0.9, "darkred"),
  c(1, "black")     # Extreme cases (500K+ deaths)
)

global_map_deaths <- plot_geo(filtered_global_deaths, 
                              locationmode = "country names", 
                              frame = ~year) %>%
  add_trace(
    locations = ~location,
    z = ~total_deaths,
    zmin = 0,
    zmax = 500000,  # ✅ Set max value for color scale
    color = ~total_deaths,
    colorscale = global_deaths_colors, 
    hoverinfo = "text",
    text = ~paste(location, "<br>Total Deaths:", format(total_deaths, big.mark = ",")), # CHANGED: Added format with comma separator
    marker = list(
      line = list(color = "black", width = 0.8)),
    colorbar = list(
      title = "Deaths (Thousands)",
      titleside = "bottom",
      titlefont = list(size = 16),
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 0.05,
      yanchor = "top",
      tickmode = "array",
      tickvals = c(0, 100000, 200000, 300000, 400000, 500000),
      ticktext = c("0", "100", "200", "300", "400", "500"),
      len = 0.8,
      thickness = 15
    )) %>%
  layout(
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,     # ✅ Enables coastlines
      showcountries = TRUE,      # ✅ Displays country borders
      showland = TRUE,           # ✅ Highlights land areas distinctly
      landcolor = "white",       # ✅ Keeps land visible while ensuring borders stand out
      countrycolor = "black",    # ✅ Ensures country borders appear in black
      projection = list(type = "mercator"),
      lataxis = list(range = c(-40, 75)),
      dragmode = FALSE,          # ✅ Disables dragging
      scrollZoom = FALSE         # ✅ Disables scroll zoom on geo plots
    ),
    margin = list(l = 30, r = 30, t = 50, b = 30)  # Standard margins
  ) %>%
  config(
    displayModeBar = FALSE,      # ✅ Removes the entire toolbar
    scrollZoom = FALSE,          # ✅ Disables scroll zooming
    doubleClick = FALSE,         # ✅ Disables double-click zoom
    staticPlot = FALSE           # ✅ Keeps interactivity but removes zoom
  )

global_map_deaths


## GLOBAL ROW 3 - Continent boxes {height="250px"}


#| include: false
#| label: continent_analysis
#| message: false

# Filter data based on selected continents
filtered_data <- country_cases_data %>%
  filter(continent %in% continents_available)

# Create continent statistics data for value boxes
continent_stats_data <- filtered_data %>%
  group_by(continent) %>%
  summarise(
    # Total cases and deaths for the continent
    total_cases = sum(total_cases, na.rm = TRUE),
    total_deaths = sum(total_deaths, na.rm = TRUE),
    
    # Average cases and deaths per country-year
    avg_cases_per_record = mean(total_cases, na.rm = TRUE),
    avg_deaths_per_record = mean(total_deaths, na.rm = TRUE),
    
    # Number of countries and records
    countries_count = n_distinct(location),
    total_records = n(),
    
    .groups = 'drop'
  ) %>%
  arrange(desc(total_cases))

# Find the country with highest cases in each continent
continent_highest_cases <- filtered_data %>%
  group_by(continent) %>%
  slice_max(total_cases, n = 1, with_ties = FALSE) %>%
  select(continent, location, total_cases, total_deaths, year) %>%
  rename(
    highest_cases_country = location,
    highest_cases_value = total_cases,
    highest_cases_deaths = total_deaths,
    highest_cases_year = year
  )

# Find the country with highest deaths in each continent
continent_highest_deaths <- filtered_data %>%
  group_by(continent) %>%
  slice_max(total_deaths, n = 1, with_ties = FALSE) %>%
  select(continent, location, total_cases, total_deaths, year) %>%
  rename(
    highest_deaths_country = location,
    highest_deaths_value = total_deaths,
    highest_deaths_cases = total_cases,
    highest_deaths_year = year
  )

# Combine all continent statistics
continent_stats_data <- continent_stats_data %>%
  left_join(continent_highest_cases, by = "continent") %>%
  left_join(continent_highest_deaths, by = "continent")

# Additional analysis: Top 3 countries per continent by cases
top_countries_per_continent <- filtered_data %>%
  group_by(continent) %>%
  slice_max(total_cases, n = 3, with_ties = FALSE) %>%
  select(continent, location, total_cases, total_deaths, year) %>%
  arrange(continent, desc(total_cases))

# Continent comparison metrics
continent_comparison <- continent_stats_data %>%
  select(continent, total_cases, total_deaths, countries_count, 
         highest_cases_country, highest_cases_value,
         highest_deaths_country, highest_deaths_value) %>%
  mutate(
    cases_per_country = round(total_cases / countries_count, 0),
    deaths_per_country = round(total_deaths / countries_count, 0),
    case_fatality_rate = round((total_deaths / total_cases) * 100, 2)
  )

# Year-over-year trends by continent
continent_yearly_trends <- filtered_data %>%
  group_by(continent, year) %>%
  summarise(
    yearly_cases = sum(total_cases, na.rm = TRUE),
    yearly_deaths = sum(total_deaths, na.rm = TRUE),
    countries_reporting = n_distinct(location),
    .groups = 'drop'
  ) %>%
  arrange(continent, year)

# Calculate percentage change for each continent (earliest vs latest year)
continent_change_analysis <- continent_yearly_trends %>%
  group_by(continent) %>%
  summarise(
    earliest_year = min(year),
    latest_year = max(year),
    earliest_cases = first(yearly_cases),
    latest_cases = last(yearly_cases),
    earliest_deaths = first(yearly_deaths),
    latest_deaths = last(yearly_deaths),
    .groups = 'drop'
  ) %>%
  mutate(
    cases_pct_change = round(((latest_cases - earliest_cases) / earliest_cases) * 100, 1),
    deaths_pct_change = round(((latest_deaths - earliest_deaths) / earliest_deaths) * 100, 1)
  )

# Final continent stats data with change metrics
continent_stats_data <- continent_stats_data %>%
  left_join(continent_change_analysis %>% 
              select(continent, cases_pct_change, deaths_pct_change), 
            by = "continent")

# Display summary
print("Continent Statistics Summary:")
print(continent_stats_data %>% 
        select(continent, total_cases, total_deaths, highest_cases_country, 
               highest_cases_value, cases_pct_change))


### Europe


#| content: valuebox
#| title: "Europe"
#| icon: globe-europe-africa
#| color: "#2535D9"
#| fig-height: 250
europe_stats <- continent_stats_data %>% filter(continent == "Europe")
list(
  value = paste0(format(europe_stats$total_cases, big.mark = ","), " total  ", 
                 europe_stats$highest_cases_country, ": ", 
                 format(europe_stats$highest_cases_value, big.mark = ","), " (", 
                 europe_stats$highest_cases_year, ")")
)


### Americas


#| content: valuebox
#| title: "Americas"
#| icon: globe-americas
#| color: "#B22222"
#| fig-height: 250
americas_stats <- continent_stats_data %>% filter(continent == "Americas")
list(
  value = paste0(format(americas_stats$total_cases, big.mark = ","), " total  ", 
                 americas_stats$highest_cases_country, ": ", 
                 format(americas_stats$highest_cases_value, big.mark = ","), " (", 
                 americas_stats$highest_cases_year, ")")
)


### Africa


#| content: valuebox
#| title: "Africa"
#| icon: globe-europe-africa
#| color: "#65463E"
#| fig-height: 250
africa_stats <- continent_stats_data %>% filter(continent == "Africa")
list(
  value = paste0(format(africa_stats$total_cases, big.mark = ","), " total  ", 
                 africa_stats$highest_cases_country, ": ", 
                 format(africa_stats$highest_cases_value, big.mark = ","), " (", 
                 africa_stats$highest_cases_year, ")")
)


### Asia


#| content: valuebox
#| title: "Asia"
#| icon: globe-central-south-asia
#| color: "#0A7029"
#| fig-height: 250
asia_stats <- continent_stats_data %>% filter(continent == "Asia")
list(
  value = paste0(format(asia_stats$total_cases, big.mark = ","), " total  ", 
                 asia_stats$highest_cases_country, ": ", 
                 format(asia_stats$highest_cases_value, big.mark = ","), " (", 
                 asia_stats$highest_cases_year, ")")
)


### Oceania


#| content: valuebox
#| title: "Oceania"
#| icon: globe-asia-australia
#| color: "#C08D2C"
#| fig-height: 250
oceania_stats <- continent_stats_data %>% filter(continent == "Oceania")
list(
  value = paste0(format(oceania_stats$total_cases, big.mark = ","), " total  ", 
                 oceania_stats$highest_cases_country, ": ", 
                 format(oceania_stats$highest_cases_value, big.mark = ","), " (", 
                 oceania_stats$highest_cases_year, ")")
)


## Global ROW 4 - Cases by Continent {height="800px"}

### Continent Cases {.tabset}

#### Continent Cases (bar view)


# Prepare continental data
continent_cases <- filtered_global_cases %>%
  left_join(continent_mapping, by = "location") %>%
  filter(!is.na(continent)) %>%
  group_by(continent, year) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE), .groups = "drop") %>%
  mutate(cases_millions = total_cases / 1e6)

continent_deaths <- filtered_global_deaths %>%
  left_join(continent_mapping, by = "location") %>%
  filter(!is.na(continent)) %>%
  group_by(continent, year) %>%
  summarise(total_deaths = sum(total_deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(deaths_thousands = total_deaths / 1e3)

# Prepare country data with continents
country_cases_data <- filtered_global_cases %>%
  left_join(continent_mapping, by = "location") %>%
  filter(!is.na(continent))

country_deaths_data <- filtered_global_deaths %>%
  left_join(continent_mapping, by = "location") %>%
  filter(!is.na(continent))

# Note: Y-axis ranges will adjust dynamically with the animation frames

# Continental Cases Bar Chart
continental_cases_bar <- continent_cases %>%
  plot_ly(
    x = ~continent,
    y = ~cases_millions,
    frame = ~year,
    type = 'bar',
    color = ~continent,
    colors = continent_colors,
    hovertemplate = paste(
      "<b>%{x}</b><br>",
      "Cases: %{y:.2f}M<br>",
      "Year: %{frame}<br>",
      "<extra></extra>"
    )
  ) %>%
  layout(
    xaxis = list(title = "Continent", titlefont = list(size = 14)),
    yaxis = list(
      title = "Cases (Millions)", 
      titlefont = list(size = 14),
      fixedrange = FALSE,
      autorange = TRUE
    ),
    showlegend = FALSE
  ) %>%
  config(
    displayModeBar = FALSE,
    scrollZoom = FALSE,
    doubleClick = FALSE,
    staticPlot = FALSE
  ) %>%
  animation_opts(frame = 1000, transition = 500) %>%
  animation_slider(currentvalue = list(prefix = "Year: "))

continental_cases_bar


#### (line view)


# Continental Cases Line Chart ----
continental_cases_line <- continent_cases %>%
  plot_ly(
    x = ~year,
    y = ~cases_millions,
    color = ~continent,
    colors = continent_colors,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 3),
    marker = list(size = 6),
    hovertemplate = paste(
      "<b>%{fullData.name}</b><br>",
      "Year: %{x}<br>",
      "Cases: %{y:.2f}M<br>",
      "<extra></extra>"
    )
  ) %>%
  layout(
    xaxis = list(title = "Year", titlefont = list(size = 16)),
    yaxis = list(
      title = "Cases (Millions)", 
      titlefont = list(size = 16),
      fixedrange = FALSE,
      autorange = TRUE
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 1.02,
      yanchor = "bottom"
    )
  ) %>%
  config(
    displayModeBar = FALSE,
    scrollZoom = FALSE,
    doubleClick = FALSE,
    staticPlot = FALSE
  )

continental_cases_line


### Continental Deaths {.tabset}

#### Continental Deaths (Bar view)


# Continental Deaths Bar Chart ----
continental_deaths_bar <- continent_deaths %>%
  plot_ly(
    x = ~continent,
    y = ~deaths_thousands,
    frame = ~year,
    type = 'bar',
    color = ~continent,
    colors = continent_colors,
    hovertemplate = paste(
      "<b>%{x}</b><br>",
      "Deaths: %{y:.1f}K<br>",
      "<extra></extra>"
    )
  ) %>%
  layout(
    xaxis = list(title = "Continent", titlefont = list(size = 14)),
    yaxis = list(
      title = "Deaths (Thousands)", 
      titlefont = list(size = 14),
      fixedrange = FALSE,
      autorange = TRUE
    ),
    showlegend = FALSE
  ) %>%
  config(
    displayModeBar = FALSE,
    scrollZoom = FALSE,
    doubleClick = FALSE,
    staticPlot = FALSE
  ) %>%
  animation_opts(frame = 1000, transition = 500) %>%
  animation_slider(currentvalue = list(prefix = "Year: "))

continental_deaths_bar


#### (Line view)


continental_deaths_line <- continent_deaths %>%
  plot_ly(
    x = ~year,
    y = ~deaths_thousands,
    color = ~continent,
    colors = continent_colors,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 3),
    marker = list(size = 6),
    hovertemplate = paste(
      "<b>%{fullData.name}</b><br>",
      "Year: %{x}<br>",
      "Deaths: %{y:.1f}K<br>",
      "<extra></extra>"
    )
  ) %>%
  layout(
    xaxis = list(title = "Year", titlefont = list(size = 16)),
    yaxis = list(
      title = "Deaths (Thousands)", 
      titlefont = list(size = 16),
      fixedrange = FALSE,
      autorange = TRUE
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 1.02,
      yanchor = "bottom"
    )
  ) %>%
  config(
    displayModeBar = FALSE,
    scrollZoom = FALSE,
    doubleClick = FALSE,
    staticPlot = FALSE
  )

continental_deaths_line


## GLOBAL ROW 5 - MMR Vaccination Boxes {height="200px"}

### Average 1st Dose Coverage


# Define the 4-shade purple color gradient
vaccination_colors_purple <- list(
  c(0, "#D3D3D3"),   # Grey (0-25%)
  c(0.25, "#9370DB"), # Medium Purple (25-50%)
  c(0.5, "#6A0DAD"),  # Dark Purple (50-75%)
  c(1, "#4B0082")    # Deep Dark Purple (75-100%)
)

# Read and process first dose data
vac_1dose <- read_csv("data/measles_global/1dose_share-of-children-vaccinated-against-measles.csv") %>%
  rename(`1_dose` = `Share of one-year-olds who have received their first dose of measles-containing vaccine (MCV1)`,
         `location` = `Entity`, `year` = `Year`) %>%
  left_join(country_coords, by = c("location" = "country"))

# Read and process second dose data
vac_2dose <- read_csv("data/measles_global/2_doses_ share-of-children-vaccinated-with-mcv.csv") %>%
  rename(`2_dose` = `Share of children who have received two doses of measles-containing vaccine as per the national schedule (MCV2)`,
         `location` = `Entity`, `year` = `Year`) %>%
  left_join(country_coords, by = c("location" = "country"))

# Identify the later start year
start_year_1dose <- min(vac_1dose$year, na.rm = TRUE)
start_year_2dose <- min(vac_2dose$year, na.rm = TRUE)
later_start_year <- max(start_year_1dose, start_year_2dose)

# Filter datasets to match the later start year
vac_1dose_filtered <- vac_1dose %>% filter(year >= later_start_year)
vac_2dose_filtered <- vac_2dose %>% filter(year >= later_start_year)

# Merge both datasets
vac_combined <- vac_1dose_filtered %>%
  left_join(vac_2dose_filtered %>% select(location, year, `2_dose`), by = c("location", "year"))

vac_combined <- vac_combined %>%
  rename(dose_1 = `1_dose`,
         dose_2 = `2_dose`)


## GLOBAL ROW 6 - 🌍 Interactive Vaccination Coverage Maps {height="1000px"}

### Vaccination coverage {.tabset}

#### Global Trends in MMR Vaccination Coverage Over Time (1st Dose)


# 🚀 **Filtering the dataset to remove Antarctica & blank regions**
filtered_vac1 <- vac_combined

vaccination_map_1st <- plot_geo(filtered_vac1, 
                                locationmode = "country names", 
                                frame = ~year) %>%
  add_trace(
    locations = ~location,
    z = ~dose_1,
    zmin = 0,
    zmax = 100,
    color = ~dose_1,
    colorscale = vaccination_colors_purple,
    hoverinfo = "text",
    text = ~paste(location, "<br>Year:", year, "<br>1st Dose Coverage:", round(dose_1, 1), "%"),
    marker = list(line = list(color = "black", width = 0.8))
  ) %>%
  colorbar(
    title = list(
      text = "%",
      side = "left",           
      font = list(size = 14)  
    ),
    orientation = "v",          # Vertical orientation
    x = 1.02,                   # Position to the right of the plot
    y = 0.75,                   # Position in top area
    len = 0.6,                  # Increased length (60% of plot height)
    thickness = 15,             # Keeps the colorbar thin
    xanchor = "left",           # Anchors from the left side
    yanchor = "top"             # Anchors from the top
  ) %>%
  layout(
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,
      showcountries = TRUE,
      showland = TRUE,
      landcolor = "white",
      countrycolor = "black",
      projection = list(type = "mercator"),
      lataxis = list(range = c(-40, 75)),
      dragmode = FALSE,
      scrollZoom = FALSE
    ),
    margin = list(l = 50, r = 120, t = 50, b = 50)  # Increased right margin for colorbar + title
  ) %>%
  config(
    displayModeBar = FALSE,
    scrollZoom = FALSE,
    doubleClick = FALSE,
    staticPlot = FALSE
  )

vaccination_map_1st

# 2nd Dose (Fully vaccininated) map ----

filtered_vac2 <- vac_combined 

vaccination_map_2nd <- plot_geo(filtered_vac2, 
                                locationmode = "country names", 
                                frame = ~year) %>%
  add_trace(
    locations = ~location,
    z = ~ dose_2,
    zmin = 0,
    zmax = 100,
    color = ~ dose_2,
    colorscale = vaccination_colors_purple,
    hoverinfo = "text",
    text = ~paste(location, "<br>Year:", year, "<br>1st Dose Coverage:", round(dose_2, 1), "%"),
    marker = list(line = list(color = "black", width = 0.8))
  ) %>%
  colorbar(
    title = list(
      text = "%",
      side = "left",           
      font = list(size = 14)  
    ),
    orientation = "v",          # Vertical orientation
    x = 1.02,                   # Position to the right of the plot
    y = 0.75,                   # Position in top area
    len = 0.6,                  # Increased length (60% of plot height)
    thickness = 15,             # Keeps the colorbar thin
    xanchor = "left",           # Anchors from the left side
    yanchor = "top"             # Anchors from the top
  ) %>%
  layout(
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,
      showcountries = TRUE,
      showland = TRUE,
      landcolor = "white",
      countrycolor = "black",
      projection = list(type = "mercator"),
      lataxis = list(range = c(-40, 75)),
      dragmode = FALSE,
      scrollZoom = FALSE
    ),
    margin = list(l = 50, r = 120, t = 50, b = 50)  # Increased right margin for colorbar + title
  ) %>%
  config(
    displayModeBar = FALSE,
    scrollZoom = FALSE,
    doubleClick = FALSE,
    staticPlot = FALSE
  )

vaccination_map_2nd


## GLOBAL ROW 7 - SOURCES {height="400px"}

### 🌍 Global Data Sources

#**Global Cases and Deaths**
  
#-   **File**: `global_cases_deaths.csv`

#-   **Source**: Global Burden of Disease Study 2021 (GBD 2021) Results

#-   **Institution**: Institute for Health Metrics and Evaluation (IHME), Seattle, United States

#-   **Year**: 2022

#-   **Citation**: Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2021 (GBD 2021) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022. Available from https://vizhub.healthdata.org/gbd-results/
  
  
  
#**Global Cases and Deaths by Demographics**
  
#-   **File**: `global_cases_sex_age.csv`

#-   **Source**: Global Burden of Disease Study 2021 (GBD 2021) Results

#-   **Institution**: Institute for Health Metrics and Evaluation (IHME), Seattle, United States

#-   **Year**: 2022

#-   **Citation**: Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2021 (GBD 2021) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022. Available from https://vizhub.healthdata.org/gbd-results/
  
  
  
#**Global Vaccination Coverage - First Dose**
  
#-   **File**: `1dose_share-of-children-vaccinated-against-measles.csv`

#-   **Source**: [Share of one-year-olds vaccinated against measles, 2023](https://ourworldindata.org/grapher/share-of-children-vaccinated-against-measles)

#-   **Institution**: Our World in Data

#-   **Coverage**: Global vaccination rates for measles first dose (MCV1)


#**Global Vaccination Coverage - Second Dose**
  
#-   **File**: `2_doses_share-of-children-vaccinated-with-mcv.csv`

#-   **Source**: [Share of children fully vaccinated against measles, 2023](https://ourworldindata.org/grapher/share-of-children-vaccinated-with-mcv2)

#-   **Institution**: Our World in Data

#-   **Coverage**: Global vaccination rates for the second dose (MCV2)

