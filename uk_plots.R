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

## UK ROW 1 - UK BOXES {height="250px"}
region_coords <- tibble(
  region = c("North_East", "North_West", "Yorkshire_and_the_Humber",
             "East_Midlands", "West_Midlands", "East_of_England",
             "London", "South_East", "South_West", "Wales"),
  lat = c(54.97, 53.48, 53.80, 52.95, 52.48, 52.37, 51.50, 51.27, 50.85, 52.30),
  lon = c(-1.61, -2.24, -1.54, -1.15, -1.90, 0.00, -0.12, -0.47, -3.60, -3.78))

# Cases
uk_cases <- read_xlsx("data/measles_uk/cases_region_2012_2024.xlsx") %>%
  pivot_longer( # Pivot longer to get regions and cases in one column
    cols = c("North_East", "North_West", "Yorkshire_and_the_Humber",
             "East_Midlands", "West_Midlands", "East_of_England",
             "London", "South_East", "South_West", "Wales"),
    names_to = "region",
    values_to = "cases") %>%
  left_join(region_coords, by = "region")

# Sum cases by year, region and age group
uk_cases_summary1 <- uk_cases %>%
  group_by(Year, region, Age_group) %>% # Group by both region and age group
  summarize(total_cases = sum(cases, na.rm = TRUE)) %>%
  ungroup()

uk_cases_summary1 <- uk_cases_summary1 %>%
  mutate(
    age = case_when(
      Age_group == "less than 1 year" ~ "<1",
      Age_group == "1 to 4 years" ~ "1-4",
      Age_group == "5 to 9 years" ~ "5-9",
      Age_group == "10 to 14 years" ~ "10-14",
      Age_group == "15 to 19 years" ~ "15-19",
      Age_group == "20 to 24 years" ~ "20-24",
      Age_group == "25 to 29 years" ~ "25-29",
      Age_group == "30 to 34 years" ~ "30-34",
      Age_group == "35 years and older" ~ "35+",
      TRUE ~ "no_data"  # Explicitly marking unknown values
    )
  )

# Load UK region boundaries
uk_regions <- st_read("data/measles_uk/rgn2025.geojson")

# Create region mapping
region_mapping <- data.frame(
  data_region = c("North_East", "North_West", "Yorkshire_and_the_Humber",
                  "East_Midlands", "West_Midlands", "East_of_England",
                  "London", "South_East", "South_West", "Wales", "Scotland"),
  geo_region = c("North East", "North West", "Yorkshire and The Humber",
                 "East Midlands", "West Midlands", "East of England",
                 "London", "South East", "South West", "Wales", "Scotland")
)

# Prepare cases data with proper region names
uk_cases_age_clean <- uk_cases_summary1 %>%
  left_join(region_mapping, by = c("region" = "data_region")) %>%
  select(-region) %>%
  rename(region = geo_region) %>%
  mutate(
    total_cases = as.numeric(total_cases),
    Year = as.numeric(Year)
  ) %>%
  filter(!is.na(region))

# Rename columns and select relevant data
filtered_age_cases <- uk_cases_age_clean %>%
  select(Year, region, Age_group, total_cases) %>%
  rename(
    Year = Year,
    `Age Group` = Age_group,
    Region = region,
    Cases = total_cases
  )

# Calculate total cases across all years and regions
total_uk_cases <- sum(uk_cases_age_clean$total_cases, na.rm = TRUE)

# Calculate year-over-year change (2023 vs 2024 or latest available years)
latest_years <- sort(unique(uk_cases_age_clean$Year), decreasing = TRUE)[1:2]
latest_year_cases <- uk_cases_age_clean %>%
  filter(Year == latest_years[1]) %>%
  summarise(total = sum(total_cases, na.rm = TRUE)) %>%
  pull(total)

previous_year_cases <- uk_cases_age_clean %>%
  filter(Year == latest_years[2]) %>%
  summarise(total = sum(total_cases, na.rm = TRUE)) %>%
  pull(total)

cases_pct_change <- ((latest_year_cases - previous_year_cases) / previous_year_cases) * 100

# Find highest cases by region
highest_region_cases <- uk_cases_age_clean %>%
  group_by(region) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE)) %>%
  arrange(desc(total_cases)) %>%
  slice(1)

# Find the year when this region had its highest cases
highest_region_year <- uk_cases_age_clean %>%
  filter(region == highest_region_cases$region) %>%
  group_by(Year) %>%
  summarise(yearly_total = sum(total_cases, na.rm = TRUE)) %>%
  arrange(desc(yearly_total)) %>%
  slice(1) %>%
  pull(Year)

# Find highest cases by age group
highest_age_cases <- uk_cases_age_clean %>%
  group_by(Age_group) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE)) %>%
  arrange(desc(total_cases)) %>%
  slice(1)

# Find the year when this age group had its highest cases
highest_age_year <- uk_cases_age_clean %>%
  filter(Age_group == highest_age_cases$Age_group) %>%
  group_by(Year) %>%
  summarise(yearly_total = sum(total_cases, na.rm = TRUE)) %>%
  arrange(desc(yearly_total)) %>%
  slice(1) %>%
  pull(Year)

# Calculate children under 5 percentage
children_under_5_cases <- uk_cases_age_clean %>%
  filter(Age_group %in% c("less than 1 year", "1 to 4 years")) %>%
  summarise(total = sum(total_cases, na.rm = TRUE)) %>%
  pull(total)

children_under_5_pct <- round((children_under_5_cases / total_uk_cases) * 100)

# Calculate average annual cases
avg_annual_cases <- round(total_uk_cases / length(unique(uk_cases_age_clean$Year)))

# Calculate peak year data
peak_year_data <- uk_cases_age_clean %>%
  group_by(Year) %>%
  summarise(yearly_total = sum(total_cases, na.rm = TRUE)) %>%
  arrange(desc(yearly_total)) %>%
  slice(1)

## UK ROW 3 - REGION TABLE PLOTS {height="1000px"}
##### UK Measles Cases by Region (Bar view)
# Load required libraries
library(RColorBrewer)
library(crosstalk)

# Define the RColorBrewer Paired palette
color_palette <- brewer.pal(10, "Paired")  # Adjust number based on unique groups

# 3. ANIMATED BAR CHART BY YEAR - UK Measles Cases by Region
uk_cases_region_bar <- filtered_age_cases %>%
  group_by(Year, Region) %>%
  summarise(total_cases = sum(Cases, na.rm = TRUE), .groups = "drop") %>%
  plot_ly(y = ~reorder(Region, total_cases), x = ~total_cases, 
          frame = ~Year, 
          type = 'bar',
          color = ~Region, 
          colors = color_palette,
          orientation = 'h',
          hovertemplate = paste('<b>%{y}</b><br>',
                                'Total Cases: %{x}<br>',
                                '<extra></extra>')) %>%
  layout(xaxis = list(title = "Total Cases"),
         yaxis = list(title = ""),
         showlegend = FALSE) %>%
  animation_opts(frame = 300, transition = 200) %>%
  config(displayModeBar = FALSE)

uk_cases_region_bar


##### (Line view)
# Define the RColorBrewer Paired palette
color_palette <- brewer.pal(12, "Paired")  # Adjust number based on unique groups

# Function to create animated line plot with moving dots, incorporating custom color palette
create_animated_plot <- function(data, breakdown_type = "region") {
  
  if (breakdown_type == "age") {
    plot_data <- data %>%
      mutate(group_var = case_when(
        `Age Group` == "less than 1 year" ~ "less than 1",
        `Age Group` == "1 to 4 years" ~ "1-4",
        `Age Group` == "5 to 9 years" ~ "5-9",
        `Age Group` == "10 to 14 years" ~ "10-14",
        `Age Group` == "15 to 19 years" ~ "15-19",
        `Age Group` == "20 to 24 years" ~ "20-24",
        `Age Group` == "25 to 29 years" ~ "25-29",
        `Age Group` == "30 to 34 years" ~ "30-34",
        `Age Group` == "35 years and older" ~ "35+",
        TRUE ~ `Age Group`
      )) %>%
      mutate(group_var = factor(group_var, 
                                levels = c("less than 1", "1-4", "5-9", "10-14", "15-19", 
                                           "20-24", "25-29", "30-34", "35+"), 
                                ordered = TRUE)) %>%
      arrange(group_var) %>%
      group_by(Year, group_var) %>%
      summarise(Cases = sum(Cases, na.rm = TRUE), .groups = "drop")

    # Age group legend with explicit ordering
    legend_config <- list(
      orientation = "h",
      x = 0.5,
      xanchor = 'center',
      y = 1.02,
      yanchor = 'bottom',
      bgcolor = 'rgba(255, 255, 255, 0.9)',
      bordercolor = 'rgba(0, 0, 0, 0.3)',
      borderwidth = 1,
      traceorder = "normal",  # This helps maintain order
      font = list(size = 14)
    )
    
  } else {
    plot_data <- data %>%
      mutate(group_var = Region) %>%
      group_by(Year, group_var) %>%
      summarise(Cases = sum(Cases, na.rm = TRUE), .groups = "drop")
    
    legend_config <- list(
      orientation = "h",
      x = 0.5,
      xanchor = 'center',
      y = 1.02,
      yanchor = 'bottom',
      bgcolor = 'rgba(255, 255, 255, 0.9)',
      bordercolor = 'rgba(0, 0, 0, 0.3)',
      borderwidth = 1,
      itemsizing = "constant",
      itemwidth = 20,
      font = list(size = 14)
    )
  }
  
  years <- sort(unique(plot_data$Year))
  groups <- unique(plot_data$group_var)
  
  # For age plots, ensure groups are in the correct order
  if (breakdown_type == "age") {
    age_order <- c("less than 1", "1-4", "5-9", "10-14", "15-19", 
                   "20-24", "25-29", "30-34", "35+")
    groups <- age_order[age_order %in% groups]  # Keep only groups that exist in data
  }
  
  # Create animated data
  animated_data <- data.frame()
  
  for(current_year in years) {
    frame_data <- plot_data %>%
      filter(Year <= current_year) %>%
      mutate(frame = current_year)
    
    animated_data <- rbind(animated_data, frame_data)
  }
  
  # Create the plot
  p <- plot_ly()
  
  # Add traces in the correct order for age plots
  if (breakdown_type == "age") {
    for(i in seq_along(groups)) {
      group_data <- animated_data %>% filter(group_var == groups[i])
      
      p <- p %>%
        add_trace(
          data = group_data,
          x = ~Year,
          y = ~Cases,
          name = groups[i],
          color = I(color_palette[i]),  # Use specific color from palette
          frame = ~frame,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(width = 3),
          marker = list(size = 8),
          hovertemplate = paste(
            '<b>Year:</b> %{x}<br>',
            '<b>Cases:</b> %{y}<br>',
            '<b>', groups[i], '</b><br>',
            '<extra></extra>'
          )
        )
    }
  } else {
    # For region plots, use the original method
    p <- p %>%
      add_trace(
        data = animated_data,
        x = ~Year,
        y = ~Cases,
        color = ~group_var,
        colors = color_palette,
        frame = ~frame,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 3),
        marker = list(size = 8),
        hovertemplate = paste(
          '<b>Year:</b> %{x}<br>',
          '<b>Cases:</b> %{y}<br>',
          '<b>%{fullData.name}<br>',
          '<extra></extra>'
        )
      )
  }
  
  p <- p %>%
    layout(
      xaxis = list(
        title = "Year",
        titlefont = list(size = 16),
        range = c(min(years) - 0.5, max(years) + 0.5)
      ),
      yaxis = list(
        title = "Number of Cases",
        titlefont = list(size = 16),
        range = c(0, max(plot_data$Cases) * 1.1)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      legend = legend_config,
      margin = list(t = 30)  
    ) %>%
    animation_opts(
      frame = 500,
      transition = 300,
      redraw = FALSE
    ) %>%
    animation_slider(
      currentvalue = list(
        prefix = "Year: ",
        font = list(size = 16)
      )
    ) %>%
    animation_button(
      x = 0, xanchor = 'left', y = -0.1, yanchor = 'bottom'
    ) %>%
    config(displayModeBar = FALSE)
  
  return(p)
}

uk_cases_region_line <- create_animated_plot(filtered_age_cases, "region")

uk_cases_region_line


##### UK Measles Cases by Age Group (Bar view)
# Define the RColorBrewer Paired palette
color_palette <- brewer.pal(12, "Paired")  # Adjust number based on unique groups

# Create animated bar chart by age group
uk_cases_age_bar <- filtered_age_cases %>%
  mutate(Age_Group_Short = case_when(
    `Age Group` == "less than 1 year" ~ "less than 1",
    `Age Group` == "1 to 4 years" ~ "1-4",
    `Age Group` == "5 to 9 years" ~ "5-9",
    `Age Group` == "10 to 14 years" ~ "10-14",
    `Age Group` == "15 to 19 years" ~ "15-19",
    `Age Group` == "20 to 24 years" ~ "20-24",
    `Age Group` == "25 to 29 years" ~ "25-29",
    `Age Group` == "30 to 34 years" ~ "30-34",
    `Age Group` == "35 years and older" ~ "35+",
    TRUE ~ `Age Group`
  )) %>%
  mutate(Age_Group_Short = factor(Age_Group_Short, 
                                  levels = c("less than 1", "1-4", "5-9", "10-14", "15-19", 
                                             "20-24", "25-29", "30-34", "35+"), 
                                  ordered = TRUE)) %>%
  group_by(Year, Age_Group_Short) %>%
  summarise(total_cases = sum(Cases, na.rm = TRUE), .groups = "drop") %>%
  plot_ly(y = ~Age_Group_Short, x = ~total_cases,  # Changed this line
          frame = ~Year, 
          type = 'bar',
          color = ~Age_Group_Short, 
          colors = color_palette,
          orientation = 'h',
          hovertemplate = paste('<b>%{y}</b><br>',
                                'Total Cases: %{x}<br>',
                                '<extra></extra>')) %>%
  layout(xaxis = list(title = "Total Cases"),
         yaxis = list(title = ""),
         showlegend = FALSE) %>%
  animation_opts(frame = 1000, transition = 1000) %>%
  config(displayModeBar = FALSE)

uk_cases_age_bar


##### (Line view)
uk_cases_age_line <- create_animated_plot(filtered_age_cases, "age")

uk_cases_age_line


### UK Map column {width="40%"} ----
# Define UK region coordinates
region_coords <- tibble(
  region = c("North_East", "North_West", "Yorkshire_and_the_Humber",
             "East_Midlands", "West_Midlands", "East_of_England",
             "London", "South_East", "South_West", "Wales"),
  lat = c(54.97, 53.48, 53.80, 52.95, 52.48, 52.37, 51.50, 51.27, 50.85, 52.30),
  lon = c(-1.61, -2.24, -1.54, -1.15, -1.90, 0.00, -0.12, -0.47, -3.60, -3.78))

# Load UK measles case data
uk_cases <- read_xlsx("data/measles_uk/cases_region_2012_2024.xlsx") %>%
  pivot_longer(cols = region_coords$region, 
               names_to = "region", values_to = "cases") %>%
  left_join(region_coords, by = "region")

# Summarize cases by year, region, and age group
uk_cases_summary <- uk_cases %>%
  group_by(Year, region, Age_group) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  mutate(age = case_when(
    Age_group == "less than 1 year" ~ "<1",
    Age_group == "1 to 4 years" ~ "1-4",
    Age_group == "5 to 9 years" ~ "5-9",
    Age_group == "10 to 14 years" ~ "10-14",
    Age_group == "15 to 19 years" ~ "15-19",
    Age_group == "20 to 24 years" ~ "20-24",
    Age_group == "25 to 29 years" ~ "25-29",
    Age_group == "30 to 34 years" ~ "30-34",
    Age_group == "35 years and older" ~ "35+",
    TRUE ~ "no_data"
  ))

# Load UK region boundaries without printing metadata
uk_regions <- st_read("data/measles_uk/rgn2025.geojson", quiet = TRUE)

# Define region mapping
region_mapping <- tibble(
  data_region = c(region_coords$region, "Scotland"),
  geo_region = c("North East", "North West", "Yorkshire and The Humber",
                 "East Midlands", "West Midlands", "East of England",
                 "London", "South East", "South West", "Wales", "Scotland")
)

# Prepare cases data with proper region names
uk_cases_clean <- uk_cases_summary %>%
  left_join(region_mapping, by = c("region" = "data_region")) %>%
  select(-region) %>%
  rename(region = geo_region) %>%
  mutate(total_cases = as.numeric(total_cases),
         Year = as.numeric(Year)) %>%
  filter(!is.na(region))

# Insert Scotland data with NA values
scotland_entry <- tibble(
  Year = unique(uk_cases_clean$Year),
  Age_group = "All Ages",
  region = "Scotland",
  total_cases = NA_real_
)

uk_cases_clean <- bind_rows(uk_cases_clean, scotland_entry)

# Final case dataset preparation
complete_cases <- uk_cases_clean %>%
  mutate(
    total_cases = case_when(
      region == "Scotland" ~ NA_real_,
      is.na(total_cases) ~ 0,
      TRUE ~ total_cases
    ),
    hover_text = case_when(
      region == "Scotland" ~ paste("Region:", region, "<br>Year:", Year, "<br>Cases: No data"),
      TRUE ~ paste("Region:", region, "<br>Year:", Year, "<br>Cases:", total_cases)
    )
  )

# Merge with geospatial data
cases_sf_timeline <- uk_regions %>%
  rename(region = areanm) %>%  # Rename `areanm` to match `complete_cases`
  left_join(complete_cases, by = "region") %>%
  filter(!is.na(Year))

# Define color scale limits
color_limits <- c(0, max(cases_sf_timeline$total_cases, na.rm = TRUE))

# Create animated map plot with legend positioned on right
# Calculate exact bbox from your data for optimal fitting
bbox <- st_bbox(cases_sf_timeline)

# Create animated map plot with auto-fitting dimensions
gg_animated <- ggplot(cases_sf_timeline) +
  geom_sf(aes(fill = total_cases, frame = Year, text = hover_text), 
          color = "black", linewidth = 0.2) +
  scale_fill_gradientn(
    colors = c("#E5C1F5", "#C490D1", "#944FB2", "#5B2271", "#3B1055"),
    na.value = "grey70",
    limits = color_limits,
    name = "Cases",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(size = 10),
      barwidth = 1,
      barheight = 8
    )
  ) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"]), 
           expand = FALSE) +
  theme_void() +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 2),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

# Convert to interactive Plotly visualization with zoom functionality enabled
uk_animated_map <- ggplotly(gg_animated, tooltip = "text") %>%
  animation_opts(frame = 500, transition = 300, redraw = TRUE) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Year: ", 
      font = list(color = "black", size = 10)
    ),
    x = 0.1, y = 0, xanchor = 'left', yanchor = 'top',
    len = 0.75,
    thickness = 12
  ) %>%
  animation_button(
    x = 0.88, xanchor = 'left', y = 0, yanchor = 'top',
    width = 50, height = 20
  ) %>%
  config(
    displayModeBar = TRUE,        # Enable mode bar for zoom tools
    modeBarButtonsToRemove = c(   # Remove unwanted tools, keep zoom
      'pan2d', 'select2d', 'lasso2d', 'autoScale2d', 
      'hoverClosestCartesian', 'hoverCompareCartesian',
      'toggleSpikelines', 'resetScale2d'
    ),
    scrollZoom = TRUE,            # Enable scroll wheel zoom
    doubleClick = 'reset',        # Double-click to reset zoom
    showTips = FALSE, 
    displaylogo = FALSE,
    responsive = TRUE
  ) %>%
  layout(
    autosize = TRUE,
    xaxis = list(
      fixedrange = FALSE,         # Allow zooming on x-axis
      showgrid = FALSE, 
      zeroline = FALSE, 
      showline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      fixedrange = FALSE,         # Allow zooming on y-axis
      showgrid = FALSE, 
      zeroline = FALSE, 
      showline = FALSE,
      showticklabels = FALSE
    ),
    margin = list(t = 5, l = 5, r = 5, b = 25),
    showlegend = TRUE,
    dragmode = 'zoom'             # Set default interaction to zoom
  )

uk_animated_map

# ----
## UK ROW 4 - TABLE {height="600px"}
# Define color scale for `Cases`
color_scale_uk_cases <- c("#F3E2FC", "#DDBDE6", "#B898D0")

# Generate proper breakpoints for coloring
breaks <- quantile(filtered_age_cases$Cases, probs = seq(0, 1, length.out = 3), na.rm = TRUE)

# Apply professional styling with colors added
uk_ages_cases_table <- datatable(filtered_age_cases, 
                                 rownames = FALSE,
                                 class = "compact stripe hover order-column",  
                                 options = list(
                                   pageLength = 10,   # Show only 10 rows
                                   lengthMenu = list(c(10, 25, 50, 100), c("10", "25", "50", "100")), 
                                   autoWidth = FALSE,  
                                   scrollY = FALSE,   # Disable vertical scrolling
                                   scrollX = FALSE,   # Disable horizontal scrolling
                                   paging = TRUE,     # Enable pagination
                                   dom = '<"top-left-search"f>rt<"bottom"lp><"clear">',
                                   buttons = c("csv", "excel"),
                                   columnDefs = list(
                                     list(targets = "_all", className = "dt-left")  
                                   ),
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().container()).css({'height': '450px'});", # Fixed container height
                                     "}"
                                   )
                                 ), 
                                 filter = "top") %>%
  formatStyle(
    columns = c("Year", "Age Group", "Region"),
    fontFamily = "Arial",
    fontSize = "13px",
    textAlign = "left"
  ) %>%
  formatStyle(
    "Cases",
    backgroundColor = styleInterval(
      breaks[-1],  # Remove first element to match expected length
      color_scale_uk_cases
    ),
    fontFamily = "Arial",
    fontSize = "13px",
    textAlign = "left"
  )

uk_ages_cases_table


## UK ROW 5 - VAC BOXES {height="200px"}
uk_vac_completion <- read_xlsx("data/measles_uk/measles_vac_complete_england_2003_2024.xlsx") 
uk_vac_primary <- read_xlsx("data/measles_uk/measles_vac_primary_england_2003_2024.xlsx")

# Get the latest year's data
latest_complete <- uk_vac_completion %>% 
  filter(year == max(year)) %>% 
  pull(vac_complete)

latest_primary <- uk_vac_primary %>% 
  filter(year == max(year)) %>% 
  pull(vac_primary)

# Calculate coverage gap from WHO target
coverage_gap <- 95 - latest_complete  # WHO target is 95%

# Calculate years tracked
years_tracked <- length(unique(uk_vac_completion$year))

# Calculate trend over last 5 years
recent_data <- uk_vac_completion %>% 
  arrange(year) %>% 
  tail(5)

trend_direction <- ifelse(
  tail(recent_data$vac_complete, 1) > head(recent_data$vac_complete, 1),
  "↗ Improving",
  "↘ Declining"
)

# Calculate year-over-year change
latest_years <- sort(unique(uk_vac_completion$year), decreasing = TRUE)[1:2]
latest_year_complete <- uk_vac_completion %>%
  filter(year == latest_years[1]) %>%
  pull(vac_complete)

previous_year_complete <- uk_vac_completion %>%
  filter(year == latest_years[2]) %>%
  pull(vac_complete)

coverage_change <- latest_year_complete - previous_year_complete

## UK ROW 6 - VAC PLOTS {height="800px"}
### UK vaccination coverage column 1 {.tabset}
#### UK Trends in MMR Vaccination Coverage Over Time (Bar view)
uk_vac_completion <- read_xlsx("data/measles_uk/measles_vac_complete_england_2003_2024.xlsx") 
uk_vac_primary <- read_xlsx("data/measles_uk/measles_vac_primary_england_2003_2024.xlsx")

# Define purple color palette
purple_palette <- brewer.pal(9, "Purples")[c(5, 7)]  # Select two nice purple shades

# 1. ANIMATED LINE PLOT - Vaccination Coverage Over Time
# Combine both datasets for comparison
vac_combined <- uk_vac_completion %>%
  rename(coverage = vac_complete) %>%
  mutate(vaccination_type = "Complete Course") %>%
  bind_rows(
    uk_vac_primary %>%
      rename(coverage = vac_primary) %>%
      mutate(vaccination_type = "Primary Course")
  )

# Create animated data for cumulative lines
years <- sort(unique(vac_combined$year))
vac_types <- unique(vac_combined$vaccination_type)

animated_vac_data <- data.frame()

for(current_year in years) {
  frame_data <- vac_combined %>%
    filter(year <= current_year) %>%
    mutate(frame = current_year)
  
  animated_vac_data <- rbind(animated_vac_data, frame_data)
}


# 2. ANIMATED BAR CHART - UK MMR Vaccination Coverage
uk_vaccination_coverage_bar <- vac_combined %>%
  plot_ly(x = ~reorder(vaccination_type, coverage), y = ~coverage, 
          frame = ~year, 
          type = 'bar',
          color = ~vaccination_type, 
          colors = purple_palette,  # Use purple colors
          hovertemplate = paste('<b>%{x}</b><br>',
                                'Coverage: %{y}%<br>',
                                '<extra></extra>')) %>%
  layout(
    yaxis = list(
      title = list(text = "Coverage (%)", font = list(size = 16, family = "Arial Black")),
      tickfont = list(size = 14),
      range = c(0, 100),
      dtick = 20
    ),
    xaxis = list(
      title = "",
      tickfont = list(size = 14)
    ),
    showlegend = FALSE
  ) %>%
  animation_opts(frame = 300, transition = 200) %>%
  config(displayModeBar = FALSE)

uk_vaccination_coverage_bar


#### (Line view)
# Create animated line plot with increased legend font size
uk_vaccination_coverage_line <- plot_ly() %>%
  add_trace(
    data = animated_vac_data,
    x = ~year,
    y = ~coverage,
    color = ~vaccination_type,
    colors = purple_palette,  # Use purple colors
    frame = ~frame,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 3),
    marker = list(size = 8),
    hovertemplate = paste(
      '<b>Year:</b> %{x}<br>',
      '<b>Coverage:</b> %{y}%<br>',
      '<b>%{fullData.name}</b><br>',
      '<extra></extra>'
    )
  ) %>%
  layout(
    xaxis = list(
      title = list(text = "Year", font = list(size = 16, family = "Arial Black")),
      tickfont = list(size = 14),
      range = c(min(years) - 0.5, max(years) + 0.5)
    ),
    yaxis = list(
      title = list(text = "Coverage (%)", font = list(size = 16, family = "Arial Black")),
      tickfont = list(size = 14),
      range = c(0, 100),
      dtick = 20
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = 'center',
      y = 1.02,
      yanchor = 'bottom',
      bgcolor = 'rgba(255, 255, 255, 0.9)',
      bordercolor = 'rgba(0, 0, 0, 0)',  # Removed border by setting to transparent
      borderwidth = 0,  # Set border width to 0
      itemsizing = "constant",
      itemwidth = 30,  # Increased from 20 to 30 for larger legend box
      font = list(size = 16)  # Increased font size from 14 to 16
    ),
    margin = list(t = 30)
  ) %>%
  animation_opts(
    frame = 300,
    transition = 200,
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Year: ",
      font = list(size = 16)
    )
  ) %>%
  animation_button(
    x = 0, xanchor = 'left', y = -0.1, yanchor = 'bottom'
  ) %>%
  config(displayModeBar = FALSE)

# Display the plots
uk_vaccination_coverage_line


### UK vaccination coverage column 2
# Load the dataset
uk_parents <- read_xlsx("data/measles_uk/attitudes-of-parents-towards-vaccines-in-england-in-2022.xlsx")

# Define Paired color palette
paired_colors <- brewer.pal(10, "Paired")

# Create static horizontal bar chart
uk_parents_attitudes_bar <- uk_parents %>%
  # Shorten long statements for better display
  mutate(
    statement_short = case_when(
      str_detect(statement, "Measles and mumps continue") ~ "Measles and mumps continue to be a risk",
      str_detect(statement, "Measles can lead to serious complications") ~ "Measles can lead to serious complications",
      str_detect(statement, "Because of vaccinations, smallpox") ~ "Vaccinations have eradicated smallpox",
      str_detect(statement, "Two doses of the MMR vaccine") ~ "Two MMR doses give 99% protection",
      str_detect(statement, "Measles can cause death") ~ "Measles can cause death",
      str_detect(statement, "It doesn't matter if you have missed") ~ "Can catch up on missed MMR doses",
      str_detect(statement, "There is no treatment") ~ "There is no treatment or cure for measles",
      str_detect(statement, "None of the above") ~ "None of the above",
      str_detect(statement, "There were no cases") ~ "No measles/mumps cases in UK this year",
      str_detect(statement, "Measles is harmless") ~ "Measles is harmless",
      TRUE ~ statement
    )
  ) %>%
  plot_ly(
    y = ~reorder(statement_short, percentage_agree), 
    x = ~percentage_agree,
    type = 'bar',
    orientation = 'h',
    color = ~statement_short,
    colors = paired_colors,
    text = ~paste0(percentage_agree, "%"),
    textposition = 'outside',
    hoverinfo = 'none'
  ) %>%
  layout(
    xaxis = list(
      title = list(text = "Percentage Agree (%)", font = list(size = 16, family = "Arial Black")),
      tickfont = list(size = 14),
      range = c(0, 60),
      dtick = 10
    ),
    yaxis = list(
      title = "",
      tickfont = list(size = 12, family = "Arial Black"),
      categoryorder = "array",
      categoryarray = ~reorder(statement_short, percentage_agree),
      tickmode = "array",
      tickvals = ~reorder(statement_short, percentage_agree),
      ticktext = ~statement_short,
      side = "left",
      automargin = TRUE,
      ticklen = 0,
      tickwidth = 0,
      linewidth = 0,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    showlegend = FALSE,
    margin = list(l = 320, r = 50, t = 50, b = 100)  # Increased left margin slightly
  ) %>%
  config(displayModeBar = FALSE)

uk_parents_attitudes_bar


## UK ROW 7 - SOURCES {height="300px"}
### 🇬🇧 United Kingdom Data Sources

#**Geographic Data**
  
#-   **Source**: UK Regions Topojson Data

#-   **URL**: https://onsvisual.github.io/uk-topojson/
  
#-   **Institution**: ONS Visual (Office for National Statistics)

#-   **Usage**: Interactive mapping of UK regions for case distribution


#**UK Cases by Region and Age**
  
#-   **File**: `cases_region_2012_2024.xlsx`

#-   **Source**: [Confirmed cases of measles in England and Wales by region and age 2012-2014](https://www.gov.uk/government/publications/measles-confirmed-cases/confirmed-cases-of-measles-in-england-and-wales-by-region-and-age-2012-to-2014)

#-   **Institution**: UK Government / UK Health Security Agency (UKHSA)

#-   **Coverage**: Regional breakdown of confirmed measles cases from 2012-2024

#-   **Demographics**: Age-stratified data for epidemiological analysis


#**UK Vaccination Coverage - Primary Immunisation**
  
#-   **File**: `measles_vac_primary_england_2003_2024.xlsx`

#-   **Source**: MMR primary immunisation in England 2024

#-   **Institution**: Statista (compiled from NHS England data)

#-   **Coverage**: Primary MMR vaccination rates across England (2003-2024)

#-   **Purpose**: Tracking first dose vaccination coverage trends


#**UK Vaccination Coverage - Complete Immunisation**
  
#-   **File**: `measles_vac_complete_england_2003_2024.xlsx`

#-   **Source**: MMR immunization England 2024

#-   **Institution**: Statista (compiled from NHS England data)

#-   **Coverage**: Complete MMR vaccination rates across England (2003-2024)

#-   **Purpose**: Monitoring full vaccination series completion


#**UK Parental Attitudes Survey**
  
#-   **File**: `attitudes-of-parents-towards-vaccines-in-england-in-2022.xlsx`

#-   **Source**: Attitudes of parents towards vaccines in England 2022

#-   **Institution**: Statista (survey data)

#-   **Coverage**: Public perception and attitudes toward MMR vaccination

#-   **Purpose**: Understanding vaccine hesitancy patterns

