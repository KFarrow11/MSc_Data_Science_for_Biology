# Install and load required packages if needed
# install.packages(c("gt", "gtExtras", "dplyr", "tibble"))
library(gt)
library(gtExtras)
library(dplyr)
library(tibble)

# TABLE 1: Environmental Factors Influencing Godwit Presence/Absence
library(tibble)

presence_absence <- tribble(
  ~`Environmental Factor`, ~Relationship, ~`Supporting Evidence`, ~`Statistical Significance`,
  "Elevation", "Positive", "Higher elevations increase likelihood of godwit occurrence (Fig. 3)", "β = 0.017, p < 0.05",
  "Birch Cover", "Positive", "Greater birch cover increases likelihood of godwit occurrence (Fig. 3)", "β = 0.027, p < 0.05",
  "Water Table Depth", "Negative", "Godwit occurrence declines in areas with deeper water tables (Fig. 3)", "β = -0.018, p < 0.001",
  "Pool Cover", "Negative", "Extensive pool coverage reduces godwit occurrence (Fig. 3)", "β = -0.061, p < 0.05",
  "Sedge Pools", "Non-linear", "Strong positive effect of 1-2 pools, plateauing at higher numbers (Fig. 4)", 
  "No pools: β = -0.442 ± 0.711, p = 0.53 (not significant)\n
  One pool: β = 4.569 ± 1.064, p < 0.001\n
  Two pools: β = 4.121 ± 1.519, p < 0.01\n
  Three pools: β = 18.70 ± 1345, p = 0.989 (not significant)",
  "Sedge Pool Probability", "Positive threshold", "Probability of godwit observation:\n- No pools: 39.1%\n- One pool: 98.4%\n- Two pools: 97.5%\n- Three pools: near 100%", "Demonstrates diminishing returns beyond optimal threshold"
)

# Create table 1
table1 <- presence_absence %>%
  gt() %>%
  tab_header(
    title = md("Environmental factors influencing *Black-tailed Godwit* Presence/Absence")
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = `Environmental Factor`
    )
  ) %>%
  gt_theme_pff()

table1

# TABLE 2: Environmental Factors Influencing Godwit Abundance/Density
abundance <- tribble(
  ~`Environmental Factor`, ~Relationship, ~`Supporting Evidence`, ~`Statistical Significance`,
  "Distance to Hayfields", "Negative", "Highest densities (3-4 birds/ha) observed within 100m of hayfields; abundance declines with increasing distance (Fig. 6, 7)", "β = -0.0009, p < 0.05",
  "Sward Grass Height", "Negative", "Godwit density decreases as vegetation height increases (Fig. 6)", "β = -1.22, p < 0.05",
  "Hayfield Presence", "Slightly negative", "Godwits prefer areas near but not directly in hayfields (Fig. 6)", "β = -0.0009, p < 0.05"
)

# Create table 2
table2 <- abundance %>%
  gt() %>%
  tab_header(
    title = md("Environmental factors influencing *Black-tailed Godwit* Abundance/Density")
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = `Environmental Factor`
    )
  ) %>%
  gt_theme_pff()

table2

# TABLE 3: Principal Component Analysis Results
pca_results <- tribble(
  ~`Principal Component`, ~`Variance Explained`, ~`Key Factors`, ~`Relationship to Godwits`,
  "PC1 + PC2 (Presence)", "39.2%", "Wetland areas with Juncus vegetation, shallow water tables, proximity to hayfields", "Godwits strongly associate with these features for breeding habitat (Fig. 2)",
  "PC3 + PC4 (Presence)", "21.3%", "Moderate drainage infrastructure, sedge-dominated wetlands, consistent pools", "Godwits favor these while avoiding willow cover and sandy/gravel substrates (Fig. 2)",
  "PC1 + PC2 (Abundance)", "39.2%", "Elevational and hydrological gradient, vegetation structure", "Higher godwit abundance in higher elevation areas with moderate water table depths and shorter sward heights; lower in sandy/gravel areas with deeper water tables and areas with extensive willow cover (Fig. 5)",
  "PC3 + PC4 (Abundance)", "21.3%", "Wetland fragmentation, vegetation complexity, landscape structure", "Higher godwit density in areas with sedge pools, Juncus cover, and near hayfields; lower in areas with extensive land drainage and willow cover (Fig. 5)"
)

# Create table 3
table3 <- pca_results %>%
  gt() %>%
  tab_header(
    title = "Principal Component Analysis Results"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = `Principal Component`
    )
  ) %>%
  gt_theme_pff()

table3

