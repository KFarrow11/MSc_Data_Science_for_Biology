# DATA + PACKAGES
source("scripts/cleaning.R")

# Function to calculate standard error 
std_error <- function(x) { 
  return(sd(x) / sqrt(length(x))) 
  }

# Function to calculate descriptive statistics
calculate_stats <- function(data, var) {
  data %>%
    summarise(
      variable = var,
      min = min(.data[[var]], na.rm = TRUE),
      max = max(.data[[var]], na.rm = TRUE),
      mean = mean(.data[[var]], na.rm = TRUE),
      median = median(.data[[var]], na.rm = TRUE),
      se = std_error(.data[[var]]),
      sd = sd(.data[[var]], na.rm = TRUE),
      range = max(.data[[var]], na.rm = TRUE) - min(.data[[var]], na.rm = TRUE),
      skewness = skewness(.data[[var]], na.rm = TRUE)
    )
}

# List of variables to summarize
variables1 <- c("god_occur", "elevation", "swardht", "willow_cover", "birch_cover",
               "juncus_cover", "sand_gravel_cover", "num_ditches", "water_table_depth", "hayfield", 
               "num_sedgepools", "pool_cover")

variables2 <- c("god_density", "elevation", "swardht", "willow_cover", "birch_cover",
                "juncus_cover", "sand_gravel_cover", "num_ditches", "water_table_depth", "hayfield", 
                "num_sedgepools", "pool_cover")


# Calculate stats for all variables
all_stats1 <- lapply(variables1, function(var) {
  calculate_stats(pca_godwits, var)
}) %>%
  bind_rows() %>%
  mutate(across(where(is.numeric), ~ ifelse(. %% 1 == 0.5 | . %% 1 == 0, round(.) %>% as.integer(), round(., 2))))

all_stats1

all_stats2 <- lapply(variables2, function(var) {
  calculate_stats(stats_godwit_presence, var)
}) %>%
  bind_rows() %>%
  mutate(across(where(is.numeric), ~ ifelse(. %% 1 == 0.5 | . %% 1 == 0, round(.) %>% as.integer(), round(., 2))))

all_stats2

gt_table1 <- gt(all_stats1) %>%
  gt_theme_pff() %>%
  tab_header(
    title = md("Descriptive statistics of environmental factors influencing godwit presence")
  ) %>%
  tab_style(
    style = list(cell_text(size = "extra large")),
    locations = cells_title(groups = "title")
  )

gt_table2 <- gt(all_stats2) %>%
  gt_theme_pff() %>%
  tab_header(
    title = md("Descriptive statistics of environmental factors influencing godwit density")
  ) %>%
  tab_style(
    style = list(cell_text(size = "extra large")),
    locations = cells_title(groups = "title")
  )


gt_table1
gt_table2
# Save the gt table as an image 
gtsave(gt_table1, "tables/gt_table1.png")
gtsave(gt_table2, "tables/gt_table2.png")

# overview ----
# In this study, we analyzed the environmental factors influencing the presence of godwits, with results summarized in the table titled *"Descriptive Statistics of Environmental Factors Influencing Godwit Presence."* The table presents a range of descriptive measures—such as minimum, maximum, mean, median, standard deviation, standard error, range, and skewness—for various physical and vegetation-related variables, including elevation, sand/gravel cover, willow cover, and juncus cover.

# Our findings highlight significant patterns in the data. For example, variables like pool cover and sand/gravel cover exhibited pronounced skewness, indicating asymmetrical distributions across the study area. These results suggest that these factors are not uniformly distributed and may play a unique role in shaping habitat suitability for godwits. In contrast, variables such as sward height and water table depth displayed lower skewness, reflecting more balanced distributions and suggesting their availability might be more consistent in this habitat.

# Additionally, the large ranges observed for certain variables—such as hayfields (0–1500) and birch cover (0–90)—underline considerable heterogeneity in habitat characteristics. This variability suggests diverse ecological conditions within the study area, which could influence the availability of suitable habitats for godwits.

# Interpretation of these results points to the ecological importance of specific environmental variables in determining godwit presence. Factors like pool cover and sand/gravel cover, with their uneven distribution, may represent critical or limiting conditions that affect godwit habitat selection. On the other hand, variables with more balanced distributions may provide stable, baseline conditions supporting godwit occurrence. The substantial heterogeneity in habitat features underscores the complexity of the ecological landscape and emphasizes the need for nuanced habitat management approaches.

# These findings not only enhance our understanding of godwit ecology but also inform conservation strategies aimed at ensuring their persistence by prioritizing key habitat features and mitigating limiting environmental conditions. This research contributes valuable data for advancing habitat assessments and ecological modeling efforts in avian conservation.
