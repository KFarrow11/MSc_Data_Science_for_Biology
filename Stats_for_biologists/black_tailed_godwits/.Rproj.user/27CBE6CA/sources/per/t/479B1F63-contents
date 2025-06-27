# AIM ----
# Which environmental conditions influence the presence or absence of godwits?

# DATA + PACKAGES
source("scripts/cleaning.R")
source("scripts/explore_data/all_stats.R")
gt_table1 # descriptive stats table

# for code = look at chapter 12 frogs
# Fit a GLM
glm_model <- glm(god_occur ~ elevation + swardht + willow_cover + birch_cover + 
                 juncus_cover + sand_gravel_cover + num_ditches + water_table_depth + 
                 hayfield + num_sedgepools + pool_cover, 
                 family = binomial(link = "logit"),
                 data = godwits)

# Summarize the model
anova(glm_model, test = "Chisq")
summary(glm_model) 
performance::check_model(glm_model) # low collinearity, 4 points outside error bounds

# Alternative - Refit the model without insignificant variables from ANOVA Chisq test, not summary
glm_model_reduced <- glm(god_occur ~ swardht + birch_cover + juncus_cover + num_ditches +
                         water_table_depth + num_sedgepools + pool_cover,
                         family = binomial(link = "logit"),
                         data = godwits)

# Summarize the reduced model
anova(glm_model_reduced, test = "Chisq")
summary(glm_model_reduced) # water_table_depth* pool_cover **, birch_cover **, sedge_pools ***
performance::check_model(glm_model_reduced) # looking pretty - low collinearity, 3 error bounds
vif(glm_model_reduced)
# results - The Generalized Variance Inflation Factor (GVIF) values indicate minimal multicollinearity within your model. Key predictors such as swardht, birch_cover, num_ditches, and pool_cover exhibit low GVIF values, suggesting they do not have strong correlations with other predictors. Although juncus_cover and water_table_depth have the highest GVIF values at 2.42 and 2.61 respectively, they remain within the acceptable limit of 5, indicating manageable multicollinearity. This low multicollinearity ensures that the predictors are not strongly correlated, thereby enhancing the model's reliability and interpretability. Consequently, the relationships between the predictors and the outcome variable are clear and meaningful, contributing to the overall robustness of your model. Performance checks indicate the model fits the data well, with minimal issues related to residual distribution or influential observations, making it suitable for further analysis.

# Refit the model with elevation
glm_model_reduced_2 <- glm(god_occur ~ elevation + swardht + birch_cover + juncus_cover +
                           num_ditches + water_table_depth + num_sedgepools + pool_cover,
                           family = binomial(link = "logit"),
                           data = godwits)

# Summarize the reduced model
summary(glm_model_reduced_2)
anova(glm_model_reduced_2, test = "Chisq")
performance::check_model(glm_model_reduced_2) # reduced binned residuals 
vif(glm_model_reduced_2)
# results
# The Generalized Variance Inflation Factor (GVIF) values indicate minimal multicollinearity within your model. Key predictors such as elevation, swardht, birch_cover, num_ditches, and pool_cover exhibit low GVIF values, suggesting they do not have strong correlations with other predictors. Although water_table_depth shows the highest GVIF at 2.636326, it remains within acceptable limits, indicating moderate but manageable multicollinearity. Overall, since all predictors in your model have GVIF values significantly below the threshold of 5, it signifies low multicollinearity. This low multicollinearity ensures that the predictors are not strongly correlated, thereby enhancing the model's reliability and interpretability. Consequently, the relationships between the predictors and the outcome variable are clear and meaningful, contributing to the overall robustness of your model.

final_glm <- glm(god_occur ~ elevation + birch_cover + water_table_depth + num_sedgepools + pool_cover, 
                 family = binomial(link = "logit"),
                 data = godwits)

summary(final_glm)
anova(final_glm, test = "Chisq")
performance::check_model(final_glm)

#                   Estimate    Std. Error z value  Pr(>|z|)    
# (Intercept)       -4.420e-01  7.113e-01  -0.621   0.534320    
# elevation          1.707e-02  7.873e-03   2.168   0.030148 *  
# birch_cover        2.681e-02  1.074e-02   2.497   0.012529 *  
# water_table_depth -1.767e-02  5.227e-03  -3.381   0.000723 ***
# num_sedgepools1    4.569e+00  1.064e+00   4.295   1.75e-05 ***
# num_sedgepools2    4.121e+00  1.519e+00   2.712   0.006688 ** 
# num_sedgepools3    1.870e+01  1.345e+03   0.014   0.988904    
# pool_cover        -6.146e-02  2.947e-02  -2.086   0.037011 *  

final_glm$coefficient[2:8] # negative water table depth and pool cover
exp(final_glm$coefficient[2:8]) # reduced model
exp(glm_model$coefficient[2:14]) # full model

# results:
# The readout from the exponentiated coefficients of the generalized linear model (GLM) reveals several key environmental factors influencing the presence of black-tailed godwits. Higher elevations (1.02) and birch cover (1.03 %) are positively associated with godwit presence, indicating that these conditions are favorable for their habitat. Conversely, deeper water table depths (0.98) slightly reduce the likelihood of godwit presence, suggesting a preference for shallower water areas. The number of sedge pools has a particularly strong impact on godwit presence, with coefficients of 96.42, 61.61, and 1.32 × 10^8, highlighting the critical role of these wetland features. Lastly, pool cover (0.94%) slightly decreases the odds of godwit presence, implying that less pool cover is preferred. These results provide valuable insights into the habitat preferences and environmental dependencies of black-tailed godwits, guiding conservation efforts to maintain and enhance suitable habitats for this species.

# The full model offers a more comprehensive analysis by including a broader range of variables. It still considers elevation (1.022 m), but also adds details on sward height with different levels (5-10 cm at 4.048, 10-20 cm at 1.867, and 20-40 cm at 1.956), willow cover (0.909 % of Salix spp), birch cover (1.066 %), Juncus cover (0.995 %), sand and gravel cover (0.817 %), number of drainage ditches (0.745), water table depth (0.982 cm), distance to the nearest hayfield (0.998 m), number of sedge pools (59.085), and pool cover (0.934 %).

# Extract coefficients from the GLM
coefficients <- summary(final_glm)$coefficients

# Calculate log-odds for each level of num_sedgepools
log_odds <- list(
  num_sedgepools0 = coefficients["(Intercept)", "Estimate"],
  num_sedgepools1 = coefficients["(Intercept)", "Estimate"] + coefficients["num_sedgepools1", "Estimate"],
  num_sedgepools2 = coefficients["(Intercept)", "Estimate"] + coefficients["num_sedgepools2", "Estimate"],
  num_sedgepools3 = coefficients["(Intercept)", "Estimate"] + coefficients["num_sedgepools3", "Estimate"]
)

# Convert log-odds to probabilities using the logistic function
probabilities <- lapply(log_odds, function(logit) {
  exp(logit) / (1 + exp(logit))
})

# Combine into a data frame for easy viewing
results <- data.frame(
  Level = c("num_sedgepools0", "num_sedgepools1", "num_sedgepools2", "num_sedgepools3"),
  LogOdds = unlist(log_odds),
  Probability = unlist(probabilities)
)

# View the results
print(results)

# The consistent significance of elevation and birch cover across both models highlights these variables as strong predictors of godwit presence. Higher elevations and greater birch cover appear to be favorable conditions for godwits. Water table depth and the number of sedge pools also play a crucial role, indicating the importance of wetter areas and specific wetland features for godwit habitat.

library(patchwork)

# Adjusting each plot
elevation_plot <- ggplot(godwits, aes(x = elevation, y = as.numeric(god_occur))) + 
  geom_point(position = "jitter", color = "black") +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = TRUE) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Absent", "Present")) + 
  theme_classic() +
  custom_theme3() +
  labs(x = "Elevation (m above sea level)", y = "Black-tailed Godwit") +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

library(ggtext)

birch_cover_plot <- ggplot(godwits, aes(x = birch_cover, y = as.numeric(god_occur))) + 
  geom_point(position = "jitter", color = "black") +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = TRUE) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Absent", "Present")) +
  theme_classic() +
  custom_theme3() +
  labs(x = "% cover of dwarf Birch (<i>Betula nana</i>)", y = "") +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_markdown())

water_table_depth_plot <- ggplot(godwits, aes(x = water_table_depth, y = as.numeric(god_occur))) + 
  geom_point(position = "jitter", color = "black") +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = TRUE) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Absent", "Present")) +
  theme_classic() +
  custom_theme3() +
  labs(x = "Depth of water table (cm)", y = "Black-tailed Godwit") +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

pool_cover_plot <- ggplot(godwits, aes(x = pool_cover, y = as.numeric(god_occur))) + 
  geom_point(position = "jitter", color = "black") +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = TRUE) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Absent", "Present")) +
  theme_classic() +
  custom_theme3() +
  labs(x = "% cover of pools within the patch", y = "") +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# Combining plots using patchwork
aim1_combined_plot <- elevation_plot +
  birch_cover_plot +
  water_table_depth_plot +
  pool_cover_plot +
  plot_layout(ncol = 2)

# Display the combined plot
aim1_combined_plot # use in presentation

# Display the plots
elevation_plot
birch_cover_plot
water_table_depth_plot
pool_cover_plot

# Count occurrences of num_sedgepools vs god_occur
occurrences <- godwits %>%
  group_by(num_sedgepools, god_occur) %>%
  summarise(count = n(), .groups = 'drop') # Count the number of occurrences

print(occurrences)

# Add new row to occurrences dataframe
occurrences <- bind_rows(
  occurrences,
  data.frame(num_sedgepools = "3", god_occur = 0, count = 0) # New row
)

print(occurrences)

num_sedgepools_plot <- ggplot(occurrences, aes(x = num_sedgepools, y = count, fill = factor(god_occur))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("0" = "gray", "1" = "red"), labels = c("Absent", "Present")) +
  theme_classic() +
  custom_theme() +
  labs(
    x = "Number of Pools with sedge plants growing in the shallows",
    y = "Count",
    fill = "Black-tailed Godwit"
  ) +
  theme(
    legend.position = c(1, 1), # Position the legend at the top-right corner
    legend.justification = c(1, 1) # Align legend relative to its top-right corner
  )

num_sedgepools_plot

# Create a summary table
summary_table <- godwits %>%
  group_by(num_sedgepools, god_occur) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = god_occur, values_from = count, values_fill = 0) %>%
  rename("Absent" = `0`, "Present" = `1`)

# View the summary table
summary_table

# Save the plots
ggsave("figures/aim1/elevation_plot.png", plot = elevation_plot, width = 9, height = 6)
ggsave("figures/aim1/birch_cover_plot.png", plot = birch_cover_plot, width = 9, height = 6)
ggsave("figures/aim1/water_table_depth_plot.png", plot = water_table_depth_plot, width = 9, height = 6)
ggsave("figures/aim1/num_sedgepools_plot.png", plot = num_sedgepools_plot, width = 9, height = 6)
ggsave("figures/aim1/pool_cover_plot.png", plot = pool_cover_plot, width = 9, height = 6)


# Create a tidy dataframe from your updated regression results
tidy_glm_1 <- tibble(
  term = c("(Intercept)", "Elevation", "Birch Cover", "Water Table Depth", 
           "Num Sedge Pools 1", "Num Sedge Pools 2", "Num Sedge Pools 3", "Pool Cover"),
  estimate = c(-0.442, 0.01707, 0.02681, -0.01767, 4.569, 4.121, 18.70, -0.06146),
  std.error = c(0.7113, 0.007873, 0.01074, 0.005227, 1.064, 1.519, 1345, 0.02947),
  statistic = c(-0.621, 2.168, 2.497, -3.381, 4.295, 2.712, 0.014, -2.086),
  p.value = c("0.534", "< 0.05", "< 0.05", "< 0.001", "< 0.001", "< 0.01", "0.989", "< 0.05")
)

# Create a formatted gt table
glm_table_1 <- gt(tidy_glm_1) %>%
  gt_theme_pff() %>%
  fmt_number(columns = c(estimate, std.error, statistic), decimals = 2) %>%
  fmt_number(columns = "std.error", decimals = 0, rows = 7) %>%  # Forces 1345 to show as 1345
  cols_label(
    term = "Predictor",
    estimate = "Estimate (β)",
    std.error = "Standard Error",
    statistic = "z-Value",
    p.value = "p-Value"
  ) %>%
  tab_options(
    table.width = px(550)  # Adjust width for better readability
  ) %>%
  opt_table_lines()

# Display table
glm_table_1 # Generalized Linear Model Results: Influence of Environmental Variables on Black-tailed Godwit presence



