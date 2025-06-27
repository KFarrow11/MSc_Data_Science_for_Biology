# AIM ----
# which environmental conditions influence godwit density? 
# CLEANING ----
# DATA + PACKAGES
source("scripts/explore_data/all_stats.R") # descriptive stats
gt_table2 # descriptive stats table

source("scripts/explore_data/lookup.R") # initial scatter plots
glimpse(godwit_den)

# OBSERVATIONS ----
density_hist

# density vs [environmental variables] plots
pairs(godwit_den[,c(2:13)])

# multiple regression ----
## full model ----
density_lm1 <- lm(god_density ~ elevation + swardht + willow_cover + birch_cover +
                   juncus_cover + sand_gravel_cover + num_ditches +
                   water_table_depth + hayfield + num_sedgepools + pool_cover,
                   data = godwit_den)

summary(density_lm1) # r^2 = 8.5%
performance::check_model(density_lm1)

## model 2 ----
density_lm2 <- lm(god_density ~  swardht + hayfield,
                  data = godwit_den)

summary(density_lm2) # r^2 = 14.6%
anova(density_lm2, test = "Chisq") # hayfield = 0.03
performance::check_model(density_lm2)

# The multivariate linear regression model evaluating the effect of sward height and hayfield distance on godwit density revealed significant findings. Higher levels of sward height (swardht4) were associated with a decrease in godwit density (p = 0.0358), indicating a negative impact on godwit density. Additionally, greater distance from hayfields was also significantly associated with a decrease in godwit density (p = 0.0268), suggesting that godwits prefer areas closer to hayfields. However, not all sward height levels (swardht2 and swardht3) showed a clear impact on godwit density, as their coefficients were not statistically significant. Overall, these results highlight that both sward height and proximity to hayfields are important factors influencing godwit density.
 
# model 2 with interaction term ----
density_lm2b <- lm(god_density ~  swardht*hayfield,
                  data = godwit_den)

summary(density_lm2b) # r^2 = 13% = WORSE
anova(density_lm2b, test = "Chisq")
performance::check_model(density_lm2b) 

# model 3 ----
density_lm_3 <- lm(god_density ~ hayfield,
                   data = godwit_den)

summary(density_lm_3) # r^2 = 10% = WORSE
anova(density_lm_3, test = "Chisq")
performance::check_model(density_lm_3)

# Final model ----
density_lm_4 <- lm(god_density ~  swardht + hayfield + juncus_cover,
                  data = godwit_den)

summary(density_lm_4) # r^2 = 18%
anova(density_lm_4, test = "Chisq") # hayfield = 0.03
performance::check_model(density_lm_4)


ggplot(godwit_den, aes(x = juncus_cover, y = god_density)) +
  geom_point() +
  theme_classic()

# juncus_cover does contribute to explaining variability in god_density, even if its effect isn’t strongly statistically significant.

# **Reasons to Keep juncus_cover:**
# Even though the p-value (0.1019) isn’t below 0.05, it’s close to 0.1, which could be considered marginally relevant in some ecological studies. Removing it leads to a drop in **Adjusted R-squared** (from 0.1806 to 0.146), meaning the model loses explanatory power.


# **Reasons to Remove juncus_cover:**.
# If the relationship between **juncus_cover** and **god_density** isn’t supported by strong biological or ecological reasoning, removing it might simplify your model. If multicollinearity is a concern, checking Variance Inflation Factor (VIF) could help decide whether its presence affects other predictors.

# Checking for Variance Inflation Factor (VIF) ----
# The VIF helps assess multicollinearity in your model—values above 5 or 10 might indicate problematic correlation among predictors.
library(car)

# Calculate VIF for each predictor
vif(density_lm_4)

# Based on the available evidence, removing juncus_cover from your model seems reasonable. 
# Statistical Significance: The p-value (0.1019) suggests that juncus_cover is not a strong predictor of god_density. Model Fit: Removing juncus_cover slightly reduces Adjusted R-squared (from 0.1806 to 0.146), but this drop isn’t substantial. Multicollinearity Check: Your VIF values are low, meaning juncus_cover isn’t causing instability in the model. Ecological Evidence: While Juncus effusus plays a role in wetland ecosystems, there is limited research on its direct impact on godwit density in Iceland2. If local observations don’t suggest a strong relationship, removing it could simplify your model without losing much explanatory power.

# Final model ----
density_lm_final <- lm(god_density ~ swardht + hayfield, data = godwit_den)

summary(density_lm_final)

# The multivariate linear regression model evaluating the effect of sward height and hayfield distance on godwit density revealed significant findings. Higher levels of sward height (swardht4) were associated with a decrease in godwit density (p = 0.0358), indicating a negative impact on godwit density. Additionally, greater distance from hayfields was also significantly associated with a decrease in godwit density (p = 0.0268), suggesting that godwits prefer areas closer to hayfields. However, not all sward height levels (swardht2 and swardht3) showed a clear impact on godwit density, as their coefficients were not statistically significant. Overall, these results highlight that both sward height and proximity to hayfields are important factors influencing godwit density.

# Define custom labels for swardht
custom_labels <- c(`1` = "0-5 cm", `2` = "5-10 cm", `3` = "10-20 cm", `4` = "20-40 cm")

# Create the updated ggplot
density_lm_plot <- ggplot(godwit_den, aes(x = hayfield, y = god_density, color = swardht)) +
  geom_point(size = 2.5, 
             position = position_jitter(width = 0.3, height = 0.3)) +  # Increase jitter effect
  scale_color_manual(
    values = c("#A8E6A0", "#6ABF69", "#2F8F2F", "#145214"),  # Shades of green mapped to swardht levels
    labels = custom_labels  # Apply custom labels
  ) +
  labs(
    x = "Distance to Hayfield (m)",
    y = "Black-tailed Godwit Density (number of birds/ha)",
    color = "Sward Grass Height"  # Legend title
  ) +
  ggtitle("Effect of hayfield distance and Sward grass height on Black-tailed Godwit population density") +
  theme_classic() +
  theme(
    legend.position = c(0.85, 0.85)  # Position legend inside the top right corner
  ) +
  custom_theme3()

# Display the plot
density_lm_plot

# The population density of Black-tailed Godwits declines with increasing distance from the hayfield, while taller sward grass is generally associated with lower densities, suggesting a preference for shorter vegetation in habitat selection.

# The results indicate that **Black-tailed Godwit density** is highest **closer to hayfields** and in areas with **shorter sward grass height** (0-5 cm and 5-10 cm). As **distance from hayfields increases beyond 500 meters**, godwit density declines, suggesting a preference for **foraging and nesting near managed agricultural landscapes**. The color-coded data further highlights a **negative relationship between grass height and godwit density**, with taller sward (10-20 cm and 20-40 cm) associated with lower bird presence. These findings have important ecological implications, emphasizing the role of **grassland management** in maintaining suitable habitat for godwits. **Controlled mowing or grazing** could help sustain optimal conditions, preventing encroachment of tall vegetation that may reduce foraging efficiency or conceal predators. The observed trends reinforce the necessity of **wetland conservation efforts** to balance agricultural land use with the preservation of key breeding and feeding grounds for this species.

# LM results table
tidy_lm <- tibble(
  term = c("(Intercept)", "swardht2", "swardht3", "swardht4", "hayfield"),
  estimate = c(2.29, -0.77, -0.90, -1.22, -0.0007),
  std.error = c(0.52, 0.62, 0.58, 0.56, 0.0003),
  statistic = round(c(4.43, -1.23, -1.57, -2.17, -2.29), 2),  # Ensures 2dp rounding
  p.value = c("< 0.001", "0.225", "0.125", "< 0.05", "< 0.05")
)

# Create a formatted gt table
lm_table <- gt(tidy_lm) %>%
  gt_theme_pff() %>%
  fmt_number(columns = c(estimate, std.error), decimals = 4) %>%  # Keeping consistent formatting
  cols_label(
    term = "Predictor",
    estimate = "Estimate (β)",
    std.error = "Standard Error",
    statistic = "t-Value",
    p.value = "p-Value"
  ) %>%
  opt_table_lines() %>%
  tab_options(
    table.width = px(500)  # Adjust width for better readability
  )

# Display the table
lm_table # Multivariate Linear Regression Results: Influence of Environmental Conditions on Godwit Density

# Save the GLM results table
gtsave(lm_table, "tables/lm_results.png")

# _________________________________________________________----
# Density GLM ----

## Full glm model
full_model <- glm(god_density ~ swardht + willow_cover + birch_cover + juncus_cover + sand_gravel_cover + num_ditches + water_table_depth + hayfield + num_sedgepools + pool_cover,
                  family = poisson(link = "log"),
                  data = godwit_den)

summary(full_model)
anova(full_model, test = "Chisq")
performance::check_model(full_model)


glm(god_density ~ ., data = godwit_den) %>%
  step(direction = "backward", trace = 0) %>%
  summary()

# GLM2 ----
den_glm2 <- glm(god_density ~ swardht + hayfield + juncus_cover + birch_cover, 
                                 family = poisson(link = "log"), 
                                 data = godwit_den)

summary(den_glm2)
anova(den_glm2, test = "Chisq")
performance::check_model(den_glm2)

# GLM3 ----
den_glm3 <- glm(god_density ~ swardht + hayfield + juncus_cover, 
                family = poisson(link = "log"), data = godwit_den)

summary(den_glm3)
anova(den_glm3, test = "Chisq")
performance::check_model(den_glm3)
AIC(den_glm2, den_glm3)  # Compare AIC values

stepAIC(den_glm2, direction = "backward")


# GLM4 ----
den_glm4 <- glm(god_density ~ swardht + hayfield, 
                  family = poisson(link = "log"), 
                  data = godwit_den)

summary(den_glm4)

den_glm5 <- glm(god_density ~ swardht*hayfield, 
                family = poisson(link = "log"), 
                data = godwit_den)

summary(den_glm5)

final_model <- glm(god_density ~ hayfield, 
                   family = poisson(link = "log"), 
                   data = godwit_den)

summary(final_model) # Hayfield = 0.0262 *
anova(final_model, test = "Chisq") # Hayfield = 0.01342 *
performance::check_model(final_model)

dispersion_ratio <- sum(residuals(final_model, type = "deviance")^2) / final_model$df.residual
print(dispersion_ratio)

quasi_poisson_model <- glm(god_density ~ hayfield, 
                           family = quasipoisson(link = "log"), 
                           data = godwit_den)
summary(quasi_poisson_model)

plot(residuals(quasi_poisson_model, type = "pearson"), main = "Residuals - Quasi-Poisson")

qqnorm(residuals(quasi_poisson_model))
qqline(residuals(quasi_poisson_model))

shapiro.test(residuals(quasi_poisson_model))
#-----------------------------
# 5. Model Diagnostics
#-----------------------------
final_glm_model <- glm(god_density ~ hayfield, 
                           family = quasipoisson(link = "log"), 
                           data = godwit_den)

# Check for overdispersion
overdispersion_check <- function(model) {
  dev <- deviance(model)
  df <- df.residual(model)
  ratio <- dev / df
  cat("Residual deviance:", round(dev, 2), "\n")
  cat("Degrees of freedom:", df, "\n")
  cat("Dispersion ratio:", round(ratio, 2), "\n")
  if (ratio > 1.5) cat("Possible overdispersion detected.\n")
}
overdispersion_check(final_model)


# Hayfields are a significant predictor of black-tailed godwit density, offering a rich supply of invertebrate prey that serves as vital feeding grounds. This preference aligns with habitat selection principles, indicating that organisms are drawn to environments that enhance their survival and reproductive success. Optimal foraging theory further explains that godwits maximize energy intake by foraging close to hayfields while minimizing effort. Statistical analysis supports this relationship, with a dispersion ratio of 0.63 validating the Poisson model and indicating no overdispersion. Consequently, the higher densities of godwits observed near hayfields highlight the ecological significance of agricultural landscapes.
# Interestingly, factors such as sward height and the number of sedge pools did not exhibit statistically significant effects on godwit density. This indicates that elements of vegetation structure, like sward height, and the presence of sedge pools may not play a significant role in this context, at least not in a way that our model was able to capture. This finding may reflect the spatial distribution of these resources or suggest that other unmeasured ecological factors, such as prey availability or disturbance regimes, could be exerting a stronger influence on godwit densities than vegetation height or water features in this case.
# The findings highlight the ecological importance of hayfields as essential foraging habitats for black-tailed godwits. In contrast, other habitat attributes, including vegetation structure and the presence of sedge pools, seem to exhibit a weak direct correlation with godwit density in this study.

#-----------------------------
# 6. Visualize Effects
#-----------------------------
density_glm_plot <- ggplot(godwit_den, aes(x = hayfield, y = god_density)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0.1), color = "black", alpha = 0.7) + 
  geom_smooth(method = "glm", method.args = list(family = poisson), 
              se = TRUE, color = "blue", fill = "lightblue", linetype = "solid") +
  theme_classic() + 
  custom_theme3() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
  ) +
  labs(
    title = "Effect of Hayfield distance on Black-tailed Godwit population density",
    x = "Distance to Hayfield (m)",
    y = "Black-tailed Godwit Density (number of birds/ha)"
  )

# Display plot
density_glm_plot

# The population density of Black-tailed Godwits declines as distance to Hayfield increases, with a fitted Poisson regression curve showing the trend and a shaded confidence interval.

summary(final_glm_model)

# Tidy and modify the GLM results
tidy_den_glm <- tidy(final_glm_model) %>% 
  mutate(estimate = round(estimate, 4), 
         std.error = round(std.error, 4), 
         statistic = round(statistic, 2), 
         p.value = ifelse(p.value < 0.001, "< 0.001", 
                          ifelse(p.value < 0.01, "< 0.01", 
                                 ifelse(p.value < 0.05, "< 0.05", round(p.value, 3)))))

# Create a gt table for GLM results
glm_table <- gt(tidy_den_glm) %>%
  gt_theme_pff() %>%
  opt_table_lines() %>%
  tab_options(
    table.width = px(500)  # Adjust width for better readability
  )

glm_table # GLM results: Influence of Environmental Conditions on Godwit Density
lm_table

# Save the GLM results table
gtsave(glm_table, "tables/den_glm_results.png")

