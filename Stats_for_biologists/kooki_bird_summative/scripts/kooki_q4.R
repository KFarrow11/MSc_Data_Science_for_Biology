# KOOKI_4 = Does chick productivity of wild birds vary with the length of the breeding season? ----

# 🚚 LOAD R OBJECTS AND FUNCTIONS ----
# Import tidied data and functions
source("scripts/clean_kooki.R")

# filtering ----
kooki_breed <- dplyr::select(.data = kooki, # dplyr:: to avoid MASS conflict during linear model construction
                        wild, breeding_period)# variables want to select.
kooki_breed

# DESCRITIVE STATS ----
# Group the data by bird and calculate the mean number of chicks for per bird environment
kooki_breed_summary <- kooki_breed %>%
  summarise(n=n(),
            max=max(breeding_period),# max(1.79)
            min=min(breeding_period),# min(-1.72)
            range=max(breeding_period)-min(breeding_period), 
            # subtracting the smallest breeding_period value from the largest breeding_period value to give the range
            # range(3.51)
            mean=mean(breeding_period), # mean(0.0286)
            sd=sd(breeding_period), # standard deviation(1)
            se=sd/sqrt(n()), # standard error (0.19)
            median=median(breeding_period), # median(0.08)
            ci_lower = mean - 1.96 * se, 
            # Lower bound of the 95% CI (-0.34)
            ci_upper = mean + 1.96 * se) 
            # Upper bound of the 95% CI (0.4)

kooki_breed_summary

# ADD TIMEFRAME ----
# Create a sequence of years starting from 1980, with length equal to the number of rows in df 
kooki_breed$year <- seq(1980, by = 1, length.out = nrow(kooki_breed))
kooki_breed

# relocate year to last column
kooki_breed <- kooki_breed %>% 
  pivot_longer(cols = wild, names_to = "bird", 
               values_to = "chicks") 

kooki_breed <- kooki_breed[, c("bird", "year", "breeding_period", "chicks")]
kooki_breed

# PLOT ----
kooki_breed_hist <- ggplot(kooki_breed, aes(x = breeding_period)) +
  geom_histogram(color= "black", fill= "#5885AF", binwidth = 0.5) +
  theme_classic()+
  labs(x = "Mean no. of chicks produced per pair ",
       y = "Count")+
  custom_theme()

kooki_breed_hist
ggsave("figures/kooki_breed_hist.png", dpi = 400)


breed_results_plot <- ggplot(kooki_breed, aes(x = breeding_period, y = chicks)) +
  geom_point(aes(size = 3)) +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  theme_classic() +
  labs(x = "Length of breeding period (standardised)",
       y = "Mean no. of chicks produced per pair") +
  custom_theme()

breed_results_plot
ggsave("figures/breed_results_plot.png", dpi = 400)

# TESTING ----
kooki_breed

# Fit the model
model <- lm(chicks ~ breeding_period, data = kooki_breed)
summary(model)
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      0.46702    0.01759  26.550  < 2e-16 ***
# breeding_period  0.06815    0.01788   3.811 0.000729 ***
# Residual standard error: 0.09469 on 27 degrees of freedom
# Multiple R-squared:  0.3497,	Adjusted R-squared:  0.3257 
# F-statistic: 14.52 on 1 and 27 DF,  p-value: 0.0007285 < 0.001

# y = 9x + 118.4 R² = 0.84 p < 0.001

performance::check_model(model)






# results plot ----
# Scatter plot with regression line
breed_results_plot <- ggplot(kooki_breed, aes(x = breeding_period, y = chicks)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_classic() +
  labs(x = "Length of breeding period (standardised)",
       y = "Mean no. of chicks produced per pair") +
  custom_theme()

breed_results_plot
ggsave("figures/breed_results_plot.png", dpi = 400)
