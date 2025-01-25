# KOOKI_2 = Is there any evidence for changes in productivity over the study period among the wild birds? ----
# one sample t-test
#_________________________________________________________________________________________________----
# 🚚 LOAD R OBJECTS AND FUNCTIONS ----
# Import tidied data and functions
source("scripts/kooki_q1.R")
#_________________________________________________________________________________________________----
# filtering ----
kooki_wild
#_________________________________________________________________________________________________----
# ADD TIMEFRAME ----
# Create a sequence of years starting from 1980, with length equal to the number of rows in df 
kooki_wild$year <- seq(1980, by = 1, length.out = nrow(kooki_wild))
# relocate year to last column
kooki_wild <- kooki_wild[, c("year", "bird", "chicks")]
kooki_wild
#_________________________________________________________________________________________________----
# HISTOGRAMS ----
kooki_wild_plot <-
  ggplot(data = kooki_wild, aes(x=chicks)) + # chicks
  geom_histogram(bins = 10, binwidth = 0.05, color = "black", fill="darkgreen") + # normal distribution
  theme_classic()

kooki_wild_plot 
ggsave("figures/kooki_wild_plot.png", dpi = 400)
#_________________________________________________________________________________________________----
# DESCRITIVE STATS ----
# Group the data by bird and calculate the mean number of chicks for per bird environment
kooki_wild_summary <- kooki_wild %>%
  summarise(n=n(),
            max=max(chicks),# max(0.69)
            min=min(chicks),# min(0.22)
            range=max(chicks)-min(chicks), 
            # subtracting the smallest chicks value from the largest chicks value to give the range
            # range(0.47)
            mean=mean(chicks), # mean(0.469)
            sd=sd(chicks), # standard deviation(0.115)
            se=sd/sqrt(n()), # standard error (0.02)
            median=median(chicks), # median(0.49)
            mode=mode(chicks), # mode(0.54)
            ci_lower = mean - 1.96 * se, # Lower bound of the 95% CI (0.43)
            ci_upper = mean + 1.96 * se) # Upper bound of the 95% CI (0.51)


kooki_wild_summary

#_________________________________________________________________________________________________----
# PLOT ----
kooki_year_plot <- kooki_wild %>%
  ggplot(aes(x=year, y=chicks)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  scale_x_continuous(name = "Year", breaks = seq(1980, 2008, by = 2)) + 
  scale_y_continuous(name = "Mean no. of chicks produced per pair", limits = c(0, 0.80), breaks = seq(0, 0.80, by = 0.20)) +
  theme_classic() +
  custom_theme()

kooki_year_plot
ggsave("figures/kooki_year_plot.png", dpi = 400)
#_________________________________________________________________________________________________----
# TESTING/MODELLING ----
## wild_year_model1 ---- 
wild_year_model1 <- lm(chicks ~ year, data = kooki_wild)
wild_year_model1 # intercept (-7.477567) year(0.003985)

performance::check_model(wild_year_model1)

summary(wild_year_model1)
# lm(formula = chicks ~ year, data = kooki_wild)
#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.240995 -0.077010  0.007197  0.094946  0.165241 

#Coefficients:
#             Estimate    Std. Error  t value Pr(>|t|)
#(Intercept)  -7.477567   4.966572    -1.506    0.144
#year         0.003985    0.002491    1.600     0.121

#Residual standard error: 0.1122 on 27 degrees of freedom
#Multiple R-squared: 0.08661,	Adjusted R-squared: 0.05278, 
#F-statistic: 2.56 on 1 and 27 DF,  p-value: 0.1212

# run boxcox, suggest transformation
MASS::boxcox(wild_year_model1) # MASS Package = suggest a power (𝜆 ≈ 0.5) transformation

wild_year_model2 <- lm((chicks^0.5 - 1) / 0.5 ~ year, data = kooki_wild)

performance::check_model(wild_year_model2) # non transformation better when tested for normality and linearity
summary(wild_year_model2) # make into a table

#Residuals:
#Min        1Q    Median        3Q       Max 
#-0.40916 -0.10857  0.01971  0.14536  0.23381

#Coefficients:
#             Estimate    Std. Error t value  Pr(>|t|)
#(Intercept) -12.186374   7.553302    -1.613    0.118
#year         0.005790    0.003788    1.529     0.138

#Residual standard error: 0.07768 on 27 degrees of freedom
#Multiple R-squared: 0.07964,	Adjusted R-squared:  0.04555
#F-statistic: 2.336 on 1 and 27 DF,  p-value: 0.138

## wild_year_model1 analysis ----
# continue with testing with non-transformed data 😊
wild_year_model1 

ggplot(wild_year_model1, aes(x = .fitted, y = .resid)) +
  geom_point()+
  theme_classic()

get_regression_points(wild_year_model1)
# export regression points in a table of outcome/response variable, 
# all explanatory/predictor variables, 
# the fitted/predicted value, 
# and residuals.
ggplot(wild_year_model1, aes(x = .fitted, y = .resid)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

summary(wild_year_model1)
# Call:lm(formula = chicks ~ year, data = kooki_wild)
# Residuals:Min        1Q         Median    3Q        Max 
#           -0.240995 -0.077010   0.007197  0.094946  0.165241 

# Coefficients: Estimate    Std. Error t value  Pr(>|t|)
# (Intercept)   -7.477567   4.966572    -1.506  0.144
# year          0.003985    0.002491    1.600   0.121

# Residual standard error: 0.1122 on 27 degrees of freedom
# Multiple R-squared:  0.08661,	Adjusted R-squared:  0.05278 
# F-statistic:  2.56 on 1 and 27 DF,  p-value: 0.1212

# report = 𝑦 = 0.004× − 7.5 , r2 = 0.87, p > 0.1

# CORRELATION TEST ON wild_year_model1----
# perform a correlation test on year and chicks using Pearson's correlation coefficient. 
correlation_year <- cor.test(kooki_wild$year, kooki_wild$chicks, method = "pearson") 
correlation_year
View(kooki_wild)
nrow(kooki_wild)
# Pearson's product-moment correlation
# data:  kooki_wild$year and kooki_wild$chicks
# t = 1.6, df = 27, p-value = 0.1212
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval: -0.0809490  0.5964603
# sample estimates:cor 0.2942877 

# report = r = 0.29, n = 29, p > 0.1
