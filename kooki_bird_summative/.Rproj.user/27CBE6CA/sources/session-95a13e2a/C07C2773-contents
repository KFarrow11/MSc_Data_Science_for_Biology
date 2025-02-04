# KOOKI_5 = What are the effects of the food supplementation treatments on kooki supplementation productivity? ----
# One Way ANOVA

# levene_test(yields~variety, carrot)

# aov() = lsmodel01 <- aov(yields ~ variety, data = carrot) summary(lsmodel01)

# TukeyHSD(lsmodel01, conf.level=.95) 
# Perform a d Tukey test on our ANOVA, stored in lsmodel01 with confidence levels of 95%.

# ggplot(data = carrot_yields, aes(x = Variety, y = Yields)) + geom_boxplot()

# 🚚 LOAD R OBJECTS AND FUNCTIONS ----
# Import tidied data and functions
source("scripts/clean_kooki.R")

kooki

# FILTERING ----
kooki_sup <- dplyr::select(.data = kooki, # dplyr:: to avoid MASS conflict during linear model construction
                        no_sup, banana, mango, banana_mango)  # variables want to select.


kooki_sup <- kooki_sup %>%
  pivot_longer(cols = no_sup:banana:mango:banana_mango,
               names_to = "supplementation", # organise by food = no_sup:banana:banana_mango:mango
               values_to = "sup_chick") 
kooki_sup 

# DESCRITIVE STATS ----
# Group the data by food supplementation and calculate the mean number of chick for per food
kooki_sup_summary <- kooki_sup %>%
  group_by(supplementation) %>%
  summarise(n=n(),
            max=max(sup_chick), # max(0.97)
            min=min(sup_chick), # min(0.29)
            range=max(sup_chick)-min(sup_chick), 
            # range = no_sup(0.32), banana(0.5), mango(0.38), banana_mango(0.59)
            mean=mean(sup_chick), 
            # mean = no_sup(0.463), banana(0.573), mango(0.506), banana_mango(0.634)
            sd=sd(sup_chick), 
            # standard deviation = no_sup(0.0916), banana(0.132), mango(0.0934), banana_mango(0.129)
            se=sd/sqrt(n()), # standard error 
            # no_sup(0.02), banana(0.03), mango(0.02), banana_mango(0.02)
            median=median(sup_chick), 
            # median = no_sup(0.45), banana(0.57), mango(0.51), banana_mango(0.63)
            mode=mode(sup_chick), 
            # mode = no_sup(0.45), banana(0.64), mango(0.51), banana_mango(0.6)
            ci_lower = mean - 1.96 * se, 
            # Lower bound of the 95% CI
            # no_sup(0.43), banana(0.53), mango(0.47), banana_mango(0.59)
            ci_upper = mean + 1.96 * se) 
            # Upper bound of the 95% CI 
            # no_sup(0.50), banana(0.62), mango(0.54), banana_mango(0.68)
kooki_sup_summary

# PLOT ----
# Create the plot with custom colors 
sup_colours <- c("#90ADC6", "#DB1F48", "#FAD02C", "#76B947") #R Colours
kooki_sup

# Desired order for x-axis
desired_order_sup <- c("banana", "mango", "banana_mango", "no_sup")
# Update the factor levels
kooki_sup$supplementation <- factor(kooki_sup$supplementation, levels = desired_order_sup)

kooki_sup_plot <- kooki_sup %>% 
ggplot(aes(x = sup_chick, fill = supplementation)) + 
  geom_histogram(bins = 10, color = "black", position = "dodge") + 
  scale_fill_manual(values = sup_colours) + 
  facet_wrap(~supplementation) + 
theme_classic()+
  labs(x = "Mean no. of chicks produced per pair ",
       y = "Count")+
  custom_theme()

kooki_sup_plot 
ggsave("figures/kooki_sup_plot.png", dpi = 400)

# banana + combo = normally distributed
# mango + no sup = negative skewed

# TRANSFORM ----
## sqrt ----
kooki_sup_sqrt <- kooki_sup %>%
  mutate(sqrt_chick = sqrt(kooki_sup$sup_chick))
kooki_sup_sqrt

kooki_supsqrt_plot <- kooki_sup_sqrt %>%
  ggplot(aes(x=sqrt_chick, fill = supplementation)) + # chick
  geom_histogram(bins = 10, color = "black", position = "dodge") + 
  scale_fill_manual(values = sup_colours) + 
  facet_wrap(~supplementation) + 
  theme_classic()+
  labs(x = "Mean no. of chicks produced per pair ",
       y = "Count")+
  custom_theme()

kooki_supsqrt_plot
ggsave("figures/kooki_supsqrt_plot.png", dpi = 400)

## add 1 due to 0s ----
kooki_sup1 <- kooki_sup %>%
  mutate(chick1 = kooki_sup$sup_chick+1)
kooki_sup1

# +1 plot
kooki_sup1_plot <- kooki_sup1 %>%
  ggplot(aes(x=chick1, fill = supplementation)) + # chick
  geom_histogram(bins = 10, color = "black", position = "dodge") + 
  scale_fill_manual(values = sup_colours) + 
  facet_wrap(~supplementation) + 
  theme_classic()+
  labs(x = "Mean no. of chicks produced per pair ",
       y = "Count")+
  custom_theme()

kooki_sup1_plot 
ggsave("figures/kooki_sup1_plot.png", dpi = 400)

## log10 +1 ----
kooki_sup_log <- kooki_sup1 %>%
  mutate(chick_log = log10(kooki_sup1$chick1))
kooki_sup_log

# log10 +1 plot
kooki_suplog_plot <- kooki_sup_log %>%
  ggplot(aes(x=chick_log, fill = supplementation)) + # chick
  geom_histogram(bins = 10, color = "black", position = "dodge") + 
  scale_fill_manual(values = sup_colours) +
  facet_wrap(~supplementation) +
  theme_classic()+
  labs(x = "Mean no. of chicks produced per pair ",
       y = "Count")+
  custom_theme()

kooki_suplog_plot # improved distribution
ggsave("figures/kooki_suplog_plot.png", dpi = 400)

## sqrt +1 ----
kooki_sup_sqrt1 <- kooki_sup1 %>%
  mutate(sqrt_chick1 = sqrt(kooki_sup1$chick1))
kooki_sup_sqrt1

# Desired order for x-axis
desired_order_sup <- c("banana", "mango", "banana_mango", "no_sup")
# Update the factor levels
kooki_sup_sqrt1$supplementation <- factor(kooki_sup_sqrt1$supplementation, levels = desired_order_sup)

# sqrt+1 plot
kooki_supsqrt1_plot <- kooki_sup_sqrt1 %>%
  ggplot(aes(x=sqrt_chick1, fill = supplementation)) + # chick
  geom_histogram(bins = 10, color = "black", position = "dodge") + 
  scale_fill_manual(values = sup_colours) +
  facet_wrap(~supplementation) +
  theme_classic()+
  labs(x = "Mean no. of chicks produced per pair ",
       y = "Count")+
  custom_theme()

kooki_supsqrt1_plot
ggsave("figures/kooki_supsqrt1_plot.png", dpi = 400)

## transformation plots ----
kooki_supsqrt_plot # good 
kooki_supsqrt1_plot # best transformation
kooki_sup1_plot # no
kooki_suplog_plot # ok
# TESTING ----
kooki_sup_sqrt1
kooki_supsqrt1_plot
# ANOVA 
kooki_sup_model1 <- aov(sqrt_chick1 ~ supplementation, data = kooki_sup_sqrt1) 
summary(kooki_sup_model1)
#                   Df  Sum Sq  Mean Sq   F value   Pr(>F)    
# supplementation   3   0.07804 0.026012  12.64     3.54e-07 ***

# Perform a d Tukey test on our ANOVA, stored in lsmodel01 with confidence levels of 95%.
TukeyHSD(kooki_sup_model1, conf.level=.95) 
# no_sup-banana + banana_mango-mango = make a table

# Desired order for x-axis - Histogram3 ----
kooki_sup_desired_order <- c("no_sup", "banana", "mango", "banana_mango")

# Update the factor levels
kooki_sup_sqrt1$supplementation <- factor(kooki_sup_sqrt1$supplementation, levels = kooki_sup_desired_order)

kooki5_boxplot <- ggplot(data = kooki_sup, 
      aes(x = supplementation, 
           y = sup_chick,
           fill = supplementation)) + 
      geom_boxplot(width = 0.8)+
      scale_fill_manual(values = sup_colours) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_classic()+
      labs(y = "Mean no. of chicks produced per pair ",
           x = "Supplimentation")+
      custom_theme2()

kooki5_boxplot
ggsave("figures/kooki5_boxplot.png", dpi = 400) 
