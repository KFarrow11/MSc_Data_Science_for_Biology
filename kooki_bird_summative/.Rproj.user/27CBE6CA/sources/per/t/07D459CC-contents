# KOOKI_1 = How does chick productivity of wild birds compare to that of the captive birds? ----
# two sample t test
#_________________________________________________________________________________________________----
# 🚚 LOAD R OBJECTS AND FUNCTIONS ----
# Import tidied data and functions
source("scripts/clean_kooki.R")
#_________________________________________________________________________________________________----
# filtering ----
kooki1 <- dplyr::select(.data = kooki, # dplyr:: to avoid MASS conflict during linear model construction
                        wild, captive)  # variables want to select.

kooki1
kooki_wc <- kooki1 %>% pivot_longer(cols = wild:captive,
                                    names_to = "bird", # organise by bird = wild:captive
                                    values_to = "chicks")
kooki_wc
#_________________________________________________________________________________________________----
# DESCRITIVE STATS ----
# Group the data by bird and calculate the mean number of chicks for per bird environment

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

kooki_wc_summary <- kooki_wc %>%
  group_by(bird) %>%
  summarise(n=n(), # 29
            max=max(chicks),# max(0.81)
            min=min(chicks), # min(0.01)
            range=max(chicks)-min(chicks),
            # subtracting the smallest chicks value from the largest chicks value to give the range
            # range = captive(0.8), wild(0.47)
            mean=mean(chicks), # mean = captive(0.121), wild(0.469)
            sd=sd(chicks), # standard deviation = captive(0.196), wild(0.115)
            se=sd/sqrt(n()), # standard error = captive(0.04), wild(0.02)
            median=median(chicks), # median = captive(0.05), wild(0.49)
            mode=mode(chicks), # mode = captive(0.02), wild(0.54)
            ci_lower = mean - 1.96 * se, # Lower bound of the 95% CI = captive(0.05), wild(0.42)
            ci_upper = mean + 1.96 * se) # Upper bound of the 95% CI = captive(0.19), wild(0.51)

kooki_wc_summary
#_________________________________________________________________________________________________----
# look at distribution of data - Histogram ----
# Desired order for x-axis
desired_order_wc <- c("wild", "captive")

# Update the factor levels
kooki_wc$bird <- factor(kooki_wc$bird, levels = desired_order_wc)

kooki_wc_histogram <- ggplot(kooki_wc, aes(x = chicks, fill= bird)) +
  geom_histogram(binwidth = 0.05, bins = 10, color = "black") +
  scale_fill_manual(values = c("wild" = "darkgreen", "captive" = "mediumpurple4"), 
                    labels = c("wild" = "Wild", "captive" = "Captive")) + # Rename fill categories
  facet_wrap(~bird) + # Rename the categories on the x axis 
  theme_classic()+
  labs(x = "Mean no. of chicks produced per pair ",
       y = "Count")+
  custom_theme()

kooki_wc_histogram # wild (normal😀), captive (not normally distributed = transform😱)
ggsave("figures/kooki_wc_histogram.png", dpi = 400) # use in process

#_________________________________________________________________________________________________----
# WILD ----
kooki_wild <- kooki_wc %>%  # pipe kooki data frame into next function
  filter(bird == "wild") # filter the piped data frame and only keep bird wild
kooki_wild

kooki_wild_plot <-
  ggplot(data = kooki_wild, aes(x=chicks)) + # chicks
  geom_histogram(binwidth = 0.05, bins = 10, color = "black", fill="darkgreen") +
  theme_classic()
kooki_wild_plot # pretty good for distribution 📊
#_________________________________________________________________________________________________----
# CAPTIVE ----
kooki_captive <- kooki_wc %>%  # pipe kooki data frame into next function
  filter(bird == "captive") # filter the piped data frame and only keep bird wild
kooki_captive

kooki_captive_plot <- kooki_captive %>%
  ggplot(aes(x=chicks)) + # chicks
  geom_histogram(binwidth = 0.05, bins = 10, color = "black", fill="mediumpurple4") +
  theme_classic()
kooki_captive_plot # negative skew 📉
#_________________________________________________________________________________________________----
# TRANSFORMING ----
## sqrt ----
kooki_wc_sqrt <- kooki_wc %>%
  mutate(sqrt_chicks = sqrt(kooki_wc$chicks))
kooki_wc_sqrt

kooki_sqrt_plot <- kooki_wc_sqrt %>%
  ggplot(aes(x=sqrt_chicks, fill = bird)) + # chicks
  geom_histogram(binwidth = 0.05, bins = 10) +
  scale_fill_manual(values = c("wild" = "darkgreen", "captive" = "mediumpurple4")) +
  facet_wrap(~bird, scales = "free_x") +
  theme_classic()
kooki_sqrt_plot

## add 1 due to 0s ----
kooki_wc1 <- kooki_wc %>%
  mutate(chicks1 = kooki_wc$chicks+1)
kooki_wc1

# +1 plot
kooki_wc1_plot <- kooki_wc1 %>%
  ggplot(aes(x=chicks1, fill = bird)) + # chicks
  geom_histogram(binwidth = 0.05, bins = 10) +
  scale_fill_manual(values = c("wild" = "darkgreen", "captive" = "mediumpurple4")) +
  facet_wrap(~bird, scales = "free_x") +
  theme_classic()
kooki_wc1_plot 

## log10 +1 ----
kooki_wc_log <- kooki_wc1 %>%
  mutate(chicks_log = log10(kooki_wc1$chicks1))
kooki_wc_log

# log10 +1 plot
kooki_log_plot <- kooki_wc_log %>%
  ggplot(aes(x=chicks_log, fill = bird)) + # chicks
  geom_histogram(binwidth = 0.05, bins = 10) +
  scale_fill_manual(values = c("wild" = "darkgreen", "captive" = "mediumpurple4")) +
  facet_wrap(~bird, scales = "free_x") +
  theme_classic()
kooki_log_plot # improved distribution

## sqrt +1 ----
kooki_wc_sqrt1 <- kooki_wc1 %>%
  mutate(sqrt_chicks1 = sqrt(kooki_wc1$chicks1))
kooki_wc_sqrt1

# sqrt+1 plot
kooki_sqrt1_plot <- kooki_wc_sqrt1 %>%
  ggplot(aes(x=sqrt_chicks1, fill = bird)) + # chicks
  geom_histogram(binwidth = 0.05, bins = 10) +
  scale_fill_manual(values = c("wild" = "darkgreen", "captive" = "mediumpurple4")) +
  facet_wrap(~bird, scales = "free_x") +
  theme_classic()
kooki_sqrt1_plot

## transformation plots ----
kooki_sqrt_plot # good =  best transformation
kooki_sqrt1_plot # no
kooki_wc1_plot # no
kooki_log_plot # ok
#_________________________________________________________________________________________________----
# Combined Log10 histogram 2 ----
# Update the factor levels
kooki_wc_sqrt$bird <- factor(kooki_wc_sqrt$bird, levels = desired_order_wc)

# Create the histogram plot with the desired order
kooki_sqrt_histogram <- ggplot(kooki_wc_sqrt, aes(x = sqrt_chicks, fill = bird)) +
  geom_histogram(binwidth = 0.05, bins = 10, color = "black") +
  scale_fill_manual(values = c("wild" = "darkgreen", "captive" = "mediumpurple4")) +
  facet_wrap(~bird) +
  theme_classic()+
  labs(x = "Mean no. of chicks produced per pair ",
       y = "Count")+
  custom_theme()

kooki_wc_histogram # raw data = original
ggsave("figures/kooki_wc_histogram.png", dpi = 400)

kooki_sqrt_histogram # sqrt
ggsave("figures/kooki_sqrt_histogram.png", dpi = 400)

#_________________________________________________________________________________________________----
# sqrt Descriptive Stats ----
kooki_sqrt_summary <- kooki_wc_sqrt %>%
  group_by(bird) %>%
  summarise(n=n(), # 29
            max=max(sqrt_chicks),# max wild(0.831), captive(0.9)
            min=min(sqrt_chicks), # wild(0.469), captive(0.1)
            range=max(sqrt_chicks)-min(sqrt_chicks),
            # subtracting the smallest sqrt_chicks value from the largest sqrt_chicks value to give the range
            # range = captive(0.8), wild(0,362)
            mean=mean(sqrt_chicks), # mean = captive(0.283), wild(0.679)
            sd=sd(sqrt_chicks), # standard deviation = captive(0.207), wild(0.0873)
            se=sd/sqrt(n()), # standard error = captive(0.04), wild(0.02)
            median=median(sqrt_chicks), # median = captive(0.224), wild(0.7)
            mode=mode(sqrt_chicks)) # mode = captive(0.141), wild(0.735)

kooki_sqrt_summary

#_________________________________________________________________________________________________----
# TESTING ----
# outcome = while wild bird data follows a normal distribution, captive birds haven't met the criteria for normal distribution, even after transforming.
# Test using = non-parametric test = Mann Whitney U test
## Testing Transformed data ----
kooki_wc_sqrt

# Perform Levene's Test 
kooki1_leveneTest <- leveneTest(sqrt_chicks ~ bird, data = kooki_wc_sqrt)
kooki1_leveneTest   #       Df  F value Pr(>F) 
                    # group  1  3.578   0.06373 

# Mann Whitney U / Wilcoxan 
kooki1_test <- wilcox.test(sqrt_chicks ~ bird, data=kooki_wc_sqrt) 
print(kooki1_test)
# W = 767, p-value = 7.239e-08 = < 0.001
# alternative hypothesis: true location shift is not equal to 0

# Kooki1 sqrt boxplot ----
# add lables and set y axis lables 
kooki1_boxplot <- kooki_wc_sqrt %>%
  ggplot(aes(x = bird, y = sqrt_chicks, fill = bird)) +
  geom_boxplot()+
  scale_x_discrete(limits = desired_order_wc) + # order = wild, captive
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(labels = c("wild", "captive"), values = c("darkgreen", "mediumpurple4"))+
  theme_classic()+
  labs(y = "Mean no. of chicks produced per pair",
       x = "Birds") +
  custom_theme()

kooki1_boxplot
ggsave("figures/kooki1_boxplot.png", dpi = 400) # USE IN RESULTS REPORT

#_________________________________________________________________________________________________----
## Testing un-Transformed data ----
kooki_wc
view(kooki_wc)

# Perform Levene's Test 
kooki1_leveneTest2 <- leveneTest(chicks ~ bird, data = kooki_wc)
kooki1_leveneTest2  #       Df  F value Pr(>F) 
                    # group  1  0.009   0.9248

# Mann Whitney U / Wilcoxan 
kooki1_test2 <- wilcox.test(chicks ~ bird, data=kooki_wc) 
print(kooki1_test2)
# W = 74, p-value = 7.239e-08 = < 0.001
# alternative hypothesis: true location shift is not equal to 0

# Kooki1 boxplot ----
# add lables and set y axis lables 
kooki1_boxplot <- kooki_wc %>%
  ggplot(aes(x = bird, y = chicks, fill = bird)) +
  geom_boxplot(width = 0.7)+
  scale_fill_manual(labels = c("wild", "captive"), values = c("darkgreen", "mediumpurple4"))+
  scale_x_discrete(limits = desired_order_wc) + # order = wild, captive
  scale_y_continuous(limits = c(-0.1, 1)) +
  theme_classic() +
  labs(y = "Mean no. of chicks produced per pair",
       x = "Bird") +
  custom_theme()

kooki1_boxplot
ggsave("figures/kooki1_boxplot.png", dpi = 400) 

kooki1_boxplot1 <- kooki_wc_sqrt %>%
  ggplot(aes(x = bird, y = sqrt_chicks, fill = bird)) +
  geom_boxplot(width = 0.7)+
  scale_fill_manual(labels = c("wild", "captive"), values = c("darkgreen", "mediumpurple4"))+
  scale_x_discrete(limits = desired_order_wc) + # order = wild, captive
  scale_y_continuous(limits = c(-0.1, 1)) +
  theme_classic() +
  labs(y = "Mean no. of chicks produced per breeding pair",
       x = "Bird") +
  custom_theme()

kooki1_boxplot1
ggsave("figures/kooki1_boxplot1.png", dpi = 400)
