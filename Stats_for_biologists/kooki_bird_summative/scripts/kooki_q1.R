# KOOKI_1 = How does chick productivity of wild birds compare to that of the captive birds? ----
# two sample t test

# 🚚 LOAD R OBJECTS AND FUNCTIONS ----
# Import tidied data and functions
source("scripts/clean_kooki.R")

# filtering ----
kooki1 <- dplyr::select(.data = kooki, # dplyr:: to avoid MASS conflict during linear model construction
                        wild, captive)  # variables want to select.

kooki1
kooki_wc <- kooki1 %>% pivot_longer(cols = wild:captive,
                                names_to = "bird", # organise by bird = wild:captive
                                values_to = "chicks") 
kooki_wc

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
            median=median(chicks), # median = captive(0.05), wild(0.49)
            mode=mode(chicks)) # mode = captive(0.02), wild(0.54)

kooki_wc_summary

# look at distribution of data
kooki_wc_histogram <- ggplot(kooki_wc, aes(x = chicks, fill= bird)) +
  geom_histogram(binwidth = 0.05, bins = 10, color = "black") +
  scale_fill_manual(values = c("wild" = "darkgreen", "captive" = "mediumpurple4")) +
  facet_wrap(~bird) +
  geom_density(fill = NA, color = "black", size = 0.75) + 
  theme(strip.text = element_text(size = 14),
               axis.title.y.right = element_blank(), 
               axis.text.y.right = element_blank(), 
               axis.ticks.y.right = element_blank()) +
  theme_classic()+
  labs(x = "Mean number of chicks produced")

kooki_wc_histogram # wild (normal😀), captive (not normally distributed = transform😱)
ggsave("figures/kooki_wc_histogram.png", dpi = 300) # change colours


# WILD ----
kooki_wild <- kooki_wc %>%  # pipe kooki data frame into next function 
  filter(bird == "wild") # filter the piped data frame and only keep bird wild
kooki_wild

kooki_wild_plot <-
  ggplot(data = kooki_wild, aes(x=chicks)) + # chicks
  geom_histogram(bins = 10, color = "black", fill="darkgreen") + 
  theme_classic()
kooki_wild_plot # pretty good for distribution 📊

# CAPTIVE ----
kooki_captive <- kooki_wc %>%  # pipe kooki data frame into next function 
  filter(bird == "captive") # filter the piped data frame and only keep bird wild
kooki_captive

kooki_captive_plot <- kooki_captive %>%
  ggplot(aes(x=chicks)) + # chicks
  geom_histogram(bins = 5, color = "black", fill="mediumpurple4") + 
  theme_classic()
kooki_captive_plot # negative skew 📉

# log 10 TRANSFORM ----
kooki_captive_log <- kooki_captive %>%
  mutate(chicks_log = log(kooki_captive$chicks))
kooki_captive_log

# log10 plot
kooki_log_captive_plot <- kooki_captive_log %>%
  ggplot(aes(x=chicks_log)) + # chicks
  geom_histogram(bins = 10, color = "black", fill="mediumpurple4") + 
  theme_classic()
kooki_log_captive_plot # improved distribution

# combine wild and new captive data frames ----
kooki_wild
kooki_captive_log

combined_df <- kooki_wild %>% 
  bind_cols(kooki_captive_log)
combined_df

# update variable names
kooki_combined <- dplyr::rename(combined_df,  # use rename from the dplyr package
                       "w_bird" = "bird...1",
                       "w_chicks" = "chicks...2",
                       "c_bird" = "bird...3",
                       "c_chicks" = "chicks...4",
                       "c_chicks_log" = "chicks_log")

# kooki_combined
kooki_combined <- dplyr::select(.data = kooki_combined, # select dataframe
                        w_chicks, c_chicks_log)  # variables want to select.

kooki_combined

# update variable names
kooki_combined <- dplyr::rename(kooki_combined,  # use rename from the dplyr package
                                "wild" = "w_chicks",
                                "captive" = "c_chicks_log")

kooki_combined <- kooki_combined %>% 
  pivot_longer(cols = wild:captive,
              names_to = "bird", # organise by bird = wild:captive
              values_to = "chicks") 

kooki_combined # same format as kooki_wc

# FINAL combined histogram 2 ----
## Original order for x-axis
original_order <- c("captive", "wild")
# Update the factor levels 
kooki_combined$bird <- factor(kooki_combined$bird, levels = original_order)

## Desired order for x-axis
desired_order <- c("wild", "captive")
# Update the factor levels 
kooki_combined$bird <- factor(kooki_combined$bird, levels = desired_order)

# HISTOGRAM ----
# Create the histogram plot with the original/desired (just run before running plot) order
kooki_combined_histogram <- kooki_combined %>%
  ggplot(aes(x = chicks, fill= bird)) +
  geom_histogram(bins = 10, color = "black") +
  scale_fill_manual(values = c("wild" = "darkgreen", "captive" = "mediumpurple4")) +
  facet_wrap(~bird, scales = "free_x") +
  geom_density(fill = NA, color = "black", size = 0.75) + 
  theme(strip.text = element_text(size = 14),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(), 
        axis.ticks.y.right = element_blank()) +
  theme_classic()+
  labs(x = "Mean number of chicks produced",
       y = "Count")

kooki_combined_histogram # wild (normal😀), captive (not normally distributed = transform😱)
# ORIGINAL ORDER LOOKS BETTER 😀
ggsave("figures/kooki_combined_histogram.png", dpi = 300) # USE FOR REPORT 😀

# TESTING ----
# outcome = while wild bird data follows a normal distribution, captive birds haven't met the criteria for normal distribution, even after transforming.
# Test using = non-parametric test = Mann Whitney U test
# Mann Whitney U / Wilcoxan 

kooki1_test <- wilcox.test(chicks ~ bird, data=kooki_combined) 

print(kooki1_test)
# W = 0, p-value = 6.29e-11 = < 0.001
# alternative hypothesis: true location shift is not equal to 0

# Kooki1 boxplot ----
# add lables and set y axis lables 
kooki1_boxplot <- kooki_combined %>%
  ggplot(aes(x = bird, y = chicks, fill = bird)) +
  geom_boxplot()+
  scale_fill_manual(values = c("wild" = "darkgreen", "captive" = "mediumpurple4")) +
  scale_x_discrete(limits = desired_order) + # order = wild, captive
  scale_y_continuous(limits = c(-6,2)) +
  theme_classic() +
  theme( 
       # plot.title = element_text(size = 20, face = "bold"), # Plot title 
        axis.title.x = element_text(size = 16), # X-axis title 
        axis.title.y = element_text(size = 16), # Y-axis title 
        axis.text.x = element_text(size = 14), # X-axis text 
        axis.text.y = element_text(size = 14), # Y-axis text 
        legend.title = element_text(size = 14), # Legend title 
        legend.text = element_text(size = 12)) # Legend text

kooki1_boxplot

ggsave("figures/kooki1_boxplot.png", dpi = 300) # USE IN RESULTS REPORT
