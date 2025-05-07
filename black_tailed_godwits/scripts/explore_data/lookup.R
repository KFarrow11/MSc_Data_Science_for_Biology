# # DATA + PACKAGES ----
source("scripts/cleaning.R")
# ______________________________________________________________________________________----
# AIM 1 LOOKUP ----
## Define a custom plotting function ----
lookup_plot <- function(data, x_var, x_lab) { 
  ggplot(data, aes_string(x = x_var, y = "god_occur")) + 
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5), fill = "#CFEED1", alpha = 0.2) + 
    geom_point(position = "jitter", color = "black") + 
    scale_y_continuous(breaks = c(0, 1), labels = c("Absent", "Present")) + 
    theme_classic() + 
    custom_theme() +
    labs(y = "Black-tailed Godwit", 
         x = x_lab) 
}

## Generate individual plots using the custom function ----
# variable_scatter<- lookup_plot(data, x_var, y_var, x_label, y_label)
elevation_scatter <- lookup_plot(godwits, "elevation", "Elevation (m)")
willow_cover_scatter <- lookup_plot(godwits, "willow_cover", "Willow Cover (%)")
birch_cover_scatter <- lookup_plot(godwits, "birch_cover", "Birch Cover (%)")
juncus_cover_scatter <- lookup_plot(godwits, "juncus_cover", "Juncus Cover (%)")
sand_gravel_cover_scatter <- lookup_plot(godwits, "sand_gravel_cover", "Sand/Gravel Cover (%)")
water_table_depth_scatter <- lookup_plot(godwits, "water_table_depth", "Water Table Depth (cm)")
hayfield_scatter <- lookup_plot(godwits, "hayfield", "Distance to the nearest hayfield (m)")
pool_cover_scatter <- lookup_plot(godwits, "pool_cover", "Pool Cover (%)")

swardht_scatter <- godwits %>%
  ggplot(aes(x=swardht, y = god_occur)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5), fill = "#CFEED1", alpha = 0.2) + 
  geom_point(position = "jitter", color = "black") + 
  scale_y_continuous(breaks = c(0, 1), labels = c("Absent", "Present")) +  
  theme_classic() + 
  custom_theme() +
  labs(x = "Sward Grass Height (cm)", 
       y = "Black-tailed Godwit") +
  theme(legend.position = "none")

num_ditches_scatter <- godwits %>%
  ggplot(aes(x=num_ditches, y = god_occur)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5), fill = "#CFEED1", alpha = 0.2) + 
  geom_point(position = "jitter", color = "black") + 
  scale_y_continuous(breaks = c(0, 1), labels = c("Absent", "Present")) + 
  theme_classic() + 
  custom_theme() +
  labs(x = "Number of Ditches", 
       y = "Black-tailed Godwit") +
  theme(legend.position = "none")

num_sedgepools_scatter <- godwits %>%
  ggplot(aes(x=num_sedgepools, y = god_occur)) +  
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5), fill = "#CFEED1", alpha = 0.2) + 
  geom_point(position = "jitter", color = "black") + 
  scale_y_continuous(breaks = c(0, 1), labels = c("Absent", "Present")) + 
  theme_classic() + 
  custom_theme() +
  labs(x = "Number of pools with sedge plants growing in the shallows", 
       y = "Black-tailed Godwit") +
  theme(legend.position = "none")

elevation_scatter # less godwits at higher elevation 
willow_cover_scatter # increase in percentace of willow (Salix spp) cover less godwits
birch_cover_scatter # same as willow
juncus_cover_scatter # same as willow
sand_gravel_cover_scatter # same as willow
num_ditches_scatter # increase in ditches = less godwits
water_table_depth_scatter # increase in depth = less godwits, prefer depths between 50 and 150
hayfield_scatter # closer to hayfields = more godwits
num_sedgepools_scatter # increase in number of sedgepools more godwits
pool_cover_scatter # less pool cover more godwits
swardht_scatter # godwits prefer 20-40 cm

# Save plots to the figures folder ----
ggsave(filename = "figures/lookup/aim1/elevation_scatter.png", plot = elevation_scatter, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim1/swardht_scatter.png", plot = swardht_scatter, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim1/willow_cover_scatter.png", plot = willow_cover_scatter, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim1/birch_cover_scatter.png", plot = birch_cover_scatter, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim1/juncus_cover_scatter.png", plot = juncus_cover_scatter, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim1/sand_gravel_cover_scatter.png", plot = sand_gravel_cover_scatter, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim1/ditches_scatter.png", plot = num_ditches_scatter, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim1/water_table_depth_scatter.png", plot = water_table_depth_scatter, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim1/hayfield_scatter.png", plot = hayfield_scatter, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim1/sedgepools_scatter.png", plot = num_sedgepools_scatter, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim1/pool_cover_scatter.png", plot = pool_cover_scatter, width = 8, height = 6)

# ______________________________________________________________________________________----
# AIM 2 LOOKUP ----
godwit_den2 <- godwit_den
godwit_den2$god_density <- round(godwit_den2$god_density, 2)
glimpse(godwit_den2)

density_hist <- ggplot(data = godwit_den2, aes(x = god_density)) +
  geom_histogram(fill = "#4682B4", color = "black", bins = 30) + 
  labs(
    x = "Black-tailed Godwits Density\n(number/ha)", 
    y = "Frequency"
  ) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) + 
  theme_classic() + 
  custom_theme()

density_hist
ggsave(filename = "figures/lookup/aim2/density_hist.png", plot = density_hist, width = 8, height = 6)

## Define a custom function for scatterplot for aim 2 ----
lookup_plot2 <- function(data, x_var, x_lab) { 
  ggplot(data, aes_string(x = x_var, y = "god_density")) + 
    geom_point(position = "jitter") + 
    theme_classic() + 
    custom_theme() + 
    labs(y = "Black-tailed Godwits Density\n(number/ha)", 
         x = x_lab) 
}

## Generate the individual plots ----
elevation_scatter2 <- lookup_plot2(godwit_den2, "elevation", "Elevation (m)")
swardht_scatter2 <- lookup_plot2(godwit_den2, "swardht", "Sward Grass Height (cm)")
willow_cover_scatter2 <- lookup_plot2(godwit_den2, "willow_cover", "Willow Cover (%)")
birch_cover_scatter2 <- lookup_plot2(godwit_den2, "birch_cover", "Birch Cover (%)")
juncus_cover_scatter2 <- lookup_plot2(godwit_den2, "juncus_cover", "Juncus Cover (%)")
sand_gravel_cover_scatter2 <- lookup_plot2(godwit_den2, "sand_gravel_cover", "Sand/Gravel Cover (%)")
num_ditches_scatter2 <- lookup_plot2(godwit_den2, "num_ditches", "Number of Ditches")
water_table_depth_scatter2 <- lookup_plot2(godwit_den2, "water_table_depth", "Water Table Depth (cm)")
hayfield_scatter2 <- lookup_plot2(godwit_den2, "hayfield", "Distance to the nearest hayfield (m)")
num_sedgepools_scatter2 <- lookup_plot2(godwit_den2, "num_sedgepools", "Number of Sedge Pools")
pool_cover_scatter2 <- lookup_plot2(godwit_den2, "pool_cover", "Pool Cover (%)")

# view plots
elevation_scatter2
swardht_scatter2 # boxplot
willow_cover_scatter2
birch_cover_scatter2
juncus_cover_scatter2
sand_gravel_cover_scatter2
num_ditches_scatter2 # boxplot
water_table_depth_scatter2
hayfield_scatter2
num_sedgepools_scatter2 # boxplot
pool_cover_scatter2

swardht_box <- godwit_den2 %>%
  ggplot(aes(x = factor(swardht, levels = c(1, 2, 3, 4), 
                        labels = c("0-5 cm", "5-10 cm", "10-20 cm", "20-40 cm")), 
             y = god_density)) +
  geom_boxplot(aes(fill = swardht)) +
  geom_point(position = "jitter") +
  theme_classic() +
  custom_theme() +
  labs(x = "Sward Grass Height (cm)", 
       y = "Black-tailed Godwits Density\n(number/ha)") +
  scale_fill_manual(values = c("#d0f0c0", "#91c788", "#4caf50", "#2e7d32")) +
  theme(legend.position = "none")

swardht_box
  
ditches_box <- godwit_den2 %>%
  ggplot(aes(x = num_ditches, y = god_density)) +
  geom_boxplot(aes(fill = num_ditches)) +
  geom_point(position = "jitter") +
  theme_classic() +
  custom_theme() +
  labs(x = "Number of Ditches",
       y = "Black-tailed Godwits Density\n(number/ha)") +
  scale_fill_manual(values = c("#f7e1d7", "#e3c7a4", "#d2a679", "#a68a64", "#8b5e3c")) +
  theme(legend.position = "none")

ditches_box

sedgepools_box <- godwit_den2 %>%
  ggplot(aes(x = num_sedgepools, y = god_density)) +
  geom_boxplot(aes(fill = num_sedgepools)) +
  geom_point(position = "jitter") +
  theme_classic() +
  custom_theme() +
  labs(x = "Number of pools with sedge plants growing in the shallows",
       y = "Black-tailed Godwits Density\n(number/ha)") +
  scale_fill_manual(values = c("#e4f1fa", "#aed6f1", "#76bce6", "#3793cf")) +
  theme(legend.position = "none")

sedgepools_box

## Save plots to figures folder ----
ggsave(filename = "figures/lookup/aim2/elevation_scatter2.png", plot = elevation_scatter2, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim2/swardht_boxplot.png", plot = swardht_box, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim2/willow_cover_scatter2.png", plot = willow_cover_scatter2, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim2/birch_cover_scatter2.png", plot = birch_cover_scatter2, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim2/juncus_cover_scatter2.png", plot = juncus_cover_scatter2, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim2/sand_gravel_cover_scatter2.png", plot = sand_gravel_cover_scatter2, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim2/ditches_boxplot.png", plot = ditches_box, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim2/water_table_depth_scatter2.png", plot = water_table_depth_scatter2, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim2/hayfield_scatter2.png", plot = hayfield_scatter2, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim2/sedgepools_boxplot.png", plot = sedgepools_box, width = 8, height = 6)
ggsave(filename = "figures/lookup/aim2/pool_cover_scatter2.png", plot = pool_cover_scatter2, width = 8, height = 6)
