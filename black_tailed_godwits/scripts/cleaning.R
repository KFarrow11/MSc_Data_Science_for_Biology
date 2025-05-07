# METADATA ----

# elevation           m above sea level 
# swardht             1 = 0-5 cm, 2 = 5-10 cm, 3 = 10-20 cm, 4 = 20-40 cm = factor
# willows             % cover of willows, Salix spp = tree
# birch               % cover of dwarf birch, Betula nana = shrub
# juncus              % cover of Juncus arcticus = typically found in wetlands
# sand                % cover of sand and gravel
# ditches             No.of surrounding drainage ditches (a measure of the extent of land drainage)
# watertab            Depth of the water table (cm)
# disthay             Distance to the nearest hayfield (m) (these are often used by feeding godwits)
# sedpools            Number of sedge pools (pools with sedges plants growing in the shallows)
# poolcov             % cover of pools within the patch
# godoccur            godwit presence or absence (presence = 1, absence = 0)
# goddens             density of black-tailed godwits (number/ha)

# PACKAGES ----
source("scripts/packages_library.R")

# LOAD DATA ----
godwits <- read.csv("data/godwit_data.csv") # DONT USE FOR PCA
glimpse(godwits)
colnames(godwits)
nrow(godwits)

#_______________________________________________________________________________ ----
# CLEAN ----
# Converting variable to a factor [DONT USE FOR PCA]
godwits$swardht <- as.factor(godwits$swardht)
godwits$ditches <- as.factor(godwits$ditches)
godwits$sedpools <- as.factor(godwits$sedpools)

## change variable names ----
godwits <- dplyr::rename(godwits,  # use rename from the dplyr package
    #  elevation, swardht,
    "willow_cover" = "willows",
    "birch_cover" = "birch",
    "juncus_cover" = "juncus",
    "sand_gravel_cover" = "sand",
    "num_ditches" = "ditches",
    "water_table_depth" = "watertab",
    "hayfield" = "disthay",
    "num_sedgepools" = "sedpools",
    "pool_cover" = "poolcov",
    "god_occur" = "godoccur",
    "god_density" = "goddens")

## new names ----
#"elevation" "swardht" "willow_cover"  "birch_cover"   "juncus_cover" "sand_gravel_cover"  "num_ditches"   "water_table_depth"  "hayfield"  "num_sedgepools"  "pool_cover"  "god_occur"   "god_density" 

# AIM 2 DATASET ----
# filter godwit presence from absence
godwit_presence <- godwits %>% filter(god_occur == 1)

# View the filtered data frame
glimpse(godwit_presence)

# Rearrange columns: move god_occur and god_density before elevation
godwit_den <- godwit_presence %>% 
  relocate(god_occur, god_density, .before = 1)

colnames(godwit_den)
glimpse(godwit_den)

#______________________________________________________________________________ ----
# PCA DATA ----
## Aim 1 ----
pca_godwits <- read.csv("data/godwit_data.csv")

## change variable names ----
pca_godwits <- dplyr::rename(pca_godwits,  # use rename from the dplyr package
                         #  elevation, swardht,
                         "willow_cover" = "willows",
                         "birch_cover" = "birch",
                         "juncus_cover" = "juncus",
                         "sand_gravel_cover" = "sand",
                         "num_ditches" = "ditches",
                         "water_table_depth" = "watertab",
                         "hayfield" = "disthay",
                         "num_sedgepools" = "sedpools",
                         "pool_cover" = "poolcov",
                         "god_occur" = "godoccur",
                         "god_density" = "goddens")

colnames(pca_godwits)

# names = "elevation" "swardht" "willow_cover"  "birch_cover"   "juncus_cover"  "sand_gravel_cover"  "num_ditches"   "water_table_depth"  "hayfield"  "num_sedgepools"  "pool_cover"  "god_occur"   "god_density" 

# AIM 2 DATASET ----
# filter godwit presence from absence
stats_godwit_presence <- pca_godwits %>% filter(god_occur == 1)

# View the filtered data frame
glimpse(stats_godwit_presence)

# Rearrange columns: move god_occur and god_density before elevation
stats_godwit_den <- stats_godwit_presence %>% 
  relocate(god_occur, god_density, .before = 1)

colnames(stats_godwit_den)
glimpse(stats_godwit_den)

#______________________________________________________________________________ ----
# SET PLOT SETTINGS ----
# Text sizes
title_size <- 16 # set title size
face_wrap_size <- 14 # set facet text size
text_size <- 14 # set axis text size

# Text size for alternative plots = custom_theme2
title_size2 <- 20
text_size2 <- 15

title_size3 <- 16 # set title size
face_wrap_size3 <- 14 # set facet text size
text_size3 <- 12 # set axis text size

# Custom theme function
custom_theme <- function() {
  theme(
    legend.position = "none",
    plot.title = element_text(size = title_size3, , face = "bold", color = "black", margin = margin(10, 10, 10, 10)),
    strip.text = element_text(size = face_wrap_size, face = "bold", color = "black"), # Adjust the text size for facet labels
    axis.text.x = element_text(size = text_size, color = "black"),     # X-axis text 
    axis.text.y = element_text(size = text_size, color = "black"),     # Y-axis text 
    axis.title.x = element_text(size = title_size, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # X-axis title 
    axis.title.y = element_text(size = title_size, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # Y-axis title
    legend.title = element_text(size = title_size, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # Legend title 
    legend.text = element_text(size = text_size, color = "black"),     # Legend text
    axis.title.y.right = element_blank(),   # element blank for right (wild) graph
    axis.text.y.right = element_blank(),    # element blank for right (wild) graph
    axis.ticks.y.right = element_blank()    # element blank for right (wild) graph
  )
}

custom_theme2 <- function() {
  theme(
    legend.position = "none",
    plot.title = element_text(size = title_size3, , face = "bold", color = "black", margin = margin(10, 10, 10, 10)),
    strip.text = element_text(size = face_wrap_size, face = "bold", color = "black"), # Adjust the text size for facet labels
    axis.text.x = element_text(size = text_size2, face = "bold", color = "black"),     # X-axis text 
    axis.text.y = element_text(size = text_size2, face = "bold", color = "black"),     # Y-axis text 
    axis.title.x = element_text(size = title_size2, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # X-axis title 
    axis.title.y = element_text(size = title_size2, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # Y-axis title
    legend.title = element_text(size = title_size2, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # Legend title 
    legend.text = element_text(size = text_size2, face = "bold", color = "black"),     # Legend text
    axis.title.y.right = element_blank(),   # element blank for right (wild) graph
    axis.text.y.right = element_blank(),    # element blank for right (wild) graph
    axis.ticks.y.right = element_blank()    # element blank for right (wild) graph
  )
}

custom_theme3 <- function() {
  theme(
    plot.title = element_text(size = title_size3, , face = "bold", color = "black", margin = margin(10, 20, 10, 0)),
    strip.text = element_text(size = face_wrap_size3, face = "bold", color = "black"), # Adjust the text size for facet labels
    axis.text.x = element_text(size = text_size3, color = "black"),     # X-axis text 
    axis.text.y = element_text(size = text_size3, color = "black"),     # Y-axis text 
    axis.title.x = element_text(size = face_wrap_size3, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # X-axis title 
    axis.title.y = element_text(size = face_wrap_size3, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # Y-axis title
    legend.title = element_text(size = face_wrap_size3, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # Legend title 
    legend.text = element_text(size = face_wrap_size3, color = "black"),     # Legend text
    axis.title.y.right = element_blank(),   # element blank for right (wild) graph
    axis.text.y.right = element_blank(),    # element blank for right (wild) graph
    axis.ticks.y.right = element_blank()    # element blank for right (wild) graph
  )
}
