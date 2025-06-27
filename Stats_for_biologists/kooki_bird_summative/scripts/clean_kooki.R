# PACKAGES ----

library(tidyverse) # Tidy, plots + descriptive stats
library(car) # leveneTest
library(janitor) # replace column names to lowercase 
library(performance) 
library(moderndive)
#_________________________________________________________________________________________________----
# LOAD DATA ----
kooki <- read_csv("data/kooki.csv")# asign data to object kooki
kooki
glimpse(kooki)
colnames(kooki)

#_________________________________________________________________________________________________----
# CLEAN ----
## clean up column names ----
kooki <- janitor::clean_names(kooki)

colnames(kooki) # quickly check the new variable names

## change variable names ----
kooki <- dplyr::rename(kooki,  # use rename from the dplyr package
                      "wild" = "wild_birds_1",
                      "captive" = "captive_bred_birds_1",
                      "breeding_period" = "breeding_period_standardised_5",
                      "jan_rain" = "jan_rain_cm_3",
                      "feb_rain" = "feb_rain_cm_3",
                      "sept_temp" = "sept_temp_c_4",
                      "oct_temp" = "oct_temp_c_4",
                      "no_sup" = "bo_mo_2",
                      "mango" = "b0_m1_2",
                      "banana" = "b1_mo_2",
                      "banana_mango" = "b1_m1_2") 

colnames(kooki)
glimpse(kooki)

# SET PLOT SIZINGS ----
# Define your sizes
title_size <- 25 # set title size
face_wrap_size <- 20 # set facet text size
text_size <- 20 # set axis text size
title_size2 <- 20
text_size2 <- 15

# Custom theme function
custom_theme <- function() {
  theme(
    legend.position = "none",
    strip.text = element_text(size = face_wrap_size, face = "bold", color = "black"), # Adjust the text size for facet labels
    axis.text.x = element_text(size = text_size, face = "bold", color = "black"),     # X-axis text 
    axis.text.y = element_text(size = text_size, face = "bold", color = "black"),     # Y-axis text 
    axis.title.x = element_text(size = title_size, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # X-axis title 
    axis.title.y = element_text(size = title_size, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # Y-axis title
    legend.title = element_text(size = title_size, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # Legend title 
    legend.text = element_text(size = text_size, face = "bold", color = "black"),     # Legend text
    axis.title.y.right = element_blank(),   # element blank for right (wild) graph
    axis.text.y.right = element_blank(),    # element blank for right (wild) graph
    axis.ticks.y.right = element_blank()    # element blank for right (wild) graph
  )
}

custom_theme2 <- function() {
  theme(
    legend.position = "none",
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
