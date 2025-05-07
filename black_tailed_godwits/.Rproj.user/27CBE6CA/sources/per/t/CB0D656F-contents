# AIM ----
# Which environmental conditions influence the presence or absence of godwits?
# Require godoccur - godwit presence or absence (presence = 1, absence = 0)

# DATA + PACKAGES ----
source("scripts/cleaning.R")

glimpse(pca_godwits) # swardht, num_ditches and num_sedgepools not converted to factor

# filter godwit presence from absence
pca_presence <- pca_godwits %>% filter(god_occur == 1)

# View the filtered data frame
glimpse(pca_presence)

# Rearrange columns: move god_occur and god_density before elevation
pca_den <- pca_presence %>% 
  relocate(god_occur, god_density, .before = 1)

colnames(pca_den)
glimpse(pca_den)

#running a DCA to investigate the lengths of gradients
decorana(veg = pca_den, iweigh=0, iresc=4, ira=0, mk=26, short=0,
         before=NULL, after=NULL)

# Axis lengths = 1.6283 = linear = PCA only

# The godwit environmental variables are measured in different units (distance, percent cover, number, depth, pres etc.) so we need to center and standardise.

## numeric columns ----
numeric_columns <- c("elevation", "swardht", "willow_cover", "birch_cover", "juncus_cover", "sand_gravel_cover", "num_ditches", "water_table_depth", "hayfield", "num_sedgepools", "pool_cover") 

## Run PCA and standardize the data ----
pca2 <- prcomp(pca_den[, numeric_columns], center = TRUE, scale. = TRUE)

summary(pca2)

# output
#                        PC1   PC2   PC3    PC4
# Standard deviation     1.710 1.351 1.1597 1.0852
# Proportion of Variance 0.266 0.166 0.1223 0.1071
# Cumulative Proportion  0.266 0.432 0.5543 0.6613

# The first two principal components (PC1 and PC2) together explain 69% of the total variance. This might be a good indication that they capture a significant portion of the information in the dataset. The cumulative proportion of variance explained suggests that including additional components (such as PC3 and PC4) will provide a more comprehensive representation of the dataset but with diminishing returns.

# Eigenvalues from PCA
eig.val2 <- get_eigenvalue(pca2)
eig.val2
# The first few principal components (particularly Dim.1 and Dim.2) capture a substantial proportion of the variance in the dataset, making them significant for further analysis. Retaining the first four components might be a practical choice, as they collectively explain over 66% of the total variance as confirmed with scree plot.

# Create scree plot. 
fviz_eig(pca2, addlabels = TRUE) +
  theme_classic() +
  custom_theme3() 

# The scree plot or eigenvalues plot can also help visualize the importance of each principal component in explaining the variance, guiding you in selecting the number of components for further analysis.

# Results for Variables
res.var2 <- get_pca_var(pca2) # extracts the variables' results from the PCA
res.var2$coord          # Variable Coordinates = variables with high absolute values in a given principal component are significant contributors to that component.

# **Dimension 1 (26.6% of variance)** represents a primary wetness gradient across the landscape. This component contrasts drier, well-drained areas characterized by deeper water tables (-0.784), higher elevations (-0.581), and more drainage ditches (-0.541) against wetter habitats with greater pool cover (0.653), more sedge pools (0.430), and abundant Juncus coverage (0.737). This environmental gradient likely reflects the fundamental hydrological conditions that structure the ecosystem.

# **Dimension 2 (16.6% of variance)** captures vegetation structure differences, primarily distinguishing areas with woody vegetation from more open habitats. Sites with negative scores exhibit greater birch cover (-0.720), taller sward heights (-0.551), and hayfield characteristics (-0.615). In contrast, sites with positive scores feature deeper water tables (0.423), more sedge pools (0.348), and exposed sand/gravel substrates (0.277), indicating more open, less vegetated conditions.

# **Dimension 3 (12.2% of variance)** specifically isolates willow-dominated habitats from other vegetation types. This dimension is strongly associated with willow cover (-0.737) and also correlates with sedge pools (-0.580), exposed substrate (-0.440), and standing water (-0.357). The lack of strong positive loadings suggests this component primarily distinguishes willow thickets from all other habitat features rather than representing a gradient between two different habitat types.

# **Dimension 4 (10.7% of variance)** highlights management-influenced features across the landscape. Areas with high scores on this dimension show more drainage ditches (0.622), greater sand/gravel cover (0.462), some standing water (0.359), and hayfield characteristics (0.303). Conversely, willow cover (-0.404) is negatively associated. This suggests PC4 represents anthropogenic habitat modifications related to drainage and substrate management.

# Together, these four dimensions capture 66.1% of the total environmental variation, revealing that hydrology, vegetation structure, specific plant communities, and management practices are the primary factors differentiating habitats in this ecosystem. These components provide a robust framework for understanding habitat selection, species distributions, and ecological processes across the study area. The PCA successfully reduces eleven correlated variables into four interpretable ecological gradients that can inform conservation and management decisions.

res.var2$contrib        # Contributions = variables with high contributions are key drivers of the patterns observed in the PCA.

# **Dimension 1 (26.6% of variance)** is primarily driven by water_table_depth (21.03%), juncus_cover (18.57%), and pool_cover (14.58%). These three variables contribute over 54% to this dimension, confirming its interpretation as a wetness gradient. Elevation (11.54%) and num_ditches (10.00%) are also important contributors, further supporting the wet-to-dry habitat interpretation. Together, these hydrological variables shape the primary environmental pattern in the dataset.

# **Dimension 2 (16.6% of variance)** is dominated by birch_cover (28.42%), hayfield (20.73%), and swardht (16.65%), collectively accounting for nearly 66% of this dimension's variation. This reinforces the interpretation that Dimension 2 represents a gradient of vegetation structure, specifically contrasting woody, managed vegetation against more open areas. The strong contributions from these vegetation structure variables indicate their importance in differentiating habitat types.

# **Dimension 3 (12.2% of variance)** is overwhelmingly influenced by willow_cover (40.34%) and num_sedgepools (25.03%), which together contribute over 65% to this dimension. Sand_gravel_cover (14.40%) and pool_cover (9.47%) make secondary contributions. This confirms that Dimension 3 specifically isolates willow-dominated wetland habitats as a distinct ecological feature within the landscape.

# **Dimension 4 (10.7% of variance)** is most strongly defined by num_ditches (32.85%), with substantial contributions also from sand_gravel_cover (18.09%), willow_cover (13.89%), and pool_cover (10.92%). This pattern of contributions validates the interpretation that Dimension 4 represents management-related habitat modifications, particularly concerning drainage features and substrate characteristics across the landscape.

# These contribution values quantify precisely how much each variable influences each principal component, providing statistical support for the ecological interpretations of the PCA dimensions. The analysis clearly identifies which environmental variables are most important in structuring habitat variation across the study area.

# Results for individuals
res.ind2 <- get_pca_ind(pca2)
res.ind2$coord          # Ind Coordinates = individuals close to each other in the PCA space have similar profiles based on the original variables.

#Combine the original godwit data and the PC Dimension coords (loadings) into a list res.ind$coord
pca_list2 <- list(pca_den, res.ind2$coord)
pca_scores2 <- as.data.frame(pca_list2) # Convert list to data frame godwit_pca1
glimpse(pca_scores2)

## Plot PCA results using ggplot2 ----
# Scatter plot of principal components
# PC1 + PC2
ggplot(pca_scores2, aes(x = Dim.1, y = Dim.2)) +
  geom_point() +
  theme_classic() +
  custom_theme3() +
  labs(
    title = "PCA of Environmental Variables by Godwit Density",
    x = "PC1 (26.6%)",
    y = "PC2 (16.6%)"
  ) +
  xlim(-4, 6) +
  ylim(-4, 4) 

## custom ----
# Create a color-coded PCA plot based on godwit density
godwit_den3 <- godwit_den
godwit_den3$god_density <- round(godwit_den3$god_density, 2)

# Create a new color variable based on density thresholds
godwit_den3$density_color <- case_when(
  godwit_den3$god_density < 0.5 ~ "Low (<0.5)",
  godwit_den3$god_density <= 1 ~ "Medium-Low (0.5-1)",
  godwit_den3$god_density <= 2 ~ "Medium (1-2)",
  godwit_den3$god_density <= 3 ~ "Medium-High (2-3)",
  godwit_den3$god_density > 3 ~ "High (3+)",
  TRUE ~ "Unknown"
)

godwit_den3
# Convert to factor with proper ordering
godwit_den3$density_color <- factor(godwit_den3$density_color, 
                                    levels = c("Low (<0.5)", 
                                               "Medium-Low (0.5-1)", 
                                               "Medium (1-2)", 
                                               "Medium-High (2-3)", 
                                               "High (3+)"))

# Merge PCA scores with the density information
pca_scores_with_density <- cbind(pca_scores2, 
                                 density = godwit_den3$god_density,
                                 density_color = godwit_den3$density_color)

# Create color palette (earthy & colorblind-friendly)
density_colors <- c(
  "Low (<0.5)" = "#3CB371",  # medium sea green (lush foliage)
  "Medium-Low (0.5-1)" = "#7BAFD4",  # soft sky blue (coastal water)
  "Medium (1-2)" = "#DAA520",   # golden rod (river banks)
  "Medium-High (2-3)" = "#8B4513",   # saddle brown (rich earth)
  "High (3+)" = "#B22222"            # firebrick red (eroded cliffs)
)

# Plot with customized legend
plot1b <- ggplot(pca_scores_with_density, aes(x = Dim.1, y = Dim.2, color = density_color)) +
  geom_point(size = 3, alpha = 1, position = "jitter") +
  scale_color_manual(
    values = density_colors,
    name = "Population Density of black-tailed godwits (number/ha)",
    guide = guide_legend(
      title.position = "top",   # Puts title above the legend keys
      title.hjust = 0.5         # Centers the title
    )
  ) +
  theme_classic() +
  custom_theme3() +
  labs(
    title = "PCA of Environmental Variables by Godwit Density",
    x = "PC1 (26.6%)",
    y = "PC2 (16.6%)"
  ) +
  xlim(-4, 6) +
  ylim(-4, 4) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",           # Ensures vertical stack with title above
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.box.just = "center"
  )

# PC1 + PC3
ggplot(pca_scores2, aes(x = Dim.1, y = Dim.3)) + 
  geom_point() + 
  theme_classic() +
  custom_theme3() +
  labs( title = "PCA of Godwit Environmental Variables", 
        x = "PC1", 
        y = "PC3", 
        color = "Black-tailed Godwit"
  ) +
  xlim(-3, 6) +
  ylim(-4, 2)


plot2b <- ggplot(pca_scores_with_density, aes(x = Dim.1, y = Dim.3, color = density_color)) +
  geom_point(size = 3, alpha = 1, position = "jitter") +
  scale_color_manual(
    values = density_colors,
    name = "Population Density of black-tailed godwits (number/ha)",
    guide = guide_legend(
      title.position = "top",   # Puts title above the legend keys
      title.hjust = 0.5         # Centers the title
    )
  ) +
  theme_classic() +
  custom_theme3() +
  labs(
    title = "",
    x = "PC1 (26.6%)",
    y = "PC3 (12.2%)"
  ) +
  xlim(-4, 6) +
  ylim(-4, 2) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",           # Ensures vertical stack with title above
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.box.just = "center"
  )

# PC1 + PC4
ggplot(pca_scores2, aes(x = Dim.1, y = Dim.4)) + 
  geom_point() + 
  theme_classic() +
  custom_theme3() +
  labs( title = "PCA of Godwit Environmental Variables", 
        x = "PC1", 
        y = "PC4", 
        color = "Black-tailed Godwit"
  )  +
  xlim(-4, 6) +
  ylim(-2, 4)

plot3b <- ggplot(pca_scores_with_density, aes(x = Dim.1, y = Dim.4, color = density_color)) +
  geom_point(size = 3, alpha = 1, position = "jitter") +
  scale_color_manual(
    values = density_colors,
    name = "Population Density of black-tailed godwits (number/ha)",
    guide = guide_legend(
      title.position = "top",   # Puts title above the legend keys
      title.hjust = 0.5         # Centers the title
    )
  ) +
  theme_classic() +
  custom_theme3() +
  labs(
    title = "",
    x = "PC1 (26.6%)",
    y = "PC4 (10.7%)"
  ) +
  xlim(-4, 6) +
  ylim(-2, 4) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",           # Ensures vertical stack with title above
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.box.just = "center"
  )

plot1b
plot2b
plot3b

### patchwork ----
library(patchwork)

# Combine plots with shared legend below and reduced gap
combined_plot2 <- (plot1b + plot2b + plot3b) +
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "bottom",
    plot.margin = margin(t = 0, r = 0, b = 5, l = 0), # Adjust margins as needed
    legend.margin = margin(t = -10) # Reduce gap above legend
  )

combined_plot2
# ________________________________________________________________________----

# pca Scatter plot ----
# Merge PCA scores with the density information
density3 <- cbind(pca_scores2, 
                  density = godwit_den3$god_density)
glimpse(density3)

# Scatter plot showing relationship between Dim.1 and godwit density ----
# Create the scatter plot with updated colors and labels
den_scatter_pca1 <- ggplot(density3, aes(x = Dim.1,
                                         y = god_density)) +
  geom_point(size = 2.5, alpha = 0.8, position = "jitter") +
  geom_smooth(method = 'lm', color = "blue", linewidth = 1) +  # Adding a linear regression line (lm)
  theme_classic() +
  custom_theme3() +
  labs(
    title = "Relationship Between Black-tailed Godwit Population Density and Principal Component 1 (PCA1)",
    x = "PCA1 (26.6%)",
    y = "Black-tailed Godwit Density (number/ha)"
  )

den_scatter_pca1

# Scatter-plot for any variable with a strong positive correlation with Dim.1, against Dim.1
pca_results2
pca_results2 <- data.frame(pca2$x)
pca_results2$juncus_cover <- pca_den$juncus_cover

# Create the scatter plot
den_pca1_juncus_scatter <- ggplot(pca_results2, aes(x = PC1, y = juncus_cover)) +
  geom_point(position = "jitter") +
  geom_smooth(method = 'lm') +
  theme_classic() +
  labs(
    title = "Juncus Cover vs Principal Component 1",
    x = "PCA 1",
    y = "Percentage cover of Juncus arcticus"
  ) +
  ylim(-25, 75) +
  xlim(-4, 6)

den_pca1_juncus_scatter

pca_results2 <- data.frame(pca2$x)
pca_results2$hayfield <- pca_den$hayfield

den_pca1_hayfield_scatter <- ggplot(pca_results2, aes(x = PC1, y = hayfield)) +
  geom_point(position = "jitter") +
  geom_smooth(method = 'lm') +
  theme_classic() +
  labs(
    title = "Distance from Hayfield vs Principal Component 1",
    x = "PCA 1",
    y = "Distance from Hayfield (m)"
  ) 

den_pca1_hayfield_scatter

pca_results2 <- data.frame(pca2$x)
pca_results2$num_sedgepools <- pca_den$num_sedgepools


den_pca1_sedge_scatter <- ggplot(pca_results2, aes(x = PC1, y = num_sedgepools)) +
  geom_point(position = "jitter") +
  geom_smooth(method = 'lm') +
  theme_classic() +
  labs(
    title = "Number of sedge plants in pools vs Principal Component 1",
    x = "PCA 1",
    y = "Number of sedge plants in pools"
  ) +
  ylim(-0.5, 4) +
  xlim(-4, 6)

den_pca1_sedge_scatter

pca_results2 <- data.frame(pca2$x)
pca_results2$pool_cover <- pca_den$pool_cover

den_pca1_pool_scatter <- ggplot(pca_results2, aes(x = PC1, y = pool_cover)) +
  geom_point(position = "jitter") +
  geom_smooth(method = 'lm') +
  theme_classic() +
  labs(
    title = "% cover of pools within the patch vs Principal Component 1",
    x = "PCA 1",
    y = "Percentage cover of pools within patch"
  ) +
  scale_y_continuous(breaks = seq(-10, 50, by = 10))

den_pca1_pool_scatter

# It is often helpful in interpreting (as above) which ecological gradients (variables) strongly associated with each PC (Dimension), to visualise the correlations between variables and PCs (Dimensions) by plotting these correlation coefficients as Coordinates in a two-dimensional plot for any pair of PCs (Dimensions) we’re most interested in – typically PC1 and PC2

# PCA 2D PLOT ----
# Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.

# V# Visualize PCA variables with customized appearance
# Show labels at end of arrow heads using xlim/ylim and adjusting text position
fviz_pca_var(pca2,
             col.var = "contrib",   # Color variables by their contribution to PCs
             gradient.cols = c("black", "orange", "blue4"), # Gradient color scheme
             repel = FALSE,         # Turn off repel to have more control
             arrowsize = 1,         # Adjust arrow size for better visibility
             labelsize = 4,         # Increase label size for readability
             title = "PCA Variable Contributions", # Add a meaningful title
             ggtheme = theme_classic(),
             label = "var",         # Show variable names
             # Expand the plot area to accommodate labels at arrow tips
             xlim = c(-1.3, 1.3),
             ylim = c(-1.3, 1.3))

# Create a custom PCA variable plot with labels at arrow heads without geom_circle
# First get the variable coordinates
var_coords1_2 <- as.data.frame(get_pca_var(pca2)$coord[,1:2])
var_coords1_2$variable <- rownames(var_coords_2)
var_coords1_2$contrib <- get_pca_var(pca2)$contrib[,1]

# Update variable names in var_coords
var_coords1_2$variable <- recode(var_coords1_2$variable,
                              sand_grave_cover = "Sand/Gravel",
                              juncus_cover = "Juncus",
                              num_sedgepools = "Sedgepools",
                              swardht = "Swardht",
                              birch_cover = "Birch",
                              willow_cover = "Willow",
                              elevation = "Elevation",
                              water_table_depth = "Watertable",
                              num_ditches = "Ditches",
                              hayfield = "Hayfield",
                              pool_cover = "Pool")

# Create circle points
theta <- seq(0, 2 * pi, length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))

# Create the plot with updated label positions and names
pca_1_2 <- ggplot() +
  # Add a unit circle using points
  geom_path(data = circle, aes(x = x, y = y), color = "gray", size = 0.5) +
  # Add axis lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  # Add arrows from origin to variable coordinates (but slightly shorter)
  geom_segment(data = var_coords1_2, 
               aes(x = 0, y = 0, 
                   xend = Dim.1 * 1, # Make arrows shorter to leave room for labels
                   yend = Dim.2 * 1, 
                   color = contrib),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.9) +
  # Add labels slightly offset from arrow endpoints and in bold
  geom_text(data = var_coords1_2, 
            aes(x = Dim.1 *1.4, y = Dim.2 * 1.5, # Updated positions
                label = variable, color = contrib),
            size = 3.5, hjust = 0.5, vjust = -0.5, fontface = "bold") + # Bold text
  # Set color scale with new legend label
  scale_color_gradient2(low = "black", mid = "orange", high = "blue4", 
                        midpoint = median(var_coords1_2$contrib),
                        name = "Contribution (%)") + # Updated legend label
  # Labels and theme
  labs(x = paste0("PCA1 (26.6%)"),
       y = paste0("PCA2 (16.6%)"),
       title = "PCA 1 + 2") +
  theme_classic() +
  custom_theme3() +
  coord_fixed(ratio = 1) +  # Maintain aspect ratio
  theme(legend.position = "bottom") +
  xlim(-1.25, 1.25) +
  ylim(-1.25, 1.25) +
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5))

pca_1_2

### Interpretation of the PCA Biplot
# The PCA biplot visualizes the relationships between the variables in your dataset based on the first two principal components (Dim1 and Dim2). Here's a detailed interpretation:

#### Key Observations:
# - **Axes**: 
# - **Dim1 (21.2%)**: Explains 21.2% of the variance in the data.
# - **Dim2 (18%)**: Explains 18% of the variance in the data.
# - Together, these two components explain 39.2% of the total variance.

#### Variables and Contributions:
# - **Arrows**: Each arrow represents a variable, pointing in the direction of increasing values of that variable. The length of the arrow indicates the strength of the variable's contribution to the principal components.
# - **Color Gradient**: The arrows are color-coded from blue (low contribution) to red (high contribution).

#### Notable Variables:
# **High Contribution (Red Arrows)**
# - **juncus_cover**: Strongly associated with Dim1, indicating its significant influence on the first principal component.
# - **birch_cover**: Strongly associated with Dim2, highlighting its major influence on the second principal component.
# - **water_table_depth**: Also has a substantial influence on the first principal component.

# **Moderate to Low Contribution (Blue to Green Arrows)**:
# - **swardht**, **willow_cover**, and **hayfield** show moderate contributions.
# - Other variables like **sand_gravel_cover**, **num_ditches**, **pool_cover**, **elevation**, and **num_sedgepools** have varying degrees of influence.

#### Directions and Correlations:
# - **Positive Correlations**: Variables pointing in the same direction are positively correlated. For example, **juncus_cover** and **num_ditches** both contribute significantly to Dim1 and point in similar directions, indicating a positive correlation.
# - **Negative Correlations**: Variables pointing in opposite directions are negatively correlated. For example, **water_table_depth** and **elevation** point in nearly opposite directions, suggesting a negative correlation between these variables.

### Practical Insights:
# - **Dim1 (PC1)**: Primarily driven by **juncus_cover** and **water_table_depth**, indicating these variables capture the most significant variance patterns related to the first principal component.
# - **Dim2 (PC2)**: Dominated by **birch_cover** and **willow_cover**, suggesting these variables explain the second major variance patterns in the dataset.

### Conclusion: The biplot helps identify the most influential variables and their relationships, providing a clear visual representation of how each variable contributes to the overall data structure. This information can be used for data reduction, interpretation, and further analysis.



# Visualise Dim 3 and 4	
# Attempting Dim 3 and 4 Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(pca2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("black", "orange", "blue4"),
             repel = TRUE,    # Avoid text overlapping
             axes = c(3, 4))


var_coords3_4 <- as.data.frame(get_pca_var(pca2)$coord[,3:4])
var_coords3_4$variable <- rownames(var_coords_2)
var_coords3_4$contrib <- get_pca_var(pca2)$contrib[,3]

# Update variable names in var_coords
var_coords3_4$variable <- recode(var_coords3_4$variable,
                                sand_grave_cover = "Sand/Gravel",
                                juncus_cover = "Juncus",
                                num_sedgepools = "Sedgepools",
                                swardht = "Swardht",
                                birch_cover = "Birch",
                                willow_cover = "Willow",
                                elevation = "Elevation",
                                water_table_depth = "Watertable",
                                num_ditches = "Ditches",
                                hayfield = "Hayfield",
                                pool_cover = "Pool")

# Create circle points
theta <- seq(0, 2 * pi, length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))

# Create the plot with updated label positions and names
pca_3_4 <- ggplot() +
  # Add a unit circle using points
  geom_path(data = circle, aes(x = x, y = y), color = "gray", size = 0.5) +
  # Add axis lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  # Add arrows from origin to variable coordinates (but slightly shorter)
  geom_segment(data = var_coords3_4, 
               aes(x = 0, y = 0, 
                   xend = Dim.3 * 1, # Make arrows shorter to leave room for labels
                   yend = Dim.4 * 1, 
                   color = contrib),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.9) +
  # Add labels slightly offset from arrow endpoints and in bold
  geom_text(data = var_coords3_4, 
            aes(x = Dim.3 * 1.3, y = Dim.4 * 1.6, # Updated positions
                label = variable, color = contrib),
            size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") + # Bold text
  # Set color scale with new legend label
  scale_color_gradient2(low = "black", mid = "orange", high = "blue4", 
                        midpoint = median(var_coords3_4$contrib),
                        name = "Contribution (%)") + # Updated legend label
  # Labels and theme
  labs(x = paste0("PCA3 (10.9%)"),
       y = paste0("PCA4 (10.4%)"),
       title = "PCA 3 + 4") +
  theme_classic() +
  custom_theme3() +
  coord_fixed(ratio = 1) +  # Maintain aspect ratio
  theme(
    legend.position = "bottom", # Legend positioned below x-axis
  ) +
  xlim(-1.25, 1.25) +
  ylim(-1.25, 1.25) +
  # Customize legend appearance with reduced gap
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5, title.vjust = 0))

pca_3_4

# Combine the plots
pca_combined_den_pca <- (pca_1_2 + pca_3_4)

# Display the combined plot
pca_combined_den_pca


# Visualise the percentage of total variation accounted for by each PC, that is contributed by each variable ----
# Visualise the percentage of total variation accounted for by each PC, that is contributed by each variable ----
# Contributions of variables to PC1
Dim1b_a <- fviz_contrib(pca2, choice = "var", axes = 1, top = 11) +
  theme_classic() +
  custom_theme3() +
  coord_flip() +
  labs(x = "Environmental Factors", y = "") +
  scale_x_discrete(labels = c(
    birch_cover = "Birch cover (%)",
    willow_cover = "Willow cover (%)",
    swardht = "Sward grass height (cm)",
    elevation = "Elevation (m)",
    hayfield = "Distance from Hayfield (m)",
    pool_cover = "Pool cover (%)",
    water_table_depth = "Watertable depth (cm)",
    num_sedgepools = "Pools with sedge plants",
    juncus_cover = "Juncus cover (%)",
    sand_gravel_cover = "Sand/Gravel cover (%)",
    num_ditches = "Number of ditches"
  )) 
# Contributions of variables to PC2
Dim2b_a <- fviz_contrib(pca2, choice = "var", axes = 2, top = 11) +
  theme_classic() +
  custom_theme3() +
  coord_flip() +
  labs(x = "", y = "") +
  scale_x_discrete(labels = c(
    birch_cover = "Birch cover (%)",
    willow_cover = "Willow cover (%)",
    swardht = "Sward grass height (cm)",
    elevation = "Elevation (m)",
    hayfield = "Distance from Hayfield (m)",
    pool_cover = "Pool cover (%)",
    water_table_depth = "Watertable depth (cm)",
    num_sedgepools = "Pools with sedge plants",
    juncus_cover = "Juncus cover (%)",
    sand_gravel_cover = "Sand/Gravel cover (%)",
    num_ditches = "Number of ditches"
  ))
# Contributions of variables to PC3
Dim3b_a <- fviz_contrib(pca2, choice = "var", axes = 3, top = 11) +
  theme_classic() +
  custom_theme3() +
  coord_flip() +
  labs(x = "Environmental Factors", y = "Contributions (%)") +
  scale_x_discrete(labels = c(
    birch_cover = "Birch cover (%)",
    willow_cover = "Willow cover (%)",
    swardht = "Sward grass height (cm)",
    elevation = "Elevation (m)",
    hayfield = "Distance from Hayfield (m)",
    pool_cover = "Pool cover (%)",
    water_table_depth = "Watertable depth (cm)",
    num_sedgepools = "Pools with sedge plants",
    juncus_cover = "Juncus cover (%)",
    sand_gravel_cover = "Sand/Gravel cover (%)",
    num_ditches = "Number of ditches"
  )) 
# Contributions of variables to PC4
Dim4b_a <- fviz_contrib(pca2, choice = "var", axes = 4, top = 11) +
  theme_classic() +
  custom_theme3() +
  coord_flip() +
  labs(x = "", y = "Contributions (%)") +
  scale_x_discrete(labels = c(
    birch_cover = "Birch cover (%)",
    willow_cover = "Willow cover (%)",
    swardht = "Sward grass height (cm)",
    elevation = "Elevation (m)",
    hayfield = "Distance from Hayfield (m)",
    pool_cover = "Pool cover (%)",
    water_table_depth = "Watertable depth (cm)",
    num_sedgepools = "Pools with sedge plants",
    juncus_cover = "Juncus cover (%)",
    sand_gravel_cover = "Sand/Gravel cover (%)",
    num_ditches = "Number of ditches"
  ))

# Remove titles from individual plots
Dim1b_a <- Dim1b_a + ggtitle("PCA1")
Dim2b_a <- Dim2b_a + ggtitle("PCA2")
Dim3b_a <- Dim3b_a + ggtitle("PCA3")
Dim4b_a <- Dim4b_a + ggtitle("PCA4")

# Combine the plots using patchwork
combined_plot1b_a <- (Dim1b_a | Dim2b_a) /
  (Dim3b_a | Dim4b_a) +
  plot_annotation(
    title = "Environmental Factors Contributions Across Dimensions 1 to 4")

combined_plot1b_a

# Combine contributions from all dimensions into a single data frame 
# Reshape the data and sort by Dimension order
combined_data_b <- data.frame(
  Environmental_Factor = c(
    "Birch cover (%)", 
    "Willow cover (%)", 
    "Sward grass height (cm)", 
    "Elevation (m)",
    "Distance from Hayfield (m)", 
    "Pool cover (%)", 
    "Watertable depth (cm)",
    "Pools with sedge plants", 
    "Juncus cover (%)", 
    "Sand/Gravel cover (%)",
    "Number of ditches"
  ),
  Dim1 = get_pca_var(pca2)$contrib[, 1],
  Dim2 = get_pca_var(pca2)$contrib[, 2],
  Dim3 = get_pca_var(pca2)$contrib[, 3],
  Dim4 = get_pca_var(pca2)$contrib[, 4]
)

# Reshape the data and arrange for plotting
plot_data_b <- combined_data_b %>%
  pivot_longer(cols = Dim1:Dim4, names_to = "Dimension", values_to = "Contribution") %>%
  arrange(Dimension, Contribution)  # Sort first by Dimension, then Contribution in ascending order

# Reverse the order of Environmental_Factor for plotting
plot_data_b$Environmental_Factor <- factor(plot_data_b$Environmental_Factor, levels = rev(unique(plot_data_b$Environmental_Factor)))  # Reverse order

# Maintain PCA legend order as "1, 2, 3, 4"
plot_data_b$Dimension <- factor(plot_data_b$Dimension, levels = c("Dim1", "Dim2", "Dim3", "Dim4"))

# Create the plot with the legend in "1, 2, 3, 4" order
combined_plot2_b <- ggplot(plot_data_b, aes(x = Environmental_Factor, y = Contribution, fill = Dimension)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  custom_theme3() +
  coord_flip() +
  labs(
    title = "Environmental factors contributions",
    x = "Environmental Factors",
    y = "Contributions (%)",
    fill = "PCA Dimension"
  ) +
  scale_fill_manual(
    values = c("#B22222", "#8B4513", "#DAA520", "#5DA9E9"),  # Colors mapped for correct PCA order
    labels = c("1", "2", "3", "4")                           # Legend labels in natural order
  )

# Display the plot
combined_plot2_b


