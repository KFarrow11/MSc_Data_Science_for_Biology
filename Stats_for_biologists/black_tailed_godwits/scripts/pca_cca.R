# DATA + PACKAGES
source("scripts/cleaning.R")

# Load your dataframe
pca_godwits

# Select relevant variables
variables <- c("elevation", "swardht", "willow_cover", "birch_cover", "juncus_cover", "sand_gravel_cover", "num_ditches", "water_table_depth", "hayfield", "num_sedgepools", "pool_cover")
X <- pca_godwits %>% select(all_of(variables))

## Run PCA and standardize the data ----
pca <- prcomp(pca_godwits[, numeric_columns], center = TRUE, scale. = TRUE)
summary(pca)

## Extract PCA scores ---- 
pca_scores <- data.frame(pca$x, godwits$god_occur) 

# Scree plot
screeplot <- fviz_eig(pca_result, , addlabels = TRUE)
screeplot

# Combine PCA scores with original dataframe
pca_godwits_combined <- cbind(pca_godwits, pca_df[, 1:2])  # Select first two principal components
pca_godwits_combined

# Select the response variable (presence/absence of godwits)
response <- as.factor(pca_godwits_combined$god_occur)

# Conduct CCA using the first two principal components
cca_result <- cca(pca_scores[, 1:2] ~ response)

# Summary of the CCA result
summary(cca_result)

# Extract CCA scores
cca_scores <- scores(cca_result, display = "sites")
cca_df <- as.data.frame(cca_scores)

# Visualize PCA variables in a Biplot
biplot <- fviz_pca_biplot(pca_result, repel = TRUE)
print(biplot)



# aim 2 ----
# Filter for present godwits
filtered_godwits <- pca_godwits2 %>% filter(godwit_presence == 1)

# Select relevant variables
variables2 <- c("elevation", "swardht", "willow_cover", "birch_cover", "juncus_cover", "sand_gravel_cover", "num_ditches", "water_table_depth", "hayfield", "num_sedgepools", "pool_cover")
X2 <- filtered_godwits %>% select(all_of(variables2))

# Standardize the data
X2_scaled <- scale(X2)

# Conduct PCA
pca_result2 <- prcomp(X2_scaled, scale. = TRUE)

# Scree plot
screeplot2 <- fviz_eig(pca_result2)
screeplot2

# Extract PCA scores
pca_scores2 <- pca_result2$x
pca_df2 <- as.data.frame(pca_scores2)
pca_df2

# Combine PCA scores with original dataframe
filtered_godwits_combined <- cbind(filtered_godwits, pca_df2[, 1:2])  # Select first two principal components
filtered_godwits_combined

# Select the response variable (godwit density)
response2 <- filtered_godwits_combined$godwit_density

# Conduct CCA using the first two principal components
cca_result2 <- cca(pca_scores2[, 1:2] ~ response2)

# Summary of the CCA result
summary(cca_result2)

# Extract CCA scores
cca_scores2 <- scores(cca_result2, display = "sites")
cca_df2 <- as.data.frame(cca_scores2)
cca_df2

# Visualize PCA variables in a Biplot
biplot2 <- fviz_pca_biplot(pca_result2, repel = TRUE)
biplot2


