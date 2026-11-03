# Prepare and filter data (subsetting only continuous numeric columns)
data_subset <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]

# Perform PCA (scale = TRUE standardizes the variables)
pca_result <- stats::prcomp(data_subset, center = TRUE, scale. = TRUE)

# View standard deviations and variable proportions
summary(pca_result)

# Calculate variance and variance proportions
pca_var <- pca_result$sdev^2
pca_var_perc <- round(pca_var / sum(pca_var) * 100, 1)

# Print percentage of variance explained (e.g., PC1, PC2...)
print(pca_var_perc)

biplot(pca_result, scale = 0)

library(ggplot2)

# Convert PCA scores into a data frame
pca_df <- data.frame(
	PC1 = pca_result$x[, 1],
	PC2 = pca_result$x[, 2],
	Cylinders = as.factor(mtcars$cyl) # Meta variable for coloring
)

# Plot observations along PC1 and PC2
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cylinders)) +
	geom_point(size = 3) +
	labs(
		x = paste0("PC1 (", pca_var_perc[1], "%)"),
		y = paste0("PC2 (", pca_var_perc[2], "%)")
	) +
	theme_minimal()
