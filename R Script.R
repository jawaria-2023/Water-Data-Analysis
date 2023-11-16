# Loading Necessary Libraries
library(readxl)  # For reading Excel files
library(ggplot2) # For data visualization
library(FactoMineR) # For PCA
library(factoextra) # For visualizing PCA and clustering results
library(stats) # For statistical analysis

# Reading Water Data from Excel File
# Replace the file path with the actual path of your water data file
file_path <- "/path/to/WATER ANALYSIS.xlsx"
water_data <- read_excel(file_path)

# Descriptive Statistics
water_summary <- summary(water_data)

# Shapiro-Wilk Test for Normality (for the 'PH' variable)
shapiro_result_ph <- shapiro.test(water_data$PH)

# ANOVA to Compare 'PH' Across Different Cities
anova_result_ph <- aov(PH ~ Cities, data = water_data)
anova_summary_ph <- summary(anova_result_ph)

# Regression Analysis (e.g., TDS predicted by Conductivity)
regression_model <- lm(TDS ~ Conductivity, data = water_data)
regression_summary <- summary(regression_model)

# Principal Component Analysis (PCA)
# Ensure only numerical data is used
water_data_numeric <- water_data[, sapply(water_data, is.numeric)]
pca_result <- PCA(water_data_numeric, scale.unit = TRUE, graph = FALSE)
pca_biplot <- fviz_pca_biplot(pca_result, label = "var", habillage = water_data$Cities, addEllipses = TRUE)

# Hierarchical Clustering
water_data_scaled <- scale(water_data_numeric)  # Standardizing the data
hc_result <- hclust(dist(water_data_scaled))  # Performing clustering
hc_plot <- plot(hc_result)  # Plotting the dendrogram

# Boxplot for 'PH' Levels Across Cities
boxplot_ph <- ggplot(water_data, aes(x = Cities, y = PH)) +
  geom_boxplot() +
  labs(title = "Boxplot of PH Levels by City", x = "City", y = "PH Level")

# Interaction Plot for 'PH' and 'Hardness' Across Cities
interaction_plot_ph_hardness <- interaction.plot(water_data$PH, water_data$Cities, water_data$Hardness)

# Saving Results to a List for Easy Access
results_list <- list(
  water_summary = water_summary,
  normality_test_ph = shapiro_result_ph,
  anova_ph = anova_summary_ph,
  regression_model = regression_summary,
  pca_biplot = pca_biplot,
  hierarchical_clustering = hc_plot,
  boxplot_ph = boxplot_ph,
  interaction_ph_hardness = interaction_plot_ph_hardness)

# Replace '/path/to/...' with the actual path to your water data Excel file.
