# Project Work 3
# Insights from Data
# Viivi Väisänen

install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("cluster")
install.packages("plotly")
install.packages("caret")
install.packages("class")


library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(cluster)
library(plotly)
library(caret)
library(class)

diabetes <- read.csv('C:/Users/viivi/OneDrive/Tiedostot/TKT kurssit/DNK/diabetes.csv',
                                sep = ",", stringsAsFactors = FALSE)

View(diabetes)
str(diabetes)


# Preprocessing and data cleaning

# 1. Handle invalid values

# counting the amount of 0 values in each column (excluding the Outome col)
zero_counts <- colSums(diabetes[ , colnames(diabetes) != "Outcome"] == 0)
print(zero_counts)

# barplot for zero_counts
barplot(zero_counts,
        main = "Number of Zeros in Each Column",
        xlab = "Columns",
        ylab = "Zero Counts",
        col = "skyblue",
        las = 2)


# Replacing 0 values with the median of the respective values
diabetes[ , colnames(diabetes) != "Outcome"] <- lapply(diabetes[ , colnames(diabetes) != "Outcome"], function(column) {
  # Replacing zeros with median (excluding zeros from the median calculation)
  column[column == 0] <- median(column[column != 0], na.rm = TRUE)
  return(column)
})

# Converting Outcome column into categorical datatype
diabetes$Outcome <- as.factor(diabetes$Outcome)




#---------------------------------------------------------------------

# 2. Outliers

# Looking for the outliers:
# Boxplot for each numeric column
boxplot(diabetes[ , sapply(diabetes, is.numeric)], 
        main = "Boxplot of Numeric Features",
        col = "lightgreen",
        las = 2)

# Insulin looks unusual, lets see the distripution more with histogram and boxplot
hist(diabetes$Insulin, 
     main = "Histogram of Insulin Levels", 
     xlab = "Insulin Level", 
     col = "lightgreen", 
     breaks = 30)

boxplot(diabetes$Insulin, 
        main = "Boxplot of Insulin",
        col = "lightgreen",
        las = 2)


# Seeing SkinThickness better with boxplot
boxplot(diabetes$SkinThickness, 
        main = "Boxplot of SkinThickness",
        col = "lightgreen",
        las = 2)


# HANDLING OUTLIERS: removing values that are over 600 in Insulin
diabetes <- diabetes[diabetes$Insulin <= 600, ]

max_insulin <- max(diabetes$Insulin)
print(max_insulin)

# Removing values that are over 70, so we can get rid of the one extreme outlier
diabetes <- diabetes[diabetes$SkinThickness <= 70, ]

max_skin <- max(diabetes$SkinThickness)
print(max_skin)


################################################################

# Data Exploration

# 1. Class distribution

# calculating class frequencies
class_distribution <- table(diabetes$Outcome)
print(class_distribution)

# barplot
barplot(
  class_distribution,
  main = "Class Distribution of Diabetes Diagnosis",
  xlab = "Outcome (0: No Diabetes, 1: Diabetes)",
  ylab = "Frequency",
  col = c("skyblue", "salmon"),
  names.arg = c("No Diabetes", "Diabetes")
)

#-------------------------------------------------------------------

# Calculate variance for each numeric column
variances <- apply(diabetes, 2, var)

# Convert to a data frame for visualization
variance_df <- data.frame(Variable = names(variances), Variance = variances)

# Load ggplot2 library
library(ggplot2)

# Exclude the "Outcome" variable
variance_df_filtered <- variance_df[variance_df$Variable != "Outcome", ]

# Reorder the filtered data by variance
variance_df_filtered$Variable <- reorder(variance_df_filtered$Variable, variance_df_filtered$Variance)

# Plot with ordered bars
ggplot(variance_df_filtered, aes(x = Variable, y = Variance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Variance of Input Variables (Excluding Outcome)", 
       x = "Variable", 
       y = "Variance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate axis labels if needed


#-------------------------------------------------------------------------


# 3. Correlation Analysis

# Exclude the "Outcome" variable
diabetes_filtered <- diabetes[ , !(names(diabetes) %in% "Outcome")]

# Correlation matrix
cor_matrix <- cor(diabetes_filtered, use = "pairwise.complete.obs")

# Visualize as a heatmap
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           title = "Correlation Heatmap")



##########################################################################



# Cluster Analysis

# Data transformation: scaling the data

scaled_diabetes <- scale(diabetes_filtered)



# Determining the Number of Clusters

# Elbow method

# Setting up a vector to store WCSS for each k
wcss <- numeric()

# Computing WCSS for k = 1 to 10
for (k in 1:10) {
  kmeans_result <- kmeans(scaled_diabetes, centers = k, nstart = 25)
  wcss[k] <- kmeans_result$tot.withinss
}

# Plotting the elbow curve
plot(1:10, wcss, type = "b", pch = 19, col = "blue",
     xlab = "Number of Clusters (k)", ylab = "WCSS",
     main = "Elbow Method for Optimal Clusters")

# Elbow method does not show any clear result


# Silhoutte method

avg_silhouette <- numeric()
for (k in 2:10) {
  km <- kmeans(scaled_diabetes, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(scaled_diabetes))
  avg_silhouette[k] <- mean(sil[, 3])  # Mean silhouette width
}
plot(2:10, avg_silhouette[2:10], type = "b", main = "Average Silhouette Width",
     xlab = "Number of Clusters", ylab = "Average Silhouette Width")

# by silhouette width, k=2

#-------------------------------------------------------------------------

# K-means Clustering

# optimal number of k, based on silhouette analysis
k <- 3  # 3 showed best results

# Running k-means clustering
kmeans_result <- kmeans(scaled_diabetes, centers = k, nstart = 25)

# cluster labels to the dataset
diabetes_with_clusters <- cbind(diabetes, Cluster = kmeans_result$cluster)

#--------------------------------------------------------------------

# Cluster Visualization

# Principal Component Analysis (PCA)
pca_result <- prcomp(scaled_diabetes, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x)

# Adding cluster labels from k-means
pca_data$Cluster <- factor(kmeans_result$cluster)

# Plotting the first two principal components
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  ggtitle("Clusters Visualized with PCA")



# Bar plot of Outcome distribution by Cluster
ggplot(diabetes_with_clusters, aes(x = factor(Cluster), fill = factor(Outcome))) +
  geom_bar(position = "fill") +  # Use 'fill' for proportions
  labs(x = "Cluster", y = "Proportion", fill = "Outcome") +
  ggtitle("Outcome Distribution by Cluster") +
  theme_minimal()


ggplot(diabetes_with_clusters, aes(x = Pregnancies, y = Glucose, color = as.factor(Cluster))) +
  geom_point() +
  labs(title = "Cluster Visualization: Pregnancies vs Glucose", x = "Number of Pregnancies", y = "Plasma Glucose Concentration") +
  theme_minimal()


# Contingency table comparing cluster assignments to the outcome variable to
# show how many patients in each cluster have diabetes or not
contingency_table <- table(diabetes_with_clusters$Cluster, diabetes_with_clusters$Outcome)
print(contingency_table)


# Profile the clusters by comparing means of input variables
cluster_profile <- aggregate(diabetes_with_clusters[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")], 
                             by = list(cluster = diabetes_with_clusters$Cluster), 
                             FUN = mean)
print(cluster_profile)



# install.packages("GGally")
library(GGally)
# Pairwise scatterplot with coloring based on Outcome or Cluster
ggpairs(diabetes_with_clusters[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome", "Cluster")],
        aes(color = factor(Cluster), alpha = 0.5)) +
  theme_minimal() +
  labs(title = "Pairwise Plot of Input Variables with Clusters")




############################################################################


# Classification Analysis: KNN to predict diabetes 

set.seed(123)

# 1. Train-Test Split
split <- createDataPartition(diabetes_with_clusters$Outcome, p = 0.8, list = FALSE)
Xdata <- diabetes_with_clusters %>% select(-Outcome)  # Exclude the target variable
Xtrain <- Xdata[split, ]
Xtest <- Xdata[-split, ]
Ytrain <- diabetes_with_clusters$Outcome[split]
Ytest <- diabetes_with_clusters$Outcome[-split]


# Training KNN classifiers for different values of k and display confusion matrices
set.seed(123)
k_values <- c(1, 5, 7, 15, 20)  # Five different values of k

# Storing confusion matrices for each k
results <- list()

for (k in k_values) {
  knns <- knn(train = trainsc, test = testsc, cl = Ytrain, k = k)
  cm <- confusionMatrix(as.factor(Ytest), knns)
  results[[as.character(k)]] <- cm
  cat("\nConfusion Matrix for k =", k, ":\n")
  print(cm$table)
  cat("\nAccuracy for k =", k, ":", cm$overall["Accuracy"], "\n")
}

