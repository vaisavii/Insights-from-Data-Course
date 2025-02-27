# PROJECT WORK 2
# Insights from data
# Viivi Väisänen

install.packages("ggplot2")  
install.packages("dplyr")    
install.packages("tidyr")    
install.packages("cluster")
install.packages("klaR")

library(ggplot2)
library(dplyr)
library(klaR)
library(tidyr)
library(cluster)


# ** 1. Data loading and preprocessing **

# Loading the vaalikone question and profile dataset

vaalikone_questions <- read.csv('C:/Users/viivi/OneDrive/Tiedostot/TKT kurssit/DNK/vaalikone_questions_all.csv',
                                sep = ";", stringsAsFactors = FALSE)

vaalikone_profiles <- read.csv('C:/Users/viivi/OneDrive/Tiedostot/TKT kurssit/DNK/vaalikone_profiles_all.csv',
                               sep = ";", stringsAsFactors = FALSE)


# Checking the structure
str(vaalikone_questions)

View(vaalikone_questions)

# all variables in the dataset
colnames(vaalikone_questions)



# Removing summary variables from the dataset

# Columns to remove
columns <- c("CountAnsw", "CountQ21.Fb", "CountQ31.GvtPrt", "CountNegW", "CountPosW")

vaalikone_questions <- subset(vaalikone_questions, select = !(names(vaalikone_questions) %in% columns))


# Show first few rows to confirm
head(vaalikone_questions)

  



# Exploring outliers
# Selecting numeric columns
numeric_columns <- vaalikone_questions %>% select_if(is.numeric)

# Check if numeric_columns is empty
if (ncol(numeric_columns) == 0) {
  stop("No numeric columns found.")
}

# Split the numeric columns into 5 groups (you can adjust the number of groups)
num_groups <- 5
cols_per_group <- ceiling(ncol(numeric_columns) / num_groups)

# Divide the columns into groups
column_groups <- split(names(numeric_columns), rep(1:num_groups, each = cols_per_group, length.out = ncol(numeric_columns)))

# Create a list of boxplots for each group of columns
plot_list <- lapply(1:num_groups, function(i) {
  cols <- column_groups[[i]]
  
  # Subset the data for the current group of columns
  subset_data <- vaalikone_questions[, cols]
  
  # Gather the data in long format for easier plotting
  long_data <- gather(subset_data, key = "variable", value = "value")
  
  # Create the boxplot for the current subset of variables
  ggplot(long_data, aes(x = variable, y = value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste("Boxplot of Numeric Variables - Group", i))  # Use 'i' to label each plot group
})

# Display the plots (each will be shown separately)
plot_list[[1]]  # Show the first group
plot_list[[2]]  # Show the second group
plot_list[[3]]  # Show the third group
plot_list[[4]]  # Show the fourth group
plot_list[[5]]  # Show the fifth group (if applicable








# Exploring missing values
# the percentage of missing values for each column
missing_percentage <- vaalikone_questions %>%
  summarise_all(~ mean(is.na(.)) * 100)  # mean(is.na(.)) calculates the proportion of missing values

# Convert the result into a tidy data frame for easier viewing
missing_percentage_long <- as.data.frame(t(missing_percentage))  # Transpose to make variables as rows
colnames(missing_percentage_long) <- "missing_percentage"  # Rename column
missing_percentage_long$variable <- rownames(missing_percentage_long)  # Add variable names

# Display the results
missing_percentage_long <- missing_percentage_long %>%
  arrange(desc(missing_percentage))  # Optional: to sort by the percentage of missing values

# View the result
print(missing_percentage_long)

# Average percentage of missing values across all columns
average_missing_percentage <- mean(missing_percentage_long$missing_percentage)
cat("Average percentage of missing values across all variables: ", average_missing_percentage, "%\n")






# Let's examine the missing values in the data


# the percentage of missing values for each column
missing_percentage <- vaalikone_questions %>%
  summarise_all(~ mean(is.na(.)) * 100)  # mean(is.na(.)) calculates the proportion of missing values

# Convert the result into a tidy data frame for easier viewing
missing_percentage_long <- as.data.frame(t(missing_percentage))  # Transpose to make variables as rows
colnames(missing_percentage_long) <- "missing_percentage"  # Rename column
missing_percentage_long$variable <- rownames(missing_percentage_long)  # Add variable names

# Display the results
missing_percentage_long <- missing_percentage_long %>%
  arrange(desc(missing_percentage))  # Optional: to sort by the percentage of missing values

# View the result
print(missing_percentage_long)

# This shows, that ID is never NA, but the precentage
# across other columns is about 20%. 

# Let's remove rows where all the responses (excluding the ID)
# are NA, since they do not provide any awnsers, which are useful inofmation to the clustering.

# Select columns to check for NAs (excluding ID)
columns_to_check <- setdiff(names(vaalikone_questions), c("ID"))

# Remove rows where all of the selected columns have NAs
vaalikone_questions <- vaalikone_questions[!apply(vaalikone_questions[columns_to_check], 1, function(x) all(is.na(x))), ]

#Let's also remove rows that are 50% NA
threshold <- 0.5
vaalikone_questions <- vaalikone_questions[rowMeans(is.na(vaalikone_questions)) <= threshold, ]

# View the cleaned data
View(vaalikone_questions)



# Let's check the precentage of NA values after handling them.

# Exploring missing values
# the percentage of missing values for each column
missing_percentage <- vaalikone_questions %>%
  summarise_all(~ mean(is.na(.)) * 100)  # mean(is.na(.)) calculates the proportion of missing values

# Convert the result into a tidy data frame 
missing_percentage_long <- as.data.frame(t(missing_percentage))  # Transpose to make variables as rows
colnames(missing_percentage_long) <- "missing_percentage"  # Rename column
missing_percentage_long$variable <- rownames(missing_percentage_long)  # Add variable names

# Display the results
missing_percentage_long <- missing_percentage_long %>%
  arrange(desc(missing_percentage))  # Optional: to sort by the percentage of missing values

# View the result
print(missing_percentage_long)


# Average percentage of missing values across all columns
average_missing_percentage <- mean(missing_percentage_long$missing_percentage)
cat("Average percentage of missing values across all variables: ", average_missing_percentage, "%\n")
# returns 0,14333%

# Define the get_mode function
get_mode <- function(x) {
  x <- na.omit(x)  # Remove NAs for mode calculation
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

# Apply imputation to all character columns
vaalikone_questions <- vaalikone_questions %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), get_mode(.), .)))


# Imputing missing values with the avergage value of each column
vaalikone_questions[] <- lapply(vaalikone_questions, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE) # Replace NA with column mean
  }
  return(x)
})



# all columns starting with "Q" to factors
q_columns <- grep("^Q", names(vaalikone_questions), value = TRUE)
vaalikone_questions[q_columns] <- lapply(vaalikone_questions[q_columns], as.factor)

# all columns starting with "W" to ordered factors
w_columns <- grep("^W", names(vaalikone_questions), value = TRUE)
vaalikone_questions[w_columns] <- lapply(vaalikone_questions[w_columns], function(x) as.ordered(x))

# structure of the dataset after the conversion
str(vaalikone_questions)





# ** 2. Clustering candidates **


# Clustering the cleaned candidate data

# Remove the 'ID' column before computing the Gower distance
vaalikone_questions_no_id <- vaalikone_questions[, !names(vaalikone_questions) %in% "ID"]

# Computing Gower distance
gower_dist <- daisy(vaalikone_questions_no_id, metric = "gower")

summary(gower_dist)

# the result to a matrix
gower_matrix <- as.matrix(gower_dist)

print(gower_matrix)


# Exclude diagonal elements
diag(gower_matrix) <- NA

# Most similar pair
most_similar <- which(gower_matrix == min(gower_matrix, na.rm = TRUE), arr.ind = TRUE)
vaalikone_questions_no_id[most_similar[1, ], ]

# Most dissimilar pair
most_dissimilar <- which(gower_matrix == max(gower_matrix, na.rm = TRUE), arr.ind = TRUE)
vaalikone_questions_no_id[most_dissimilar[1, ], ]



# Finding out the optimal number of clusters with elbow-method
mds_result <- cmdscale(as.dist(gower_dist), k = 2)

# Calculate WSS for different cluster counts
wss <- function(k) {
  kmeans(mds_result, centers = k, nstart = 10)$tot.withinss
}
k_values <- 1:10
wss_values <- sapply(k_values, wss)

# Plot WSS values
plot(k_values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K", ylab = "Total within-cluster sum of squares",
     main = "Elbow Method for Optimal Clusters")
# by that, it can be 3.


# Perform PAM clustering for different numbers of clusters
sil_width <- sapply(2:10, function(k) {
  pam_result <- pam(as.dist(gower_dist), k = k)
  mean(silhouette(pam_result$clustering, dist(gower_dist))[, 3])
})

# Plot Silhouette Width
plot(2:10, sil_width, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K", ylab = "Average silhouette width",
     main = "Silhouette Method for Optimal Clusters")


# by silhoutte width, the optimal k is 2 or 3


# PAM works directly with Gower distances and is a better alternative for mixed data types.

# Perform PAM clustering
pam_result <- pam(gower_dist, k = 3)  # k is the number of clusters

# Cluster assignment
pam_result$clustering

install.packages('FactorMineR')
install.packages('factoextra')

library(FactoMineR)
library(factoextra)

# Perform PCoA (Principal Coordinates Analysis) on the Gower distance
pcoa_result <- cmdscale(gower_dist, k = 2)  # Reduce to 2 dimensions for visualization

# data frame with PCoA coordinates and cluster labels
pcoa_df <- data.frame(
  Dim1 = pcoa_result[, 1],
  Dim2 = pcoa_result[, 2],
  Cluster = as.factor(pam_result$clustering)  # Assign cluster labels
)

# Plot the PCoA results with ggplot2
library(ggplot2)
ggplot(pcoa_df, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "PCoA: Clusters Based on Categorical Data",
       x = "PCoA Dimension 1", y = "PCoA Dimension 2",
       color = "Cluster") +
  theme_minimal()

View(vaalikone_questions_no_id)

# Merging the datasets

# first adding clustering results to vaalikone_questions
vaalikone_questions$cluster <- pam_result$clustering

# Merging the datasets on the ID column
vaalikone_combined <- merge(vaalikone_questions, vaalikone_profiles, by = "ID", all.x = TRUE)

View(vaalikone_combined)


# visualizing merged dataset with results

# Bar plot of clusters vs. political parties
ggplot(vaalikone_combined, aes(x = as.factor(cluster), fill = Party)) +
  geom_bar(position = "fill") +  # Use 'fill' to show proportions
  labs(
    x = "Cluster",
    y = "Proportion",
    title = "Cluster Alignment with Political Parties"
  ) +
  theme_minimal()



# Cluster Alignment with Age visualization: ggplot
ggplot(vaalikone_combined, aes(x = Age, y = cluster, color = as.factor(cluster))) +
  geom_jitter(width = 0, height = 0.2, size = 2, alpha = 0.7) +  # Adds jitter to separate points
  scale_color_viridis_d(name = "Cluster") +  # Color by cluster
  labs(title = "Cluster Alignment with Age", x = "Age", y = "Cluster") +
  theme_minimal()

# Cluster Alignment with Age visualization: boxplot
ggplot(vaalikone_combined, aes(x = as.factor(cluster), y = Age, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Age Distribution by Cluster", x = "Cluster", y = "Age") +
  scale_fill_viridis_d(name = "Cluster") +
  theme_minimal()

# Cluster Alignment with Age visualization: boxplot (more ditailed version of distripution)
ggplot(vaalikone_combined, aes(x = as.factor(cluster), y = Age, fill = as.factor(cluster))) +
  geom_violin() +
  labs(title = "Age Distribution by Cluster", x = "Cluster", y = "Age") +
  scale_fill_viridis_d(name = "Cluster") +
  theme_minimal()



# Cluster alignment with gender: side by side barblot to show count of gender in each cluster
ggplot(vaalikone_combined, aes(x = as.factor(cluster), fill = as.factor(Gender))) +
  geom_bar(position = "dodge") +  # Side-by-side bars
  labs(title = "Gender Distribution Across Clusters", x = "cluster", y = "Count") +
  scale_fill_manual(values = c("blue", "pink"), labels = c("Male", "Female")) +
  theme_minimal()


# stacked barplot to show the distripution of education levels within each cluster
ggplot(vaalikone_combined, aes(x = as.factor(cluster), fill = as.factor(EduLvl))) +
  geom_bar(position = "fill") +  # Stacked bars normalized to show proportions
  labs(title = "Education Level Distribution Across Clusters", x = "Cluster", y = "Proportion") +
  scale_fill_brewer(palette = "Set3", name = "Education Level") +
  theme_minimal()


#side-byside bar plot, each education level presented side by side
ggplot(vaalikone_combined, aes(x = as.factor(cluster), fill = as.factor(EduLvl))) +
  geom_bar(position = "dodge") +  # Side-by-side bars
  labs(title = "Education Level Distribution Across Clusters", x = "Cluster", y = "Count") +
  scale_fill_brewer(palette = "Set3", name = "Education Level") +
  theme_minimal()


ggplot(vaalikone_combined, aes(x = as.factor(cluster), fill = as.factor(MP))) +
  geom_bar(position = "fill") +  # Stacked bars normalized to proportions
  labs(
    title = "MP Status Distribution Across Clusters",
    x = "Cluster",
    y = "Proportion"
  ) +
  scale_fill_manual(
    values = c("orange", "blue"),
    name = "MP Status",
    labels = c("Non-MP", "MP")
  ) +
  theme_minimal()