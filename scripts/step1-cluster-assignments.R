#load packages
library(data.table)
library(ClustImpute)
library(cluster)
library(ggplot2)
library(clustMixType)
library(dplyr)
library(flexclust)
library(ClustOfVar)

#read df
cluster_des_df <- read.csv("/Users/miajovanova/t2d/variables-for-clustering.csv") 
cluster_des_df$Complications = as.factor(cluster_des_df$Complications)


#### [All: CGM + Nutrition cluster assignment]

set.seed(1234)

#numeric features 
num_cluster_des_df <- cluster_des_df %>% dplyr::select(
  Age..years.,BMI, 
  Duration.of.diabetes..years., ratio_ppg,
  premeal_1h_mean_glu_mean_24, 
  premeal_1h_mean_glu_sd_24,
  sum_total_g, 
  ratio_staples_total_24, 
  ratio_dairy_total_24, 
  ratio_legum_total_24,                          
  ratio_fruits_total_24,                          
  ratio_animals_total_24,
  ratio_veg_total_24)                      

## Feature Selection -----------------
### Numerical Variables
##first scale variables
data_scaled <- cluster_des_df %>%
  mutate_if(is.numeric, scale) 

##Check for Collinearity -----------------

#cluster_des_df <- data_scaled
correlation_matrix <- abs(cor(num_cluster_des_df))

# Optional: Define clean, human-readable labels for the variables
label_map <- c(
  Age..years. = "Age",
  BMI = "BMI",
  Duration.of.diabetes..years. = "Diabetes Duration",
  ratio_ppg = "PPG Ratio",
  premeal_1h_mean_glu_mean_24 = "Pre-meal Glucose (Mean)",
  premeal_1h_mean_glu_sd_24 = "Pre-meal Glucose (SD)",
  sum_total_g = "Total Intake (g)",
  ratio_staples_total_24 = "Staple Ratio",
  ratio_dairy_total_24 = "Dairy Ratio",
  ratio_legum_total_24 = "Legume Ratio",
  ratio_fruits_total_24 = "Fruit Ratio",
  ratio_animals_total_24 = "Animal Protein Ratio",
  ratio_veg_total_24 = "Vegetable Ratio"
)

# Melt the correlation matrix
corr_df <- as.data.frame(as.table(correlation_matrix))
corr_df$Var1 <- factor(corr_df$Var1, levels = names(label_map), labels = label_map)
corr_df$Var2 <- factor(corr_df$Var2, levels = names(label_map), labels = label_map)

# Plot
plt <- ggplot(data = corr_df, aes(x = Var1, y = Var2, 
                                  fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), color = "white", size = 4) +
  scale_fill_gradientn(colors = colorRampPalette(c("purple", "blue", "red"))(50), name = "Correlation") +
  theme_minimal() +
  labs(title = "Clustering Feature Correlation Matrix", x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

print(plt)

# Remove diagonal (self-correlations) and convert to vector
cor_values <- correlation_matrix[lower.tri(correlation_matrix, diag = FALSE)]

# Calculate summary statistics
mean_cor <- mean(cor_values, na.rm = TRUE)
min_cor <- min(cor_values, na.rm = TRUE)
max_cor <- max(cor_values, na.rm = TRUE)

# Print results
cat("Mean correlation (off-diagonal):", round(mean_cor, 3), "\n")
cat("Minimum correlation:", round(min_cor, 3), "\n")
cat("Maximum correlation:", round(max_cor, 3), "\n")



### Optimal Cluster Nr. --------------------------------------
### calculate optimal number of cluster, index values and clusterpartition with Silhouette-index
# set.seed(12345)
# 
# set.seed(12)
# nr_seeds <- 5
# seeds_vec <- sample(1:1000,nr_seeds)
# 
# savedImp <- data.frame(matrix(0,nr_seeds,dim(cluster_des_df_vars)[2]))
# count <- 1
# for (s in seeds_vec) {
#   set.seed(s)
#   res <- kproto(cluster_des_df_vars,k=val$k_opt)
#   FeatureImp_res <- FeatureImpCluster(res,as.data.table(cluster_des_df_vars),sub = 1,biter = 1)
#   savedImp[count,] <- FeatureImp_res$featureImp[sort(names(FeatureImp_res$featureImp))]
#   count <- count + 1
# }
# names(savedImp) <- sort(names(FeatureImp_res$featureImp))
# boxplot(savedImp)

## Clustering variables -----------------
cluster_des_df_vars <-cluster_des_df  %>% dplyr::select(Gender..Female.1..Male.2., 
                                                        Age..years.,BMI, 
                                                        Complications, 
                                                        Duration.of.diabetes..years., 
                                                        ratio_ppg,
                                                        premeal_1h_mean_glu_mean_24,
                                                        premeal_1h_mean_glu_sd_24, 
                                                        sum_total_g, 
                                                        ratio_staples_total_24, 
                                                        ratio_dairy_total_24, 
                                                        ratio_legum_total_24,                          
                                                        ratio_fruits_total_24,                          
                                                        ratio_animals_total_24,
                                                        ratio_veg_total_24)  

# Prepare data
cluster_des_df_vars <- cluster_des_df %>%
  select(-ID) %>% 
  mutate(Complications = as.factor(Complications)) %>%
  dplyr::select(
    Gender..Female.1..Male.2., 
    Age..years., BMI, 
    Complications, 
    Duration.of.diabetes..years., 
    ratio_ppg,
    premeal_1h_mean_glu_mean_24,
    premeal_1h_mean_glu_sd_24, 
    sum_total_g, 
    ratio_staples_total_24, 
    ratio_dairy_total_24, 
    ratio_legum_total_24,                          
    ratio_fruits_total_24,                          
    ratio_animals_total_24,
    ratio_veg_total_24
  )

# Scale numeric variables
data_scaled <- cluster_des_df_vars %>%
  mutate_if(is.numeric, scale)

# Clustering: find optimal k and run kproto
set.seed(11)
val_improv <- validation_kproto(
  method = "silhouette", 
  data = data_scaled, 
  k = 2:3, 
  nstart = 100, 
  lambda = lambdaest(data_scaled)
)

kpres_improv <- kproto(data_scaled, val_improv$k_opt, keep.data = TRUE)

# Feature importance based on final clustering
importance <- FeatureImpCluster::FeatureImpCluster(
  kpres_improv,
  as.data.table(data_scaled)
)

print(importance)
plot(importance)

# Optional: robustness check with multiple seeds (can skip if not needed)
# set.seed(12)
# nr_seeds <- 5
# seeds_vec <- sample(1:1000, nr_seeds)
# 
# savedImp <- data.frame(matrix(0, nr_seeds, dim(data_scaled)[2]))
# count <- 1
# for (s in seeds_vec) {
#   set.seed(s)
#   res <- kproto(data_scaled, k = val_improv$k_opt)
#   FeatureImp_res <- FeatureImpCluster::FeatureImpCluster(res, as.data.table(data_scaled), sub = 1, biter = 1)
#   savedImp[count, ] <- FeatureImp_res$featureImp[sort(names(FeatureImp_res$featureImp))]
#   count <- count + 1
# }
# names(savedImp) <- sort(names(FeatureImp_res$featureImp))
# boxplot(savedImp)

# Add cluster labels to data
data_scaled$cluster_des <- kpres_improv$cluster
table(data_scaled$cluster_des)
# 1  2 
# 37 37 
cluster_des_df$cluster_all_2clusters <- kpres_improv$cluster

######################################################

#### [CGM cluster assignment]


cluster_des_df1 <- data_scaled  %>% dplyr::select(Gender..Female.1..Male.2., 
                                                    Age..years.,BMI, Complications, 
                                                    Duration.of.diabetes..years., 
                                                    ratio_ppg,
                                                    premeal_1h_mean_glu_mean_24,
                                                    premeal_1h_mean_glu_sd_24) 

num_cluster_des_df <- data_scaled %>% select(
  Age..years.,BMI, 
  Complications,
  Duration.of.diabetes..years., ratio_ppg,
  premeal_1h_mean_glu_mean_24, 
  premeal_1h_mean_glu_sd_24)                      

set.seed(12345)
set.seed(12)
nr_seeds <- 5
seeds_vec <- sample(1:1000,nr_seeds)

savedImp <- data.frame(matrix(0,nr_seeds,dim(cluster_des_df1)[2]))
count <- 1
for (s in seeds_vec) {
  set.seed(s)
  res <- kproto(cluster_des_df_vars,k=val$k_opt)
  FeatureImp_res <- FeatureImpCluster(res,as.data.table(cluster_des_df_vars),sub = 1,biter = 1)
  savedImp[count,] <- FeatureImp_res$featureImp[sort(names(FeatureImp_res$featureImp))]
  count <- count + 1
}
names(savedImp) <- sort(names(FeatureImp_res$featureImp))
boxplot(savedImp)

# Prepare data
 cluster_des_df_vars <- cluster_des_df %>%
   select(-ID) %>%
  # mutate(Complications = as.factor(Complications)) %>%
   dplyr::select(
     Gender..Female.1..Male.2.,
     Age..years., BMI,
     Complications,
     Duration.of.diabetes..years.,
     ratio_ppg,
     premeal_1h_mean_glu_mean_24,
     premeal_1h_mean_glu_sd_24)

# Scale numeric variables
 data_scaled <- cluster_des_df_vars %>%
   mutate_if(is.numeric, scale)

# Clustering: find optimal k and run kproto
set.seed(11)
val_improv <- validation_kproto(
  method = "silhouette", 
  data = data_scaled, 
  k = 2:3, 
  nstart = 100, 
  lambda = lambdaest(data_scaled)
)

kpres_improv <- kproto(data_scaled, val_improv$k_opt, keep.data = TRUE)

# Feature importance based on final clustering
importance <- FeatureImpCluster(
  kpres_improv,
  as.data.table(data_scaled)
)

print(importance)
plot(importance)

# Add cluster labels to data
data_scaled$cluster_des <- kpres_improv$cluster
table(data_scaled$cluster_des)
# 1  2 
# 43 31 
cluster_des_df$cluster_only_cgm <- kpres_improv$cluster

######################################################

#### [Nutrition cluster assignment]

# Descriptive Clustering -----------------------
cluster_des_df1 <- cluster_des_df  %>% dplyr::select(Gender..Female.1..Male.2., 
                                                  Age..years.,BMI, Complications, 
                                                  Duration.of.diabetes..years., 
                                                  sum_total_g, 
                                                  ratio_staples_total_24, 
                                                  ratio_dairy_total_24, 
                                                  ratio_legum_total_24,                          
                                                  ratio_fruits_total_24,                          
                                                  ratio_animals_total_24,
                                                  ratio_veg_total_24) 

data_scaled <- cluster_des_df1 %>%
  mutate_if(is.numeric, scale) 
#mean_ppg

# 
# num_cluster_des_df <- data_scaled %>% select(
#   Age..years.,BMI, 
#   Complications,
#   Duration.of.diabetes..years., ratio_ppg,
#   premeal_1h_mean_glu_mean_24, 
#   premeal_1h_mean_glu_sd_24)                      

## Feature Selection -----------------
set.seed(12345)

set.seed(12)
nr_seeds <- 5
seeds_vec <- sample(1:1000,nr_seeds)

savedImp <- data.frame(matrix(0,nr_seeds,dim(cluster_des_df1)[2]))
count <- 1
for (s in seeds_vec) {
  set.seed(s)
  res <- kproto(cluster_des_df_vars,k=val$k_opt)
  FeatureImp_res <- FeatureImpCluster(res,as.data.table(cluster_des_df_vars),sub = 1,biter = 1)
  savedImp[count,] <- FeatureImp_res$featureImp[sort(names(FeatureImp_res$featureImp))]
  count <- count + 1
}
names(savedImp) <- sort(names(FeatureImp_res$featureImp))
boxplot(savedImp)

# Clustering: find optimal k and run kproto
set.seed(11)
val_improv <- validation_kproto(
  method = "silhouette", 
  data = data_scaled, 
  k = 2:3, 
  nstart = 100, 
  lambda = lambdaest(data_scaled)
)

kpres_improv <- kproto(data_scaled, val_improv$k_opt, keep.data = TRUE)

# Feature importance based on final clustering
importance <- FeatureImpCluster(
  kpres_improv,
  as.data.table(data_scaled)
)

print(importance)
plot(importance)

# Add cluster labels to data
data_scaled$cluster_des <- kpres_improv$cluster
table(data_scaled$cluster_des)
# 1  2  3 
# 30 19 25 

cluster_des_df$cluster_nutri <- kpres_improv$cluster

# cluster_assignments = cluster_des_df %>% 
#   select(ID, cluster_only_cgm, cluster_nutri, cluster_all_2clusters)
# write.csv(cluster_assignments, "cluster_assignments.csv")
# write.csv(cluster_des_df, "cluster_descriptives")


