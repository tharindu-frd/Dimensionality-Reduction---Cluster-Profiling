library("FactoMineR")
library("factoextra")
library(FactoMineR)
library(cluster)
library(factoextra)
library("psych")
library("MASS")
library('vcvComp')
library(cluster)
library(dplyr)

df = read.csv('C:/Users/Tharindu/Downloads/fraud.csv')
numerical_features <- c("safty_rating","past_num_of_claims","liab_prct","age_of_vehicle","vehicle_price","vehicle_weight","month","year","age_of_driver","annual_income","claim_est_payout","day" )
all_features <- colnames(df)
categorical_features <- setdiff(all_features, numerical_features)
categorical_features <- setdiff(categorical_features, "fraud")
categorical_features <- setdiff(categorical_features, "claim_number")
categorical_features <- union(categorical_features, "zip_code")

str(df)







############   Fishers Discriminant Analysis For Numerical Variables  ################
#####################################################################################

df.numerical <- df[, numerical_features]
df.numerical <- na.omit(df.numerical)



B = cov.B(df.numerical, df$fraud)  ## Between class covariance
W = cov.W(df.numerical, df$fraud)  ## Within class covariance
df.eig <- eigen(solve(W)%*% B)
df.eig$values


V <- df.eig$vectors[,1:2]
z <- as.matrix(df.numerical)%*% V
z_real <- data.frame(x = Re(z[, 1]), y = Re(z[, 2]))
ggplot(z_real,aes(x = x, y = y)) +
  geom_point() +
  labs(x = "Real part of Z[,1]", y = "Real part of Z[,2]", title = "Scatter plot of Real Parts")







#####  Multiple Correspondance Analysis for   categorical variables ###########
df.active <- df[, categorical_features]
df.active <- na.omit(df.active)
df.active[] <- lapply(df.active, factor)
df.active[categorical_features] <- lapply(df.active[categorical_features], factor)
mca <- MCA(df.active, graph = FALSE)








################  Merge the scores of MCA and FDA ########
mca_scores <- as.data.frame(mca$ind$coord[, 1:5])
colnames(mca_scores) <- paste0("Dim", 1:5)
final_df <- cbind(z_real, mca_scores)
head(final_df)


#### scale the data
final_df_scaled = scale(final_df)

### Distance 
new_df = dist(final_df_scaled)

###### WCSS plot 
fviz_nbclust(final_df_scaled,kmeans,method="wss") + labs(subtitle = 'Elbow Method')



set.seed(123)  
kmeans_result <- kmeans(final_df_scaled, centers = 6, nstart = 25)
silhouette_scores <- silhouette(kmeans_result$cluster, dist(final_df_scaled))
avg_silhouette_score <- mean(silhouette_scores[, 'sil_width'])
avg_silhouette_score

km.clusters <- kmeans_result$cluster
fviz_cluster(list(data=final_df_scaled,cluster=km.clusters))




#######################  k-mediod for the original data set ################

# Convert specified columns to factors
df <- df %>%
  mutate(across(all_of(categorical_features), as.factor))

str(df)

dist_matrix <- daisy(df, metric = "gower")
pam_result <- pam(dist_matrix, k = 6)
silhouette_scores <- silhouette(pam_result$clustering, dist_matrix)
avg_silhouette_score <- mean(silhouette_scores[, 'sil_width'])
avg_silhouette_score
















