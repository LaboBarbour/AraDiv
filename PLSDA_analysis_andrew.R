library(janitor)
library(pls)
library(readr)
library(tidyverse)



# read in data
full_df <- read_csv2("full_df.csv")

full_df_clean <- full_df |> 
  clean_names()


write_csv2(full_df_clean, file = "full_df_clean.csv")

# not in
`%notin%` <- Negate(`%in%`)


# What is the target variable? 
inVar <- "AOP_status"


### Create plsrda dataset

full_df_clean |> 
  select(starts_with("x")) |> 
  select(-x1001g_id) |> 
  as.matrix()

names(full_df_clean) |> tail(20)

full_df_clean |> count(aop_status, classification_name)


set.seed(1234)
plsda_small <- full_df_clean %>%
  select(x500:x550,
         aop_status) |> 
  # JUST TAKE TWO CATEGORIES!
  filter(aop_status %in% c("Alk", "MSO")) |> 
  mutate(aop_status = as.numeric(aop_status == "Alk")) |> 
  # RANDOMLY SUBSAMPLE AND BALANCE CLASSES
  group_by(aop_status) |> 
  sample_n(100, replace = FALSE)
  

# ARE THERE MISSING DATA

# make a column that is a matrix
plsda_model_data <- plsda_small |> select(aop_status) |> as.data.frame()
wv_mat <- plsda_small |> 
  ungroup() |> 
  select(starts_with("x")) |> 
  as.matrix()

plsda_model_data$wv_mat <- wv_mat

str(plsda_model_data)

# n_comp <- 2  # Number of PLS components to use, adjust as needed
plsda_model <- plsr(aop_status ~ wv_mat, data = plsda_model_data, ncomp = 10, validation = "LOO")

plsda_model

alk_mso <- full_df_clean %>%
  select(x500:x550,
         aop_status) |> 
  # JUST TAKE TWO CATEGORIES!
  filter(aop_status %in% c("Alk", "MSO")) |> 
  mutate(aop_status = as.numeric(aop_status == "Alk")) 

summary(plsda_model)

predict(plsda_model, newdata = alk_mso)


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#


#Random Forest analysis


#make this example reproducible
set.seed(1234)


#Make sure the predictor data is numeric of a factor
class(full_df_clean$classification_name)

#make another data set to not change the base dataset

random_forest_data <- full_df_clean

random_forest_data$classification_name <- as.factor(random_forest_data$classification_name)


#fit the random forest model
model <- randomForest(
  formula = classification_name ~ .,
  data = random_forest_data
)

#display fitted model
model

#find number of rows with missing values
sum(!complete.cases(random_forest_data))

#replace NAs with column medians only if there is
for(i in 1:ncol(random_forest_data)) {
  airquality[ , i][is.na(random_forest_data[ , i])] <- median(random_forest_data[ , i], na.rm=TRUE)
}

#remove every line not usefull, keep predictor and response data
random_forest_data <- random_forest_data |> 
  select(x350:x500,
         classification_name)


call:
  randomForest(formula = classification_name ~ ., data = random_forest_data) 


#find number of trees that produce lowest test MSE
which.min(model$mse)



#find RMSE of best model
sqrt(model$mse[which.min(model$mse)]) 


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
  
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
 # --------------------------------------------------------------------------------------------------#
  
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
  
#PCA analysis


# Generate a sample dataset (replace this with your own dataset)
set.seed(1234)
num_samples <- 2150
num_variables <- 7

# Create a random dataset
pca_data <- matrix(rnorm(num_samples * num_variables), nrow = num_samples)
colnames(pca_data) <- c("Allyl", "OH-But", "3OHP", "3MSO", "4OHB", "4MSO", "Butenyl")

# Center the data (subtract the mean of each variable)
centered_data <- scale(pca_data, center = TRUE, scale = FALSE)

# Calculate the covariance matrix
cov_matrix <- cov(centered_data)

# Perform singular value decomposition (SVD) of the covariance matrix
svd_result <- svd(cov_matrix)

# Extract the principal components
pcs <- svd_result$u

# Calculate the proportion of variance explained by each PC
variance_explained <- (svd_result$d^2) / sum(svd_result$d^2)

# Create a scree plot
plot(1:num_variables, variance_explained, type = "b", 
     xlab = "Principal Component", ylab = "Variance Explained",
     main = "Scree Plot")

# Biplot to visualize principal components
# Create a biplot manually
biplot_scale <- 1  # Adjust this scale for biplot arrow length

# Plot the data points
plot(pcs[, 1], pcs[, 2], type = "n", xlab = "PC1", ylab = "PC2")
text(pcs[, 1], pcs[, 2], labels = rownames(pcs), col = "blue")

# Add arrows for variable loadings
loading_arrows <- svd_result$v[, 1:2] * sqrt(svd_result$d[1:2])
arrows(0, 0, loading_arrows[, 1] * biplot_scale, loading_arrows[, 2] * biplot_scale, col = "red", length = 0.1)

#---------------
d<-pca_data

pca <- prcomp(d[,-1], center=TRUE, scale.=TRUE, retx=TRUE)
biplot(pca, scale=0, xlabs=rep("o",2150)) #distance entre objets (A)
biplot(pca, scale=1, xlabs=rep("o",2150)) #distance entre descripteurs 
(B

installe.packages("plot3D")
library(plot3D)





