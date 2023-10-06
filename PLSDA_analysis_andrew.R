library(janitor)
library(pls)
library(readr)
library(tidyverse)


write_csv2(full_df_clean, file = "full_df_clean.csv")

# read in data
full_df <- read_csv2("full_df.csv")

full_df_clean <- full_df |> 
  clean_names()

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
  select(x350:x2500,
         classification_name)


call:
  randomForest(formula = classification_name ~ ., data = random_forest_data) 


#find number of trees that produce lowest test MSE
which.min(model$mse)



#find RMSE of best model
sqrt(model$mse[which.min(model$mse)]) 






