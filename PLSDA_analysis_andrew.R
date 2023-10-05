library(janitor)
library(pls)
library(readr)
library(tidyverse)




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

predict(plsda_model, newdata = alksum_mso)



# 
# train <- plsda.split_sample(plsda_small)$train
# test <- plsda.split_sample(plsda_small)$test
#head(train)
#head(test)

#model <- plsda.fit(AOP_status~., plsda_small, ncomp = 2)

#model$pls.coef

#predict_post <- plsda.predict(model,test, type = "posterior")
#predict_class <- plsda.predict(model,test, type = "class")

