
# source in data

source("manage_data.R")

library(plsda)

plsda_small <- plsda_data %>%
  select(AOP_status,Wave_500,Wave_501,Wave_502) 

train <- plsda.split_sample(plsda_small)$train
test <- plsda.split_sample(plsda_small)$test
head(train)
head(test)

model <- plsda.fit(AOP_status~., plsda_small, ncomp = 2)

model$pls.coef

predict_post <- plsda.predict(model,test, type = "posterior")
predict_class <- plsda.predict(model,test, type = "class")


















