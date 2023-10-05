
# source in data

source("manage_data.R")

plsda_data$AOP_status |> table()

library(pls)
# a package i dowloaded from : https://drive.google.com/drive/folders/1Gq9mqbAZA8czgCLlKUuapq521GuxebKp
library(plsda)


inVar <- "AOP_status"

# not in
`%notin%` <- Negate(`%in%`)


# Script options
pls::pls.options(plsralg = "oscorespls")
pls::pls.options("plsralg")


# Default par options
opar <- par(no.readonly = T)

# What is the target variable?
inVar <- "AOP_status"

#renaming all the column names of data frame

range <- 350:2500

colnames(full_df)[6:2156] <- c(range)

### Create plsrda dataset

Start.wave <- 500 #(entre 500 et 2400nm )
End.wave <- 2400
wv <- seq(Start.wave,End.wave,1)
Spectra <- as.matrix(full_df[,names(full_df) %in% wv])

#skip col names?
colnames(Spectra) <- c(paste0("Wave_",wv))

head(Spectra)[1:6,1:10]
sample_info <- full_df[,names(full_df) %notin% seq(350,2500,1)] 
head(sample_info)


sample_info2 <- sample_info %>%
  select(X1001g_ID,AOP_status,Classification_name) ########## vraiment ces données la ou changer pour autre chose??
head(sample_info2)

plsda_data <- data.frame(sample_info2,Spectra) #join le sample et spectra

plsda_small <- plsda_data %>%
  select(X500,X501,X502,AOP_status) 

#marchait pas vrm


#train <- plsda.split_sample(plsda_small)$train
#test <- plsda.split_sample(plsda_small)$test
#head(train)
#head(test)

#model <- plsda.fit(AOP_status~., plsda_small, ncomp = 2)

#model$pls.coef

#predict_post <- plsda.predict(model,test, type = "posterior")
#predict_class <- plsda.predict(model,test, type = "class")

#--------------------------------------------------------------------------------------------------#
  
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
  

# Assuming 'plsda_small' is your data frame
# Convert numeric columns to numeric data type
numeric_columns <- c("X500", "X501", "X502")
plsda_small[numeric_columns] <- lapply(plsda_small[numeric_columns], function(x) as.numeric(gsub(",", ".", x)))
# Encode the categorical variable as somthing else, i have to chose between the 3 option i think
#1
plsda_small$AOP_status <- as.factor(plsda_small$AOP_status)
#2
plsda_small$NumericResponse <- as.numeric(factor(plsda_small$AOP_status, levels = c("Alk", "Acid", "Neutral")))
#3

encoded_df <- plsda_small

encoded_df <- cbind(encoded_df, model.matrix(~ 0 + AOP_status, data = plsda_small))

encoded_df <- encoded_df[, -which(names(encoded_df) == "AOP_status")]

colnames(encoded_df) <- gsub("AOP_status", "", colnames(encoded_df))

head(encoded_df)


# Split the data into training and testing sets (e.g., 70% training, 30% testing)
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(plsda_small), 0.7 * nrow(plsda_small))
X_train <- plsda_small[train_index, c("X500", "X501", "X502")]
Y_train <- plsda_small[train_index, "AOP_status"]
X_test <- plsda_small[-train_index, c("X500", "X501", "X502")]
Y_test <- plsda_small[-train_index, "AOP_status"]

# Perform PLS-DA
n_comp <- 2  # Number of PLS components to use, adjust as needed
plsda_model <- plsr(Y_train ~ ., data = data.frame(Y_train, X_train), ncomp = n_comp)

# Predict the class labels for the test set
Y_pred <- predict(plsda_model, newdata = data.frame(X_test), type = "class")

# Evaluate the model (e.g., calculate accuracy)
accuracy <- sum(Y_pred == Y_test) / length(Y_test)
cat("Accuracy:", accuracy, "\n")

# You can also visualize the PLS-DA model if you have 2 components
if (n_comp == 2) {
  plot(plsda_model, comps = 1:2)
}


# with encoded DF, choice #3

set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(encoded_df), 0.7 * nrow(encoded_df))
X_train <- encoded_df[train_index, -1]  # Exclude the response variable
Y_train <- encoded_df[train_index, 1]   # The first column is the response variable
X_test <- encoded_df[-train_index, -1]  # Exclude the response variable
Y_test <- encoded_df[-train_index, 1]   # The first column is the response variable

# Perform PLS-DA
n_comp <- 3  # Number of PLS components to use, adjust as needed
plsda_model <- plsr(Y_train ~ ., data = data.frame(Y_train, X_train), ncomp = n_comp)

# Predict the class labels for the test set
Y_pred <- predict(plsda_model, newdata = data.frame(X_test), type = "class")

# Evaluate the model (e.g., calculate accuracy)
accuracy <- sum(Y_pred == Y_test) / length(Y_test)
cat("Accuracy:", accuracy, "\n")

# You can also visualize the PLS-DA model if you have 2 components
if (n_comp == 2) {
  plot(plsda_model, comps = 1:2)
}