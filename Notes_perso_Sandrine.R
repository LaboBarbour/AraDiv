

install.packages(c('quantmod','ff','foreign','R.matlab'),dependency=T)

suppressPackageStartupMessages(library(tidyverse)) #this dont work /// might work now



install.packages("tidyverse") 
library(tidyverse) #pour loader mes affaires


README_meteorological_datarecord <- read.delim("dataverse_files/README_meteorological_datarecord.txt"
View(README_meteorological_datarecord)

README_nirs_datarecord <- read.delim("dataverse_files/README_nirs_datarecord.txt")
View(README_nirs_datarecord)

README_phenotypic_datarecord <- read.delim("dataverse_files/README_phenotypic_datarecord.txt")
View(README_phenotypic_datarecord)

meteorological_datarecord <- read_delim("dataverse_files/meteorological_datarecord.txt")
View(meteorological_datarecord)


nirs_datarecord <- read_delim("dataverse_files/nirs_datarecord.txt")
View(nirs_datarecord)

phenotypic_datarecord <- read_delim("dataverse_files/phenotypic_datarecord.txt")
View(phenotype_data_record) #voir le data

str(phenotypic_datarecord) #Compactly Display the Structure of an Arbitrary R Object


Ljoined <- left_join(nirs_datarecord,phenotypic_datarecord)
Fjoined <- full_join(nirs_datarecord,phenotypic_datarecord)
Rjoined <- right_join(nirs_datarecord,phenotypic_datarecord)

Summary(Ljoined) # Fjoin et R join aussi, fait un résumé
# ou juste écrire par exemple ca et ca va donner un résumé
phenotypic_datarecord

View(Ljoined) # Fjoin et R join aussi 

# moyen utile pck apres code a mattew fonctionne
Ljoined[10000:10012,2000:2004] #  pour voir les lignes 1-12 colonne 1-4
str(Ljoined)

capture.output(Ljoined,file="Ljoined.txt") #pour enregistrer dans mon ordi // donc les lignes de joined ne servent plus a rien

Fjoined$traitName <- NULL # pour supprimer une colonne du data frame

colnames(phenotypic_datarecord) # pour voir les noms des colonnes 
# sub = subset = éliminer des affaire, un sous tableau
#-----------------------------------------------------------------
  ## setup ----

# load libraries
library(tidyverse)
library(readxl)

# load data.
# paths are specific to my computer so ou will have to change them. 
# also, I'm using read_delim function that I showed you yesterday

nirs_datarecord <- read_delim("dataverse_files/nirs_datarecord.txt")
phenotypic_datarecord <- read_delim("dataverse_files/phenotypic_datarecord.txt") 

# subset phenotypic data to only use P- plants for verbatimOccurenceID
phenotypic_sub <- phenotypic_datarecord %>%
  separate(col = verbatimOccurrenceID, into = c("AP","Number"), sep = 1, remove = FALSE) %>% # this will help us filter by P-only plants
  filter(AP == "P") %>% # only keep "P" plants
  select(-AP, -Number) %>% # remove the new columns we made. We just made them temporarily to filter the data
  select(X1001g_ID, verbatimOccurrenceID, verbatimBlockID, DateSowing, DateHarvest) %>% # only keep useful columns. We don't need additional trait information right now
  distinct() # keep only unique rows

# join data
phenotypic_nirs_df <- left_join(phenotypic_sub, nirs_datarecord) %>% 
  drop_na()


#-----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
  
 ##############PLSR 
  
  
  source("manage_data.R")

list.of.packages <- c("pls","dplyr","reshape2","here","plotrix","ggplot2","gridExtra",
                      "spectratrait")
invisible(lapply(list.of.packages, library, character.only = TRUE))

req.packages <- c("devtools","remotes","readr","RCurl","httr","pls","dplyr","reshape2","here",
                  "plotrix","scales","ggplot2","gridExtra")
new.packages <- req.packages[!(req.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, dependencies=TRUE)
library(pls)
library(dplyr)
library(reshape2)
library(here)
library(plotrix)
library(ggplot2)
library(gridExtra)
library(spectratrait)
library(devtools)
library(remotes)
library(readr)
library(RCurl)
library(scales)


# not in
`%notin%` <- Negate(`%in%`)


# Script options
pls::pls.options(plsralg = "oscorespls")
pls::pls.options("plsralg")


# Default par options
opar <- par(no.readonly = T)

# What is the target variable?
inVar <- "classification_name"

# faire ca pour trouver les dernières colonnes
full_df[1:5,2160:2166] # ligne 1-12 colonne 1-4

#---------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------


#renaming all the column names of data frame

range <- 350:2500

colnames(full_df2)[6:2156] <- c(range)

### Create plsr dataset
Start.wave <- 500 #(entre 500 et 2400nm )
End.wave <- 2400
wv <- seq(Start.wave,End.wave,1)
Spectra <- as.matrix(full_df[,names(full_df) %in% wv])

colnames(Spectra) <- c(paste0("Wave_",wv))
head(Spectra)[1:6,1:10]
sample_info <- full_df[,names(full_df) %notin% seq(350,2500,1)] # on a oter les wavelengt 350
head(sample_info)


sample_info2 <- sample_info %>%
  select(X1001g_ID,AOP_status,Classification_name)
head(sample_info2)

plsr_data <- data.frame(sample_info2,Spectra) #join le sample et spectra

rm(sample_info,sample_info2,Spectra) #(((pas encore sure de ca)))

#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------

#### Example data cleaning.  End user needs to do what's appropriate for their 
#### data.  This may be an iterative process.
# Keep only complete rows of inVar and spec data before fitting

plsr_data <- plsr_data[complete.cases(plsr_data[,names(plsr_data) %in% 
                                                  c(inVar,paste0("Wave_",wv))]),] 


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Create cal/val datasets
## Make a stratified random sampling in the strata USDA_Species_Code and Domain

method <- "dplyr" #base/dplyr
# base R - a bit slow
# dplyr - much faster

split_data <- spectratrait::create_data_split(dataset=plsr_data, approach=method, 
                                              split_seed=7529075,prop=0.8, 
                                              group_variables="AOP_status")
names(split_data)
cal.plsr.data <- split_data$cal_data
head(cal.plsr.data)[1:8]
val.plsr.data <- split_data$val_data
head(val.plsr.data)[1:8]
rm(split_data) 

# Datasets:
print(paste("Cal observations: ",dim(cal.plsr.data)[1],sep=""))
print(paste("Val observations: ",dim(val.plsr.data)[1],sep=""))


text_loc <- c(max(hist(cal.plsr.data[,paste0(inVar)])$counts),
              max(hist(cal.plsr.data[,paste0(inVar)])$mids))


cal_hist_plot <- qplot(cal.plsr.data[,paste0(inVar)],geom="histogram",
                       main = paste0("Calibration Histogram for ",inVar),
                       xlab = paste0(inVar),ylab = "Count",fill=I("grey50"),col=I("black"),
                       alpha=I(.7)) +
  annotate("text", x=text_loc[2], y=text_loc[1], label= "1.",size=10)
val_hist_plot <- qplot(val.plsr.data[,paste0(inVar)],geom="histogram",
                       main = paste0("Validation Histogram for ",inVar),
                       xlab = paste0(inVar),ylab = "Count",fill=I("grey50"),col=I("black"),
                       alpha=I(.7))
histograms <- grid.arrange(cal_hist_plot, val_hist_plot, ncol=2)
ggsave(filename = file.path(outdir,paste0(inVar,"_Cal_Val_Histograms.png")), plot = histograms, 
       device="png", width = 30, height = 12, units = "cm", dpi = 300)
# output cal/val data
write.csv(cal.plsr.data,file=file.path(outdir,paste0(inVar,'_Cal_PLSR_Dataset.csv')),row.names=FALSE)
write.csv(val.plsr.data,file=file.path(outdir,paste0(inVar,'_Val_PLSR_Dataset.csv')),row.names=FALSE)
  
  
#--------------------------------------------------------------------------------------------------#



#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
########## Changer pour PLSDA


library(spectrolab)
library(caret)
library(plyr)


# not in
`%notin%` <- Negate(`%in%`)


# Script options
pls::pls.options(plsralg = "oscorespls")
pls::pls.options("plsralg")


# Default par options
opar <- par(no.readonly = T)

# What is the target variable?
inVar <- "AOP_status"

#---------------------------------------------------------------------------------

#renaming all the column names of data frame

range <- 350:2500

colnames(full_df)[6:2156] <- c(range)


### Create plsr dataset

Start.wave <- 500 #(entre 500 et 2400nm )
End.wave <- 2400
wv <- seq(Start.wave,End.wave,1)
Spectra <- as.matrix(full_df[,names(full_df) %in% wv])

colnames(Spectra) <- c(paste0("Wave_",wv))
head(Spectra)[1:6,1:10]
sample_info <- full_df[,names(full_df) %notin% seq(350,2500,1)] 
head(sample_info)


sample_info2 <- sample_info %>%
  select(X1001g_ID,AOP_status,Classification_name) 
head(sample_info2)


plsda_data <- data.frame(sample_info2,Spectra) #join le sample et spectra

plsda_small <- plsda_data %>%
  select(Wave_500,Wave_501, Wave_502,AOP_status)
  
  

### Create training/testing datasets

## split data into 60% training and 40% testing datasets, stratified by species
set.seed(123456789)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(plsda_data), replace=TRUE, prob=c(0.7,0.3))
train  <- plsda_data[sample, ]
test   <- plsda_data[!sample, ]

## here are two ways to deal with imbalance in #samples per group
## ctrl1 downsamples so that no group has more members than the smallest
## ctrl2 upsamples by duplicating members of smaller groups at random
## another option is to use sample="smote" but that works less well here

ctrl1 <- trainControl(method = "repeatedcv", repeats = 10, number=10,
                      summaryFunction = multiClassSummary, sampling="down")

ctrl2 <- trainControl(method = "repeatedcv", repeats = 10, number=10,
                      summaryFunction = multiClassSummary, sampling="up")

#--------------------------------------------------------------------------------------------------#



#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#




# Install and load the 'pls' package if not already installed
if (!requireNamespace("pls", quietly = TRUE)) {
  install.packages("pls")
}

library(pls)

# Load your data
# Replace 'your_data.csv' with the path to your data file
data <- read.csv("your_data.csv")

# Split the data into predictors (X) and the class labels (Y)
X <- plsda_small[, -ncol(plsda_small)]  # Assuming the last column contains the class labels
Y <- plsda_small[, ncol(plsda_small)]

# Split the data into training and testing sets (e.g., 70% training, 30% testing)
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(plsda_small), 0.7 * nrow(plsda_small))
X_train <- X[train_index, ]
Y_train <- Y[train_index]
X_test <- X[-train_index, ]
Y_test <- Y[-train_index]

# Perform PLS-DA
n_comp <- 2  # Number of PLS components to use, you can change this
plsda_model <- plsr(X_train, Y_train, ncomp = n_comp)
plsda_model <- plsr(Y_train ~ ., data = data.frame(Y_train, X_train), ncomp = n_comp)

# Predict the class labels for the test set
Y_pred <- predict(plsda_model, newdata = X_test, type = "class")

# Evaluate the model (e.g., calculate accuracy)
accuracy <- sum(Y_pred == Y_test) / length(Y_test)
cat("Accuracy:", accuracy, "\n")

# You can also visualize the PLS-DA model if you have 2 components
if (n_comp == 2) {
  plot(plsda_model, comps = 1:2)
}
#--------------------------------------------------------------------------------------------------#



#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#



# Load the 'pls' package if not already installed
if (!requireNamespace("pls", quietly = TRUE)) {
  install.packages("pls")
}

library(pls)

# Assuming 'plsda_small' is your data frame
# Convert numeric columns to numeric data type
numeric_columns <- c("Wave_500", "Wave_501", "Wave_502")
plsda_small[numeric_columns] <- lapply(plsda_small[numeric_columns], function(x) as.numeric(gsub(",", ".", x)))

# Encode the categorical variable as a factor
plsda_small$AOP_status <- as.factor(plsda_small$AOP_status)

# Split the data into training and testing sets (e.g., 70% training, 30% testing)
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(plsda_small), 0.7 * nrow(plsda_small))
X_train <- plsda_small[train_index, c("Wave_500", "Wave_501", "Wave_502")]
Y_train <- plsda_small[train_index, "AOP_status"]
X_test <- plsda_small[-train_index, c("Wave_500", "Wave_501", "Wave_502")]
Y_test <- plsda_small[-train_index, "AOP_status"]

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

#---------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#Re try lower










































# source in data

source("manage_data.R")

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


# for #1 of 2

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
Y_pred <- predict(plsda_model, newdata = data.frame(X_test), type = "response")


# Determine the predicted class labels based on the highest probability
predicted_labels <- apply(Y_pred, 1, function(row) colnames(Y_pred)[which.max(row)])

# Evaluate the model (e.g., calculate accuracy)
accuracy <- sum(predicted_labels == Y_test) / length(Y_test)
cat("Accuracy:", accuracy, "\n")


# You can also visualize the PLS-DA model if you have 3 components
if (n_comp == 3) {
  plot(plsda_model, comps = 1:3)
}


#--------------------------------------------------------------------------------------------------#
  
  
  
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
  

#Random Forest analysis

library(janitor)
library(pls)
library(readr)
library(tidyverse)
library(randomForest)
library(recipes)


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


#RANDOM FOREST ANALYSIS

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



randomForest::randomForest
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

#--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
#  --------------------------------------------------------------------------------------------------#
  
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
  #--------------------------------------------------------------------------------------------------#
  
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#
  Canonical Correlation Analysis (CCA) 


# Load the necessary library
library(caret)

# Create two example datasets with variables X1 to X5 and Y1 to Y5
set.seed(1234)
n <- 100
X <- matrix(rnorm(n * 5), ncol = 5)
Y <- matrix(rnorm(n * 5), ncol = 5)

# Perform Canonical Correlation Analysis (CCA)
cca_result <- cancor(X, Y)

# Canonical Correlations
canonical_correlations <- cca_result$cor

# Canonical Variables (Canonical Variates)
canonical_variates_X <- cca_result$xcoef
canonical_variates_Y <- cca_result$ycoef

# Print Canonical Correlations
cat("Canonical Correlations:\n")
print(canonical_correlations)

# Print Canonical Variables (Canonical Variates) for X and Y
cat("Canonical Variables for X:\n")
print(canonical_variates_X)

cat("Canonical Variables for Y:\n")
print(canonical_variates_Y)























