
## Setup ----

# load libraries
library(tidyverse)
library(mdatools)

# load data
full_df_clean <- read_csv2("full_df_clean.csv")

# spectral data 
spectral_df <- select(full_df_clean, x350:x2500)

# chemotype classifications
chemotype_vec <- factor(full_df_clean$classification_name)


## PLDA analysis
model <- plsda(spectral_df, chemotype_vec, ncomp = 2)
summary(model)
plot(model)

#classification plot
plotPredictions(model)

#if many classes of model
plotPredictions(model, nc = 1)

plotPredictions(model, nc = 3)


#performance plot 

par(mfrow = c(3,1))

plotMisclassified(model)

plotSensitivity(model)

plotSpecificity(model)


par(mfrow = c(1, 2))
plotRegcoeffs(model, ncomp = 2, ny = 3)
plotRegcoeffs(model, ncomp = 2, show.ci = TRUE)

#predicting new data

res = predict(model)
summary(res)




# this works, i created validation and calibration model 30/70
#this is the same analysis than the example : https://www.mdatools.com/docs/plsda--calibration.html
# i changed the way i created the data split to make it work with my data

# Load the caret package
library(caret)
library(mdatools)

full_df <- read_csv2("full_df.csv")

full_df_clean <- full_df |> 
  clean_names()

# Set the seed for reproducibility
set.seed(12345)


# Specify the proportion for the validation dataset
validation_proportion <- 0.3  # You can adjust this as needed

#clean up data for val/cal dataset


full_df_clean_reduced <- full_df_clean %>%
  select(starts_with("x"), -x1001g_id, classification_name)

# Create a random sample of data points for the validation dataset
validation_indexes = createDataPartition(full_df_clean_reduced$classification_name, 
                                         p = validation_proportion, 
                                         list = FALSE)


# Create the validation dataset

Xc <- (full_df_clean_reduced[-validation_indexes,1:2151 ]) 


# The remaining data will be used for calibration
Xv <- (full_df_clean_reduced[validation_indexes,1:2151 ]) 

#those are factors and the classes you wish to predict

cc.all = (full_df_clean_reduced[-validation_indexes, 2152])
cv.all = (full_df_clean_reduced[validation_indexes, 2152])

#transform in factors
cc.all_factor <- as.factor(cc.all$classification_name)
cv.all_factor <- as.factor(cv.all$classification_name)


# calibrating the data
m.all.c <- plsda(Xc, cc.all_factor, ncomp = 2, cv = 1)

m.all.v <- plsda(Xv, cv.all_factor, ncomp = 2, cv = 1)

summary(m.all.c)

#to look for a single component/class
summary(m.all.c, nc = 3)
summary(m.all.v, nc = 3)

#show statistics only for calibration or only for cross-validation parts, 
#in this case you will see details about contribution of every component 
#to the model.

summary(m.all.c$calres)
summary(m.all.v$calres)

getConfusionMatrix(m.all.c$calres)


#classification plot

par(mfrow = c(1, 2))
plotPredictions(m.all.c)
plotPredictions(m.all.v)

#multiple classes model you can select which class to show the predictions for.


par(mfrow = c(1, 2))
plotPredictions(m.all.c, nc = 1)
plotPredictions(m.all.c, nc = 3)

# performance plots ((((i run into an issue, why dont i have 2 lines???))))

par(mfrow = c(3, 2))
plotMisclassified(m.all.c, nc = 2)
plotMisclassified(m.all.v, nc = 2)

plotSensitivity(m.all.c, nc = 2)
plotSensitivity(m.all.v, nc = 2)

plotSpecificity(m.all.c, nc = 2)
plotSpecificity(m.all.v, nc = 2)

plotRegcoeffs(m.all, ncomp = 3, ny = 3)




