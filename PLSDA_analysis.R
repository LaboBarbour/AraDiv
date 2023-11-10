
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


## PLSDA analysis
model <- plsda(spectral_df, chemotype_vec, ncomp = 2)
summary(model)
plot(model)



#i created validation and calibration model 30/70
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



#------------------------------------------------------------------------------
#making a pca analysis to know how many comp are needed
library(ggfortify)

temp_pca <- pca(select(full_df_clean_reduced, -classification_name))
summary(temp_pca)

pca_res <- prcomp(select(full_df_clean_reduced, -classification_name))
summary(pca_res)
autoplot(pca_res, data = full_df_clean_reduced, color = "classification_name") +
  scale_color_viridis_d()

pca_res_scaled <- prcomp(select(full_df_clean_reduced, -classification_name), scale = T)
autoplot(pca_res_scaled, data = full_df_clean_reduced, color = "classification_name") +
  scale_color_viridis_d()
#------------------------------------------------------------------------------

#continuing the analysis with the ncomp in mind 

# Create a random sample of data points for the validation dataset
validation_indexes = createDataPartition(full_df_clean_reduced$classification_name, 
                                         p = validation_proportion, 
                                         list = FALSE)


# Create the calibration dataset

Xc <- (full_df_clean_reduced[-validation_indexes,1:2151 ]) 


# The remaining data will be used for validation
Xv <- (full_df_clean_reduced[validation_indexes,1:2151 ]) 

#those are factors and the classes you wish to predict

cc.all = (full_df_clean_reduced[-validation_indexes, 2152])
cv.all = (full_df_clean_reduced[validation_indexes, 2152])

#transform in factors
cc.all_factor <- as.factor(cc.all$classification_name)
cv.all_factor <- as.factor(cv.all$classification_name)

table(full_df_clean_reduced$classification_name)
table(cc.all_factor)

# calibrating the data
m.all.c <- plsda(Xc, cc.all_factor, ncomp = 7, cv = 1)

m.all.v <- plsda(Xv, cv.all_factor, ncomp = 7, cv = 1)

summary(m.all.c)
summary(m.all.v)

#to look for a single component/class
summary(m.all.c, nc = 3)
summary(m.all.v, nc = 3)

#show statistics only for calibration or only for cross-validation parts, 
#in this case you will see details about contribution of every component 
#to the model.

summary(m.all.c$calres)
summary(m.all.v$calres)

getConfusionMatrix(m.all.c$calres)
getConfusionMatrix(m.all.v$calres)


#classification plot

par(mfrow = c(1, 2))
plotPredictions(m.all.c,ncomp = 3)
plotPredictions(m.all.v, ncomp = 3 )

#multiple classes model you can select which class to show the predictions for.

par(mfrow = c(1, 2))
plotPredictions(m.all.c, ncomp = 6)
plotPredictions(m.all.c, ncomp = 6)

# performance plots 
par(mfrow = c(2, 1))
plotMisclassified(m.all.c, nc = 7)
plotMisclassified(m.all.v, nc = 7)

plotSensitivity(m.all.c, nc = 7)
plotSensitivity(m.all.v, nc = 7)

plotSpecificity(m.all.c, nc = 7)
plotSpecificity(m.all.v, nc = 7)

# add show.ci = TRUE at the end if you want to see the error bars

par(mfrow = c(3, 1))

plotRegcoeffs(m.all.c, ncomp = 7, ny = 1)
plotRegcoeffs(m.all.c, ncomp = 7, ny = 2)
plotRegcoeffs(m.all.c, ncomp = 7, ny = 3)
plotRegcoeffs(m.all.c, ncomp = 7, ny = 4)
plotRegcoeffs(m.all.c, ncomp = 7, ny = 5)
plotRegcoeffs(m.all.c, ncomp = 7, ny = 6)
plotRegcoeffs(m.all.c, ncomp = 7, ny = 7)

plotRegcoeffs(m.all.v, ncomp = 7, ny = 1)
plotRegcoeffs(m.all.v, ncomp = 7, ny = 2)
plotRegcoeffs(m.all.v, ncomp = 7, ny = 3)
plotRegcoeffs(m.all.v, ncomp = 7, ny = 4)
plotRegcoeffs(m.all.v, ncomp = 7, ny = 5)
plotRegcoeffs(m.all.v, ncomp = 7, ny = 6)
plotRegcoeffs(m.all.v, ncomp = 7, ny = 7)



# prediction for new data

summary(m.all.v$calres)

par(mfrow = c(1, 1))
plotPredictions(m.all.v$calres, ncomp = 7)


res = predict(m.all.c, Xv, cv.all_factor)
summary(res)

