## Setup ----

# load libraries
library(mdatools)
library(tidyverse)
library(caret)

# load data
full_df_clean <- read_csv2("full_df_clean.csv")

# Set the seed for reproducibility
set.seed(12345)

# Specify the proportion for the validation dataset
validation_proportion <- 0.3  # You can adjust this as needed

#clean up data for val/cal dataset


full_df_clean_aop <- full_df_clean %>%
  select(starts_with("x"), -x1001g_id, aop_status)

#------------------------------------------------------------------------------
#making a pca analysis to know how many comp are needed
library(ggfortify)

temp_pca_aop <- pca(select(full_df_clean_aop, -aop_status))
summary(temp_pca_aop)

pca_res_aop <- prcomp(select(full_df_clean_aop, -aop_status))
summary(pca_res_aop)
autoplot(pca_res_aop, data = full_df_clean_aop, color = "aop_status") +
  scale_color_viridis_d()

pca_res_scaled_aop <- prcomp(select(full_df_clean_aop, -aop_status), scale = T)
autoplot(pca_res_scaled_aop, data = full_df_clean_aop, color = "aop_status") +
  scale_color_viridis_d()
#------------------------------------------------------------------------------

#continuing the analysis with the ncomp in mind 

# Create a random sample of data points for the validation dataset
validation_indexes_aop = createDataPartition(full_df_clean_aop$aop_status, 
                                         p = validation_proportion, 
                                         list = FALSE)
# Create the calibration dataset

Xc_aop <- (full_df_clean_aop[-validation_indexes_aop,1:2151 ]) 


# The remaining data will be used for validation
Xv_aop <- (full_df_clean_aop[validation_indexes_aop,1:2151 ]) 

#those are factors and the classes you wish to predict

cc.all_aop = (full_df_clean_aop[-validation_indexes_aop, 2152])
cv.all_aop = (full_df_clean_aop[validation_indexes_aop, 2152])

#transform in factors
cc.all_aop_factor <- as.factor(cc.all_aop$aop_status)
cv.all_aop_factor <- as.factor(cv.all_aop$aop_status)

#to make sure its 30/70
table(full_df_clean_aop$aop_status)
table(cc.all_aop_factor)

# calibrating the data
m.all.c_aop <- plsda(Xc_aop, cc.all_aop_factor, ncomp = 2, cv = 1)

summary(m.all.c_aop)


#to look for a single component/class
summary(m.all.c_aop, nc = 3)

#show statistics only for calibration or only for cross-validation parts, 
#in this case you will see details about contribution of every component 
#to the model.

summary(m.all.c_aop$calres)


getConfusionMatrix(m.all.c_aop$calres)


#classification plot

par(mfrow = c(1, 2))
plotPredictions(m.all.c_aop,ncomp = 2)

#multiple classes model you can select which class to show the predictions for.

par(mfrow = c(1, 1))
plotPredictions(m.all.c_aop, ncomp = 2)


# performance plots 
par(mfrow = c(3, 1))

plotMisclassified(m.all.c_aop, nc = 2)


plotSensitivity(m.all.c_aop, nc = 2)


plotSpecificity(m.all.c_aop, nc = 2)


# add show.ci = TRUE at the end if you want to see the error bars

par(mfrow = c(3, 1))

plotRegcoeffs(m.all.c_aop, ncomp = 2, ny = 1)
plotRegcoeffs(m.all.c_aop, ncomp = 2, ny = 2)
plotRegcoeffs(m.all.c_aop, ncomp = 2, ny = 3)


# prediction for new data

res = predict(m.all.c_aop, Xv_aop, cv.all_aop_factor)
summary(res)












