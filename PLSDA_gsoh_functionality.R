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
#force tidyverse 
library(tidyverse)
full_df_clean_gsoh <- full_df_clean %>%
  select(starts_with("x"), -x1001g_id, gsoh_functionality)

#------------------------------------------------------------------------------
#making a pca analysis to know how many comp are needed
library(ggfortify)

temp_pca_gsoh <- pca(select(full_df_clean_gsoh, -gsoh_functionality))
summary(temp_pca_gsoh)

pca_res_gsoh <- prcomp(select(full_df_clean_gsoh, -gsoh_functionality))
summary(pca_res_gsoh)
autoplot(pca_res_gsoh, data = full_df_clean_gsoh, color = "gsoh_functionality") +
  scale_color_viridis_d()

pca_res_scaled_gsoh <- prcomp(select(full_df_clean_gsoh, -gsoh_functionality), scale = T)
autoplot(pca_res_scaled_gsoh, data = full_df_clean_gsoh, color = "gsoh_functionality") +
  scale_color_viridis_d()
#------------------------------------------------------------------------------

#continuing the analysis with the ncomp in mind 

# Create a random sample of data points for the validation dataset
#force the package
library(caret)
validation_indexes_gsoh = createDataPartition(full_df_clean_gsoh$gsoh_functionality, 
                                             p = validation_proportion, 
                                             list = FALSE)
# Create the calibration dataset

Xc_gsoh <- (full_df_clean_gsoh[-validation_indexes_gsoh,1:2151 ]) 


# The remaining data will be used for validation
Xv_gsoh <- (full_df_clean_gsoh[validation_indexes_gsoh,1:2151 ]) 

#those are factors and the classes you wish to predict

cc.all_gsoh = (full_df_clean_gsoh[-validation_indexes_gsoh, 2152])
cv.all_gsoh = (full_df_clean_gsoh[validation_indexes_gsoh, 2152])

#transform in factors
cc.all_gsoh_factor <- as.factor(cc.all_gsoh$gsoh_functionality)
cv.all_gsoh_factor <- as.factor(cv.all_gsoh$gsoh_functionality)

#to make sure its 30/70
table(full_df_clean_gsoh$gsoh_functionality)
table(cc.all_gsoh_factor)

# calibrating the data
#forcing mdatools
install.packages("mdatools")
library(mdatools)
m.all.c_gsoh <- plsda(Xc_gsoh, cc.all_gsoh_factor, ncomp = 2, cv = 1)

summary(m.all.c_gsoh)


#to look for a single component/class
summary(m.all.c_gsoh, nc = 2)

#show statistics only for calibration or only for cross-validation parts, 
#in this case you will see details about contribution of every component 
#to the model.

summary(m.all.c_gsoh$calres)


getConfusionMatrix(m.all.c_gsoh$calres)


#classification plot

par(mfrow = c(1, 2))
plotPredictions(m.all.c_gsoh,ncomp = 2)
plotPredictions(m.all.c_gsoh,ncomp = 1)

#multiple classes model you can select which class to show the predictions for.

par(mfrow = c(1, 2))
plotPredictions(m.all.c_gsoh, ncomp = 2)
plotPredictions(m.all.c_gsoh, ncomp = 1)


# performance plots 
par(mfrow = c(3, 1))

plotMisclassified(m.all.c_gsoh, nc = 2)


plotSensitivity(m.all.c_gsoh, nc = 2)


plotSpecificity(m.all.c_gsoh, nc = 2)


# add show.ci = TRUE at the end if you want to see the error bars

par(mfrow = c(2, 1))

plotRegcoeffs(m.all.c_gsoh, ncomp = 2, ny = 1)
plotRegcoeffs(m.all.c_gsoh, ncomp = 2, ny = 2)



# prediction for new data

res = predict(m.all.c_gsoh, Xv_gsoh, cv.all_gsoh_factor)
summary(res)












