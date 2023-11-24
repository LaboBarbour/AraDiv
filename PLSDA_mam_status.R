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
full_df_clean_mam <- full_df_clean %>%
  select(starts_with("x"), -x1001g_id, mam_status)

#------------------------------------------------------------------------------
#making a pca analysis to know how many comp are needed
library(ggfortify)

temp_pca_mam <- pca(select(full_df_clean_mam, -mam_status))
summary(temp_pca_mam)

pca_res_mam <- prcomp(select(full_df_clean_mam, -mam_status))
summary(pca_res_mam)
autoplot(pca_res_mam, data = full_df_clean_mam, color = "mam_status") +
  scale_color_viridis_d()

pca_res_scaled_mam <- prcomp(select(full_df_clean_mam, -mam_status), scale = T)
autoplot(pca_res_scaled_mam, data = full_df_clean_mam, color = "mam_status") +
  scale_color_viridis_d()
#------------------------------------------------------------------------------

#continuing the analysis with the ncomp in mind 

# Create a random sample of data points for the validation dataset
library(caret)
validation_indexes_mam = createDataPartition(full_df_clean_mam$mam_status, 
                                             p = validation_proportion, 
                                             list = FALSE)
# Create the calibration dataset

Xc_mam <- (full_df_clean_mam[-validation_indexes_mam,1:2151 ]) 


# The remaining data will be used for validation
Xv_mam <- (full_df_clean_mam[validation_indexes_mam,1:2151 ]) 

#those are factors and the classes you wish to predict

cc.all_mam = (full_df_clean_mam[-validation_indexes_mam, 2152])
cv.all_mam = (full_df_clean_mam[validation_indexes_mam, 2152])

#transform in factors
cc.all_mam_factor <- as.factor(cc.all_mam$mam_status)
cv.all_mam_factor <- as.factor(cv.all_mam$mam_status)

#to make sure its 30/70
table(full_df_clean_mam$mam_status)
table(cc.all_mam_factor)

# calibrating the data
install.packages("mdatools")
library(mdatools)
m.all.c_mam <- plsda(Xc_mam, cc.all_mam_factor, ncomp = 2, cv = 1)

summary(m.all.c_mam)


#to look for a single component/class
summary(m.all.c_mam, nc = 2)

#show statistics only for calibration or only for cross-validation parts, 
#in this case you will see details about contribution of every component 
#to the model.

summary(m.all.c_mam$calres)


getConfusionMatrix(m.all.c_mam$calres)


#classification plot

par(mfrow = c(1, 2))
plotPredictions(m.all.c_mam,ncomp = 2)

#multiple classes model you can select which class to show the predictions for.

par(mfrow = c(1, 1))
plotPredictions(m.all.c_mam, ncomp = 2)


# performance plots 
par(mfrow = c(3, 1))

plotMisclassified(m.all.c_mam, nc = 2)


plotSensitivity(m.all.c_mam, nc = 2)


plotSpecificity(m.all.c_mam, nc = 2)


# add show.ci = TRUE at the end if you want to see the error bars

par(mfrow = c(2, 1))

plotRegcoeffs(m.all.c_mam, ncomp = 2, ny = 1)
plotRegcoeffs(m.all.c_mam, ncomp = 2, ny = 2)



# prediction for new data

res = predict(m.all.c_mam, Xv_mam, cv.all_mam_factor)
summary(res)























