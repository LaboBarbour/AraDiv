
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























