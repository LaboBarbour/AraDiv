
## Setup ----

# load libraries
library(tidyverse)
library(mdatools)

# load data
full_df <- read_csv2("full_df.csv")

# spectral data 
spectral_df <- select(full_df, X350:X2500)

# chemotype classifications
chemotype_vec <- factor(full_df$Classification_name)

## PLDA analysis
model <- plsda(spectral_df, chemotype_vec, ncomp = 2)
summary(model)
plot(model)
