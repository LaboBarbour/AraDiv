
## setup ----

# source in data
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

#---------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------

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
  select(X1001g_ID,AOP_status,Classification_name) ########## vraiment ces données la ou changer pour autre chose??
head(sample_info2)

plsr_data <- data.frame(sample_info2,Spectra) #join le sample et spectra

# rm(sample_info,sample_info2,Spectra) (((pas encore sure de ca)))



c(inVar,paste0("Wave_",wv)) 


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

# rm(split_data)  (((je sais pas si je veux rm)))

print(paste("Cal observations: ",dim(cal.plsr.data)[1],sep=""))
print(paste("Val observations: ",dim(val.plsr.data)[1],sep=""))













