

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
-----------------------------------------------------------------
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
----------------------------------------------------------------------------------------------------------------------------
  
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






  



