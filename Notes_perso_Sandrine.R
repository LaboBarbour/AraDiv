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
View(Ljoined) # Fjoin et R join aussi 

# moyen utile pck apres code a mattew fonctionne
Ljoined[10000:10012,2000:2004] #  pour voir les lignes 1-12 colonne 1-4
str(Ljoined)

capture.output(Ljoined,file="Ljoined.txt") #pour enregistrer dans mon ordi // donc les lignes de joined ne servent plus a rien

Fjoined$traitName <- NULL # pour supprimer une colonne du data frame

-----------------------------------------------------------------
  ## setup ----

# load libraries
library(tidyverse)

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


capture.output(phenotypic_nirs_df,file="phenotypic_nirs_df.txt")



