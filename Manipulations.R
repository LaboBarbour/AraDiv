install.packages(c('quantmod','ff','foreign','R.matlab'),dependency=T)

suppressPackageStartupMessages(library(tidyverse)) #this dont work /// might work now

setwd("C:/Users/Sandrine/Documents/Automne 2023 Mattew barbour/AraDiv_Dataset") #pour moi

install.packages("gapminder") #pas nécessaire pour la
install.packages("ggplot2") #pas nécessaire pour la
install.packages("binom") #pas nécessaire pour la
install.packages("rgl") #pas nécessaire pour la
install.packages("tidyverse") 


README_meteorological_datarecord <- read.delim("~/Automne 2023 Mattew barbour/AraDiv_Dataset/README_meteorological_datarecord.txt")
View(README_meteorological_datarecord)

README_nirs_datarecord <- read.delim("~/Automne 2023 Mattew barbour/AraDiv_Dataset/README_nirs_datarecord.txt")
View(README_nirs_datarecord)

README_phenotypic_datarecord <- read.delim("~/Automne 2023 Mattew barbour/AraDiv_Dataset/README_phenotypic_datarecord.txt", comment.char="#")
View(README_phenotypic_datarecord)

meteorological_datarecord <- read.delim2("~/Automne 2023 Mattew barbour/AraDiv_Dataset/meteorological_datarecord.txt", comment.char="#")
View(meteorological_datarecord)

nirs_datarecord <- read.delim2("~/Automne 2023 Mattew barbour/AraDiv_Dataset/nirs_datarecord.txt")#, header=FALSE)
View(nirs_datarecord)

phenotypic_datarecord <- read.delim("~/Automne 2023 Mattew barbour/AraDiv_Dataset/phenotypic_datarecord.txt")#, header=FALSE)
View(phenotypic_datarecord)

str(phenotypic_datarecord)

library(tidyverse)


test <- read_delim("~/Automne 2023 Mattew barbour/AraDiv_Dataset/phenotypic_datarecord.txt")#, header=FALSE)


test2 <- read_delim("~/Automne 2023 Mattew barbour/AraDiv_Dataset/nirs_datarecord.txt")#, header=FALS

Ljoined <- left_join(nirs_datarecord,phenotypic_datarecord)
Fjoined <- full_join(nirs_datarecord,phenotypic_datarecord)
Rjoined <- right_join(nirs_datarecord,phenotypic_datarecord)

Summary(Ljoined) # Fjoin et R join aussi
View(Ljoined) # Fjoin et R join aussi

Ljoined[10000:10012,2000:2004] #  pour voir les lignes 1-12 colonne 1-4
str(Ljoined)

capture.output(Ljoined,file="Ljoined.txt") #pour enregistrer dans mon ordi // donc les lignes de joined ne servent plus a rien

Fjoined$traitName <- NULL # pour supprimer une colonne du data frame










