
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

