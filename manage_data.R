
## setup ----

## load libraries
library(tidyverse)
library(readxl) # required for read_excel

## load data.

# master accession table
master_list <- read_csv("master_accession_table.csv") %>%
  select(X1001g_ID = pk, name, country, latitude, longitude, cs_number) %>%
  drop_na()

# hyperspectral data
nirs_datarecord <- read_delim("dataverse_files/nirs_datarecord.txt") 

# plant ID info and additional phenotype data
phenotypic_datarecord <- read_delim("dataverse_files/phenotypic_datarecord.txt") 

# subset phenotypic data to only use P- plants for verbatimOccurenceID
phenotypic_sub <- phenotypic_datarecord %>%
  separate(col = verbatimOccurrenceID, into = c("AP","Number"), sep = 1, remove = FALSE) %>% # this will help us filter by P-only plants
  filter(AP == "P") %>% # only keep "P" plants
  select(-AP, -Number) %>% # remove the new columns we made. We just made them temporarily to filter the data
  select(X1001g_ID, verbatimOccurrenceID, verbatimBlockID, DateSowing, DateHarvest) %>% # only keep useful columns. We don't need additional trait information right now
  distinct() # keep only unique rows

# chemotype data
katz <- read_excel("Chemotype_data_Katz_2021_eLife/Katz_2021_elife-67784-supp1-v2.xlsx", sheet = 4) %>%
  mutate(Total_GSL = `3OHP`+`3MSO`+`4OHB`+`2-OH-3-Butenyl`+`4MSO`+
           Allyl+`5MSO`+`3-Butenyl`+Branched+OHPentenyl+`4OHI3M`+`6MSO`+
           `3MT`+`7MSO`+`4Pentenyl`+`4MT`+`8MSO`+I3M+`7MT`+`4MOI3M`+
           BZO+`8MT`+`8MTder`) %>%
  select(cs_number = CS, Total_GSL, AOP_status = `AOP status`, MAM_status = `MAM status`, GSOH_functionality = `GS-OH functionality`, Classification_name)

# join datasets
master_katz_df <- left_join(master_list, katz)

phenotypic_nirs_df <- left_join(phenotypic_sub, nirs_datarecord) %>% 
  drop_na()

full_df <- left_join(phenotypic_nirs_df, master_katz_df) %>%
  drop_na()

# checking data set
check_katz_df <- full_df %>%
  select(X1001g_ID, verbatimOccurrenceID, cs_number, Classification_name) 
check_katz_df
length(unique(check_katz_df$cs_number))
length(unique(check_katz_df$X1001g_ID))
