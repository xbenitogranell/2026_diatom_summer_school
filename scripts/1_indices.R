## Diatom monitoring workshop @ 2nd Diatom Summer School, Szczecin, Poland
## August 26th, 2026
## Xavier Benito
## contact: xavier.benito@irta.cat
## https://xbenitogranell.github.io/ 

#----------------------------------------------------------------------------------------------------------------------------------
# This document contains scripts to calculate the diatom-based IPS index and obtain other ecological characteristics of the taxa
#----------------------------------------------------------------------------------------------------------------------------------

# The library pacman loads the necessary packages if they are installed in your R environment. Else pacman() will install and load them
#install.packages("pacman") #install the library
pacman::p_load(openxlsx, tidyverse, diathor, egg, vegan, corrplot, Hmisc)

# Create a folder called "results" where to store the different outputs created
if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
  message("Directory 'results' has been created")
} else {
  message("Directory 'results' is already present")
}

# Assign the result folder path
results_path <- file.path("results")

# Prepare the two working study site datasets in the proper format for diathor to run
ebro_harm_t <- t(ebro_harm) %>%
  as.data.frame() %>%
  janitor::row_to_names(row_number = 1) %>%
  rownames_to_column(var="species") %>%
  mutate(across(-species, as.numeric)) 

seg_harm_t <- t(seg_harm) %>%
  as.data.frame() %>%
  janitor::row_to_names(row_number = 1) %>%
  rownames_to_column(var="species") %>%
  mutate(across(-species, as.numeric))

# check and convert the input data in the proper format that the package diathor() needs to calculate indices
# Each time diat_loadData() is executed, a new set of results in the output folder "results" is created
df_ebro <- diat_loadData(ebro_harm_t, maxDistTaxa = 2, resultsPath = results_path)
df_seg <- diat_loadData(seg_harm_t, maxDistTaxa = 2, resultsPath = results_path)

# use the diat_ips() function to calculate the IPS index 
# Each time diat_ips() is executed, a new set of results in the output folder "resuilts" is created
ips_ebro <- diat_ips(df_ebro) 
ips_seg <- diat_ips(df_seg) 

# see the output
print(ips_ebro)
# IPS: original IPS values
# IPS20: normalized to the standard 0-20 range
# num_taxa: n diatom taxa used to compute the index

## In the results folder, there are three exported files:
#num_taxa.csv: number of how many taxa were used to calculate the index for each sample. 
#Taxa_included.csv: taxa recognized for the calculation of the index
#Taxa_excluded.csv: taxa not recognized for the calculation of the index

## Additional ecological information
# Van Dam classification
vandam_ebro <- diat_vandam(df_ebro, vandamReports = TRUE)
vandam_seg <- diat_vandam(df_seg, vandamReports = TRUE)

