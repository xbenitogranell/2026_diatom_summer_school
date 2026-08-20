## Diatom monitoring workshop @ 2nd Diatom Summer School, Szczecin, Poland
## August 26th, 2026
## Xavier Benito
## contact: xavier.benito@irta.cat
## https://xbenitogranell.github.io/ 

#----------------------------------------------------------------------------------------------------------------------------------
# This document contains scripts to prepare the two diatom datasets for the diatom monitoring exercise, including:
# 1) loading the datasets
# 2) harmonizing diatom names by the most current accepted synonym and aggregate counts if two or more column names have been updated
#----------------------------------------------------------------------------------------------------------------------------------

# Create a folder called "results" where to store the different outputs created
if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
  message("Directory 'results' has been created")
} else {
  message("Directory 'results' is already present")
}

# The library pacman loads the necessary packages if they are installed in your R environment. Else pacman() will install and load them
#install.packages("pacman") #install the library
pacman::p_load(openxlsx, tidyverse, diathor, egg, vegan, corrplot, Hmisc)

## Elegant way to read multiple excel sheets per Excel file
# load names of excel files 
files <- list.files(path = "data/", full.names = TRUE, pattern = ".xlsx")
print(files)

# create function (see t that transpose the dataframe for later gather)
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  sapply(sheets, function(f) as.data.frame(readxl::read_excel(filename, sheet = f, col_names = TRUE)), 
         simplify = FALSE)
}

# execute function for all excel files in "files"
all_data <- lapply(files, read_excel_allsheets)

## Read in diatom datasets and format the data
#Segura river
seg <- data.frame(all_data[[1]]$`Segura river`) %>%
  group_by(Basin, Year, Site, Taxon) %>%
  summarise(count = sum(Abundance)) %>%
  ungroup() %>%
  group_by(Basin, Year, Site) %>%
  mutate(relative_abundance_percent = count / sum(count) * 100) %>%
  mutate(total_sample=sum(count)) %>%
  select(Basin, Year, Site, Taxon, relative_abundance_percent) %>%
  spread(key = Taxon, value = relative_abundance_percent) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  mutate(sample_id=paste(Site, Year, sep = "_")) %>%
  ungroup() %>%
  select(-c("Basin","Site","Year"))

# Ebro Delta
ebro <- data.frame(all_data[[1]]$`Ebro Delta`) %>%
  mutate(sample_id = paste(habitat, area, period, sep = "_"))

colnames(ebro) <- gsub(".", " ", colnames(ebro), fixed = TRUE)
ebro <- ebro %>%
  select(-c("habitat","area","period"))

# Now the two datasets are in wide-format (site-by-species abundance matrix)

## Diatom nomenclature harmonization
# Create a look-up table for the Segura and Ebro Delta diatom names list
taxa_names_seg <- colnames(seg)
taxa_names_ebro <- colnames(ebro)

taxa_names_all <- c(taxa_names_seg, taxa_names_ebro)
taxa_names_all <- taxa_names_all[taxa_names_all != "sample_id"] %>%
  as.data.frame() %>%
  distinct()
names(taxa_names_all) <- "user_taxa"

# Load the harmonization function
source("scripts/check_diatom_names.R")

# Run the function
conversion_df <- check_diatom_names(diatnames="taxa_names_all", 
                           omnidia="Omnidia_2015_full_synonyms_table")

# Have a look a the resulting list
print(conversion_df)

# If you want, you can export the diatom look up table as Excel
openxlsx::write.xlsx(conversion_df, "results/conversion_table.xlsx")

# Update the diatom counts with the harmonized table of names
ebro_harm <- ebro %>% 
  gather(taxa, abundance, -sample_id) %>% 
  mutate(taxa_updated = plyr::mapvalues(taxa, from = conversion_df[, "user_taxa"], to = conversion_df[, "taxon_name_updated"])) %>% 
  aggregate(abundance ~ sample_id + taxa_updated, data = ., FUN = sum) %>%  #Collapse any species values that are now synonymized
  pivot_wider(names_from = taxa_updated, values_from = abundance) # transform to wide format

seg_harm <- seg %>% 
  gather(taxa, abundance, -sample_id) %>% 
  mutate(taxa_updated = plyr::mapvalues(taxa, from = conversion_df[, "user_taxa"], to = conversion_df[, "taxon_name_updated"])) %>% 
  aggregate(abundance ~ sample_id + taxa_updated, data = ., FUN = sum) %>%  #Collapse any species values that are now synonymized
  pivot_wider(names_from = taxa_updated, values_from = abundance) # transform to wide format

