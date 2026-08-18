#Load packages
library(tidyverse)
library(ggplot2)
library(diathor)

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

## Nomenclature harmonization
# Create a look-up table for the Segura and Ebro Delta diatom names list
taxa_names_seg <- colnames(seg)
taxa_names_ebro <- colnames(ebro)

taxa_names_all <- c(taxa_names_seg, taxa_names_ebro)
taxa_names_all <- taxa_names_all[taxa_names_all != "sample_id"] %>%
  as.data.frame()
names(taxa_names_all) <- "user_taxa"


source("scripts/check_OMNIDIA_synonyms.R")

# read full synonyms OMNIDIA table (2015 version)
Omnidia_2015_full_synonyms_table <- openxlsx::read.xlsx("data/Omnidia_2015_full_synonyms_table_XB.xlsx")

# Run the function
list <- check_diatom_names(diatnames="taxa_names_all", 
                           omnidia="Omnidia_2015_full_synonyms_table")

# Update the diatom counts with the harmonized list of names
ebro_harm <- ebro %>% 
  gather(taxa, abundance, -sample_id) %>% 
  mutate(taxa_updated = plyr::mapvalues(taxa, from = list[, "user_taxa"], to = list[, "taxon_name_synonym"])) %>% 
  aggregate(abundance ~ sample_id + taxa_updated, data = ., FUN = sum) %>%  #Collapse any species values that are now synonymized
  pivot_wider(names_from = taxa_updated, values_from = abundance) # transform to wide format

seg_harm <- seg %>% 
  gather(taxa, abundance, -sample_id) %>% 
  mutate(taxa_updated = plyr::mapvalues(taxa, from = list[, "user_taxa"], to = list[, "taxon_name_synonym"])) %>% 
  aggregate(abundance ~ sample_id + taxa_updated, data = ., FUN = sum) %>%  #Collapse any species values that are now synonymized
  pivot_wider(names_from = taxa_updated, values_from = abundance) # transform to wide format

