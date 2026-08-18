
if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
}

results_path <- file.path("results")

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
colSums(seg_harm_t[,-1])

# check and convert the input data in the proper format to calculate indices
df_ebro <- diat_loadData(ebro_harm_t, maxDistTaxa = 2, resultsPath = results_path)
df_seg <- diat_loadData(seg_harm_t, maxDistTaxa = 2, resultsPath = results_path)

# use the diat_ips() function to calculate the IPS index 
# Each time diat_ips() is executed, a new set of results in the output folder is created
ips_ebro <- diat_ips(df_ebro) 
ips_seg <- diat_ips(df_seg) 

# see the output
print(ips_ebro)
# IPS: original IPS values
# IPS20: normalized to the standard 0-20 range
# num_taxa: n diatom taxa used to compute the index

## Additional ecological information
# Van Dam classification
vandam_ebro <- diat_vandam(df_ebro, vandamReports = TRUE)
vandam_ebro <- diat_vandam(df_seg, vandamReports = TRUE)

