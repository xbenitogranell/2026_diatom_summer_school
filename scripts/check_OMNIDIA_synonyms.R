#Function to check ACA codes and extract synonyms and names from OMNIDIA 2015 database
check_diatom_names <- function(diatnames="", 
                              omnidia="") {
  
  library(tidyverse)
  library(readxl)
  library(qdapTools)
  
  #Add user list to new conversion dataframe
  conversion_df <- taxa_names_all
  names(conversion_df) <- "user_taxa"
  
  ## Step 1: make sure var. cf., and forma, etc are written correctly for Omnidia standards. The result is lev1 column
  conversion_df$user_taxa_lev1 <- trimws(gsub("\\s+", " ", conversion_df[,"user_taxa"]))
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" sp ", replacement=" sp. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" var ", replacement=" var. ", fixed = TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" cf ", replacement=" cf. ", fixed=TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" for. ", replacement=" fo. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" f. ", replacement=" fo. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" f ", replacement=" fo. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern="spp(?!\\.)", replacement="spp.", perl =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" aff ", replacement=" aff. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" sl ", replacement=" s.l. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" ss ", replacement=" s.s. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" s. l. ", replacement=" s.l. ", fixed =TRUE)

  #Compare user names to Omnidia taxon list
  diat_omnidia <- Omnidia_2015_full_synonyms_table

  # Add the name of user taxa's code
  conversion_df$Omnidia_code<- lookup_e(conversion_df$user_taxa_lev1, diat_omnidia[,c("DENOM_lev1","CODE")])
  conversion_df$taxon_name_authority <- lookup_e(conversion_df$user_taxa_lev1, diat_omnidia[,c("DENOM_lev1","DENOM")])
  conversion_df$taxon_name <- lookup_e(conversion_df$Omnidia_code, diat_omnidia[,c("CODE","DENOM_lev1")])
  
  # Add the most recent syn Omnidia code
  conversion_df$most_recent_omnidia_code <- lookup_e(conversion_df$Omnidia_code, diat_omnidia[,c("CODE","MOST_RECENT_SYN")])
  
  #Create new columns with taxon names
  conversion_df$taxon_name_synonym_authority <- lookup_e(conversion_df$most_recent_omnidia_code, diat_omnidia[,c("CODE","DENOM")])
  conversion_df$taxon_name_synonym <- lookup_e(conversion_df$taxon_name_synonym_authority, diat_omnidia[,c("DENOM","DENOM_lev1")])
  
  #replace empty most synonym column with original user name
  conversion_df[is.na(conversion_df$most_recent_omnidia_code)==TRUE,]$taxon_name_synonym <- conversion_df[is.na(conversion_df$most_recent_omnidia_code)==TRUE,]$user_taxa_lev1
  
  # add last column with other synonyms
  conversion_df$other_synonims <- lookup_e(conversion_df$most_recent_omnidia_code, diat_omnidia[,c("CODE","ALL_SYNONYMS_CODES")])
  
  # Add descriptive mismatch flag
  flagged <- conversion_df %>%
    rowwise() %>%
    mutate(
      # Extract clean 4-letter codes
      syns_all = list(str_extract_all(other_synonims, "\\b[A-Z]{4}\\b")[[1]] %>% unique() %>% na.omit()),
      all_codes = list(str_extract_all(c_across(c(Omnidia_code, most_recent_omnidia_code, other_synonims)), "\\b[A-Z]{4}\\b") %>% unlist() %>% unique() %>% na.omit()),
      
      # Boolean flag
      is_mismatch = length(all_codes) > 2,
      
      # Detailed description accounting for multiple synonyms
      mismatch_desc = if (!is_mismatch) {
        "Match"
      } else if (length(syns_all) > 1) {
        paste0("Mismatch: ", length(all_codes), " distinct codes found. ALL_SYNONYMS_CODES contains ", length(syns_all), " synonyms (", paste(syns_all, collapse = ", "), ").")
      } else {
        paste0("Mismatch: CODE (", most_recent_omnidia_code, ") and MOST_RECENT_SYN (", most_recent_omnidia_code, ") differ.")
      }
    ) %>%
    select(-syns_all, -all_codes) %>%
    ungroup()
  
  conversion_df <- data.frame(conversion_df,flagged$is_mismatch,flagged$mismatch_desc)
  names(conversion_df)[c(10,11)] <- c("synonym_mismatch","synonym_mismatch_notes")
  
  return(conversion_df)
}


