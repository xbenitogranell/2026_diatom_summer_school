## Diatom monitoring workshop @ 2nd Diatom Summer School, Szczecin, Poland
## August 26th, 2026
## Xavier Benito
## contact: xavier.benito@irta.cat
## https://xbenitogranell.github.io/ 

#----------------------------------------------------------------------------------------------------------------------------------
# This document contains scripts to perform statistical correlations between IPS values and nutrient data.
# It will also allow the user to visualize sample's IPS values and compare with ecological status boundaries
#----------------------------------------------------------------------------------------------------------------------------------

# The library pacman loads the necessary packages if they are installed in your R environment. Else pacman() will install and load them
#install.packages("pacman") #install the library
pacman::p_load(openxlsx, tidyverse, diathor, egg, vegan, corrplot)


## First, prepare the dataset to visualize the IPS results 
# Ebro Delta dataset
ebro_env <- data.frame(all_data[[2]]$EbroDelta_env) %>%
  mutate(sample_id = paste(habitat, area, period, sep = "_"),
         sample_id = make.unique(sample_id, sep = ".")) 

# join the environmental dataset with IPS data
env_ips_ebro <- ips_ebro %>%
  rownames_to_column(var = "sample_id") %>%
  left_join(ebro_env, by="sample_id")

ggplot(env_ips_ebro, aes(x = area, y = IPS20)) +
  #these are broad, generic ecological status boundaries to visualize wher IPS values fall
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 17, ymax = 20, fill = "#2b83ba", alpha = 0.15) + # High (Blue)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 13, ymax = 17, fill = "#abdda4", alpha = 0.15) + # Good (Green)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 9,  ymax = 13, fill = "#ffffbf", alpha = 0.15) + # Moderate (Yellow)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 5,  ymax = 9,  fill = "#fdae61", alpha = 0.15) + # Poor (Orange)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = 5,  fill = "#d7191c", alpha = 0.15) + # Bad (Red)
  # geom_hline(yintercept = status_lines, linetype = "dashed", color = "grey40", size = 0.5) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.fill = "red") +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black") + # Adds individual data points
  scale_fill_viridis_d(option = "mako") +                 # Uses a clean color palette
  labs(x = "",y = "IPS value") +
  theme_article() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exploring env data distribution
par(mfrow=c(4,4))
for (i in 8:ncol(env_ips_ebro)) hist(env_ips_ebro[,i], main=names(env_ips_ebro)[i])
par(mfrow=c(1,1))

# make transformation of nutrient variables to improve linearity
env_ips_ebro[,c("NT","PT","PO4","NH4","NO2","NO3","SO4","chl.a")]<-log(env_ips_ebro[,c("NT","PT","PO4","NH4","NO2","NO3","SO4","chl.a")])

# select relevant nutrient variables to perform the correlations
env_ips_ebro <- env_ips_ebro %>%
  select(IPS20,NT,PT,PO4,NH4,NO2,NO3,SO4,chl.a)


# Calculate matrix for all numeric environmental variables
cor_matrix <- rcorr(as.matrix(env_ips_ebro), type = "spearman")

# Plot heatmap displaying significant correlations
corrplot(
  cor_matrix$r,
  p.mat = cor_matrix$P,
  sig.level = 0.05,
  insig = "blank",        # Hide non-significant correlations (p > 0.05)
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black")  # Display r_s values inside cells

## Repeat the
# Segura dataset
seg_env <- data.frame(all_data[[2]]$Segura_env) %>%
  mutate(sample_id = paste(site,year, sep = "_"),
         sample_id = make.unique(sample_id, sep = ".")) 

env_ips_ebro <- ips_seg %>%
  rownames_to_column(var = "sample_id") %>%
  left_join(seg_env, by="sample_id")


