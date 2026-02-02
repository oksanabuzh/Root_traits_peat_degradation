# Script to wrangle raw data into analysis-ready format

# Load required packages ------------------------------------------------------
library(tidyverse)

## Species composition matrix ------------
sp_compos <- read_csv("data/raw_data/species_cover_BB_dataset.csv") %>% 
  select(- Calliergonella_cuspidata) # remove the only moss species

sp_compos %>% 
  print(n=Inf)

# Data exploration:
# check for NA‘s
anyNA(sp_compos) 
summary(sp_compos)

# Which plots has no cover across all species
sp_compos %>% 
  select(-plot_ID) %>%
  filter(rowSums(.) == 0, na.rm = TRUE)

# filter species with NA cover values
sp_compos %>%
  pivot_longer(-plot_ID, names_to = "species", values_to = "cover") %>%  
  filter(is.na(cover))

# replace NA cover values with 0 ----
sp_compos <-sp_compos %>%
  mutate(across(-plot_ID, ~replace_na(., 0)))

write_csv(sp_compos, "data/processed_data/species_composition.csv")



# Environmental variables ---------
envar_var <- read_csv("data/raw_data/CWM_dataset_160126.csv")  %>%  
 # combine eutrophic-rich  and eutrophic-vigorous into single “eutrophic” group
  mutate(trophic_state = ifelse(trophic_state %in% c("eutrophic-rich", "eutrophic-vigorous"), 
                               "eutrophic", trophic_state)) %>% 
  # create soil status disturbance order variable
  # peat is the least disturbed and mud is the most disturbed
    mutate(soil_status_disturbance_order= case_when(
    soil_status == "mud" ~ 1,
    soil_status == "mulm" ~ 2,
    soil_status == "earthified" ~ 3,
    soil_status == "decomposed" ~ 4,
    soil_status == "peat" ~ 5), .after = soil_status) %>% 
  # remove root traits from the data
  select(-c(SRL, AD, RTD, AMF))

envar_var %>% 
  print(n=Inf)

names(envar_var)


write_csv(envar_var, "data/processed_data/environmental_data.csv")


# Traits --------------------------------------------------
traits <- read_csv("data/raw_data/traits_species_mean_dataset.csv") %>%
  # fix species name with extra space
  mutate(species=ifelse(species=="Achillea_ millefolium", 
                        "Achillea_millefolium", species)) 
traits

# NAs
summary(traits)

traits %>% 
  filter(is.na(aerenchyma))


# Do species match with the community data?

# 1) Which species in the traits data are missing from the community data
sp_list <- colnames(sp_compos)[-1] # Get species list from community data

traits %>% 
  filter(!species %in% sp_list) %>% 
  distinct(species)


# 2) Which species in the community data are present in the traits data
sp_compos %>% 
  pivot_longer(-plot_ID, names_to = "species", values_to = "cover") %>%  
  filter(cover>0) %>% 
  distinct(species) %>% 
  filter(!species %in% traits$species) 


write_csv(traits, "data/processed_data/interspecific_traits.csv")
