# Description: Calculate community-weighted means (CWM) and functional diversity metrics for traits.

# Load required packages ----
library(tidyverse)

# Load  data ----
sp_compos <- read_csv("data/processed_data/species_composition.csv") %>% 
  column_to_rownames("plot_ID") 

traits <- read_csv("data/processed_data/interspecific_traits.csv") %>% 
  column_to_rownames("species")


# Check NAs in traits

summary(traits)

traits %>% 
  filter(is.na(aerenchyma))

# Trait coverage: how much each trait covers in each plot out of total community cover? -------------------------
# for each species and plot_ID in sp_compos, check % of cover that each trait covers
traits_coverage  <- sp_compos %>%
  pivot_longer(-plot_ID, names_to = "species", values_to = "cover") %>%
  filter(cover > 0) %>%
  left_join(traits %>% rownames_to_column("species"), by = "species") %>%
# for all traits if NA ~ NA, if not NA ~ column "cover"  
  mutate(across(-c(plot_ID, species, cover), 
                ~ ifelse(is.na(.x), NA, cover))) %>%
  group_by(plot_ID) %>%
# for each trait, sum cover where trait is not NA / total cover * 100
  summarise(across(-c(species, cover), 
                   ~ sum(.x, na.rm = TRUE)/sum(cover) * 100)) %>%
  ungroup() 

## 1) CWM for functional traits ----
dim(sp_compos)
dim(traits)

FuncComp <- FD::functcomp(traits, as.matrix(sp_compos), 
                          CWM.type = "all", 
                          bin.num=c(  #  bin.num - indicates binary traits to be treated as continuous  
                            "raunkiaer_phanerophyte", "lifeform_tree.shrub",
                            "lifeform_herbPoli", "lifeform_herbMono")) 

FuncComp

dim(FuncComp)

write_csv(as.data.frame(FuncComp) %>% 
            rownames_to_column("Plot") %>% 
            separate(Plot, into = c("PlotNo", "Subplot", "Month"), sep = "_",
                     remove = TRUE),
          "data/processed_data/CWM_FunctCompos_1m2.csv")


## 2) Functional diversity ----

# When calculating functional diversity, FD::dbFD Error occurs when mixed categorical and numeric traits
# thus, convert categorical traits to wide format for each category
Func_groups_modif <- Func_groups %>% 
  select(-starts_with("raunkiaer_"), # correlate strongly with lifespan
         -starts_with("lifeform_"),  # correlate strongly with lifespan
         # exclude all disturb indic for calculating functional diversity
         -Disturbance.Frequency,-Disturbance.Severity,  
         -Mowing.Frequency, -Grazing.Pressure, -Soil.Disturbance) %>%
  rownames_to_column("Taxon") %>%
  # for column status, create wide a format 
  mutate(present = 1) %>%
  pivot_wider(names_from = status, values_from=present, values_fill = 0,
              names_prefix = "status_") %>%
  mutate(across(starts_with("status_"), ~ replace(.x, status_NA==1, NA))) %>%
  select(-status_NA) %>% 
  # for column functional_group, create a wide format 
  mutate(present = 1) %>%
  pivot_wider(names_from = functional_group, values_from=present, values_fill = 0,
              names_prefix = "FunGr_") %>%
  column_to_rownames("Taxon")

str(Func_groups_modif)

FuncDiv <- FD::dbFD(Func_groups_modif, as.matrix(species_data), 
                    calc.CWM = F,
                    corr = c("cailliez")) 
FuncDiv

str(FuncDiv)

FuncDiv <- as.data.frame(FuncDiv) %>% 
  rownames_to_column("Plot") %>%
  select(-nbsp, -qual.FRic, -sing.sp) %>% 
  separate(Plot, into = c("PlotNo", "Subplot", "Month"), sep = "_",
           remove = TRUE)


# save Functional diversity indices
write_csv(FuncDiv, "data/processed_data/Functional_Diversity_1m2.csv")