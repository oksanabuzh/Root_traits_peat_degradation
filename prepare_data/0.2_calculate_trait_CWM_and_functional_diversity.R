# Description: Calculate community-weighted means (CWM) and functional diversity metrics for traits.

# Load required packages ----
library(tidyverse)

# Load  data ----
sp_compos <- read_csv("data/processed_data/species_composition.csv") %>% 
  column_to_rownames("plot_ID") 

trait_data <- read_csv("data/processed_data/interspecific_traits.csv") 

# Prepare trait matrix ----
# make sure species names are same order as in sp_compos
traits <- sp_compos %>% 
  pivot_longer(everything(),names_to = "species", values_to = "cover") %>%
  select(-cover) %>%
  distinct() %>% 
  left_join(trait_data,
            by = "species") %>% 
  column_to_rownames("species")


# Check NAs in traits
summary(traits)

traits %>% 
  filter(is.na(aerenchyma))

# Trait coverage check: how much each trait covers in each plot out of total community cover? -------------------------
# for each species and plot_ID in sp_compos, check % of cover that each trait covers
traits_coverage  <- sp_compos %>% rownames_to_column("plot_ID") %>%
  pivot_longer(-plot_ID, names_to = "species", values_to = "cover") %>%
  filter(cover > 0) %>%
  left_join(traits %>% rownames_to_column("species"), by = "species") %>%
# for all traits if NA ~ NA, if not NA ~ column "cover"  
  mutate(across(-c(plot_ID, species, cover), 
                ~ ifelse(is.na(.x), NA, cover))) %>%
# for each trait, sum cover where trait is not NA / total cover * 100
  summarise(across(-c(species, cover), 
                   ~ sum(.x, na.rm = TRUE)/sum(cover) * 100),
            .by=plot_ID) 


write_csv(traits_coverage,  
          "data/for_data_checks/trait_coverage_per_plot.csv")

# Which traits are not sufficiently covered? ----
# less than 60% coverage 
traits_coverage %>%
  pivot_longer(-plot_ID, names_to = "trait", values_to = "coverage") %>%
  filter(coverage < 60) %>%
   arrange(trait, coverage) %>% 
  print(n=Inf)

## ->  Remove insufficient trait data ----
# Based on the trait coverage results, remove traits with insufficient data coverage across the study plots

traits <- traits %>% 
  select(-aeren_percent)


# Calculate CWM and Functional diversity -------------------------

## 1) CWM for functional traits ----
dim(sp_compos)
dim(traits)

FuncComp <- FD::functcomp(traits, as.matrix(sp_compos), 
                          CWM.type = "all")
                      #    bin.num=c("")  #  bin.num - indicates binary traits to be treated as continuous

FuncComp

dim(FuncComp)

write_csv(as.data.frame(FuncComp) %>% 
            rownames_to_column("plot_ID"), 
          "data/processed_data/CWM_FunctCompos.csv")


## 2) Functional diversity ----

# When calculating functional diversity, FD::dbFD Error occurs when mixed categorical and numeric traits
# thus, convert categorical traits to wide format for each category

traits_modif <- traits %>% rownames_to_column("species") %>% 
  # for column aerenchyma, create a wide format 
  mutate(present = 1) %>%
  pivot_wider(names_from = aerenchyma, values_from=present, values_fill = 0,
              names_prefix = "Aerenchyma_") %>% 
  mutate(across(starts_with("Aerenchyma_"), ~ replace(.x, Aerenchyma_NA==1, NA))) %>%
  select(-Aerenchyma_NA) %>% 
# for column AMF_type, create a wide format 
  mutate(present = 1) %>%
  pivot_wider(names_from = AMF_type, values_from=present, values_fill = 0,
              names_prefix = "AMF_") %>% 
  mutate(across(starts_with("AMF_"), ~ replace(.x, AMF_NA==1, NA))) %>%
  select(-AMF_NA) %>% 
# for column peat_association, create a wide format 
  mutate(present = 1) %>%
  pivot_wider(names_from = peat_association, values_from=present, values_fill = 0,
              names_prefix = "Peat.Sp_") %>% 
  mutate(across(starts_with("Peat.Sp_"), ~ replace(.x, Peat.Sp_NA==1, NA))) %>%
  select(-Peat.Sp_NA) %>% 
# for column PFT, create a wide format 
  mutate(present = 1) %>%
  pivot_wider(names_from = PFT, values_from=present, values_fill = 0,
              names_prefix = "PFT_") %>% 
  mutate(across(starts_with("PFT_"), ~ replace(.x, PFT_NA==1, NA))) %>%
  select(-PFT_NA) %>% 
# for column lifespan, create a wide format 
  mutate(present = 1) %>%
  pivot_wider(names_from = lifespan, values_from=present, values_fill = 0,
              names_prefix = "LifeSpan_") %>% 
  mutate(across(starts_with("LifeSpan_"), ~ replace(.x, LifeSpan_NA==1, NA))) %>%
  select(-LifeSpan_NA) %>% 
  column_to_rownames("species")
  
str(traits_modif)


FuncDiv <- FD::dbFD(traits_modif, as.matrix(sp_compos), 
              calc.CWM = F,
         corr = c("cailliez"))

str(FuncDiv)

FuncDiv <- as.data.frame(FuncDiv) %>% 
  rownames_to_column("Plot_ID") %>%
  select(-nbsp, -qual.FRic, -sing.sp) 


# save Functional diversity indices
write_csv(FuncDiv, "data/processed_data/Functional_Diversity.csv")