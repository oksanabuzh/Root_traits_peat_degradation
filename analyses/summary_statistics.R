
library(tidyverse)

# Load and prepare the raw data -----------------------------------------------

## species composition ------------
sp_compos <- read_csv("data/species_cover_BB_dataset.csv")

sp_compos

## environmental variables ---------
envar_var <- read_csv("data/CWM_dataset_160126.csv")
envar_var


envar_var %>% 
  group_by(soil_status) %>% 
  count()

# Degradation gradient 
# mud -> mulm -> earthified -> decomposed -> peat

envar_var %>% 
  group_by(trophic_state) %>% 
  count()
# merge eutrophic into one



envar_var %>% 
  group_by(soil_status, trophic_state) %>% 
  count()


## traits
traits <- read_csv("data/traits_species_mean_dataset.csv")
traits

summary(traits)

traits %>% 
  filter(is.na(SLA))
