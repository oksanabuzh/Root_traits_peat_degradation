
library(tidyverse)

# Load and prepare the raw data -----------------------------------------------

## Species composition ------------
# in original dataset called "species_cover_BB_dataset.xlsx"
sp_compos <- read_csv("data/processed_data/species_composition.csv")

sp_compos

## Environmental variables ---------
envar_var <- read_csv("data/processed_data/environmental_data.csv") 

envar_var
names(envar_var)

### Degradation gradient (most disturbed to least disturbed):----
#  mud -> mulm -> earthified -> decomposed -> peat,
# where peat is the least disturbed and mud is the most disturbed
envar_var %>% 
  group_by(soil_status, soil_status_disturbance_order) %>% 
  count()

envar_var %>% 
  group_by(trophic_state) %>% 
  count()

### Trophic state --------
envar_var %>% 
  group_by(soil_status, trophic_state) %>% 
  count()


## Traits
traits <- read_csv("data/traits_species_mean_dataset.csv")
traits

summary(traits)

traits %>% 
  filter(is.na(SLA))
