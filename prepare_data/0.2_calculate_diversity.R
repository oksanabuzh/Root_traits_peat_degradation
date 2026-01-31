# Description: This script calculates species diversity metrics from raw species composition data.

# Load required packages ------------------------------------------------------
library(tidyverse)

# Create a temporary directory for storing intermediate files
tempdir()
dir.create(tempdir())

# Load and prepare the raw data -----------------------------------------------

## species composition ------------
sp_compos <- read_csv("data/processed_data/species_composition.csv")

sp_compos

## Calculate species diversity metrics ----------------------------------------
species_div <- sp_compos %>% 
  pivot_longer(-plot_ID, names_to = "species", values_to = "cover") %>%  
  filter(cover>0) %>% 
  summarise(
  SR = n_distinct(species),
  hill_simpson = vegan::diversity(cover, index = "invsimpson"),
  hill_shannon = exp(vegan::diversity(cover, index = "shannon")),
  .by = plot_ID)

species_div

# Save processed data ---------------------------------------------------------
write_csv(species_div, "data/processed_data/species_diversity.csv")
