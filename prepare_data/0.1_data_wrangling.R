# Script to wrangle raw data into analysis-ready format


# environmental variables ---------
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
    soil_status == "peat" ~ 5)) 

envar_var
