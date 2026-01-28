# ---- Load packages ----
library(tidyverse)
library(vegan)
library(ggrepel)
library(tibble)
library(lattice)
library(patchwork)
library(ggcorrplot)
library(ggfortify)

# ---- Read and Prepare Data ----
cwm_data <- read_csv2("data/CWM_dataset.csv")
str(cwm_data)

spec_data <- read_csv2("data/species_mean.csv") %>%
  drop_na()



# ---- Correlation Analysis ----


# --- CWM Dataset ---

#1 compute correlation matrix
cor_matx <- cwm_data %>%
  select(SRL, AD, RTD, AMF) %>%
  cor(method = "pearson", use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")

#2 plot matrix
ggplot(cor_matx, aes(Var1, Var2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0
  ) +
  theme_minimal() +
  coord_fixed()


# --- Species Mean Dataset ---

#1 compute correlation matrix
cor_maty <- spec_data %>%
  select(SRL, AD, RTD, AMF, rootN) %>%
  cor(method = "pearson", use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")

#2 plot matrix
ggplot(cor_maty, aes(Var1, Var2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0
  ) +
  theme_minimal() +
  coord_fixed()


#---- Basic PCA overview ----


# --- CWM Dataset ---
cwmdata_pca <- select(cwm_data, SRL, AD, RTD, AMF)

CWMPCA_1 <- prcomp(cwmdata_pca, center = TRUE, scale. = TRUE)

autoplot(CWMPCA_1, data= cwmdata_pca, loadings=TRUE, 
         loadings.label=TRUE, loadings.label.vjust = 1.7, 
         loadings.label.colour='black', loadings.label.size=3, 
         loadings.colour='black')+
  theme_classic()

summary(CWMPCA_1) # summarizes importance of components ("proportion of variance" equals percentage of pca component explaining variation)

print(CWMPCA_1)

# --- Species Mean Dataset ---
specdata_pca <- select(spec_data, SRL, AD, RTD, AMF, rootN)

specPCA_1 <- prcomp(specdata_pca, center = TRUE, scale. = TRUE)

autoplot(specPCA_1, data= specdata_pca, loadings=TRUE, 
         loadings.label=TRUE, loadings.label.vjust = 1.7, 
         loadings.label.colour='black', loadings.label.size=3, 
         loadings.colour='black')+
  theme_classic()

summary(specPCA_1) # summarizes importance of components ("proportion of variance" equals percentage of pca component explaining variation)

print(specPCA_1)
