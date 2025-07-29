# Looking at relationships across coastal wetlands from other studies
# Carla Lopez Lloreda
# Last updated 7/29/2025

# load libraries
library(readr)
library(ggplot2)
library(factoextra) # for PCA plotting

# read in data with site averages across studies
global <- read_csv("global/global means.csv")

# plots

ggplot(global, aes(x=Cl_mgL, y = SO4_mgL, color = Study)) +
  geom_point()

ggplot(global, aes(x=SO4_mgL, y = CH4_ugL, color = Study)) +
  geom_point()

ggsave("global/CH4 vs SO4_global.jpg")

ghg_pca <- global %>%
  dplyr::select(Cl_mgL, SO4_mgL, CH4_ugL, Study)

# Select rows without NA in PCA variables
ghg_pca_complete <- ghg_pca[complete.cases(ghg_pca[, 1:3]), ]

# Scale the numeric variables
ghg_pca_scaled <- scale(ghg_pca_complete[, 1:3])

# Run PCA
Z <- princomp(ghg_pca_scaled, cor = TRUE, scores = TRUE)

# Plot with correct color vector length

biplot(Z)

fviz_pca_biplot(Z, 
                geom.ind = "point", 
                col.ind = ghg_pca_complete$Study, 
                addEllipses = TRUE, 
                legend.title = "Wetland type")