# PCA for PR wetland synoptic data
# Carla López Lloreda

# load libraries
library(dplyr)
library(tidyr)
library(readr)
library(factoextra) # for PCA plotting
library(corrgram)
library(corrplot)
library(ggfortify) # for autoplot
library(factoextra)
library(dplyr)

# read in data
merge <- read_csv("synoptic/data/synoptic merged.csv")

salts <- merge %>%
  dplyr::select(Cl_avg, Br_avg, NO3_N_avg, SO4_avg, Na_avg, K_avg, Mg_avg, Ca_avg, NH4_ppb_mean, PO4_ppb_mean)

# Correlations

# Creating a correlation matrix and plotting the corrolelogram
cor <- cor(na.omit(salts))

testRes = cor.mtest(cor, conf.level = 0.95)

# specialized the insignificant value according to the significant level
corrplot(cor, p.mat = testRes$p, sig.level = 0.05, addrect = 2, type = "lower")

# Do the PCA

salt_pca <- merge %>%
  dplyr::select(Cl_avg, Br_avg, NO3_N_avg, SO4_avg, Na_avg, K_avg, Mg_avg, Ca_avg, `Wetland type`, Site, Date_corrected)

# Select rows without NA in PCA variables
salt_pca_complete <- salt_pca[complete.cases(salt_pca[, 1:8]), ]

# Scale the numeric variables
salt_pca_scaled <- scale(salt_pca_complete[, 1:8])

# Run PCA
Z <- princomp(salt_pca_scaled, cor = TRUE, scores = TRUE)

# Plot

biplot(Z)

fviz_pca_biplot(Z, 
                geom.ind = "point", 
                col.ind = salt_pca_complete$Site, 
                addEllipses = TRUE, 
                legend.title = "Wetland")

# Correlation for a subset of merge

subset <- merge %>%
  dplyr::select(CO2_gL, CH4_mgL, DOC_avg, Cl_avg, SO4_avg, Temp_C, DO_mgL)

# Creating a correlation matrix and plotting the corrolelogram
cor <- cor(na.omit(subset))

testRes = cor.mtest(cor, conf.level = 0.90)

## specialized the insignificant value according to the significant level
corrplot(cor, p.mat = testRes$p, sig.level = 0.10, addrect = 2, type = "lower")

# PCA

# subset variables of interest
ghg_pca <- merge %>%
  dplyr::select(CO2_gL, CH4_mgL, DOC_avg, Cl_avg, SO4_avg, Temp_C, DO_mgL, `Wetland type`, Site, `Wetland condition`, Date_corrected)

# Select rows without NA in PCA variables
ghg_pca_complete <- ghg_pca[complete.cases(ghg_pca[, 1:7]), ]

# Scale the numeric variables
ghg_pca_scaled <- scale(ghg_pca_complete[, 1:7])

# Run PCA
Z <- princomp(ghg_pca_scaled, cor = TRUE, scores = TRUE)

# Plot with correct color vector length

biplot(Z)

fviz_pca_biplot(Z, 
                geom.ind = "point", 
                col.ind = ghg_pca_complete$Site, 
                addEllipses = TRUE, 
                legend.title = "Wetland")

# Looking at weights (or eigenvalues = gives you the magnitude of vectors and variance explained by each)
Z$sdev
summary(Z)

# Looking at loadings (or eigenvectors= influence of each variable on all the PCs)
Z$loadings

# Looking at scores in each PC
Z$scores[,1]