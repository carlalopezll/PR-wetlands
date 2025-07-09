# PCA for PR wetland synoptic data

library(dplyr)
library(tidyr)
library(factoextra) # for PCA plotting

# read in data
ghg <- read_csv("synoptic/data/synoptic merged.csv")

# Correlations

# load libraries
library(corrgram)
library(corrplot)
library(ggfortify) # for autoplot
library(factoextra)
library(dplyr)


subset <- merge2 %>%
  select(CO2_avg, CH4_avg, TKN_N, TP, DP, NO3_N, DNH4_N, TOC, Hardness, Al, Ca, Fe, K, Mg, Mn, Na, Si, Cl)


# Creating a correlation matrix and plotting the corrolelogram
cor <- cor(na.omit(subset))
view(cor)

# Plot a corrolelogram
corrplot(cor, method="circle", type="lower", na.label=" ")

testRes = cor.mtest(cor, conf.level = 0.95)

## specialized the insignificant value according to the significant level
corrplot(cor, p.mat = testRes$p, sig.level = 0.05, addrect = 2, type = "lower")


# Use this one when I have more IC data
# ghg_pca <- ghg %>%
#   dplyr::select(CO2_uM, CH4_uM, F_avg, SO4_avg, NH4_ppb_mean, PO4_ppb_mean, DOC_avg, TDN_avg, `Wetland type`, Site, Date_corrected)

ghg_pca <- ghg %>%
  dplyr::select(CO2_uM, CH4_uM, Temp_C, `Wetland type`, Site, Date_corrected)

ghg_pca_scaled <- scale(ghg_pca[,1:3])

# creating a correlation matrix
corr <- cor(ghg_pca_scaled)

corr <- cor(ghg_pca[,1:3])

# running the PCA
Z <- princomp(ghg_pca_scaled, cor = T, scores = T)

# Looking at weights (or eigenvalues = gives you the magnitude of vectors and variance explained by each)
Z$sdev
summary(Z)

# Looking at loadings (or eigenvectors= influence of each variable on all the PCs)
Z$loadings

# Looking at scores in each PC
Z$scores[,1]

####  Some generic plotting code   ######

biplot(Z)

fviz_pca_biplot(Z, 
                geom.ind = "point", 
                col.ind = ghg_pca$Site,  # color by group if needed
                addEllipses = TRUE, 
                legend.title = "Wetland type")

#  Basic scatterplot
plot(Z$scores[,1], Z$scores[,2], xlab='PC1', ylab='PC2', main='PC1 vs. PC2 for the Tuesday Lake data')

#lattice plot (must load the lattice package first); you don't need to install it though. It is pre-installed in base-R
library(lattice)

# Plot using lattice
xyplot(Z$scores[,2]~Z$scores[,1], xlab='PC1', ylab='PC2', main='PC1 vs. PC2 for the Tuesday Lake data')

# Plot using lattice subsetted by season
xyplot(Z$scores[,2]~Z$scores[,1]|ghg$Season, xlab='PC1', ylab='PC2', main='PC1 vs. PC2 for the Tuesday Lake data')