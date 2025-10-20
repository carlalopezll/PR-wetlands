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
library(ggplot2)

# read in data
merge <- read_csv("synoptic/data/synoptic merged.csv")

salts <- merge %>%
  dplyr::select(Cl_avg, Br_avg, SO4_avg, NO3_N_avg, Na_avg, K_avg, Mg_avg, Ca_avg)


# Correlation matrix
cor <- cor(salts, use = "pairwise.complete.obs", method = "pearson")

# Round for readability
round(cor_matrix, 2)

testRes = cor.mtest(cor, conf.level = 0.90)

# specialized the insignificant value according to the significant level
corrplot(cor, p.mat = testRes$p, sig.level = 0.1, addrect = 2, type = "lower")

# Do the PCA

salt_pca <- merge %>%
  dplyr::select(Cl_avg, Br_avg, SO4_avg, Na_avg, K_avg, Mg_avg, `Wetland type`, Site, Date_corrected)

# Select rows without NA in PCA variables
salt_pca_complete <- salt_pca[complete.cases(salt_pca[, 1:6]), ]

# Scale the numeric variables
salt_pca_scaled <- scale(salt_pca_complete[, 1:6])

# Run PCA
Z <- prcomp(salt_pca_scaled)

# Plot

biplot(Z)

fviz_pca_biplot(Z, 
                geom.ind = "point", 
                col.ind = salt_pca_complete$Site, 
                addEllipses = TRUE, 
                legend.title = "Wetland")

summary(Z)



Z$loadings

loadings <- Z$loadings
loadings

# Absolute contributions
abs_loadings <- abs(loadings)

# Percent contribution of each variable to each PC
contrib <- sweep(abs_loadings, 2, colSums(abs_loadings), FUN = "/") * 100
contrib

barplot(contrib[, "PC1"], names.arg = rownames(contrib),
        main = "Variable contributions to PC1",
        ylab = "Contribution (%)", col = "steelblue")

library(ggbiplot)  # if installed
ggbiplot(Z, obs.scale = 1, var.scale = 1,
         labels = rownames(salt_pca_complete), var.axes = TRUE) +
  theme_minimal()


# Correlation for a subset of merge

subset <- merge %>%
  dplyr::select(CO2_gL, CH4_mgL, DOC_avg, Cl_avg, SO4_avg, Temp_C, DO_mgL, TDN_avg)

# Creating a correlation matrix and plotting the corrolelogram
cor <- cor(na.omit(subset))

testRes = cor.mtest(cor, conf.level = 0.90)

## specialized the insignificant value according to the significant level
corrplot(cor, p.mat = testRes$p, sig.level = 0.10, addrect = 2, type = "lower")

# PCA

# subset variables of interest

ghg_pca <- merge %>%
  filter(Site == "Palmas" | Site == "Tortuguero") %>%
  dplyr::select(CO2_gL, CH4_mgL, DOC_avg, Cl_avg, SO4_avg, Temp_C, DO_mgL, `Wetland type`, Site, `Wetland condition`, Date_corrected, Season)

# Select rows without NA in PCA variables
ghg_pca_complete <- ghg_pca[complete.cases(ghg_pca[, 1:7]), ]

# Scale the numeric variables
ghg_pca_scaled <- scale(ghg_pca_complete[, 1:7])

# Run PCA
Z <- princomp(ghg_pca_scaled)

# Plot with correct color vector length

biplot(Z)

fviz_pca_biplot(Z, 
                geom.ind = "point", 
                col.ind = ghg_pca_complete$Site, 
                addEllipses = T,
                legend.title = "Wetland",
                repel = T)

library(ggbiplot)  # if installed
ggbiplot(Z, obs.scale = 1, var.scale = 1,
         labels = rownames(ghg_pca_complete), var.axes = TRUE) +
  theme_minimal()

ggbiplot(Z,
         obs.scale = 1,
         var.scale = 1,
         groups = ghg_pca_complete$Site, # group colors
         labels = rownames(ghg_pca_complete),
         var.axes = TRUE) +
  stat_ellipse(aes(color = ghg_pca_complete$Site),
               level = 0.95) +
  theme_minimal() +
  labs(color = "Site")


# Run stats on the PCA groups

library(vegan)

# Use PERMANOVA to test for differences between groups using the function adonis2() from the vegan package

permanova_results <- adonis2(ghg_pca_scaled ~ Site, data = ghg_pca_complete, method = "euclidean", permutations = 999)

write.csv(permanova_results, "synoptic/output/tables/PCA v1 PERMANOVA results.csv")

# permanova_results1 <- adonis2(Z$scores[,1] ~ Site, data = ghg_pca_complete, permutation = 999, method = "euclidean")
# permanova_results2 <- adonis2(Z$scores[,2] ~ Site, data = ghg_pca_complete, permutation = 999, method = "euclidean")

# If PERMANOVA is significant, it could be due to differences in location (centroids) or dispersion (spread) of the groups. Consider using PERMDISP to explicitly test for differences in multivariate dispersion.

# Differences between groups was determined by applying pairwise PERMANOVAs using the function pairwise.adonis()
# Pairwise PERMANOVA addresses the difference in centroid

library(devtools)
# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

a <- pairwise.adonis(ghg_pca_scaled, factors = ghg_pca_complete$Site, sim.function = "vegdist", sim.method = "euclidian", perm = 999)

a <- pairwise.adonis(dist(ghg_pca_scaled), ghg_pca_complete$Site, perm = 999)

write.csv(a, "synoptic/output/tables/pairwise PERMANOVA.csv", row.names = F)

# betadisper looks at difference in dispersion
# The function betadisper() from the vegan package was used to test for homogeneity of variance of groups. 

dist_matrix <- dist(Z$scores)  # Z$x in prcomp is the full PCA scores
dispersion_analysis <- betadisper(dist_matrix, ghg_pca_complete$Site)
disp_anova <- anova(dispersion_analysis)
TukeyHSD(dispersion_analysis)


write.csv(disp_anova, "synoptic/output/tables/ANOVA dispersion.csv")

# Looking at weights (or eigenvalues = gives you the magnitude of vectors and variance explained by each)
Z$sdev
summary <- summary(Z)

write.csv(summary, "synoptic/output/PCA v1 summary.csv")

# Looking at loadings (or eigenvectors= influence of each variable on all the PCs)
Z$loadings
loadings <- Z$loadings
write.csv(loadings, "synoptic/output/PCA v1 loadings.csv")

# Looking at scores

ghg_pca_complete$PC1 <- Z$scores[,1]
ghg_pca_complete$PC2 <- Z$scores[,2]

ggplot(ghg_pca_complete, aes(x=Site, y = PC1)) +
  geom_boxplot() +
  geom_jitter()

ggplot(ghg_pca_complete, aes(x=Site, y = PC2)) +
  geom_boxplot() +
  geom_jitter()

