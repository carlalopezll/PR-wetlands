# Looking at breakpoints in non-linear relationships in PR synoptic data
# Carla Lopez Lloreda
# 7/29/2025

# load libraries
library(segmented)

# read in data
merge <- read_csv("synoptic/data/synoptic merged.csv")

# CH4-SO4 model
mod_so4 <- lm(CH4_uM ~ SO4_avg, data = merge)
seg_so4 <- segmented(mod_so4, seg.Z = ~SO4_avg)
summary(seg_so4)
plot(seg_so4)

# CH4-Cl model
mod_cl <- lm(CH4_uM ~ Cl_avg, data = merge)
seg_cl <- segmented(mod_cl, seg.Z = ~Cl_avg)
summary(seg_cl)
plot(seg_cl)

AIC(mod_so4, seg_so4)
AIC(mod_cl, seg_cl)

plot(CH4_uM ~ SO4_avg, data = merge, pch = 16, col = as.factor(merge$Site))
plot(seg_so4, add = TRUE, col = "black", lwd = 2)
abline(v = 2.188, lty = 2, col = "red") # threshold line

plot(CH4_uM ~ Cl_avg, data = merge, pch = 16, col = as.factor(Site), 
     xlab = "Cl (mg/L)", ylab = "CH4 (µM)", main = "Segmented: CH₄ vs Cl⁻")
plot(seg_cl, add = TRUE, col = "black", lwd = 2)
abline(v = 114.74, lty = 2, col = "red")

ggsave("CH4 vs Cl_segmented.jpg")
