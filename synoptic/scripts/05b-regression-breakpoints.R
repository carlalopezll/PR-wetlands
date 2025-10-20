# Looking at breakpoints in non-linear relationships in PR synoptic data
# Carla Lopez Lloreda
# 7/29/2025

# load libraries
library(segmented)
library(broom)

# read in data
merge <- read_csv("synoptic/data/synoptic merged.csv")

# CH4-SO4 model
mod_so4 <- lm(CH4_mgL ~ SO4_avg, data = merge)
seg_so4 <- segmented(mod_so4, seg.Z = ~SO4_avg)
plot(seg_so4)

summary(mod_so4)
summary(seg_so4)
CH4_SO4_seg <- summary(seg_so4)

tidy_summary <- tidy(CH4_SO4_seg)

write_csv(tidy_summary, "synoptic/output/tables/CH4 vs SO4 segmented.csv")

# CH4-Cl model
mod_cl <- lm(CH4_mgL ~ Cl_avg, data = merge)
seg_cl <- segmented(mod_cl, seg.Z = ~Cl_avg)
summary(mod_cl)
CH4_Cl <- summary(seg_cl)

CH4_Cl

tidy_summary_cl <- tidy(seg_cl)

write_csv(tidy_summary, "synoptic/output/tables/CH4 vs Cl segmented.csv")

AIC(mod_so4, seg_so4)
AIC(mod_cl, seg_cl)

plot(CH4_mgL ~ SO4_avg, data = merge, pch = 16, col = as.factor(merge$Site))
plot(seg_so4, add = TRUE, col = "black", lwd = 2)
abline(v = 2.188, lty = 2, col = "red") # threshold line

plot(CH4_mgL ~ Cl_avg, data = merge, pch = 16, col = as.factor(Site))
plot(seg_cl, add = TRUE, col = "black", lwd = 2)
abline(v = 114.74, lty = 2, col = "red")

ggsave("CH4 vs Cl_segmented.jpg")
