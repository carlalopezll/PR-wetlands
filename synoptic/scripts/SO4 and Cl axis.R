# 1. Subset to rows with complete data for SO4 and Cl
subset_complete <- merge[complete.cases(merge[, c("SO4_avg", "Cl_avg")]), ]

# 2. Standardize
scaled_vars <- scale(subset_complete[, c("SO4_avg", "Cl_avg")])

# 3. Run PCA
pc2var <- prcomp(scaled_vars, center = TRUE, scale. = TRUE)

# 4. Extract PC1
subset_complete$PC_SO4Cl <- pc2var$x[, "PC1"]

# 5. Compute standardized average
subset_complete$Avg_SO4Cl <- rowMeans(scaled_vars)

# 6. Compare
correlation <- cor(subset_complete$PC_SO4Cl, subset_complete$Avg_SO4Cl)
print(paste("Correlation between PC1 and standardized average:", round(correlation, 3)))

# 7. Optional: plot
ggplot(subset_complete, aes(x = Avg_SO4Cl, y = PC_SO4Cl)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(x = "Standardized average (SO4 + Cl)",
       y = "PC1 (SO4 + Cl PCA)",
       title = "Comparison of PC1 vs Standardized Average")


summary(lm(CH4_log~Avg_SO4Cl, data = merge))
summary(lm(CH4_log~SO4_avg, data = merge))
summary(lm(CH4_log~Cl_avg, data = merge))

# 5th row
Cl_a <- ggplot(merge, aes(x = Avg_SO4Cl, y = CO2_gL)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  labs(x= "Chloride (mg/L)", y= CO2_lab) +
  theme

Cl_a

Cl_b <- ggplot(merge, aes(x = Avg_SO4Cl, y = CH4_log)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  labs(x= "Chloride-sulfate gradient", y= CH4_log_lab) +
  geom_smooth(method = "lm") +
  ylim(0,NA) +
  theme

Cl_b
