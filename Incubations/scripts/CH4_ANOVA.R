library(dplyr)
library(emmeans)
library(multcompView)

# Filter your background data
rates_bkgd <- rates %>%
  filter(treatment_label == "Background", timepoint == 3)

# 1. Run ANOVA
anova_CH4 <- aov(CH4_total ~ site * period, data = rates_bkgd)

# 2. Estimated marginal means
emm <- emmeans(anova_CH4, ~ site * period)

# 3. Tukey pairwise comparisons
tukey <- multcomp::cld(emm, Letters = letters, adjust = "tukey")

# 4. Compute mean and SD from raw data
mean_sd_table <- rates_bkgd %>%
  group_by(site, period) %>%
  summarise(
    CH4_mean = mean(CH4_total, na.rm = TRUE),
    CH4_sd = sd(CH4_total, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Combine with letters from Tukey
final_table <- mean_sd_table %>%
  left_join(tukey %>%
              as.data.frame() %>%
              select(site, period, .group), 
            by = c("site", "period")) %>%
  mutate(label = paste0(round(CH4_mean, 2), " ± ", round(CH4_sd, 2), .group)) %>%
  select(site, period, label)

write_csv(final_table, "CH4 production ANOVA.csv")

