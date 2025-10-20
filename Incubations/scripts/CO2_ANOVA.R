library(dplyr)
library(emmeans)
library(multcompView)
library(readr)

rates <- read_csv("incubations/data/Incubation rates.csv")

# Filter your background data
rates_bkgd <- rates %>%
  filter(treatment_label == "Background", timepoint == 3)

# 1. Run ANOVA
anova_CO2 <- aov(CO2_total ~ site * period, data = rates_bkgd)

# 2. Estimated marginal means
emm <- emmeans(anova_CO2, ~ site * period)

# 3. Tukey pairwise comparisons
tukey <- multcomp::cld(emm, Letters = letters, adjust = "tukey")

# 4. Compute mean and SD from raw data
mean_sd_table <- rates_bkgd %>%
  group_by(site, period) %>%
  summarise(
    CO2_mean = mean(CO2_total, na.rm = TRUE),
    CO2_sd = sd(CO2_total, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Combine with letters from Tukey
final_table <- mean_sd_table %>%
  left_join(tukey %>%
              as.data.frame() %>%
              dplyr::select(site, period, .group), 
            by = c("site", "period")) %>%
  mutate(label = paste0(round(CO2_mean, 2), " ± ", round(CO2_sd, 2), .group)) %>%
  dplyr::select(site, period, label)

write_csv(final_table, "CO2 production ANOVA.csv")
