library(dplyr)
library(multcompView)


rates <- read_csv("incubations/data/Incubation rates.csv")

rates$treatment_label <- factor(rates$treatment_label, levels = c("Background", "+1000 µS/cm", "+2000 µS/cm"))

# filtering out outliers
rates_CO2 <- rates %>%
  filter(CO2_total > 0)

# 1. Filter for background treatment only
bg_data <- rates_CO2 %>%
  filter(treatment_label == "Background",
         timepoint == 3) # since your plot filters timepoint 3

# 2. Run ANOVA
anova_model <- aov(CO2_total ~ site * period, data = bg_data)
summary(anova_model)

# 3. Tukey HSD
tukey_res <- TukeyHSD(anova_model)

# 4. Get letters and turn into a data frame
letters_df <- multcompLetters4(anova_model, tukey_res)
letters_df <- as.data.frame.list(letters_df$`site:period`)
letters_df <- tibble(
  site_period = rownames(letters_df),
  tukey_letter = letters_df$Letters
) %>%
  tidyr::separate(site_period, into = c("site", "period"), sep = ":")

# 5. Summary stats
summary_table <- bg_data %>%
  group_by(site, period) %>%
  summarise(
    mean_CO2 = mean(CO2_total, na.rm = TRUE),
    sd_CO2 = sd(CO2_total, na.rm = TRUE),
    n = sum(!is.na(CO2_total)),
    .groups = "drop"
  ) %>%
  left_join(letters_df, by = c("site", "period")) %>%
  mutate(
    label = paste0(
      round(mean_CO2, 2), " ± ",
      round(sd_CO2, 2),
      tukey_letter,
      " (n=", n, ")"
    )
  ) %>%
  select(site, period, label)

summary_table

writexl::write_xlsx(summary_table, "incubations/CO2 Tukey.xlsx")
