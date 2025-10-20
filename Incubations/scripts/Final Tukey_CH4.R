library(dplyr)
library(multcompView)


rates <- read_csv("incubations/data/Incubation rates.csv")

rates$treatment_label <- factor(rates$treatment_label, levels = c("Background", "+1000 µS/cm", "+2000 µS/cm"))

# filtering out outliers
rates_CH4 <- rates

# 1. Filter for background treatment only
bg_data <- rates_CH4 %>%
  filter(treatment_label == "Background",
         timepoint == 3) # since your plot filters timepoint 3

# 2. Run ANOVA
anova_model <- aov(CH4_total ~ site * period, data = bg_data)
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
    mean_CH4 = mean(CH4_total, na.rm = TRUE),
    sd_CH4 = sd(CH4_total, na.rm = TRUE),
    n = sum(!is.na(CH4_total)),
    .groups = "drop"
  ) %>%
  left_join(letters_df, by = c("site", "period")) %>%
  mutate(
    label = paste0(
      round(mean_CH4, 2), " ± ",
      round(sd_CH4, 2),
      tukey_letter,
      " (n=", n, ")"
    )
  ) %>%
  select(site, period, label)

summary_table

writexl::write_xlsx(summary_table, "incubations/CH4 Tukey.xlsx")
