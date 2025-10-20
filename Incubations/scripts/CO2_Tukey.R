library(dplyr)
library(emmeans)
library(multcompView)
library(readr)

rates <- read_csv("incubations/data/Incubation rates.csv")

rates <- rates %>%
  mutate(
    timepoint = as.factor(timepoint),
    treatment_label = as.factor(treatment_label)
  )

# filtering out outliers
rates_CO2 <- rates %>%
  filter(outlier_flag_CO2 == FALSE)

letters_by_group <- rates_CO2 %>%
  mutate(group = interaction(treatment_label, timepoint)) %>%
  group_by(site, period) %>%
  group_modify(~ {
    dat <- .x
    mod <- aov(CO2_total ~ group, data = dat)
    tukey <- TukeyHSD(mod)
    
    # Extract p-values and convert to named vector
    tukey_df <- as.data.frame(tukey$group)
    pvals <- tukey_df$`p adj`
    names(pvals) <- rownames(tukey_df)
    
    # Get letters
    cld <- multcompLetters(pvals)$Letters
    tibble(
      group = names(cld),
      letters = cld
    ) %>%
      separate(group, into = c("treatment_label", "timepoint"), sep = "\\.", convert = TRUE)
  }) %>%
  ungroup()

rates <- rates %>%
  mutate(timepoint = as.character(timepoint))

letters_by_group <- letters_by_group %>%
  mutate(timepoint = as.character(timepoint))

rates_with_letters <- left_join(
  rates_CO2,
  letters_by_group,
  by = c("site", "period", "treatment_label", "timepoint")
)

ggplot(rates_with_letters, aes(x = timepoint, y = CO2_total, fill = treatment_label)) +
  geom_boxplot(aes(group = interaction(timepoint, treatment_label)), na.rm = TRUE) +
  geom_jitter(aes(group = interaction(timepoint, treatment_label)), 
              position = position_dodge(width = 0.9), alpha = 0.6) +
  geom_text(aes(label = letters), 
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5, na.rm = TRUE) +
  labs(y = "Total CO2 production (g CO2/g AFDM)") +
  facet_grid(site ~ period, scales = "free") +
  geom_hline(yintercept = 0)
