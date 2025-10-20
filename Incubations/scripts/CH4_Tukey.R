letters_by_group <- rates %>%
  mutate(group = interaction(treatment_label, timepoint)) %>%
  group_by(site, period) %>%
  group_modify(~ {
    dat <- .x
    mod <- aov(CH4_total ~ group, data = dat)
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
  rates,
  letters_by_group,
  by = c("site", "period", "treatment_label", "timepoint")
)

label_positions <- rates_with_letters %>%
  group_by(site, period, treatment_label, timepoint, letters) %>%
  summarise(
    y_pos = max(CH4_total, na.rm = TRUE),
    .groups = "drop"
  )


rates_with_letters$treatment_label <- factor(rates_with_letters$treatment_label, levels = c("Background", "+1000 µS/cm", "+2000 µS/cm"))

scale_fill_viridis_d(option = "C", direction = 1)
                         
ggplot(rates_with_letters, aes(x = timepoint, y = CH4_total, fill = treatment_label)) +
  geom_boxplot(aes(group = interaction(timepoint, treatment_label)), na.rm = TRUE) +
  geom_jitter(aes(group = interaction(timepoint, treatment_label)), 
              position = position_dodge(width = 0.9), alpha = 0.6) +
  geom_text(data = label_positions,
            aes(label = letters, y = y_pos + 0.05),  # small bump above box
            position = position_dodge(width = 0.9),
            vjust = 0, size = 4, color = "black") +
  labs(y = "Total CH4 production (g CO2/g AFDM)") +
  scale_fill_viridis_d(option = "C", direction = -1) +
  facet_grid(site ~ period, scales = "free") +
  geom_hline(yintercept = 0)
