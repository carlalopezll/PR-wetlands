DOC_rate_lab <- "DOC rate (mg DOC/g AFDM/day)"
TDN_rate_lab <- "TDN rate (mg TDN/g AFDM/day)"

# ------------------ DOC ------------------ #
lm_DOC <- rates %>%
  filter(timepoint != 0) %>%
  group_by(site, timepoint, period) %>%
  group_modify(~ {
    dat <- na.omit(.x[, c("DOC_rate_day", "spc_us_cm")])
    if (nrow(dat) >= 2 && length(unique(dat$spc_us_cm)) > 1) {
      tidy(lm(DOC_rate_day ~ spc_us_cm, data = dat))
    } else {
      tibble(term = NA, estimate = NA, std.error = NA,
             statistic = NA, p.value = NA)
    }
  }) %>%
  ungroup()

lm_DOC_summary <- lm_DOC %>%
  filter(term == "spc_us_cm") %>%
  mutate(sig = ifelse(!is.na(p.value) & p.value < 0.1, "solid", "dashed"))

fits_DOC <- rates %>%
  filter(timepoint != 0) %>%
  group_by(site, timepoint, period) %>%
  group_modify(~ {
    dat <- na.omit(.x[, c("DOC_rate_day", "spc_us_cm")])
    if (nrow(dat) >= 2 && length(unique(dat$spc_us_cm)) > 1) {
      model <- lm(DOC_rate_day ~ spc_us_cm, data = dat)
      data.frame(spc_us_cm = seq(min(dat$spc_us_cm), max(dat$spc_us_cm), length.out = 100),
                 fit = predict(model, newdata = data.frame(spc_us_cm = seq(min(dat$spc_us_cm), max(dat$spc_us_cm), length.out = 100))))
    } else {
      data.frame(spc_us_cm = NA, fit = NA)
    }
  }) %>%
  left_join(lm_DOC_summary, by = c("site", "timepoint", "period"))

p_DOC <- rates %>%
  filter(timepoint != 0) %>%
  ggplot(aes(x = spc_us_cm, y = DOC_rate_day, color = period)) +
  geom_point() +
  geom_line(data = fits_DOC, aes(y = fit, linetype = sig), linewidth = 1) +
  geom_hline(yintercept = 0) +
  facet_grid(site ~ timepoint) +
  labs(x = "Specific conductance (µS/cm)", y = DOC_rate_lab) +
  scale_linetype_identity() +
  theme

# ------------------ TDN ------------------ #
lm_TDN <- rates %>%
  filter(timepoint != 0) %>%
  group_by(site, timepoint, period) %>%
  group_modify(~ {
    dat <- na.omit(.x[, c("TDN_rate_day", "spc_us_cm")])
    if (nrow(dat) >= 2 && length(unique(dat$spc_us_cm)) > 1) {
      tidy(lm(TDN_rate_day ~ spc_us_cm, data = dat))
    } else {
      tibble(term = NA, estimate = NA, std.error = NA,
             statistic = NA, p.value = NA)
    }
  }) %>%
  ungroup()

lm_TDN_summary <- lm_TDN %>%
  filter(term == "spc_us_cm") %>%
  mutate(sig = ifelse(!is.na(p.value) & p.value < 0.1, "solid", "dashed"))

fits_TDN <- rates %>%
  filter(timepoint != 0) %>%
  group_by(site, timepoint, period) %>%
  group_modify(~ {
    dat <- na.omit(.x[, c("TDN_rate_day", "spc_us_cm")])
    if (nrow(dat) >= 2 && length(unique(dat$spc_us_cm)) > 1) {
      model <- lm(TDN_rate_day ~ spc_us_cm, data = dat)
      data.frame(spc_us_cm = seq(min(dat$spc_us_cm), max(dat$spc_us_cm), length.out = 100),
                 fit = predict(model, newdata = data.frame(spc_us_cm = seq(min(dat$spc_us_cm), max(dat$spc_us_cm), length.out = 100))))
    } else {
      data.frame(spc_us_cm = NA, fit = NA)
    }
  }) %>%
  left_join(lm_TDN_summary, by = c("site", "timepoint", "period"))

p_TDN <- rates %>%
  filter(timepoint != 0) %>%
  ggplot(aes(x = spc_us_cm, y = TDN_rate_day, color = period)) +
  geom_point() +
  geom_line(data = fits_TDN, aes(y = fit, linetype = sig), linewidth = 1) +
  geom_hline(yintercept = 0) +
  facet_grid(site ~ timepoint) +
  labs(x = "Specific conductance (µS/cm)", y = TDN_rate_lab) +
  scale_linetype_identity() +
  theme

# ------------------ Combine all plots ------------------ #
(p_DOC / p_TDN) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("DOC_TDN_vs_Spc_all_days_w_lines.jpg", width = 8, height = 10, dpi = 300)
