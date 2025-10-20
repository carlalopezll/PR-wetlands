library(dplyr)
library(broom)
library(ggplot2)

# ------------------ CO2 ------------------ #
# Fit linear models safely
lm_CO2 <- rates %>%
  filter(timepoint != 0) %>%
  group_by(site, timepoint, period) %>%
  group_modify(~ {
    dat <- na.omit(.x[, c("CO2_rate_day", "spc_us_cm")])
    if (nrow(dat) >= 2 && length(unique(dat$spc_us_cm)) > 1) {
      tidy(lm(CO2_rate_day ~ spc_us_cm, data = dat))
    } else {
      tibble(term = NA, estimate = NA, std.error = NA,
             statistic = NA, p.value = NA)
    }
  }) %>%
  ungroup()

# Extract slope stats and significance
lm_CO2_summary <- lm_CO2 %>%
  filter(term == "spc_us_cm") %>%
  mutate(sig = ifelse(!is.na(p.value) & p.value < 0.1, "solid", "dashed"))

# Prepare fitted lines
fits_CO2 <- rates %>%
  filter(timepoint != 0) %>%
  group_by(site, timepoint, period) %>%
  group_modify(~ {
    dat <- na.omit(.x[, c("CO2_rate_day", "spc_us_cm")])
    if (nrow(dat) >= 2 && length(unique(dat$spc_us_cm)) > 1) {
      model <- lm(CO2_rate_day ~ spc_us_cm, data = dat)
      data.frame(spc_us_cm = seq(min(dat$spc_us_cm), max(dat$spc_us_cm), length.out = 100),
                 fit = predict(model, newdata = data.frame(spc_us_cm = seq(min(dat$spc_us_cm), max(dat$spc_us_cm), length.out = 100))))
    } else {
      data.frame(spc_us_cm = NA, fit = NA)
    }
  }) %>%
  left_join(lm_CO2_summary, by = c("site", "timepoint", "period"))

# Plot CO2
p1 <- rates_CO2 %>%
  filter(timepoint != 0) %>%
  ggplot(aes(x = spc_us_cm, y = CO2_rate_day, color = period)) +
  geom_point() +
  geom_line(data = fits_CO2, aes(y = fit, linetype = sig), linewidth = 1) +
  geom_hline(yintercept = 0) +
  facet_grid(site ~ timepoint) +
  labs(x = "Specific conductance (µS/cm)", y = CO2_rate_lab) +
  scale_linetype_identity() +
  theme

p1

# ------------------ CH4 ------------------ #
lm_CH4 <- rates %>%
  filter(timepoint != 0) %>%
  group_by(site, timepoint, period) %>%
  group_modify(~ {
    dat <- na.omit(.x[, c("CH4_rate_day", "spc_us_cm")])
    if (nrow(dat) >= 2 && length(unique(dat$spc_us_cm)) > 1) {
      tidy(lm(CH4_rate_day ~ spc_us_cm, data = dat))
    } else {
      tibble(term = NA, estimate = NA, std.error = NA,
             statistic = NA, p.value = NA)
    }
  }) %>%
  ungroup()

lm_CH4_summary <- lm_CH4 %>%
  filter(term == "spc_us_cm") %>%
  mutate(sig = ifelse(!is.na(p.value) & p.value < 0.1, "solid", "dashed"))

fits_CH4 <- rates %>%
  filter(timepoint != 0) %>%
  group_by(site, timepoint, period) %>%
  group_modify(~ {
    dat <- na.omit(.x[, c("CH4_rate_day", "spc_us_cm")])
    if (nrow(dat) >= 2 && length(unique(dat$spc_us_cm)) > 1) {
      model <- lm(CH4_rate_day ~ spc_us_cm, data = dat)
      data.frame(spc_us_cm = seq(min(dat$spc_us_cm), max(dat$spc_us_cm), length.out = 100),
                 fit = predict(model, newdata = data.frame(spc_us_cm = seq(min(dat$spc_us_cm), max(dat$spc_us_cm), length.out = 100))))
    } else {
      data.frame(spc_us_cm = NA, fit = NA)
    }
  }) %>%
  left_join(lm_CH4_summary, by = c("site", "timepoint", "period"))

# Plot CH4
p2 <- rates %>%
  filter(timepoint != 0) %>%
  ggplot(aes(x = spc_us_cm, y = CH4_rate_day, color = period)) +
  geom_point() +
  geom_line(data = fits_CH4, aes(y = fit, linetype = sig), size = 1) +
  geom_hline(yintercept = 0) +
  facet_grid(site ~ timepoint, scales = "free") +
  labs(x = "Specific conductance (µS/cm)", y = CH4_rate_lab) +
  scale_linetype_identity() +
  theme

# Display plots
p1
p2

(p1 / p2) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave("GHG vs Spc_all days_w lines.jpg", width = 8, height= 10, dpi = 300)
