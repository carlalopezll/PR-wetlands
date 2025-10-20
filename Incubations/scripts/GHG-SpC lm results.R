library(dplyr)
library(broom)

# Function to fit lm and extract stats
extract_lm_stats <- function(data, formula) {
  model <- lm(formula, data = data)
  tidy_stats <- broom::tidy(model)         # estimates, std error, p-values
  glance_stats <- broom::glance(model)     # r-squared, F-statistic, etc.
  
  tibble(
    estimate = tidy_stats$estimate[2],     # slope for spc_us_cm
    std_error = tidy_stats$std.error[2],
    p_value = tidy_stats$p.value[2],
    f_statistic = glance_stats$statistic,
    r_squared = glance_stats$r.squared
  )
}

# CO2 models
table_CO2 <- rates %>%
  filter(timepoint != 0, !is.na(spc_us_cm), !is.na(CO2_rate_day)) %>%
  group_by(site, period, timepoint) %>%
  filter(n() > 1) %>%   # only keep groups with at least 2 observations
  do(extract_lm_stats(., CO2_rate_day ~ spc_us_cm)) %>%
  ungroup()

# CH4 models
table_CH4 <- rates %>%
  filter(timepoint != 0, !is.na(spc_us_cm), !is.na(CH4_rate_day)) %>%
  group_by(site, period, timepoint) %>%
  do(extract_lm_stats(., CH4_rate_day ~ spc_us_cm)) %>%
  ungroup()

write.csv(table_CO2, "CO2 lm coefficients.csv", row.names = F)
write.csv(table_CH4, "CH4 lm coefficients.csv", row.names = F)
