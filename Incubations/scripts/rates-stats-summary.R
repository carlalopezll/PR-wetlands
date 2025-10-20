### Get means ###

rates <- read_csv("incubations/data/Incubation rates.csv")

# Helper function for standard error
se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

# summarise average and standard errors for plots

summary_rates <- rates %>%
  group_by(site, treatment, timepoint, period) %>%
  summarise(
    CH4_total_mean = mean(CH4_total, na.rm = TRUE),
    CH4_total_se   = sd(CH4_total, na.rm = TRUE) / sqrt(n()),
    CO2_total_mean = mean(CO2_total, na.rm = TRUE),
    CO2_total_se   = sd(CO2_total, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# write_csv(means, "incubations/data/incubation averages.csv")