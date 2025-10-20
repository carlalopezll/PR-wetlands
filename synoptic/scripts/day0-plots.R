inc <- read_csv("incubations/data/Incubation rates.csv")

# plot day 0 data

inc %>%
  filter(timepoint == 0) %>%
  ggplot(aes(x=treatment, y = wCO2_gL_med, color = period)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  facet_grid(period~site)

inc %>%
  filter(timepoint == 0) %>%
  ggplot(aes(x=treatment, y = wCH4_mgL_med, color = period)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  facet_grid(period~site)

inc %>%
  filter(timepoint == 0) %>%
  ggplot(aes(x=treatment, y = DOC_corrected, color = period)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  facet_grid(period~site)

inc %>%
  filter(timepoint == 0) %>%
  ggplot(aes(x=treatment, y = TDN_corrected, color = period)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  facet_grid(period~site)

inc %>%
  filter(period == "summer") %>%
  ggplot(aes(x=treatment, y = DOC_corrected)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  facet_grid(site~timepoint, scales = "free")