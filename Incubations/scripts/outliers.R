
rates <- read_csv("incubations/data/Incubation rates.csv")


# flagged plots

rates %>%
  filter(!timepoint == 0) %>%
  ggplot(aes(x=spc_us_cm, y = CO2_total, color = outlier_flag_CO2, shape = site)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  facet_grid(period~timepoint)

rates %>%
  filter(!timepoint == 0) %>%
  ggplot(aes(x=spc_us_cm, y = CH4_total, color = outlier_flag_CH4, shape = site)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  facet_grid(period~timepoint) 

rates %>%
  filter(timepoint == 3) %>%
  ggplot(aes(x = spc_us_cm, y = CO2_total, color = outlier_flag_CO2, shape = site)) +
  geom_point() +
  facet_wrap(~period) +
  geom_hline(yintercept = 0)

gggsave("Flagged CO2 values.jpg")

rates %>%
  filter(timepoint == 3) %>%
  ggplot(aes(x = spc_us_cm, y = CH4_total, color = outlier_flag_CH4, shape = site)) +
  geom_point() +
  facet_wrap(~period) +
  geom_hline(yintercept = 0)

ggsave("Flagged CH4 values")
