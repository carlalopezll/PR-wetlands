# Calculating site averages and plotting regressions

# calculating site avgs
site_avg <- merge %>%
  filter(!is.na(Site)) %>%
  group_by(Site) %>%
  mutate(Site = first(Site), 
            CO2_avg = mean(CO2_uM, na.rm = TRUE), 
            CH4_avg = mean(CH4_uM, na.rm = TRUE), 
            CO2_min = min(CO2_uM, na.rm = TRUE),
            CO2_sd = sd(CO2_uM),
            CO2_max = max(CO2_uM, na.rm = TRUE),
            CH4_sd = sd(CH4_uM),
            CH4_min = min(CH4_uM, na.rm = TRUE),
            CH4_max = max(CH4_uM, na.rm = TRUE))

ggplot(site_avg, aes(x = `LDI index`, y = CO2_avg, color = Site)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CO2_avg - CO2_sd, ymax = CO2_avg + CO2_sd), width = 0.1)

ggsave("CO2 site avg vs LDI.jpg")

ggplot(site_avg, aes(x = `LDI index`, y = CH4_avg, color = Site)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CH4_avg - CH4_sd, ymax = CH4_avg + CH4_sd), width = 0.1)

ggsave("CH4 site avg vs LDI.jpg")

ggplot(site_avg, aes(x=`LDI index`, y = CO2_avg)) +
  geom_point()

ggplot(site_avg, aes(x=`LDI index`, y = CH4_avg)) +
  geom_point()

ggplot(site_avg, aes(x=Area_ha, y = CO2_avg)) +
  geom_point()

ggplot(site_avg, aes(x=Area_ha, y = CH4_avg)) +
  geom_point()
