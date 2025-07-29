# GHG plots with the full data set (no rep averaging)

# read in full GHG dataset
ghg <- read_csv("synoptic/data/PR synoptic_GHG.csv")

ggplot(ghg, aes(x = Site, y = wCO2_uM_med, color = Quadrant)) +
  geom_boxplot() +
  geom_jitter()

# plot within wetland-variability
ghg_het <- ghg %>%
  filter(!is.na(Quadrant)) %>%
  mutate(SiteQuad = paste(Site, Quadrant, sep = "-"))

ggplot(ghg_het, aes(x = SiteQuad, y = wCO2_uM_med, fill = as.factor(Date))) +
  geom_boxplot() +
  geom_jitter(width = 0)

ggplot(ghg_het, aes(x = SiteQuad, y = wCH4_uM_med, fill = as.factor(Date))) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  scale_y_log10()

ggplot(ghg_het, aes(x = Site, y = wCO2_uM_med, fill = Quadrant)) +
  geom_boxplot()

ggplot(ghg_het, aes(x = Site, y = wCH4_uM_med, fill = Quadrant)) +
  geom_boxplot() +
  scale_y_log10()
