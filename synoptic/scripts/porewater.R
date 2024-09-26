# Soil porewater

library(tidyr)
library(dplyr)

soilpw <- read.csv("Data/Soil porewater.csv")

summary <- left_join(soilpw, ghg_pr3, by = "Site")

# Filter sites I want

soilpw2 <- filter(soilpw, Wetland == "Palmas" & Wetland == "Loiza")

ggplot(soilpw, aes(x= Condition, y= TOC)) +
  geom_boxplot() +
  geom_jitter()
