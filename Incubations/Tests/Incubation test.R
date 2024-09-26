library(ggplot2)
library(dplyr)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/")

inc <- read.csv("Incubation test.csv")

inc <- read.csv("GC incubations_240627.csv")

str(inc)
inc$Timepoint <-as.factor(inc$Timepoint)

# Keep just incubation data

inc <- inc %>% 
  filter(!is.na(Timepoint))

water <- filter(inc, Treatment == "Water")
water_sed <- filter(inc, Treatment == "Water+sediment")

ggplot(inc, aes(x=Timepoint, y= CH4_ppm, color = Treatment)) +
  geom_boxplot() +
  geom_point()

ggsave("incubation CH4.jpg")

ggplot(water, aes(x=Timepoint, y= CH4_ppm)) +
  geom_boxplot() +
  geom_point() +
  ggtitle("Water")

ggsave("incubation CH4_water.jpg")

ggplot(water_sed, aes(x=Timepoint, y= CH4_ppm)) +
  geom_boxplot() +
  geom_point() +
  ggtitle("Water+sediment")

ggsave("incubation CH4_water_sed.jpg")

ggplot(inc, aes(x=Timepoint, y= CO2_ppm, color = Treatment)) +
  geom_boxplot() +
  geom_point()

ggsave("incubation CO2.jpg")

ggplot(water, aes(x=Timepoint, y= CO2_ppm)) +
  geom_boxplot() +
  geom_point() +
  ggtitle("Water")

ggsave("incubation CO2_water.jpg")

ggplot(water_sed, aes(x=Timepoint, y= CO2_ppm)) +
  geom_boxplot() +
  geom_point() +
  ggtitle("Water+sediment")

ggsave("incubation CO2_water_sed.jpg")

# Creating production column

inc$CO2_production <- 
  
# production <- inc %>%
#   group_by(Timepoint, Treatment) %>%
#   mutate(CO2_mean = mean(CO2_ppm)) %>%
#   arrange(Timepoint) %>%  # Ensure the data is ordered by time
#   mutate(difference = CO2_mean - lag(CO2_mean))
# 
# 
# ggplot(inc, aes(x=Timepoint, y= CO2_ppm, color = Treatment)) +
#   geom_boxplot() +
#   geom_point()

