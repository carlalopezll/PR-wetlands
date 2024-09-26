library(ggplot2)
library(dplyr)
library(readr)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Humedales Puerto Rico/Incubations")

# Read in conductivity and GHG data

cond <- read_csv("Conductivities.csv")
inc <- read.csv("Incubations_GHG.csv")
inc$timepoint <-as.factor(inc$timepoint)

# Merge datasets
inc <- left_join(inc, cond, by = c("sample_name" = "Sample"))

ggplot(cond, aes(x= SpC_us_cm, y = SpC_us_cm, color = Site, label = Sample)) +
  geom_point() +
  xlim(0, 3500) + ylim(0,3500) +
  geom_label()

# Keep just incubation data

PA <- filter(inc, site == "PA")
TO <- filter(inc, site == "TO")

ggplot(PA, aes(x=timepoint, y= CO2_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Palmas")

ggsave("Palmas_CO2.jpg")


ggplot(PA, aes(x=timepoint, y= CH4_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Palmas")

ggsave("Palmas_CH4.jpg")


ggplot(TO, aes(x=timepoint, y= CO2_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Tortuguero")

ggsave("Tortuguero_CO2.jpg")

ggplot(TO, aes(x=timepoint, y= CH4_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Tortuguero")

ggsave("Tortuguero_CH4.jpg")

# Averaging the 3 reps

inc_avg <- inc %>%
  group_by(site, timepoint) %>%
  mutate(
    CO2_mean = mean(CO2_ppm),
    CH4_mean = mean(CH4_ppm))

ggplot(inc_avg, aes(x= SpC_us_cm, y= CO2_mean, color = site, shape = timepoint)) +
  geom_point(size = 5)

ggplot(inc_avg, aes(x= SpC_us_cm, y= CH4_mean, color = site, shape = timepoint)) +
  geom_point(size = 5)

# Creating production column

inc_avg <- inc %>%
  group_by(site, timepoint) %>%
  mutate(
    CO2_production = 
  )

inc$CO2_production