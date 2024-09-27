library(ggplot2)
library(dplyr)
library(readr)

# Read in conductivity and GHG data

cond <- read_csv("incubations/data/Conductivities.csv")
inc <- read.csv("incubations/data/Incubations_GHG.csv")
inc$timepoint <-as.factor(inc$timepoint)

# Merge datasets
inc <- left_join(inc, cond, by = c("sample_name" = "Sample"))

# Averaging the 3 reps

inc_avg <- inc %>%
  group_by(site, treatment, timepoint) %>%
  mutate(
    CO2_mean = mean(CO2_ppm),
    CH4_mean = mean(CH4_ppm))

ggplot(inc, aes(x= SpC_us_cm, y = SpC_us_cm, color = Site, label = sample_name)) +
  geom_point() +
  xlim(0, 3500) + ylim(0,3500) +
  geom_label()

# Keep just incubation data

PA <- filter(inc, site == "PA")
TO <- filter(inc, site == "TO")

ggplot(PA, aes(x=timepoint, y= CO2_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  ggtitle("Palmas")

ggsave("incubations/output/Palmas CO2_clean.jpg")

ggplot(PA, aes(x=timepoint, y= CO2_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Palmas")

ggsave("Palmas_CO2.jpg")

ggplot(PA, aes(x=timepoint, y= CH4_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  ggtitle("Palmas")

ggsave("incubations/output/Palmas CH4_clean.jpg")

ggplot(PA, aes(x=timepoint, y= CH4_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Palmas")

ggsave("Palmas_CH4.jpg")


ggplot(TO, aes(x=timepoint, y= CO2_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  ggtitle("Tortuguero")

ggsave("Tortuguero_CO2 clean.jpg")

ggplot(TO, aes(x=timepoint, y= CO2_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Tortuguero")

ggsave("Tortuguero_CO2.jpg")

ggplot(TO, aes(x=timepoint, y= CH4_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  ggtitle("Tortuguero")

ggsave("Tortuguero_CH4 clean.jpg")

ggplot(TO, aes(x=timepoint, y= CH4_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Tortuguero")

ggsave("Tortuguero_CH4.jpg")

ggplot(inc_avg, aes(x= SpC_us_cm, y= CO2_mean, color = site, shape = timepoint)) +
  geom_point(size = 5) +
  geom_line()

ggsave("incubations/output/CO2 avg vs SpC.jpg")

ggplot(inc_avg, aes(x= SpC_us_cm, y= CH4_mean, color = site, shape = timepoint)) +
  geom_point(size = 5) +
  geom_line() +
  scale_y_log10()

ggsave("incubations/output/CH4 avg vs SpC.jpg")

# Creating production column

inc_avg <- inc %>%
  group_by(site, timepoint) %>%
  mutate(
    CO2_production = 
  )

inc$CO2_production