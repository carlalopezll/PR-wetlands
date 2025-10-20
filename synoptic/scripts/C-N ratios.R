# Calculating C:N ratios

# Load libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)

theme_set(theme_classic())

# read in averaged data
merge <- read_csv("synoptic/data/synoptic merged.csv")

summary(merge$DOC_avg)
summary(merge$TDN_avg)

merge$C_N <- merge$DOC_avg / merge$TDN_avg

summary(merge$C_N)

ggplot(merge, aes(x= Site, y = C_N)) +
  geom_point()

ggplot(merge, aes(x= NO3_N_avg, y = TDN_avg, color = Site)) +
  geom_point() +
  scale_x_log10()

ggplot(merge, aes(x= DOC_avg, y = TDN_avg, color = Site)) +
  geom_point()

ggplot(merge, aes(x= TDN_avg, y = C_N, color = Site)) +
  geom_point()

