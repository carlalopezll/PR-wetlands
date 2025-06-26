# Merging GHG data with the conductivity data for the PR incubations
# Carla López Lloreda

# load libratires
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(ggsignif)
library(tidyverse)
library(broom)

# read in conductivity
cond <- read_csv("incubations/data/Conductivities.csv")

# read in production rates
inc_summer <- read.csv("incubations/data/GHG production rates_summer.csv")
inc_winter <- read.csv("incubations/data/GHG production rates_winter.csv")

# add period column
inc_summer$period <- "summer"
inc_winter$period <- "winter"

# combine the datasets
inc <- rbind(inc_summer, inc_winter)

# reorder sites
inc$site <- factor(inc$site, levels = c("TO", "PA"))

# make timepoint as factor
inc$timepoint <-as.factor(inc$timepoint)

# merge datasets
inc <- left_join(inc, cond, by = c("sample_name" = "sample", "period" = "period"))

# Save merged dataset
write.csv(inc, "incubations/data/merged incubations.csv", row.names = FALSE)
