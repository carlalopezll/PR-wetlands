library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(ggsignif)
library(tidyverse)
library(broom)

# Read in conductivity and GHG data

cond <- read_csv("incubations/data/Conductivities.csv")

inc_summer <- read.csv("incubations/data/GHG production rates_summer.csv")
inc_winter <- read.csv("incubations/data/GHG production rates_winter.csv")

# Add 'period' column
inc_summer$period <- "summer"
inc_winter$period <- "winter"

# Combine the datasets
inc <- rbind(inc_summer, inc_winter)

# Reorder sites
inc$site <- factor(inc$site, levels = c("TO", "PA"))

inc$timepoint <-as.factor(inc$timepoint)

# Merge datasets
inc <- left_join(inc, cond, by = c("sample_name" = "sample", "period" = "period"))

# Save merged dataset

write.csv(inc, "incubations/data/merged incubations.csv", row.names = FALSE)
