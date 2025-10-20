# Trying to quantify differences between summer and winter period
# Use total production at day 3 as the baseline for comparison
library(readr)
library(dplyr)
library(ggplot2)

rates <- read_csv("incubations/data/Incubation rates.csv")

rates$treatment_label <- factor(rates$treatment_label, levels = c("Background", "+1000 µS/cm", "+2000 µS/cm"))

# filtering out outliers
rates_CO2 <- rates %>%
  filter(outlier_flag_CO2 == FALSE)

rates_CH4 <- rates %>%
  filter(outlier_flag_CH4 == FALSE)

# Boxplots of total CO2 production by site, parsed by treatment
rates %>%
  filter(timepoint == 3) %>%
  ggplot(aes(x = site, y = CO2_total, fill = period)) +
  geom_boxplot(position = position_dodge()) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.75),
              color = "black", size = 2, alpha = 0.8) +
  labs(y = "Total CO2 production (mg CO2/g AFDM)",
       x = "Site") +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment_label)

rates %>%
  filter(timepoint == 3) %>%
  ggplot(aes(x = site, y = CO2_total, fill = period)) +
  geom_boxplot(position = position_dodge()) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.75),
              color = "black", size = 2, alpha = 0.8) +
  labs(y = "Total CO2 production (mg CO2/g AFDM)",
       x = "Site") +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment_label) +
  ylim(0,NA)

ggsave("CO2 production boxplots by treatment.jpg")

rates_CH4 %>%
  filter(timepoint == 3) %>%
  ggplot(aes(x = site, y = CH4_total, fill = period)) +
  geom_boxplot(position = position_dodge()) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.75),
              color = "black", size = 2, alpha = 0.8) +
  labs(y = "Total CH4 production (mg CH4/g AFDM)",
       x = "Site") +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment_label)

ggsave("CH4 production boxplots by treatment.jpg")


# Table
rates_bkgd <- rates %>%
  filter(timepoint == 3) %>%
  group_by(site, period, treatment) %>%
  summarise(CO2_prod = mean(CO2_total, na.rm = TRUE),
            CH4_prod = mean(CH4_total, na.rm = TRUE))

rates_bkgd_table <- rates %>%
  filter(timepoint == 3) %>%
  group_by(site, period) %>%
  summarise(
    CO2_prod = sprintf("%.2f (%.2f)", mean(CO2_total, na.rm = TRUE), sd(CO2_total, na.rm = TRUE)),
    CH4_prod = sprintf("%.2f (%.2f)", mean(CH4_total, na.rm = TRUE), sd(CH4_total, na.rm = TRUE)),
    .groups = "drop"
  )

write_csv(rates_bkgd, "Background rates.csv")


# ANOVAs

rates_bkgd <- rates %>%
  filter(treatment_label == "Background", timepoint == 3)

ggplot(rates_bkgd, aes(x = site, y = CO2_total, fill = period)) +
  geom_boxplot(position = position_dodge()) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.75),
              color = "black", size = 2, alpha = 0.8) +
  labs(y = "Total CO2 production (mg CO2/g AFDM)",
       x = "Site") +
  theme_minimal() +
  geom_hline(yintercept = 0)

ggsave("background CO2 production.jpg")

ggplot(rates_bkgd, aes(x = site, y = CH4_total, fill = period)) +
  geom_boxplot(position = position_dodge()) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.75),
              color = "black", size = 2, alpha = 0.8) +
  labs(y = "Total CH4 production (mg CH4/g AFDM)",
       x = "Site") +
  theme_minimal() +
  geom_hline(yintercept = 0)

ggsave("background CH4 production.jpg")


anova_CO2 <- aov(CO2_total ~ site * period, data = rates_bkgd)
summary(anova_CO2)

anova_CH4 <- aov(CH4_total ~ site * period, data = rates_bkgd)
summary(anova_CH4)
