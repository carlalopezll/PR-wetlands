library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(ggsignif)
library(tidyverse)
library(broom)
library(patchwork)

rates <- read_csv("incubations/data/Incubation rates.csv")

# Recode seasons and sites
rates <- rates %>%
  mutate(
    period = recode(period, "winter" = "Winter", "summer" = "Summer"),
    site = recode(site, "PA" = "Palmas", "TO" = "Tortuguero"),
    treatment_label = factor(treatment_label, levels = c("Background", "+1000 µS/cm", "+2000 µS/cm"))
  )

# filtering out outliers
rates_CO2 <- rates %>%
  filter(outlier_flag_CO2 == FALSE)

rates_CH4 <- rates %>%
  filter(outlier_flag_CH4 == FALSE)

# theme for plots
theme <- theme_bw() +
  theme(
    text = element_text(size = 16),
    legend.title = element_blank()
  )

# axis labels
CO2_total_lab <- expression(paste("Total ", C, O[2], " production (mg CO" [2], "/g AFDM)"))
CH4_total_lab <- expression(paste("Total ", C, H[4], " production (mg CH" [4], "/g AFDM)"))

CO2_rate_lab <- expression(paste(C, O[2], " rate (mg CO" [2], "/g AFDM/day)"))
CH4_rate_lab <- expression(paste(C, H[4], " rate (mg CH" [4], "/g AFDM/day)"))

CO2_conc_lab <- expression(paste(C, O[2], " (g/L)"))
CH4_conc_lab <- expression(paste(C, H[4], " (mg/L)"))

# Boxplots of CO2 concentrations over incubation period
a <- ggplot(rates, aes(x=timepoint, y= wCO2_gL_med, fill = treatment_label)) +
  geom_boxplot(aes(group = interaction(timepoint, treatment_label), na.rm = TRUE)) +
  geom_jitter(aes(group = interaction(timepoint, treatment_label)), position = position_dodge(width=0.9), alpha = 0.6) +
  facet_grid(site ~ period, scales = "free") +
  geom_hline(yintercept = 0) +
  labs(x = "Timepoint", y = CO2_conc_lab) +
  theme

# Boxplots of CH4 concentrations over incubation period
b <- ggplot(rates, aes(x=timepoint, y= wCH4_mgL_med, fill = treatment_label)) +
  geom_boxplot(aes(group = interaction(timepoint, treatment_label), na.rm = TRUE)) +
  geom_jitter(aes(group = interaction(timepoint, treatment_label)), position = position_dodge(width=0.9), alpha = 0.6) +
  facet_grid(site ~ period, scales = "free") +
  geom_hline(yintercept = 0) +
  labs(x = "Timepoint", y = CH4_conc_lab) +
  theme +
  scale_y_log10()

(a / b) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("GHG conc timeseries.jpg", width = 8, height= 12, dpi = 300)

# Total CO2 production over time
ggplot(rates_CO2, aes(x=timepoint, y= CO2_total, fill = treatment_label)) +
  geom_boxplot(aes(group = interaction(timepoint, treatment_label), na.rm = TRUE)) +
  geom_jitter(aes(group = interaction(timepoint, treatment_label)), position = position_dodge(width=0.9), alpha = 0.6) +
  labs(y = CO2_total_lab) +
  facet_grid(site ~ period, scales = "free") +
  geom_hline(yintercept = 0) +
  labs(y = CO2_total_lab) +
  theme

# CO2 production rates
ggplot(rates_CO2, aes(x=timepoint, y= CO2_rate_day, fill = treatment_label)) +
  geom_boxplot(aes(group = interaction(timepoint, treatment_label), na.rm = TRUE)) +
  geom_jitter(aes(group = interaction(timepoint, treatment_label)), position = position_dodge(width=0.9), alpha = 0.6) +
  labs(y = CO2_rate_lab) +
  facet_grid(site ~ period, scales = "free") +
  geom_hline(yintercept = 0) +
  theme

# Total CH4 production over time
ggplot(rates, aes(x=timepoint, y= CH4_total, fill = treatment_label)) +
  geom_boxplot(aes(group = interaction(timepoint, treatment_label), na.rm = TRUE)) +
  geom_jitter(aes(group = interaction(timepoint, treatment_label)), position = position_dodge(width=0.9), alpha = 0.6) +
  labs(y = CH4_total_lab) +
  facet_grid(site ~ period, scales = "free") +
  geom_hline(yintercept = 0)

# CH4 production rates
ggplot(rates, aes(x=timepoint, y= CH4_rate_day, fill = treatment_label)) +
  geom_boxplot(aes(group = interaction(timepoint, treatment_label), na.rm = TRUE)) +
  geom_jitter(aes(group = interaction(timepoint, treatment_label)), position = position_dodge(width=0.9), alpha = 0.6) +
  labs(y = CH4_rate_lab) +
  facet_grid(site ~ period, scales = "free") +
  geom_hline(yintercept = 0) +
  theme


# Total production against concentrations
# CO2
ggplot(rates, aes(x=wCO2_mgL_med, y= CO2_total, color = period)) +
  geom_point() +
  facet_wrap(~site, scales = "free")
# CH4
ggplot(rates, aes(x=wCH4_mgL_med, y= CH4_total, color = period)) +
  geom_point() +
  facet_wrap(~site, scales = "free")

# Total production and rates against conductivity (all timepoints)

p1 <- rates_CO2 %>%
  filter(!timepoint == 0) %>%
  ggplot(aes(x=spc_us_cm, y = CO2_rate_day, color = period)) +
  labs(x= "Specific conductance (uS/cm)", y=CO2_rate_lab) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0) +
  facet_grid(site~timepoint) +
  theme

p1

p2 <- rates_CH4 %>%
  filter(!timepoint == 0) %>%
  ggplot(aes(x=spc_us_cm, y = CH4_rate_day, color = period)) +
  labs(x= "Specific conductance (uS/cm)", y=CH4_rate_lab) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0) +
  facet_grid(site~timepoint, scales = "free") +
  theme

p2

(p1 / p2) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("GHG vs Spc_all days.jpg", width = 8, height= 10, dpi = 300)

rates %>%
  filter(!timepoint == 0) %>%
  ggplot(aes(x=spc_us_cm, y = DOC_total, color = period)) +
  geom_point() +
  labs(y= "Total DOC production (mg DOC/g AFDM)") +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0) +
  facet_grid(site~timepoint) +
  theme

ggsave("DOC vs Spc_all days.jpg")

rates %>%
  filter(!timepoint == 0) %>%
  ggplot(aes(x=spc_us_cm, y = TDN_total*1000, color = period)) +
  geom_point() +
  labs(y= "Total TDN production (ug TDN/g AFDM)") +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0) +
  facet_grid(site~timepoint) +
  theme

ggsave("TDN vs Spc_all days.jpg")

# LOESS models
# only for day 3 (final incubation day)

rates_CO2 %>%
  filter(timepoint == 3) %>%
  ggplot(aes(x = spc_us_cm, y = CO2_total, color = site)) +
  labs(y=CO2_total_lab) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~period) +
  geom_hline(yintercept = 0) +
  theme

ggsave("CO2 total vs Spc_day 3_loess.jpg")

rates_CH4 %>%
  filter(timepoint == 3) %>%
  ggplot(aes(x = spc_us_cm, y = CH4_total, color = site)) +
  labs(y=CH4_total_lab) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~period) +
  geom_hline(yintercept = 0)

ggsave("CH4 total vs Spc_day 3_loess.jpg")


# DOC concentrations over incubation period

ggplot(rates, aes(x=DOC_corrected, y=wCO2_gL_med)) +
  geom_point(aes(color = treatment_label)) +
  geom_smooth(method = "lm") +
  facet_wrap(~site, scales = "free")

ggsave("CO2 vs DOC_incubations.jpg")

ggplot(rates_CO2, aes(x=TDN_corrected, y=wCO2_gL_med)) +
  geom_point(aes(color = treatment_label)) +
  geom_smooth(method = "lm") +
  facet_wrap(~site, scales = "free")

ggsave("CO2 vs TDN_incubations.jpg")

ggplot(rates, aes(x=DOC_corrected, y=wCH4_mgL_med)) +
  geom_point(aes(color = treatment_label)) +
  geom_smooth(method = "lm") +
  facet_wrap(~site, scales = "free")

ggsave("CH4 vs DOC_incubations.jpg")

ggplot(rates, aes(x=TDN_corrected, y=wCH4_mgL_med)) +
  geom_point(aes(color = treatment_label)) +
  geom_smooth(method = "lm") +
  facet_wrap(~site, scales = "free")

ggsave("CH4 vs TDN_incubations.jpg")