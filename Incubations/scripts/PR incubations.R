library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(ggsignif)
library(tidyverse)
library(broom)

theme <- theme_bw() +
  theme(
    text = element_text(size = 35),         # base text size
    axis.title = element_text(size = 40),   # axis titles
    axis.text = element_text(size = 30),    # axis tick labels
    legend.text = element_text(size = 30),  # legend text
    legend.title = element_text(size = 35), # legend title
    strip.text = element_text(size = 35)    # facet strip text
  )

inc <- read_csv("incubations/data/merged incubations.csv")

inc %>%
  select(-site, -treatment) %>%
  mutate_if(is.character,as.numeric) %>%
  str()

anova_model <- aov(CO2_production ~ period * treatment, data = inc)
summary(anova_model)

anova_model <- aov(CH4_production ~ period * treatment, data = inc)
summary(anova_model)

TukeyHSD(anova_model)

# Averaging the 3 reps

inc_avg <- inc %>%
  group_by(site, treatment, timepoint, period) %>%
  summarize(
    CO2_mean = mean(CO2_ppm),
    CH4_mean = mean(CH4_ppm))

write.csv(inc_avg, "incubations/data/incubation averages.csv")

# Keep just incubation data

# PA <- filter(inc, site == "PA")
# TO <- filter(inc, site == "TO")

# Make numeric

inc$AFDM_corrected_CH4_production <- as.numeric(inc$AFDM_corrected_CH4_production)
inc$AFDM_corrected_CO2_production <- as.numeric(inc$AFDM_corrected_CO2.production)

ggplot(inc, aes(x=timepoint, y= CH4_production, fill = treatment)) +
  labs(y = "GHG production rates (ppm GHG/hr/g AFDM)") +
  facet_wrap(~site, scales = "free") +
  geom_hline(yintercept = 0)

ggsave("production template.jpg")


ggplot(inc, aes(x=timepoint, y= AFDM_corrected_CO2_production, fill = treatment)) +
  geom_boxplot(na.rm = TRUE) +
  geom_jitter(aes(group = interaction(timepoint, treatment)), position = position_dodge(width=0.9), alpha = 0.6) +
  labs(y = "CO2 production rates (ppm CO2/hr/g AFDM)") +
  facet_wrap(~site, scales = "free") +
  geom_hline(yintercept = 0)

ggsave("CO2 production rates_no outliers.jpg")

ggplot(inc, aes(x=timepoint, y= AFDM_corrected_CH4_production, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(aes(group = interaction(timepoint, treatment)), position = position_dodge(width=0.9), alpha = 0.6) +
  labs(y = "CH4 production rates (ppm CH4/hr/g AFDM)") +
  facet_wrap(~site, scales = "free") +
  geom_hline(yintercept = 0)
  
ggsave("CH4 production rates.jpg")

inc_subset <- filter(inc, timepoint == 3)

inc_subset <- filter(inc, timepoint == 7)

ggplot(inc_subset, aes(x= site, y = CO2_production, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~period, scales = "free")

ggplot(inc_subset, aes(x= period, y = CO2_production, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~site, scales = "free")

ggsave("CO2 production rates vs Spc.jpg")

inc_subset2 <- filter(inc_subset, CO2_production > -60)

ggplot(inc_subset2, aes(x = spc_us_cm, y = CO2_production)) + 
  geom_smooth(aes(color = site), method = "loess", se = TRUE, span = 2) +
  geom_point(aes(color = site, shape = treatment), size = 6, alpha = 0.8) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~period, scales = "free", labeller = labeller(period = c("summer" = "Summer (25°C)", "winter" = "Winter (20°C)"))) + 
  labs(x = "Specific conductivity (μS/cm)", 
       y = expression(CO[2]~"rate (ppm CO"[2]*"/hr/g AFDM)"), 
       shape = "Treatment", 
       color = "Site") + 
  scale_color_manual(
    values = c("PA" = "darkblue", "TO" = "maroon"),
    labels = c("PA" = "Palmas", "TO" = "Tortuguero")
  ) + 
  scale_shape_manual(
    values = c("T1" = 16, "T2" = 17, "T3" = 15),
    labels = c("T1" = "Control", "T2" = "Low", "T3" = "High")
  ) +
  xlim(400, 3500) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.23),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 35),
    text = element_text(size = 35),
    axis.title = element_text(size = 40),
    axis.text = element_text(size = 30),
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 35)
  )




ggplot(inc_subset2, aes(x = spc_us_cm, y = CO2_production)) + 
  geom_smooth(aes(color = site), method = "loess", se = TRUE, span = 2) +
  geom_point(aes(color = site, shape = treatment), size = 6, alpha = 0.8) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~period, scales = "free", labeller = labeller(period = c("summer" = "Summer (25°C)", "winter" = "Winter (20°C)"))) + 
  labs(x = "Specific conductivity (μS/cm)", y = expression(CO[2]~"rate (ppm CO"[2]*"/hr/g AFDM)"), shape = "Treatment"  ) + 
  scale_color_manual(values = c("darkblue", "maroon")) + 
  scale_shape_manual(values = c("T1" = 16, "T2" = 17, "T3" = 15),
                     labels = c("T1" = "Control", "T2" = "Low", "T3" = "High")) +
  xlim(400, 3500) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.23),  # adjust (x, y) between 0 and 1
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_blank(),
    text = element_text(size = 35),         # base text size
    axis.title = element_text(size = 40),   # axis titles
    axis.text = element_text(size = 30),    # axis tick labels
    legend.text = element_text(size = 30),  # legend text
    strip.text = element_text(size = 35)) +  # facet strip text
  guides(color = "none")  # Hide color legend

ggsave("CO2 production rates vs Spc_smooth_v2.jpg", width = 15, height = 10, dpi = 300)


ggplot(inc_subset2, aes(x = spc_us_cm, y = CH4_production)) + 
  geom_smooth(aes(color = site), method = "loess", se = TRUE, na.rm = TRUE, span = 2) + 
  geom_point(aes(color = site, shape = treatment), size = 6, alpha = 0.8) +
  geom_hline(yintercept = 0) +
  facet_wrap(
    ~period, 
    scales = "free", 
    labeller = labeller(period = c("summer" = "Summer (25°C)", "winter" = "Winter (20°C)"))
  ) + 
  labs(
    x = "Specific conductivity (μS/cm)", 
    y = expression(CH[4]~"rate (ppm CH"[4]*"/hr/g AFDM)"), 
    shape = "Treatment"
  ) + 
  scale_color_manual(values = c("darkblue", "maroon")) + 
  scale_shape_manual(
    values = c("T1" = 16, "T2" = 17, "T3" = 15),
    labels = c("T1" = "Control", "T2" = "Low", "T3" = "High")
  ) +
  xlim(400, 3500) +
  theme_bw() +
  theme(
    legend.position = c(0.68, 0.23),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_blank(),
    text = element_text(size = 35),
    axis.title = element_text(size = 40),
    axis.text = element_text(size = 30),
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 35)
  ) +
  guides(color = "none")

ggsave("CH4 production rates vs Spc_smooth.jpg", width = 15, height = 10, dpi = 300)


ggplot(inc_subset2, aes(x = spc_us_cm, y = CO2_production, color = site, shape = treatment)) + 
  geom_point(size = 6, alpha = 0.8) + 
  labs(
    x = "Specific conductivity (μS/cm)", 
    y = expression(CO[2]~"rate (ppm CO"[2]*"/hr/g AFDM)"), 
    color = "Site"
  ) + 
  geom_smooth() +
  geom_hline(yintercept = 0) +
  facet_wrap(
    ~period, 
    scales = "free", 
    labeller = labeller(period = c("summer" = "Summer (25°C)", "winter" = "Winter (20°C)"))
  ) + 
  scale_color_manual(values = c("darkblue", "maroon")) + 
  theme_bw(base_size = 20) + 
  xlim(400, 3500) +
  theme +
  theme(legend.position = "none")

ggplot(inc_subset, aes(x = spc_us_cm, y = CH4_production, color = site)) +
  geom_point(size = 4, alpha = 0.8) +
  labs(
    x = "Specific conductivity (μS/cm)", 
    y = expression(CH[4]~"rate (ppm CH"[4]*"/hr/g AFDM)"),
    color = "Site"
  ) +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  facet_wrap(
    ~period, 
    scales = "free", 
    labeller = labeller(period = c("summer" = "Summer (25°C)", "winter" = "Winter (20°C)"))
  ) +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("darkblue", "maroon")) + 
  xlim (400, 3500) +
  theme +
  theme(legend.position ="bottom")



ggplot(inc_subset, aes(x=SpC_us_cm, y= AFDM_corrected_CH4_production, color = treatment)) +
  geom_point() +
  labs(y = "CH4 production rates (ppm CO2/hr/g AFDM)") +
  geom_hline(yintercept = 0)

ggsave("CH4 production rates vs Spc.jpg")

library(ggplot2)
library(multcomp)
library(emmeans)
library(ggsignif)  # for easy significance bars

#### CO2 Tukey ####

# Step 1: Fit a model
mod <- lm(AFDM_corrected_CO2.production ~ timepoint * treatment * site, data = inc)

# Step 2: Tukey HSD
tukey <- emmeans(mod, pairwise ~ treatment | timepoint * site)

# View the summary
summary(tukey$contrasts)

# Step 3: Prepare significance letters
library(multcompView)

# Get compact letter display (CLD)
cld_results <- cld(tukey$emmeans, Letters = letters)

# This cld_results object now contains grouping letters

# Step 4: Merge significance letters back into your plotting data
inc_with_letters <- merge(inc, cld_results, by = c("timepoint", "site", "treatment"))

# Step 5: Recreate your plot, adding significance letters
ggplot(inc, aes(x=timepoint, y=AFDM_corrected_CO2.production, fill=treatment)) +
  geom_boxplot(na.rm=TRUE) +
  geom_jitter(aes(group=interaction(timepoint, treatment)), 
              position=position_dodge(width=0.9), alpha=0.6) +
  labs(y = "CO2 production rates (ppm CO2/hr/g AFDM)") +
  facet_wrap(~site, scales="free") +
  geom_hline(yintercept=0) +
  geom_text(data=cld_results, 
            aes(x=timepoint, y=max(inc$AFDM_corrected_CO2.production, na.rm = TRUE) * 1.05, 
                label=.group, group=treatment),
            position=position_dodge(width=0.9),
            inherit.aes=FALSE)

#### CH4 Tukey ####

# Step 1: Fit a model
mod <- lm(AFDM_corrected_CH4_production ~ timepoint * treatment * site, data = inc)

# Step 2: Tukey HSD
tukey <- emmeans(mod, pairwise ~ treatment | timepoint * site)

# View the summary
summary(tukey$contrasts)

# Step 3: Prepare significance letters
library(multcompView)

# Get compact letter display (CLD)
cld_results <- cld(tukey$emmeans, Letters = letters)

# This cld_results object now contains grouping letters

# Step 4: Merge significance letters back into your plotting data
inc_with_letters <- merge(inc, cld_results, by = c("timepoint", "site", "treatment"))

# Step 5: Recreate your plot, adding significance letters
ggplot(inc, aes(x=timepoint, y=AFDM_corrected_CH4_production, fill=treatment)) +
  geom_boxplot(na.rm=TRUE) +
  geom_jitter(aes(group=interaction(timepoint, treatment)), 
              position=position_dodge(width=0.9), alpha=0.6) +
  labs(y = "CH4 production rates (ppm CH4/hr/g AFDM)") +
  facet_wrap(~site, scales="free") +
  geom_hline(yintercept=0) +
  geom_text(data=cld_results, 
            aes(x=timepoint, y=max(inc$AFDM_corrected_CH4_production, na.rm = TRUE) * 1.05, 
                label=.group, group=treatment),
            position=position_dodge(width=0.9),
            inherit.aes=FALSE)

inc %>%
  drop_na(DOC_AFDM_corrected) %>%
  ggplot(aes(x=timepoint, y= DOC_AFDM_corrected, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(aes(group = interaction(timepoint, treatment)), position = position_dodge(width=0.9), alpha = 0.6) +
  labs(y = "DOC (mg/L)") +
  facet_wrap(~site, scales = "free")

inc %>%
  drop_na(TDN_mgL) %>%
  ggplot(aes(x=timepoint, y= TDN_mgL, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(aes(group = interaction(timepoint, treatment)), position = position_dodge(width=0.9), alpha = 0.6) +
  labs(y = "TDN (mg/L)") +
  facet_wrap(~site, scales = "free")

#### Winter incubations ####

ggplot(inc, aes(x=timepoint, y= CO2_production, fill = treatment)) +
  geom_boxplot(na.rm = TRUE) +
  geom_jitter(aes(group = interaction(timepoint, treatment)), position = position_dodge(width=0.9), alpha = 0.6) +
  labs(y = "CO2 production rates (ppm CO2/hr/g AFDM)") +
  facet_wrap(~site, scales = "free") +
  geom_hline(yintercept = 0)

ggsave("CO2 production rates_winter.jpg")

ggplot(inc, aes(x=timepoint, y= CH4_production, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(aes(group = interaction(timepoint, treatment)), position = position_dodge(width=0.9), alpha = 0.6) +
  labs(y = "CH4 production rates (ppm CH4/hr/g AFDM)") +
  facet_wrap(~site, scales = "free") +
  geom_hline(yintercept = 0)

ggsave("CH4 production rates_winter.jpg")

 ggplot(PA, aes(x=timepoint, y= CO2_ppm_corrected, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  ggtitle("Palmas")

# ggsave("incubations/output/Palmas CO2_clean.jpg")

ggplot(PA, aes(x=timepoint, y= CO2_ppm_corrected, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Palmas")

# ggsave("Palmas_CO2.jpg")

ggplot(PA, aes(x=timepoint, y= CH4_ppm_corrected, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  ggtitle("Palmas")

# ggsave("incubations/output/Palmas CH4_clean.jpg")

ggplot(PA, aes(x=timepoint, y= CH4_ppm_corrected, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Palmas")

# ggsave("Palmas_CH4.jpg")


ggplot(TO, aes(x=timepoint, y= CO2_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  ggtitle("Tortuguero")

# ggsave("Tortuguero_CO2 clean.jpg")

ggplot(TO, aes(x=timepoint, y= CO2_ppm_corrected, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Tortuguero")

# ggsave("Tortuguero_CO2.jpg")

ggplot(TO, aes(x=timepoint, y= CH4_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  ggtitle("Tortuguero")

# ggsave("Tortuguero_CH4 clean.jpg")

ggplot(TO, aes(x=timepoint, y= CH4_ppm, fill = treatment, color = treatment, group = interaction(treatment, timepoint))) +
  geom_boxplot(alpha = 0.1)+
  geom_point(position = position_dodge(width = 0.75), alpha = 0.2) +
  ggtitle("Tortuguero")

# ggsave("Tortuguero_CH4.jpg")

ggplot(inc_avg, aes(x= SpC_us_cm, y= CO2_mean, color = site, shape = timepoint)) +
  geom_point(size = 5) +
  geom_line()

ggsave("incubations/output/CO2 avg vs SpC.jpg")

ggplot(inc_avg, aes(x= SpC_us_cm, y= CH4_mean, color = site, shape = timepoint)) +
  geom_point(size = 5) +
  geom_line() +
  scale_y_log10()

ggsave("incubations/output/CH4 avg vs SpC.jpg")