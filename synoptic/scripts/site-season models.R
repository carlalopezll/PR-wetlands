# ANOVA models for PR synoptic
# Carla Lopez Lloreda
# Last updated 7/29/2025

# load libraries
library(lubridate)
library(ggplot2)
library(effects) # for predicted means

# read in data
merge <- read_csv("synoptic/data/synoptic merged.csv")

merge <- merge %>%
  filter(!Season == "Fall")

# extract month and day of year
merge$month <- month(merge$Date_corrected)
merge$DOY <- yday(merge$Date_corrected)

# ANOVAs

# 1. Checking assumptions

# A. Equal variance

# F test to compare variance
# Bartlett test is for more than two groups
# We are checking for equal variance (homoscedasticity)
# If p-value is not significant, it indicates homogeneity of variance across the groups
bartlett.test(CO2_gL~Site, data = merge)
bartlett.test(CO2_gL~Season, data = merge)

# If data is not normally distributed or you are worried about outliers, use the Fligner-Killeen test
fligner.test(CO2_gL~Season, data = merge)
fligner.test(CO2_gL~Site, data = merge)

# 2. Evaluate what temporal scale is best

model_season <- lm(CO2_gL~Season+Site, merge)
model_month <- lm(CO2_gL~month+Site, merge)
model_day <- lm(CO2_gL~DOY+Site, merge)

# Identifying which scale is best (higher r^2)
CO2_r2 <- data.frame(
  season_r2 = summary(model_season)$r.squared,
  month_r2 = summary(model_month)$r.squared,
  day_r2 = summary(model_day)$r.squared
)

# Identifying which scale is best (lower AIC)
AIC(model_season, model_month, model_day)

# 3. ANOVAs for the three different temporal scales
anova_season <- anova(model_season)
anova_month <- anova(model_month)
anova_day <- anova(model_day)

summary(model_season)
anova_season

# Looking at if there's an interactive effect
anova(lm(CO2_gL~Season*Site, merge))

# Predicted group means
Effect(c("Site", "Season"), model_season)

write.csv(anova_season, "synoptic/output/tables/ANOVA table_CO2.csv")

# 4. Extracting info from the ANOVA table

# Calculate percent variance explained
season_var_explained <- anova_season$`Sum Sq` / sum(anova_season$`Sum Sq`) * 100

# Combine into summary table
season_var_summary <- data.frame(
  Component = rownames(anova_season),
  Percent_Variance = round(season_var_explained, 1)
)

write.csv(season_var_summary, "synoptic/output/tables/season-site var_CO2.csv", row.names = F)

#### CH4 model ####

# 1. Checking assumptions

# A. Equal variance

# F test to compare variance
# Bartlett test is for more than two groups
# We are checking for equal variance (homoscedasticity)
# If p-value is not significant, it indicates homogeneity of variance across the groups
bartlett.test(CH4_mgL~Site, data = merge)
bartlett.test(CH4_mgL~Season, data = merge)

# If data is not normally distributed or you are worried about outliers, use the Fligner-Killeen test
fligner.test(CH4_mgL~Season, data = merge)
fligner.test(CH4_mgL~Site, data = merge)

# ANOVAs
model_season <- lm(CH4_mgL~Season+Site, merge)
model_month <- lm(CH4_mgL~month+Site, merge)
model_day <- lm(CH4_mgL~DOY+Site, merge)

CH4_r2 <- data.frame(
  season_r2 = summary(model_season)$r.squared,
  month_r2 = summary(model_month)$r.squared,
  day_r2 = summary(model_day)$r.squared
)

anova_season <- anova(model_season)
anova_month <- anova(model_month)
anova_day <- anova(model_day)

AIC(model_season, model_month, model_day)

summary(model_season)
anova_season

# Looking at if there's an interactive effect
anova(lm(CH4_mgL~Season*Site, merge))

write.csv(anova_season, "synoptic/output/tables/ANOVA table_CH4.csv")

# Calculate percent variance explained
season_var_explained <- anova_season$`Sum Sq` / sum(anova_season$`Sum Sq`) * 100

# Combine into summary table
season_var_summary <- data.frame(
  Component = rownames(anova_season),
  Percent_Variance = round(season_var_explained, 1)
)

write.csv(season_var_summary, "synoptic/output/tables/season-site var_CH4.csv", row.names = F)

# Season+Site plots

ggplot(ghg, aes(x = Season, y = wCO2_uM_med, fill = Season)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0) +
  facet_grid(. ~Site) +
  labs(x = "Season",
       y = expression(paste("CO"[2], " (", mu, "M)"))) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

ggsave("CO2 by season-site_boxplot all reps.jpg")

ggplot(ghg, aes(x = Season, y = wCH4_uM_med, fill = Season)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0) +
  facet_grid(. ~Site) +
  labs(x = "Season",
       y = expression(paste("CH"[4], " (", mu, "M)"))) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none") +
  scale_y_log10()

ggsave("CH4 by season-site_boxplot all reps.jpg")