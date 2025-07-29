library(lubridate)
library(ggplot2)

merge$month <- month(merge$Date_corrected)
merge$DOY <- yday(merge$Date_corrected)

# CO2 seasonal-site linear model

model_season <- lm(CO2_uM~Season+Site, merge)
model_month <- lm(CO2_uM~month+Site, merge)
model_day <- lm(CO2_uM~DOY+Site, merge)

summary(model_season)$r.squared
summary(model_month)$r.squared
summary(model_day)$r.squared

anova_season <- anova(model_season)
anova(model_month)
anova(model_day)

AIC(model_season, model_month, model_day)

# Calculate percent variance explained
season_var_explained <- anova_season$`Sum Sq` / sum(anova_season$`Sum Sq`) * 100

# Combine into summary table
season_var_summary <- data.frame(
  Component = rownames(anova_season),
  Percent_Variance = round(season_var_explained, 1)
)

write.csv(season_var_summary, "synoptic/output/season-site var_CO2.csv", row.names = F)

# CH4 model

# CO2 seasonal-site linear model

model_season <- lm(CH4_uM~Season+Site, merge)
model_month <- lm(CH4_uM~month+Site, merge)
model_day <- lm(CH4_uM~DOY+Site, merge)

summary(model_season)$r.squared
summary(model_month)$r.squared
summary(model_day)$r.squared

anova_season <- anova(model_season)
anova(model_month)
anova(model_day)

AIC(model_season, model_month, model_day)

# Calculate percent variance explained
season_var_explained <- anova_season$`Sum Sq` / sum(anova_season$`Sum Sq`) * 100

# Combine into summary table
season_var_summary <- data.frame(
  Component = rownames(anova_season),
  Percent_Variance = round(season_var_explained, 1)
)

write.csv(season_var_summary, "synoptic/output/season-site var_CH4.csv", row.names = F)


ggplot(merge, aes(x = Season, y = CO2_uM, fill = Season)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  theme_minimal() +
  labs(x = "Season",
       y = expression(paste("CO"[2], " (", mu, "M)"))) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

ggsave("CO2 by season_boxplot.jpg")

ggplot(merge, aes(x = Season, y = CH4_uM, fill = Season)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  theme_minimal() +
  labs(x = "Season",
       y = expression(paste("CH"[4], " (", mu, "M)"))) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none") +
  scale_y_log10()

ggsave("CH4 by season_boxplot.jpg")

ggplot(merge, aes(x = Season, y = CO2_uM, fill = Season)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0) +
  facet_grid(. ~Site) +
  labs(x = "Season",
       y = expression(paste("CO"[2], " (", mu, "M)"))) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

ggsave("CO2 by season-site_boxplot.jpg")

ggplot(merge, aes(x = Season, y = CH4_uM, fill = Season)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0) +
  facet_grid(. ~Site) +
  labs(x = "Season",
       y = expression(paste("CH"[4], " (", mu, "M)"))) +
  scale_fill_brewer(palette = "Set2") +
  theme +
  theme(legend.position = "none") +
  scale_y_log10()

ggsave("CH4 by season-site_boxplot.jpg")




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




# Using linear mixed effects model

library(lme4)

# Random effects model

model <- lmer(CO2_uM ~ 1 + (1|Season) + (1|Site), data = merge)

# Variance components
vc <- as.data.frame(VarCorr(model))
print(vc)





model <- lm(wCO2_uM_med~Season+Site, ghg)