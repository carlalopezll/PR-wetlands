# Plotting dissolved GHG concentrations in PR coastal wetlands

# Load libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(patchwork)

# For Tukey
library(emmeans)
library(multcompView)
library(multcomp)

# read in averaged data
merge <- read_csv("synoptic/data/synoptic merged.csv")

merge$Season <- factor(merge$Season,
                       levels = c("Summer", "Fall", "Winter"))

# full dataset (all reps)
ghg <- read_csv("synoptic/data/PR synoptic_GHG.csv")

# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
# CO2_lab <- expression(paste("Dissolved C","O"[2]^{}*" (g/L)"))

CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))
# CH4_lab <- expression(paste("Dissolved C","H"[4]^{}*" (mg/L)"))

theme <- theme_bw() +
  theme(text = element_text(size = 20))

#### Boxplots ####

# Site boxplot: CO2
ggplot(merge, aes(x= Site, y= CO2_uM, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  scale_fill_brewer(palette = "YlOrBr") +
  theme +
  theme(legend.position = "none") +
  labs(y = CO2_lab)

ggsave("synoptic/output/graphs/CO2 site boxplots.jpg", width = 8, height = 4, dpi = 300)

# Site boxplot: CH4
ggplot(merge, aes(x= Site, y= CH4_uM, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  scale_fill_brewer(palette = "YlOrBr") +
  theme(legend.position = "none") +
  labs(y = CH4_lab) +
  scale_y_log10()

ggsave("synoptic/output/graphs/CH4 site boxplots.jpg", width = 8, height = 4, dpi = 300)

# Season boxplots
ggplot(subset(merge, Season != "Fall"), 
       aes(x = Season, y = CO2_uM, fill = Season)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  theme(legend.position = "none") +
  labs(y= CO2_lab)

ggsave("synoptic/output/graphs/CO2 season boxplot.jpg", width = 8, height = 4, dpi = 300)

ggplot(subset(merge, Season != "Fall"), 
       aes(x = Season, y = CH4_uM, fill = Season)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  scale_y_log10() +
  theme +
  theme(legend.position = "none") +
  labs(y= CH4_lab)

ggsave("synoptic/output/graphs/CH4 season boxplot.jpg", width = 8, height = 4, dpi = 300)

ggplot(merge, aes(x = Season, y = CO2_gL, fill = Season)) +
  geom_boxplot(width = 2, size = 0.8) +
  geom_point(color = "black", position = position_jitter(width = 0), size = 1.5) +
  scale_y_log10() +
  theme +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20)
  ) +
  facet_grid(. ~ Site) +
  labs(y= CO2_lab)

ggsave("CO2 by season.png", width = 18, height = 6, dpi = 300)

ggplot(merge, aes(x=Season, y = CH4_mgL, color = Season)) +
  geom_boxplot(width = 2, size = 0.8) +
  geom_point(color = "black") +
  scale_y_log10() +
  theme +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(size = 20)) +
  facet_grid(.~ Site)

ggsave("CH4 by season.png", width = 18, height = 6, dpi = 300)


# Emergent vs forested

ggplot(merge, aes(x= `Wetland type`, y= CO2_gL, fill = `Wetland type`)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  labs(y = CO2_lab)

ggplot(merge, aes(x= `Wetland type`, y= CH4_mgL, fill = `Wetland type`)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  scale_y_log10()

ggplot(merge, aes(x= Site, y= CO2_gL, fill = `Wetland type`)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  theme(legend.position = c(0.2, 0.85))

ggplot(merge, aes(x= Site, y= CH4_mgL, fill = `Wetland type`)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  theme(legend.position = c(0.4, 0.8)) +
  scale_y_log10()



#### Plotting the whole dataset (all reps) ####

ggplot(ghg, aes(x = Season, y = wCO2_gL_med, fill = Season)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  theme(
    legend.position = "none",
    axis.title.x = element_blank()
  ) +
  facet_grid(. ~ Site)

ggsave("CO2 by season_all reps.png", width = 22, height = 4, dpi = 300)

ggplot(ghg, aes(x=Season, y = wCH4_mgL_med, fill = Season)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  scale_y_log10() +
  theme +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  facet_grid(.~ Site)

ggsave("CH4 by season_all reps.png", width = 22, height = 4, dpi = 300)


# Other boxplots of water chem
a <- ggplot(merge[!is.na(merge$DOC_avg), ], aes(x= Site, y= DOC_avg, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  scale_fill_brewer(palette = "YlOrBr") +
  theme +
  theme(legend.position = "none") +
  labs(y = "DOC (mg/L)")

b <- ggplot(merge[!is.na(merge$Cl_avg), ], aes(x= Site, y= Cl_avg, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  scale_fill_brewer(palette = "YlOrBr") +
  theme +
  theme(legend.position = "none") +
  labs(y = "Chloride (mg/L)")

c <- ggplot(merge[!is.na(merge$DO_mgL), ], aes(x= Site, y= DO_mgL, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  scale_fill_brewer(palette = "YlOrBr") +
  theme +
  theme(legend.position = "none") +
  labs(y = "DO (mg/L)")

d <- ggplot(merge[!is.na(merge$TDN_avg), ], aes(x= Site, y= TDN_avg, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  scale_fill_brewer(palette = "YlOrBr") +
  theme +
  theme(legend.position = "none") +
  labs(y = "TDN (mg/L)")

a / b | c / d

ggsave("Water chem site boxplots.jpg", width = 16, height = 8, dpi = 300)

# Season boxplots
s1 <- ggplot(subset(merge, Season != "Fall"), 
       aes(x = Season, y = DOC_avg, fill = Season)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  theme(legend.position = "none") +
  labs(y= "DOC (mg/L)")

s2 <- ggplot(subset(merge, Season != "Fall"), 
             aes(x = Season, y = Cl_avg, fill = Season)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  theme(legend.position = "none") +
  labs(y= "Chloride (mg/L)")

s3 <- ggplot(subset(merge, Season != "Fall"), 
             aes(x = Season, y = DO_mgL, fill = Season)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  theme(legend.position = "none") +
  labs(y= "DO (mg/L)")

s4 <- ggplot(subset(merge, Season != "Fall"), 
             aes(x = Season, y = TDN_avg, fill = Season)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme +
  theme(legend.position = "none") +
  labs(y= "TDN (mg/L)")

s1 / s2 | s3 / s4

ggsave("Water chem season boxplots.jpg", width = 16, height = 8, dpi = 300)

wetlands <- summary(merge)
