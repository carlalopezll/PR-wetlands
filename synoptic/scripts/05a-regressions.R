# Regressions for PR synoptic data

# Need to make sure I'm not duplicating these plots in the GHG-plots script

# load libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(segmented) # for breakpoints
library(patchwork)
# For Tukey
library(emmeans)
library(multcompView)
library(multcomp)

# read in data
merge <- read_csv("synoptic/data/synoptic merged.csv")

# full dataset (all reps)
ghg <- read_csv("synoptic/data/PR synoptic_GHG.csv")

# theme for plotting
theme <- theme_bw() +
  theme(
    text = element_text(size = 20),         # base text size
    #axis.title = element_text(size = 40),   # axis titles
    #axis.text = element_text(size = 30),    # axis tick labels
    #legend.text = element_text(size = 30),  # legend text
    #legend.title = element_text(size = 35), # legend title
    strip.text = element_text(size = 35)    # facet strip text
  )

# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
# CO2_lab <- expression(paste("Dissolved C","O"[2]^{}*" (g/L)"))

CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))
# CH4_lab <- expression(paste("Dissolved C","H"[4]^{}*" (mg/L)"))
CH4_log_lab <- expression(paste("ln C","H"[4]^{}*" ("*mu,"M)"))

merge <- merge %>%
  mutate(CH4_log = log(CH4_uM),
         CO2_log = log(CO2_uM))

hist(merge$CH4_uM)
hist(merge$CH4_log)

shapiro.test(merge$CH4_log)

#### Regressions ####

ggplot(merge, aes(x= CH4_uM, y = CO2_uM)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm") +
  theme +
  labs(x = CH4_lab, y = CO2_lab)

ggsave("CH4 vs CO2.jpg")

summary(lm(CO2_uM~CH4_log, merge))

hist(merge$CH4_uM)

# Significant linear regressions

# DOC-DO conc.
ggplot(merge, aes(x= DO_mgL, y = DOC_avg)) +
  geom_point(size = 5, aes(color = Season)) +
  theme +
  geom_smooth(method = "lm")

ggsave("DOC-DO.jpg")

summary(lm(DOC_avg~DO_uM, merge))

# Regression multi-plot

temp_a <- ggplot(merge, aes(x = Temp_C, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  geom_smooth(method = "lm") +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  labs (x = "Temperature (°C)", y = CO2_lab) +
  theme
  
temp_a

ggplot(merge, aes(x = Temp_C, y = CH4_uM)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  labs (x = "Temperature (°C)", y = CH4_lab) +
  theme

temp_b <- ggplot(merge, aes(x = Temp_C, y = CH4_log)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  labs (x = "Temperature (°C)", y = CH4_log_lab) +
  theme

temp_b

summary(lm(CO2_uM~Temp_C, merge)) # not significant
summary(lm(CH4_log~Temp_C, merge)) #  not significant

DOC_a <- ggplot(merge, aes(x= DOC_avg, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  labs(x = "DOC (mg/L)", y = CO2_lab) +
  theme

DOC_a

DOC_b <- ggplot(merge, aes(x= DOC_avg, y = CH4_log)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  theme +
  labs(x = "DOC (mg/L)", y = CH4_log_lab) +
  ylim(0,NA)

DOC_b

summary(lm(CO2_uM~DOC_avg, merge)) # not significant
summary(lm(CH4_log~DOC_avg, merge)) # almosssst significant

# 3rd row
DO_a <- ggplot(merge, aes(x = DO_mgL, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  labs(x = "DO (mg/L)", y = CO2_lab) +
  theme

DO_a

DO_b <- ggplot(merge, aes(x = DO_mgL, y = CH4_log)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  geom_smooth(method = "lm") +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  geom_smooth(method = "lm") +
  labs(x= "DO (mg/L)", y = CH4_log_lab) +
  theme

DO_b

summary(lm(CO2_uM~DO_uM, merge)) # not significant
summary(lm(CH4_log~DO_uM, merge)) # significant


# 4th row
TDN_a <- ggplot(merge, aes(x = TDN_avg, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  labs(x= "TDN (mg/L)", y = CO2_lab) +
  ylim(0,NA) +
  theme

TDN_a

TDN_b <- ggplot(merge, aes(x = TDN_avg, y = CH4_log)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  geom_smooth(method = "lm") +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  geom_smooth(method = "lm") +
  labs(x= "TDN (mg/L)", y = CH4_log_lab) +
  theme

TDN_b

summary(lm(CO2_uM~TDN_avg, merge)) # not significant
summary(lm(CH4_log~TDN_avg, merge)) # not significant

# 5th row
Cl_a <- ggplot(merge, aes(x = Cl_avg, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  labs(x= "Chloride (mg/L)", y= CO2_lab) +
  theme

Cl_a

Cl_b <- ggplot(merge, aes(x = Cl_avg, y = CH4_log)) +
  geom_point(size = 5, aes(color = Season, 
                           shape = ifelse(Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites"))) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(shape = NULL) +
  labs(x= "Chloride (mg/L)", y= CH4_log_lab) +
  geom_smooth(method = "lm") +
  theme

Cl_b

summary(lm(CO2_uM~Cl_avg, merge)) # not significant
summary(lm(CH4_log~Cl_avg, merge)) # not significant

summary(lm(CO2_uM~SO4_avg, merge)) # not significant
summary(lm(CH4_log~SO4_avg, merge)) # not significant

summary(lm(CH4_log~SO4_avg+Cl_avg, merge)) # not significant

summary(lm(SO4_avg~Cl_avg, merge)) # not significant

# Making the super plot

temp_row <- temp_a | temp_b

doc_row <- DOC_a | DOC_b

DO_row <- DO_a | DO_b

TDN_row <- TDN_a | TDN_b

cl_row <- Cl_a | Cl_b

final_plot <- temp_row / doc_row / DO_row / TDN_row / cl_row +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave("synoptic/regression multiplot.jpg", width = 12, height= 18, dpi = 600)


ggplot(merge, aes(x = CH4_uM, y = CO2_uM)) +
  geom_point(aes(color = log(DO_perc)), size = 5) +
  theme +
  geom_smooth(se = F) +
  scale_x_log10() +
  theme(legend.position = "inside", legend.position.inside = c(0.25, 0.85),
        legend.direction = "horizontal")

ggplot(merge, aes(x = CO2_uM, y = CH4_uM)) +
  geom_point(aes(color = log(DO_perc)), size = 5) +
  theme


# Fit initial linear model for CH4-DO
# Not significant actually
lm_do_ch4 <- lm(CH4_uM ~ DO_perc, data = merge)

# Estimate breakpoint
seg_model <- segmented(lm_do_ch4, seg.Z = ~DO_perc, psi = 10) # psi = initial guess
summary(seg_model)

davies.test(lm_do_ch4, seg.Z = ~DO_perc)

# Extract fitted values for plotting
merge$CH4_fit <- NA
merge$CH4_fit[!is.na(merge$CH4_uM) & !is.na(merge$SO4_avg)] <- fitted(seg_model)

SO4_b <- ggplot(merge, aes(x = SO4_avg, y = CH4_uM)) +
  geom_point(size = 5) +
  geom_line(aes(y = CH4_fit), color = "blue", linewidth = 1) +
  labs(x = "SO4_avg", y = "CH4 (mg/L)") +
  ylim(0, NA) +
  theme

SO4_b


# CH4-SpC
ggplot(merge, aes(x= SpC, y = CH4_uM, color = Site)) +
  geom_point(size = 5) +
  theme

ggsave("synoptic/output/graphs/CH4 vs SpC.jpg")

# CH4-SO4
ggplot(merge, aes(x= SO4_avg, y = CH4_uM, color = Site)) +
  geom_point(size = 5) +
  theme

ggsave("synoptic/output/graphs/CH4 vs SO4.jpg")


# CH4-SO4 with loess
ggplot(merge, aes(x = SO4_avg, y = CH4_uM, color = Site)) +
  geom_point(size = 5) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme

ggsave("CH4 vs SO4_loess.jpg")

# CH4-SO4 with loess
ggplot(merge, aes(x = Cl_avg, y = CH4_uM, color = Site)) +
  geom_point(size = 5) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme

ggsave("CH4 vs Cl_loess.jpg")

# SO4-Cl
ggplot(merge, aes(x=Cl_avg, y = SO4_avg, color = Site)) +
  geom_point(size = 3) +
  theme

ggsave("SO4 vs Cl.jpg")

# CO2-TDN
ggplot(merge, aes(x=TDN_avg, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Site)) +
  geom_smooth(method = "lm", se = F) +
  theme +
  ylim(0,NA)

summary(lm(CO2_uM~TDN_avg, merge))

# CH4-DO%
ggplot(merge, aes(x=DO_perc, y = CH4_uM)) +
  geom_point(size = 3) +
  ylim(0,50)

# CH4-Temp
ggplot(merge, aes(x=Temp_C, y= CH4_uM)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm")

ggsave("CH4 vs temp.jpg")

# CO2-Temp
ggplot(merge, aes(x=Temp_C, y= CO2_uM)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  theme

ggsave("CO2-Temp.jpg")

summary(lm(CO2_uM~Temp_C, merge))

# 

salt_lm <- lm(SpC~Cl_avg + SO4_avg, ghg)
summary(salt_lm)

ggplot(ghg, aes(x=Cl_avg, y=CO2_uM)) +
  geom_point() +
  geom_smooth(method = "lm")







# GHG with soil PW

ggplot(ghg_pr2, aes(x = Cl, y = dCO2.umol, color = Site)) +
  geom_point(size = 3) +
  labs(x = "Soil porewater chloride (mg/L)", y= CO2_lab) +
  theme

ggsave("CO2 vs Cl.jpg")

ggplot(ghg_pr2, aes(x = Cl, y = dCH4.umol, color = Site)) +
  geom_point(size = 3) +
  labs(x = "Soil porewater chloride (mg/L)", y= CH4_lab) +
  theme +
  scale_y_log10()

ggsave("CH4 vs Cl.jpg")

ggplot(ghg_pr2, aes(x = TOC, y = dCO2.umol, color = Site)) +
  geom_point(size = 3) +
  labs(x = "Soil porewater TOC (mg/L)", y= CO2_lab) +
  theme

ggsave("CO2 vs TOC.jpg")

ggplot(ghg_pr2, aes(x = TOC, y = dCH4.umol, color = Site)) +
  geom_point(size = 3) +
  labs(x = "Soil porewater TOC (mg/L)", y= CH4_lab) +
  theme

ggsave("CH4 vs TOC.jpg")


#### Site-avg regressions ####

site_avg <- read_csv("synoptic/output/tables/site means.csv")

ggplot(site_avg, aes(x = LDI, y = CO2_uM)) +
  geom_point(size = 3, aes(color = Site)) +
  geom_smooth(method = "lm")

summary(lm(CO2_uM~LDI, site_avg))

ggsave("CO2 vs LDI.jpg")

ggplot(site_avg, aes(x = LDI, y = CH4_uM)) +
  geom_point(size = 3, aes(color = Site)) +
  geom_smooth(method = "lm")

summary(lm(CH4_uM~LDI, data = site_avg))

ggsave("CH4 vs LDI.jpg")

ggplot(site_avg, aes(x= HRT_days, y= CO2_uM)) +
  geom_point(size = 3, aes(color = Site)) +
  geom_smooth(method = "lm")

summary(lm(CO2_uM~HRT_days, site_avg))

ggplot(site_avg, aes(x= HRT_days, y= CH4_uM)) +
  geom_point(size = 3, aes(color = Site)) +
  geom_smooth(method = "lm")

summary(lm(CH4_uM~HRT_days, data = site_avg))

ggplot(site_avg, aes(x= Area_ha, y= HRT_days)) +
  geom_point(size = 3, aes(color = Site)) +
  geom_smooth(method = "lm")

ggplot(site_avg, aes(x= Area_ha, y= CO2_uM)) +
  geom_point(size = 3, aes(color = Site)) +
  geom_smooth(method = "lm")

summary(lm(CO2_uM~Area_ha, site_avg))

ggplot(site_avg, aes(x= Area_ha, y= CH4_uM)) +
  geom_point(size = 3, aes(color = Site)) +
  geom_smooth(method = "lm")

summary(lm(CH4_uM~Area_ha, site_avg))


ggplot(site_avg, aes(x= Average_depth_m, y= CO2_uM)) +
  geom_point(size = 3, aes(color = Site)) +
  geom_smooth(method = "lm")

summary(lm(CO2_uM~Average_depth_m, site_avg))

ggplot(site_avg, aes(x= Average_depth_m, y= CH4_uM)) +
  geom_point(size = 3, aes(color = Site)) +
  geom_smooth(method = "lm")

summary(lm(CH4_uM~Average_depth_m, site_avg))

