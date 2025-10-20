library(ggplot2)
library(patchwork)
library(segmented)

# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
# CO2_lab <- expression(paste("Dissolved C","O"[2]^{}*" (g/L)"))

CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))
# CH4_lab <- expression(paste("Dissolved C","H"[4]^{}*" (mg/L)"))

# Define shape mapping for sites
merge$Site_type <- ifelse(merge$Site %in% c("Palmas", "Tortuguero"), "Incubation sites", "Synoptic sites")

ch4_co2 <- ggplot(merge, aes(x= CH4_uM, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  geom_smooth(method = "lm", color = "black") +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  theme +
  labs(x = CH4_lab, y = CO2_lab) +
  scale_x_log10() +
  theme(legend.position = "none")

ch4_co2

ggsave("CH4 vs CO2.jpg")

# --------------------------
# 1st row: Temperature
# --------------------------
temp_a <- ggplot(merge, aes(x = Temp_C, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  geom_smooth(method = "lm", color = "black") +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(x = "Temperature (°C)", y = CO2_lab, shape = NULL) +
  theme

temp_a

temp_b <- ggplot(merge, aes(x = Temp_C, y = CH4_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(x = "Temperature (°C)", y = CH4_lab, shape = NULL) +
  theme

temp_row <- temp_a | temp_b

# --------------------------
# 2nd row: DOC
# --------------------------
DOC_a <- ggplot(merge, aes(x = DOC_avg, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(x = "DOC (mg/L)", y = CO2_lab, shape = NULL) +
  theme

DOC_b <- ggplot(merge, aes(x = DOC_avg, y = CH4_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(x = "DOC (mg/L)", y = CH4_lab, shape = NULL) +
  ylim(0, NA) +
  theme

DOC_row <- DOC_a | DOC_b

# --------------------------
# 3rd row: DO
# --------------------------
DO_a <- ggplot(merge, aes(x = DO_mgL, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(x = "DO (mg/L)", y = CO2_lab, shape = NULL) +
  theme

DO_b <- ggplot(merge, aes(x = DO_mgL, y = CH4_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(x = "DO (mg/L)", y = CH4_lab, shape = NULL) +
  theme

DO_row <- DO_a | DO_b

# --------------------------
# 4th row: TDN
# --------------------------
TDN_a <- ggplot(merge, aes(x = TDN_avg, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(x = "TDN (mg/L)", y = CO2_lab, shape = NULL) +
  theme

TDN_b <- ggplot(merge, aes(x = TDN_avg, y = CH4_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(x = "TDN (mg/L)", y = CH4_lab, shape = NULL) +
  theme

TDN_row <- TDN_a | TDN_b

# --------------------------
# 5th row: Cl
# --------------------------
Cl_a <- ggplot(merge, aes(x = Cl_avg, y = CO2_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  labs(x = "Chloride (mg/L)", y = CO2_lab, shape = NULL) +
  theme

# Fit segmented model for CH4 ~ Cl
lm_cl_ch4 <- lm(CH4_mgL ~ Cl_avg, data = merge)
seg_model <- segmented(lm_cl_ch4, seg.Z = ~Cl_avg)

summary(seg_model)
merge$CH4_fit <- NA
merge$CH4_fit[!is.na(merge$CH4_mgL) & !is.na(merge$Cl_avg)] <- fitted(seg_model)

Cl_b <- ggplot(merge, aes(x = Cl_avg, y = CH4_uM)) +
  geom_point(size = 5, aes(color = Season, shape = Site_type)) +
  scale_shape_manual(values = c("Incubation sites" = 17, "Synoptic sites" = 16)) +
  geom_line(aes(y = CH4_fit), color = "black", linewidth = 1) +
  labs(x = "Chloride (mg/L)", y = CH4_lab, shape = NULL) +
  geom_vline(xintercept = 114.74, lty = "dashed") +
  ylim(0, NA) +
  theme

cl_row <- Cl_a | Cl_b

# --------------------------
# Combine all rows
# --------------------------
final_plot <- temp_row / DOC_row / DO_row / cl_row +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot
ggsave("synoptic/regression multiplot.jpg", width = 10, height= 12)

cl_low <- filter(merge, Cl_avg < 114.74)
cl_high <- filter(merge, Cl_avg > 114.74)

summary(lm(CH4_mgL~SO4_avg, data = cl_low))

ggplot(cl_low, aes(x=Cl_avg, y = CH4_mgL)) +
  geom_point() +
  geom_smooth(method = "lm")
