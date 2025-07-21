# Graphs for ESA

library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)

# For Tukey
library(emmeans)
library(multcompView)
library(multcomp)

# read in data


# filter out Arroyo, no winter data
ghg <- filter(ghg, Site != "Arroyo")

# Change to standard date format
ghg$Date_corrected <- as.Date(ghg$Date, format = '%m/%d/%Y')

# theme
theme <- theme_bw() +
  theme(
    text = element_text(size = 35),         # base text size
    axis.title = element_text(size = 40),   # axis titles
    axis.text = element_text(size = 30),    # axis tick labels
    legend.text = element_text(size = 30),  # legend text
    legend.title = element_text(size = 35), # legend title
    strip.text = element_text(size = 35)    # facet strip text
  )

# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))

ggplot(merge, aes(x= CO2_uM, y = CH4_uM, color = Site)) +
  geom_point(size = 5) + 
  scale_y_log10() +
  theme

ggplot(merge, aes(x = Site, y = CO2_uM)) +
  geom_boxplot() +
  scale_y_log10() +
  theme +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20)
  ) +
  labs(y = CO2_lab, x = "")

ggplot(merge, aes(x = Season, y = CO2_uM, color = Season)) +
  geom_boxplot(width = 2, size = 0.8) +
  geom_point(color = "black", position = position_jitter(width = 0), size = 1.5) +
  scale_y_log10() +
  theme +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20)
  ) +
  labs(y = CO2_lab, x = "") +
  facet_grid(. ~ Site)

ggsave("CO2 by season.png", width = 18, height = 6, dpi = 300)

ggplot(merge, aes(x=Season, y = CH4_uM, color = Season)) +
  geom_boxplot(width = 2, size = 0.8) +
  geom_point(color = "black") +
  scale_y_log10() +
  theme +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(size = 20)) +
  labs(y=CH4_lab) +
  facet_grid(.~ Site)

ggsave("CH4 by season.png", width = 18, height = 6, dpi = 300)


cbbPalette <- c(
  "#90d743",
  "#35b779",
  "#21918c",
  "#31688e",
  "#443983",
  "#fde725",
  "#440154")



# To use for fills, add
# scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
# scale_colour_manual(values=cbPalette)


calculate_stats <- function(x) {
  mean_val <- round(mean(x, na.rm = TRUE), 1)
  sd_val <- round(sd(x, na.rm = TRUE), 1)
  min_val <- round(min(x, na.rm = TRUE), 1)
  max_val <- round(max(x, na.rm = TRUE), 1)
  return(paste0(mean_val, " ± ", sd_val, " (", min_val, "-", max_val, ")"))
}

# Calculate the statistics per site and add Tukey's HSD letters
stats_summary_per_site <- ghg_avg %>%
  group_by(Site) %>%
  summarise(
    CO2 = calculate_stats(CO2_avg),
    CH4 = calculate_stats(CH4_avg)
  )

write_csv(stats_summary_per_site, "Data/GHG summary table.csv")

ggplot(waterchem, aes(x= Site, y= F, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width=0) +
  theme +
  theme(legend.position = "none")


ggplot(DOC_avg, aes(x= Site, y= DOC_avg, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width=0) +
  theme +
  ylab("DOC (mg/L)") +
  theme(legend.position = "none")

ggsave("Graphs/DOC boxplot.jpg")


ggplot(ghg_avg, aes(x= Site, y= CO2_avg, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width=0) +
  theme +
  ylab(CO2_lab) +
  theme(legend.position = "none")

ggsave("Data/Graphs/CO2 boxplot.jpg", width = 14, height = 6, dpi = 300)

ggplot(ghg_avg, aes(x= Site, y= CH4_avg, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width=0) +
  theme +
  ylab(CH4_lab) +
  scale_y_log10() +
  theme(legend.position = "none")

ggsave("Data/Graphs/CH4 boxplot.jpg", width = 14, height = 6, dpi = 300)





summary(lm(CO2_mean~HRT_days, hydro))
summary(lm(CH4_mean~HRT_days, hydro))

ggplot(hydro, aes(x= HRT_days, y= CO2_mean, fill = Site)) +
  geom_point() +
  theme +
  ylab(CO2_lab) +
  theme(legend.position = "none")

ggsave("Data/Graphs/CO2 boxplot.jpg", width = 14, height = 6, dpi = 300)

ggplot(hydro, aes(x= Site, y= CH4_avg, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width=0) +
  theme +
  ylab(CH4_lab) +
  scale_y_log10() +
  theme(legend.position = "none")

ggsave("Data/Graphs/CH4 boxplot.jpg", width = 14, height = 6, dpi = 300)







ggplot(ghg_avg, aes(x= CO2_avg, y=CH4_avg, color = Site)) +
  geom_point(size = 3) +
  theme +
  labs(y= CH4_lab, x= CO2_lab)

ggsave("Data/Graphs/CO2 vs CH4.jpg", width = 18, height = 6, dpi = 300)

ggplot(ghg_avg, aes(x=Date, y=CO2_avg, color = Date)) +
  geom_point()

ggplot(ghg_avg, aes(x= Site, y = CH4_avg, color = Season)) +
  geom_point()


# GHG with soil PW

ggplot(merge2, aes(x = Cl, y = CO2_avg, color = Site)) +
  geom_point(size = 6) +
  labs(x = "Soil porewater chloride (mg/L)", y= CO2_lab) +
  theme

ggsave("CO2 avg vs Cl.png", width = 18, height = 6, dpi = 300)

ggplot(merge2, aes(x = Cl, y = CH4_avg)) +
  geom_point(aes(color = Site), size = 6) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Soil porewater chloride (mg/L)", y= CH4_lab) +
  theme +
  scale_y_log10()

ggsave("CH4 avg vs Cl.png", width = 18, height = 6, dpi = 300)



ggplot(merge2, aes(x= Wetland.type, y= dCO2.umol, fill = Wetland.type)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(y= CO2_lab) +
  theme

ggplot(merge2, aes(x= Wetland.type, y= dCH4.umol, fill = Wetland.type)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(y= CH4_lab) +
  theme


ggplot(merge2, aes(x= Wetland.condition, y= dCO2.umol, fill = Wetland.condition)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(y= CO2_lab) +
  theme

ggplot(merge2, aes(x= Wetland.condition, y= dCH4.umol, fill = Wetland.condition)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(y= CH4_lab) +
  theme
