# Graphs for ESA

library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)

# For Tukey
library(emmeans)
library(multcompView)
library(multcomp)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Humedales Puerto Rico")

# Read in GHG data
ghg_pr <- read_csv("Data/PR wetlands_GHG_NEON.csv")

hydro <- read_csv("Data/GHG summary table w hydro.csv")

waterchem <- read_csv("Data/Water chemistry table.csv")
doc <- read_csv("Data/DOC_TDN.csv")

# Read in LDI data
ldi <- read.csv("LDI.csv")

# Read in soil PW data
soilpw <- read.csv("Data/Soil porewater.csv")

# Filter for 1st depth only
soilpw <- filter(soilpw, Depth == 1)

# Remove test and blanks

ghg_pr <- filter(ghg_pr, !is.na(Rep))

# Change to standard date format
ghg_pr$Date_corrected <- as.Date(ghg_pr$Date, format = '%m/%d/%Y')

# Theme stuff 
theme <- theme_bw() +
  theme(text = element_text(size = 35))

# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))

# Function to get season
getSeason <- function(d) {
  WS <- as.Date("2021-12-21") # Winter Solstice
  SE <- as.Date("2021-3-19") # Spring Equinox
  SS <- as.Date("2021-5-20") # Summer Solstice
  FE <- as.Date("2021-9-22") # Fall Equinox
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


# Get season for each date
ghg_pr$Season <- getSeason(ghg_pr$Date_corrected)

ggplot(ghg_pr, aes(x=Season, y = dCO2.umol, fill = Season)) +
  geom_boxplot() +
  geom_point() +
  scale_y_log10() +
  theme +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(size = 20)) +
  labs(y=CO2_lab, x = "") +
  facet_grid(.~ Site)

ggsave("CO2 by season.png", width = 18, height = 6, dpi = 300)

ggplot(ghg_pr, aes(x=Season, y = dCH4.umol, fill = Season)) +
  geom_boxplot() +
  geom_point() +
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


# Average the 3 reps
ghg_avg <- ghg_pr %>%
  group_by(Site, Date_corrected) %>%
  summarise(Date= first(Date_corrected),
            Site = first(Site), 
            CO2_avg = mean(dCO2.umol, na.rm = TRUE), 
            CH4_avg = mean(dCH4.umol, na.rm = TRUE))

ggplot(ghg_avg, aes(x= Date_corrected, y = CO2_avg, color = Site)) +
  geom_point()


# Calculate the statistics per site and add Tukey's HSD letters
stats_summary_per_site <- ghg_avg %>%
  group_by(Site) %>%
  summarise(
    CO2 = calculate_stats(CO2_avg),
    CH4 = calculate_stats(CH4_avg)
  )

write_csv(stats_summary_per_site, "Data/GHG summary table.csv")

GHG_means <- ghg_avg %>%
  group_by(Site) %>%
  summarise(
    CO2 = mean(CO2_avg),
    CH4 = mean(CH4_avg)
  )

write_csv(GHG_means, "Data/GHG means.csv")


ggplot(waterchem, aes(x= Site, y= F, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width=0) +
  theme +
  theme(legend.position = "none")


# Average the 3 reps
DOC_avg <- doc %>%
  group_by(Sample) %>%
  summarise(Sample = first(Sample),
            Site = first(Site),
            DOC_avg = mean(DOC, na.rm = TRUE), 
            TDN_avg = mean(TDN, na.rm = TRUE))

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

# Getting averages

site_avg <- ghg_pr %>%
  filter(!is.na(Site)) %>%
  group_by(Site) %>%
  summarise(Site = first(Site), 
            CO2_avg = mean(dCO2.umol, na.rm = TRUE), 
            CH4_avg = mean(dCH4.umol, na.rm = TRUE), 
            CO2_min = min(dCO2.umol, na.rm = TRUE),
            CO2_max = max(dCO2.umol, na.rm = TRUE),
            CH4_min = min(dCH4.umol, na.rm = TRUE),
            CH4_max = max(dCH4.umol, na.rm = TRUE))


# Adding to original spreadsheet

ghg <- left_join(ghg_pr, site_avg, by = "Site")

# Merge LDI and GHG data
merge <- left_join(ghg, ldi, by = c("Site" = "Wetland"))

# Merge soil PW data
merge2 <- left_join(merge, soilpw, by = c("Site" = "Wetland","Wetland.type" = "Wetland.type"))




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
