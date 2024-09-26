
library(ggplot2)
library(lubridate)
library(dplyr)

# For Tukey
library(emmeans)
library(multcompView)
library(multcomp)

# Read in GHG data
ghg_pr <- read.csv("Data/PR wetlands_GHG_NEON.csv")

# Read in LDI data
ldi <- read.csv("LDI.csv")

# Read in soil PW data
soilpw <- read.csv("Data/Soil porewater.csv")

# Filter for 1st depth only
soilpw <- filter(soilpw, Depth == 1)


# Getting averages

ghg_avg <- ghg_pr %>%
  group_by(Site) %>%
  summarise(Site = first(Site), 
            CO2_avg = mean(dCO2.umol, na.rm = TRUE), 
            CH4_avg = mean(dCH4.umol, na.rm = TRUE), 
            CO2_min = min(dCO2.umol, na.rm = TRUE),
            CO2_max = max(dCO2.umol, na.rm = TRUE),
            CH4_min = min(dCH4.umol, na.rm = TRUE),
            CH4_max = max(dCH4.umol, na.rm = TRUE))

# Adding to original spreadsheet

ghg <- left_join(ghg_pr, ghg_avg, by = "Site")

# Merge LDI and GHG data
merge <- left_join(ghg, ldi, by = c("Site" = "Wetland"))

# Merge soil PW data
merge2 <- left_join(merge, soilpw, by = c("Site" = "Wetland"))

# Remove the test and blank data

ghg_pr2 <- merge2[-c(54:65), ]

# Correct date

ghg_pr2$Date_corrected <- as.Date(parse_date_time(ghg_pr2$Date, c("mdy", "ymd")))

# Add the Site column (two letters), if the spreadsheet doesn't have the Site column
ghg_pr2$Site_ID <- substr(ghg_pr2 [ , 4], start= 1, stop= 4) # Make sure that you have the right column for Site_ID

# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))

theme <- theme_bw() +
  theme(text = element_text(size = 20))


# Plotzzzz

ggplot(ghg, aes(x= Wetland.type.x, y= dCO2.umol, fill = Wetland.type.x)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(y= CO2_lab) +
  theme

# Tukey test on CO2 across wetland type
ANOVA_CO2 <- aov(ghg_pr2$dCO2.umol~ghg_pr2$Wetland.type.x)
summary(ANOVA_CO2)
TUKEY_CO2 <- TukeyHSD(ANOVA_CO2)
cld_CO2 <- multcompLetters4(ANOVA_CO2, TUKEY_CO2)
dt_CO2 <- ghg_pr2 %>% 
  group_by(Wetland.type.x) %>%
  summarise(w = mean(dCO2.umol), sd = sd(dCO2.umol)) %>%
  arrange(desc(w))
cld_CO2 <- as.data.frame.list(cld_CO2$`ghg_pr2$Wetland.type.x`)
dt_CO2$cld <- cld_CO2$Letters
dt_CO2$dCO2.umol <- dt_CO2$w

# ANOVA for CO2
summary(ANOVA_CO2)
print(TUKEY_CO2)


ggplot(NULL, aes(x = reorder(Wetland.type.x, -dCO2.umol, mean), y = dCO2.umol)) + 
  geom_boxplot(data = ghg_pr2, aes(x = reorder(Wetland.type.x, -dCO2.umol, mean), y = dCO2.umol, fill = Wetland.type.x)) + 
  geom_jitter(data = ghg_pr2, width = 0.1) +
  labs(x = "", y = CO2_lab, tag = "a)") +
  theme +
  theme(axis.title.x=element_blank(), axis.title = element_text(size=20), axis.text = element_text(size = 20)) +
  geom_label(data = dt_CO2, aes(x = Wetland.type.x, y = w + sd + 150, label = cld), size = 6, alpha = 1)





ggplot(ghg_pr2, aes(x= Wetland.type.x, y= dCH4.umol)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(y= CO2_lab) +
  theme




# Tukey test on CH4 across wetland type
ANOVA_CH4 <- aov(ghg_pr2$dCH4.umol~ghg_pr2$Wetland.type.x)
summary(ANOVA_CH4)
TUKEY_CH4 <- TukeyHSD(ANOVA_CH4)
cld_CH4 <- multcompLetters4(ANOVA_CH4, TUKEY_CH4)
dt_CH4 <- ghg_pr2 %>% 
  group_by(Wetland.type.x) %>%
  summarise(w = mean(dCH4.umol), sd = sd(dCH4.umol)) %>%
  arrange(desc(w))
cld_CH4 <- as.data.frame.list(cld_CH4$`ghg_pr2$Wetland.type.x`)
dt_CH4$cld <- cld_CH4$Letters
dt_CH4$dCH4.umol <- dt_CH4$w

# ANOVA for CH4
summary(ANOVA_CH4)
print(TUKEY_CH4)


ggplot(NULL, aes(x = reorder(Wetland.type.x, -dCH4.umol, mean), y = dCH4.umol)) + 
  geom_boxplot(data = ghg_pr2, aes(x = reorder(Wetland.type.x, -dCH4.umol, mean), y = dCH4.umol, fill = Wetland.type.x)) + 
  geom_jitter(data = ghg_pr2, width = 0.1) +
  labs(x = "", y = CH4_lab, tag = "a)") +
  theme +
  theme(axis.title.x=element_blank(), axis.title = element_text(size=20), axis.text = element_text(size = 20)) +
  geom_label(data = dt_CH4, aes(x = Wetland.type.x, y = w + sd + 150, label = cld), size = 6, alpha = 1)





ggsave("CO2 boxplot.jpg")

ggplot(ghg_pr2, aes(x= Site, y= dCO2.umol, fill = Wetland.type.x)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(y= CO2_lab) +
  theme +
  theme(legend.position = c(0.8, 0.2))

ggsave("CO2 boxplot.jpg")




# Tukey test on CO2 across sites
ANOVA_CO2 <- aov(ghg_pr2$dCO2.umol~ghg_pr2$Site)
summary(ANOVA_CO2)
TUKEY_CO2 <- TukeyHSD(ANOVA_CO2)
cld_CO2 <- multcompLetters4(ANOVA_CO2, TUKEY_CO2)
dt_CO2 <- ghg_pr2 %>% 
  group_by(Site) %>%
  summarise(w = mean(dCO2.umol), sd = sd(dCO2.umol)) %>%
  arrange(desc(w))
cld_CO2 <- as.data.frame.list(cld_CO2$`ghg_pr2$Site`)
dt_CO2$cld <- cld_CO2$Letters
dt_CO2$dCO2.umol <- dt_CO2$w

# ANOVA for CO2
summary(ANOVA_CO2)
print(TUKEY_CO2)


ggplot(NULL, aes(x = reorder(Site, -dCO2.umol, mean), y = dCO2.umol)) + 
  geom_boxplot(data = ghg_pr2, aes(x = reorder(Site, -dCO2.umol, mean), y = dCO2.umol, fill = Wetland.type.x)) + 
  geom_jitter(data = ghg_pr2, width = 0.1) +
  labs(x = "", y = CO2_lab, tag = "a)") +
  theme +
  theme(axis.title.x=element_blank(), axis.title = element_text(size=20), axis.text = element_text(size = 20)) +
  geom_label(data = dt_CO2, aes(x = Site, y = w + sd + 150, label = cld), size = 6, alpha = 1)


# Tukey test on CH4 across sites
ANOVA_CH4 <- aov(ghg_pr2$dCH4.umol~ghg_pr2$Site)
summary(ANOVA_CH4)
TUKEY_CH4 <- TukeyHSD(ANOVA_CH4)
cld_CH4 <- multcompLetters4(ANOVA_CH4, TUKEY_CH4)
dt_CH4 <- ghg_pr2 %>% 
  group_by(Site) %>%
  summarise(w = mean(dCH4.umol), sd = sd(dCH4.umol)) %>%
  arrange(desc(w))
cld_CH4 <- as.data.frame.list(cld_CH4$`ghg_pr2$Site`)
dt_CH4$cld <- cld_CH4$Letters
dt_CH4$dCH4.umol <- dt_CH4$w

# ANOVA for CH4
summary(ANOVA_CH4)
print(TUKEY_CH4)


ggplot(NULL, aes(x = reorder(Site, -dCH4.umol, mean), y = dCH4.umol)) + 
  geom_boxplot(data = ghg_pr2, aes(x = reorder(Site, -dCH4.umol, mean), y = dCH4.umol, fill = Wetland.type.x)) + 
  geom_jitter(data = ghg_pr2, width = 0.1) +
  labs(x = "", y = CH4_lab, tag = "a)") +
  theme +
  theme(axis.title.x=element_blank(), axis.title = element_text(size=20), axis.text = element_text(size = 20)) +
  geom_label(data = dt_CH4, aes(x = Site, y = w + sd + 20, label = cld), size = 6, alpha = 1) + scale_y_log10()


ggplot(ghg_pr2, aes(x= dCO2.umol, y= dCH4.umol, color = Site)) +
  geom_point(size = 2.5) +
  labs(x = CO2_lab, y = CH4_lab) +  theme

ggsave("CH4 vs CO2.jpg")

ggplot(ghg_pr2, aes(x= dCO2.umol, y= dCH4.umol)) +
  geom_point(size = 2.5) +
  labs(x = CO2_lab, y = CH4_lab) +
  theme

ggsave("CH4 v CO2_no color.jpg")

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

# GHG with LDI variable

ggplot(ghg_pr2, aes(x = LDI.index, y = CO2_avg, color = Site)) +
  geom_point(size = 3) +
  labs(x = "LDI index", y= CO2_lab) +
  theme

summary(lm(CO2_avg~LDI.index, data = ghg_pr2))

ggsave("CO2 vs LDI.jpg")

ggplot(ghg_pr2, aes(x = LDI.index, y = CH4_avg, color = Site)) +
  geom_point(size = 3) +
  labs(x = "LDI index", y= CH4_lab) +
  theme +
  scale_y_log10()

summary(lm(CH4_avg~LDI.index, data = ghg_pr2))

ggsave("CH4 vs LDI.jpg")






ggplot(ghg_pr2, aes(x = TOC, y = CO2_avg, color = Site)) +
  geom_point(size = 3) +
  labs(x = "Soil PW TOC (mg/L)", y= CO2_lab) +
  theme

summary(lm(CO2_avg~TOC, data = ghg_pr2))

ggsave("CO2 vs TOC avg.jpg")

ggplot(ghg_pr2, aes(x = TOC, y = CH4_avg, color = Site)) +
  geom_point(size = 3) +
  labs(x = "Soil PW TOC (mg/L)", y= CH4_lab) +
  theme +
  scale_y_log10()

summary(lm(CH4_avg~TOC, data = ghg_pr2))

ggsave("CH4 vs TOC avg.jpg")





# GHG concentrations over time

ggplot(ghg_pr, aes(x=Date, y = dCO2.umol, color = Site)) +
  geom_point(size = 3) +
  theme +
  labs(x = "", y = CO2_lab) +
  theme(text = element_text(size = 15))

ggsave("CO2 over time.jpg")

ggplot(ghg_pr2, aes(x=Date, y = dCH4.umol, color = Site)) +
  geom_point(size = 3) +
  theme +
  labs(x = "", y = CH4_lab) +
  theme(text = element_text(size = 15))

ggsave("CH4 over time.jpg")

# Subset out sites that had quadrants sampled
# Do it for each sampling date

ghg_pr3 <- filter(ghg_pr2, Site == "Palmas" | Site == "Arroyo" | Site == "Lo?za")
ghg_pr3_2020 <- filter(ghg_pr3, Date_corrected == "2020-11-20" | Date_corrected == "2020-11-25" | Date_corrected == "2020-12-02")
ghg_pr3_2021 <- filter(ghg_pr3, Date_corrected == "2021-06-28" | Date_corrected == "2021-06-11")

ggplot(ghg_pr3_2020, aes(x= Site_ID, y= dCO2.umol, fill = Site)) +
  geom_boxplot() +
  geom_jitter()

ggsave("CO2_wetland het.jpg")

ggplot(ghg_pr3_2021, aes(x= Site_ID, y= dCO2.umol, fill = Site)) +
  geom_boxplot() +
  geom_jitter()

ggplot(ghg_pr3_2020, aes(x= Site_ID, y= dCH4.umol, fill = Site)) +
  geom_boxplot() +
  geom_jitter() + scale_y_log10()

ggsave("CH4_wetland het.jpg")

ggplot(ghg_pr3_2021, aes(x= Site_ID, y= dCH4.umol, fill = Site)) +
  geom_boxplot() +
  geom_jitter() + scale_y_log10()
