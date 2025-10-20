# Plotting sensor data from PR wetlands
# Carla López Lloreda

# load libraries
library(ggplot2)
library(lubridate)
library(plotly)
library(readr)
library(ggbreak)
library(dplyr)
library(cowplot)
library(patchwork)
#### Conductivity ####

# read in merged data
cond <- read_csv("sensors/data/conductivity/merged_cond.csv")
do <- read_csv("sensors/data/DO/merged_do.csv")

# filter out do data when out of water
do <- filter(do, do_mgL < 6)
do <- filter(do, temp_C <32)

# Recode seasons and sites

cond <- cond %>%
  mutate(season = dplyr::recode(season, "winter" = "Winter", "summer" = "Summer"),
         site = dplyr::recode(site, "PA" = "Palmas", "TO" = "Tortuguero"))

do <- do %>%
  mutate(
    season = dplyr::recode(season, "winter" = "Winter", "summer" = "Summer"),
    site = dplyr::recode(site, "PA" = "Palmas", "TO" = "Tortuguero")
  )

# fix datetime
cond$datetime <- as.POSIXct(cond$datetime, format = "%m/%d/%Y %H:%M")
do$datetime <- as.POSIXct(do$datetime, format = "%m/%d/%Y %H:%M")

# Define the periods of interest
periods <- tribble(
  ~start, ~end, ~label,
  ymd("2024-07-01"), ymd("2024-08-17"), "Summer 2024",
  ymd("2025-01-11"), ymd("2025-01-19"), "Winter 2025"
)

# Add period labels to your data
do_filtered <- do %>%
  mutate(period = case_when(
    datetime >= periods$start[1] & datetime <= periods$end[1] ~ periods$label[1],
    datetime >= periods$start[2] & datetime <= periods$end[2] ~ periods$label[2],
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

cond_filtered <- cond %>%
  mutate(period = case_when(
    datetime >= periods$start[1] & datetime <= periods$end[1] ~ periods$label[1],
    datetime >= periods$start[2] & datetime <= periods$end[2] ~ periods$label[2],
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

# Plot sensor data distributions

p1 <- ggplot(cond, aes(x = temp_C, fill = season)) +
  geom_density(alpha = 0.5) +
  scale_x_log10()+
  labs(x= "Temperature (C)", y= "Probability density") +
  facet_wrap(~site, ncol = 1) +
  theme

p2 <- ggplot(cond, aes(x = cond_uS_cm, fill = season)) +
  geom_density(alpha = 0.5) +
  scale_x_log10()+
  labs(x = "Specific conductance (uS/cm)", y= "Probability density") +
  facet_wrap(~site, ncol = 1) +
  theme

p3 <- ggplot(do, aes(x= do_mgL, fill = season)) +
  geom_density(alpha = 0.5) +
  labs(x ="DO concentrations (mg/L)", y= "Probability density") +
  scale_x_log10() +
  ylim(0,2) +
  facet_wrap(~site, ncol = 1) +
  theme

(p1 | p2 | p3) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("Sensor density plots.jpg", width = 18, height = 8, dpi = 300)

# Inset  
  
inset <- ggplot(filter(do, site == "PA"), aes(x= do_mgL, fill = season)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  labs(x ="", y = "DO saturation (%)") +
  facet_wrap(~site, ncol = 1) +
  ylim(NA, 2) +
  theme(legend.position = "none")

inset <- ggplot(filter(do, site == "TO"), aes(x= do_mgL, fill = season)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  labs(x ="", y = "DO saturation (%)") +
  facet_wrap(~site, ncol = 1) +
  ylim(NA, 2) +
  theme(legend.position = "none")

inset

# Plots

theme <- theme_bw() +
  theme(text = element_text(size = 20))

library(RColorBrewer)
cols <- brewer.pal(9, "YlOrBr")

plotly::ggplotly(ggplot(do_filtered, aes(x = datetime, y = do_perc, color = site)) +
                   geom_point() +
                   facet_grid(~period, scales = "free"))

# High-frequency data multiplot

a <- ggplot(do_filtered, aes(x = datetime, y = temp_C, color = site)) +
  geom_point(size = 0.7, alpha = 0.4) +
  facet_grid(~period, scales = "free") +
  scale_x_datetime(date_breaks = "5 days", date_labels = "%Y-%m-%d") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "lightgrey"),
    panel.spacing = unit(1, "lines")
  ) +
  theme +
  labs(x = "", y = "Temperature (C)") +
  scale_color_manual(values = cols[6:7])

a

# ggsave("sensors/graphs/temp.jpg")

# DO plot with facet by period
b <- ggplot(do_filtered, aes(x = datetime, y = do_mgL, color = site)) +
  geom_point(size = 0.7, alpha = 0.4) +
  facet_grid(~period, scales = "free") +
  scale_x_datetime(date_breaks = "5 days", date_labels = "%Y-%m-%d") +
  labs(x = "", y = "DO (mg/L)") +
  theme +
  scale_color_manual(values = cols[6:7])

b

# ggsave("DO timeseries.jpg")

# COND plot with facet by period AND site
c <- ggplot(cond_filtered, aes(x = datetime, y = cond_uS_cm, color = site)) +
  geom_point(size = 0.7, alpha = 0.4) +
  facet_grid(~period, scales = "free") +
  scale_x_datetime(date_breaks = "5 days", date_labels = "%Y-%m-%d") +
  labs(x = "", y = "Specific conductance (uS/cm)") +
  theme +
  scale_color_manual(values = cols[6:7])

c

# For diagonal dates in the x-axis
# theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     strip.background = element_rect(fill = "lightgrey"),
#     panel.spacing = unit(1, "lines")
#   )

(a | b | c) + 
  plot_layout(nrow = 3, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("timeseries.jpg", width = 16, height = 10)


# Adding day and night bands

# Function to determine if it's day or night based on hour
is_day <- function(hour) {
  return(hour >= 6 & hour <= 18)  # Assuming day is between 6 AM and 6 PM
}

# Add a new column indicating if it's day or night
do_summer$dayz <- is_day(as.numeric(format(do_summer$Timestamp_corrected, "%H")))



ggplot(do_summer, aes(x = Timestamp)) +
  geom_point(aes(y = DO_mgL, color = as.factor(dayz))) +
  geom_line(aes(y = Temp_C / 60, color = 'Temperature'), shape = 2) +
  scale_y_continuous(name = "DO (mg/L)",
                     limits = c(0, 0.5),
                     sec.axis = sec_axis(~ . * 60, name = "Temperature (°C)", 
                                         breaks = scales::pretty_breaks(n = 5))) +
  xlim(as.POSIXct("2021-06-11 18:21:00"), as.POSIXct("2021-06-12 18:55:00")) +
  expand_limits(y = c(0.4, 0.6)) +
  theme +
  theme(legend.position = "none")

# CEN
ggplotly(ggplot(do_summer, aes(x = Timestamp, y = DO_mgL, color= dayz)) +
  geom_point() +
  xlim(as.POSIXct("2021-06-16 12:00:00"), as.POSIXct("2021-06-19 18:55:00")) +
  ylim(0,2) +
  theme +
  theme(legend.position = "none"))

# Manantial
ggplot(do_summer, aes(x = Timestamp, y = DO_mgL, color= dayz)) +
  geom_point() +
  xlim(as.POSIXct("2021-06-27 13:00:00"), as.POSIXct("2021-06-29 13:30:00")) +
  ylim(0,4) +
  theme +
  theme(legend.position = "none")

# Tortuguero

ggplot(do_summer, aes(x = Timestamp, y = DO_mgL, color= dayz)) +
  geom_point() +
  xlim(as.POSIXct("2021-06-29 14:00:00"), as.POSIXct("2021-07-01 14:30:00")) +
  ylim(0,0.5) +
  theme +
  theme(legend.position = "none")

# Highlight deployment periods

ggplot(do_summer, aes(x= Timestamp, y = Q, color = dayz)) +
  geom_point() +
  theme +
  theme(legend.position = "none")
  

ggsave("Q timeseries.jpg")

ggplot(do_summer, aes(x= Timestamp, y = DO_mgL, color = dayz)) +
  geom_point() +
  theme +
  labs(x= "Date", y = "DO (mg/L)") +
  theme(legend.position = "none")

ggsave("DO timeseries_short.jpg")

ggplot(do_summer, aes(x= Timestamp, y = Temp_C, color = dayz)) +
  geom_point() +
  theme +
  labs(x= "Date", y = "Temperature (C)") +
  theme(legend.position = "none")

ggsave("Temp timeseries.jpg")


ggplot(do_summer, aes(x=Temp_C, y= DO_mgL, color = Timestamp)) +
  geom_point()
