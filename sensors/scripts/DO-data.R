library(ggplot2)
library(lubridate)
library(plotly)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Humedales Puerto Rico/Data/DO data")

# Water level data

wl_pa <- read_csv("sensors/water level/Palmas_Summer2024.csv")

wl_pa$Timestamp_corrected <- as_datetime(wl_pa$timestamp)
wl_to <- read_csv("sensors/water level/Tortuguero_Summer2024.csv")

# DO data for PR

do_pr <- read.csv("sensors/DO data/Data with datetime.csv")
do_summer <- read_csv("sensors/DO data/Summer DO.csv")
do_winter <- read_csv("sensors/DO data/Winter 2024 deployments/7450-695497 - Copy/Winter 2024 DO.csv")

do_winter <- filter(do_winter, Timestamp > "2023-12-27 15:00")

do_palmas <- read_csv("Summer 2024/Palmas/Palmas_summer2024.csv")

do_tortuguero <- read_csv("Summer 2024/Tortuguero/Tortuguero_summer2024.csv")

# Palmas


ggplot(wl_pa, aes(x= num, y = cond_us_cm)) +
  geom_point() +
  xlab("") +
  ylim(200, 1400)

ggsave("Palmas cond timeseries.jpg")

ggplot(wl_to, aes(x= num, y = cond_us_cm)) +
  geom_point() +
  xlab("") +
  ylim(200,1400)

ggsave("Tortuguero condtimeseries.jpg")



ggplot(wl_pa, aes(x= num, y = temp_C)) +
  geom_point() +
  xlab("")

ggsave("Palmas temp timeseries.jpg")

ggplot(wl_to, aes(x= num, y = temp_C)) +
  geom_point() +
  xlab("")

ggsave("Tortuguero temp timeseries.jpg")




ggplotly(ggplot(do_palmas, aes(x = Timestamp, y = DO_mgL)) +
           geom_point() +
           theme(legend.position = "none") +
           ylim(0,3))

ggsave("Palmas.jpg")

ggplotly(ggplot(do_palmas, aes(x = Timestamp, y = Temp_C)) +
           geom_point() +
           theme(legend.position = "none"))

ggsave("Palmas.jpg")



ggplotly(ggplot(do_tortuguero, aes(x = Timestamp, y = DO_mgL)) +
           geom_point() +
           theme(legend.position = "none") +
           ylim(0,6))

ggsave("Tortuguero.jpg")










ggplotly(ggplot(do_summer, aes(x = Timestamp, y = Temp_C, color = dayz)) +
           geom_point() +
           xlim(as.POSIXct("2021-06-11 18:21:00"), as.POSIXct("2021-06-12 18:55:00")) +
           theme +
           theme(legend.position = "none"))

# Fix time

do_summer$Timestamp <- as.POSIXct(do_summer$`Time (sec)`, origin="1970-01-01", tz="UTC")

# Changing columns names
colnames(do_summer)[3] <- "Temp_C"
colnames(do_summer)[4] <- "DO_mgL"

colnames(do_winter)[3] <- "Timestamp"
colnames(do_winter)[6] <- "DO_mgL"

theme <- theme_bw() +
  theme(text = element_text(size = 20))

# Adding day and night bands

# Function to determine if it's day or night based on hour
is_day <- function(hour) {
  return(hour >= 6 & hour <= 18)  # Assuming day is between 6 AM and 6 PM
}

# Add a new column indicating if it's day or night
do_summer$dayz <- is_day(as.numeric(format(do_summer$Timestamp_corrected, "%H")))

day_lims <- 

# Palmas
ggplotly(ggplot(do_summer, aes(x = Timestamp, y = DO_mgL, color = dayz)) +
  geom_point() +
  xlim(as.POSIXct("2021-06-11 18:21:00"), as.POSIXct("2021-06-12 18:55:00")) +
  ylim(0,0.5) +
  theme +
  theme(legend.position = "none"))

ggplotly(ggplot(do_summer, aes(x = Timestamp, y = Temp_C, color = dayz)) +
           geom_point() +
           xlim(as.POSIXct("2021-06-11 18:21:00"), as.POSIXct("2021-06-12 18:55:00")) +
           theme +
           theme(legend.position = "none"))



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

library(plotly)
ggplotly(ggplot(do_winter, aes(x = Timestamp, y = DO_mgL)) +
  geom_point())

ggplotly(ggplot(do_winter, aes(x = Timestamp, y = Temperature)) +
           geom_point())
