# Plotting sensor data from PR wetlands
# Carla López Lloreda

# load librarites
library(ggplot2)
library(lubridate)
library(plotly)
library(readr)
library(ggbreak)
library(dplyr)

#### Conductivity ####

# read in merged data
cond <- read_csv("sensors/data/conductivity/merged_cond.csv")
do <- read_csv("sensors/data/DO/merged_do.csv")

# fix datetime
cond$datetime <- as.POSIXct(cond$datetime, format = "%m/%d/%Y %H:%M")

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

#### Calculating DO metrics ####

do_filtered$month <- month(do_filtered$datetime)

do_filtered$season <- ifelse(do_filtered$month >= 3 & do_filtered$month <= 5, "Spring",
                             ifelse(do_filtered$month >= 6 & do_filtered$month <= 8, "Summer",
                                    ifelse(do_filtered$month >= 9 & do_filtered$month <= 11, "Fall", "Winter")))

do_metrics_daily <- do_filtered %>%
  mutate(date = as.Date(datetime)) %>%  # extract date
  group_by(site, date) %>%
  summarise(
    season = first(season),
    DO_avg = mean(do_mgL, na.rm = TRUE),
    DO_amp = max(do_mgL, na.rm = TRUE) - min(do_mgL, na.rm = TRUE),
    temp_avg = mean(temp_C, na.rm = TRUE),
    temp_amp = max(temp_C, na.rm = TRUE) - min(temp_C, na.rm = TRUE)
  ) %>%
  ungroup()

do_metrics <- do_metrics_daily %>%
  group_by(site, season) %>%
  summarise(
    DO_avg = mean(DO_avg),
    DO_amp_avg = mean(DO_amp),
    temp_avg = mean(temp_avg),
    temp_amp_avg = mean(temp_amp)
  )

write_csv(do_metrics, "DO_metrics.csv")


#### Calculating cond metrics ####

cond_filtered$month <- month(cond_filtered$datetime)

cond_filtered$season <- ifelse(cond_filtered$month >= 3 & cond_filtered$month <= 5, "Spring",
                             ifelse(cond_filtered$month >= 6 & cond_filtered$month <= 8, "Summer",
                                    ifelse(cond_filtered$month >= 9 & cond_filtered$month <= 11, "Fall", "Winter")))

cond_metrics_daily <- cond_filtered %>%
  mutate(date = as.Date(datetime)) %>%  # extract date
  group_by(site, date) %>%
  summarise(
    season = first(season),
    cond_avg = mean(cond_uS_cm, na.rm = TRUE),
    cond_amp = max(cond_uS_cm, na.rm = TRUE) - min(cond_uS_cm, na.rm = TRUE),
    temp_avg = mean(temp_C, na.rm = TRUE),
    temp_amp = max(temp_C, na.rm = TRUE) - min(temp_C, na.rm = TRUE)
  ) %>%
  ungroup()

cond_metrics <- cond_metrics_daily %>%
  group_by(site, season) %>%
  summarise(
    cond_avg = mean(cond_avg),
    cond_amp_avg = mean(cond_amp),
    temp_avg = mean(temp_avg),
    temp_amp_avg = mean(temp_amp)
  )

write_csv(cond_metrics, "cond_metrics.csv")
