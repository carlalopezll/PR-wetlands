# Script for merging PR sensor data
# Carla López Lloreda

library(readr)
library(dplyr)

# read in conductivity files

cond_to_summer <- read_csv("sensors/data/conductivity/Tortuguero_COND_Summer2024.csv")
cond_to_winter <- read_csv("sensors/data/conductivity/Tortuguero_COND_Winter2025.csv")
cond_pa_summer <- read_csv("sensors/data/conductivity/Palmas_COND_Summer2024.csv")
cond_pa_winter <- read_csv("sensors/data/conductivity/Palmas_COND_Winter2025.csv")

# add site names
cond_to_summer$site <- "TO"
cond_to_winter$site <- "TO"

cond_pa_summer$site <- "PA"
cond_pa_winter$site <- "PA"

# merge datasets

# function that ignores the different column names
force_bind = function(df1, df2, df3, df4) {
  colnames(df4) = colnames(df3) = colnames(df2) = colnames(df1)
  bind_rows(df1, df2, df3, df4)
}

cond <- force_bind(cond_to_summer, cond_to_winter, cond_pa_summer, cond_pa_winter)

# rename columns
cond <- cond %>%
  rename(datetime = `Date Time, GMT-04:00`,
         cond_uS_cm = `Full Range, μS/cm (LGR S/N: 20059129, SEN S/N: 20059129)`,
         temp_C = `Temp, °C (LGR S/N: 20059129, SEN S/N: 20059129)`)

write.csv(cond, "sensors/data/conductivity/merged_cond.csv", row.names = FALSE)

# summer 2023 data includes measurements for the sites at different times (no )
do_2021 <- read_csv("sensors/data/DO/DO_all sites_Summer2021.csv")

# read in DO data

# merging summer 2024 and winter 2025 data
to_winter24 <- read_csv("sensors/data/DO/raw data/Winter 2024/Tortuguero_DO_Winter2024.csv")
pa_winter24 <- read_csv("sensors/data/DO/raw data/Winter 2024/Palmas_DO_Winter2024.csv")

to_summer24 <- read_csv("sensors/data/DO/raw data/Summer 2024/Tortuguero_DO_Summer2024.csv")
pa_summer24 <- read_csv("sensors/data/DO/raw data/Summer 2024/Palmas_DO_Summer2024.csv")

to_winter25 <- read_csv("sensors/data/DO/raw data/Winter 2025/Tortuguero_DO_Winter2025.csv")
pa_winter25 <- read_csv("sensors/data/DO/raw data/Winter 2025/Palmas_DO_Winter2025.csv")

# add site names
to_winter24$site <- "TO"
pa_winter24$site <- "PA"

to_winter25$site <- "TO"
pa_winter25$site <- "PA"

to_summer24$site <- "TO"
pa_summer24$site <- "PA"

# function that ignores the different column names
force_bind = function(df1, df2, df3, df4, df5, df6) {
  colnames(df6) = colnames(df5) = colnames(df4) = colnames(df3) = colnames(df2) = colnames(df1)
  bind_rows(df1, df2, df3, df4, df5, df6)
}

do <- force_bind(to_winter24, pa_winter24, to_winter25, pa_winter25, to_summer24, pa_summer24)

# rename columns
do <- do %>%
  rename(datetime = `Eastern Standard Time`,
         do_mgL = `Dissolved Oxygen`,
         do_perc = `Dissolved Oxygen Saturation`,
         temp_C = Temperature)

write.csv(do, "sensors/data/DO/merged_do.csv", row.names = FALSE)

# merge cond and do data

