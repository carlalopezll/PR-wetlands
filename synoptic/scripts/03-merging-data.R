# Script for merging PR wetland synoptic sampling data and averaging water chem data

# load libraries
library(dplyr)
library(readr)
library(tidyverse)

# Read in data

ghg <- read_csv("synoptic/data/PR synoptic_GHG.csv")

ldi <- readxl::read_xlsx("synoptic/data/PR wetlands LDI-hydro.xlsx")

soilpw <- read_csv("synoptic/data/Soil porewater.csv")
# Filter soil PW for 1st depth only
soilpw <- filter(soilpw, Depth == 1)

chem <- read_csv("synoptic/data/Solubles w MDLs_PR synoptic.csv")

doc <- read_csv("synoptic/data/NPOC synoptic data_corrected.csv")
doc$Date_corrected <- as.Date(doc$Date_sampled, format = '%m/%d/%Y')

ic <- read_csv("synoptic/data/IC w MDLs_PR synoptic.csv")

# average the 3 reps
ghg_avg <- ghg %>%
  group_by(Site, Quadrant, Date) %>%
  summarise(
    CO2_uM = mean(wCO2_uM_med),
    CH4_uM = mean(wCH4_uM_med),
    CO2_mgL = mean(wCO2_mgL_med),
    CO2_gL = mean(wCO2_gL_med),
    CH4_mgL = mean(wCH4_mgL_med),
    Season = first(Season),
    Temp_C = mean(Temp_C),
    SpC = mean(SpC),
    Cond = mean(cond),
    DO_mgL = mean(DO_mgL),
    DO_perc = mean(DO_perc), 
    .groups = "drop"
  )

# average reps for chem data
# don't use Lachat NO3
chem_avg <- chem %>%
  group_by(Site, Date_corrected) %>%
  summarise(NH4_ppb_mean = mean(`NH4-N_ppb`),
         PO4_ppb_mean = mean(`PO4-P_ppb`))

# average reps for DOC data
DOC_avg <- doc %>%
  group_by(Site, Date_corrected) %>%
  summarise(DOC_avg = mean(DOC_mgL, na.rm = TRUE), 
            TDN_avg = mean(TDN_mgL, na.rm = TRUE))

# average reps for IC data
IC_avg <- ic %>%
  group_by(Site, Date_corrected) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE)*1000,
    Cl_avg = mean(Cl, na.rm = TRUE),
    Br_avg = mean(Br, na.rm = TRUE),
    NO3_N_avg = mean(`NO3-N`, na.rm = TRUE)*1000,
    SO4_avg = mean(SO4, na.rm = TRUE),
    Na_avg = mean(Na, na.rm = TRUE),
    K_avg = mean(K, na.rm = TRUE),
    Mg_avg = mean(Mg, na.rm = TRUE),
    Ca_avg = mean(Ca, na.rm = TRUE)
  )

# merging datasets

data_list <- list(ghg_avg, DOC_avg, IC_avg, chem_avg)

data_list <- lapply(data_list, function(df) {
  if ("Date" %in% names(df)) {
    df <- df %>% rename(Date_corrected = Date)
  }
  df
})

merge <- reduce(data_list, full_join, by = c("Site", "Date_corrected"))

# Merge LDI and GHG data
merge <- left_join(merge, ldi, by = c("Site" = "Wetland"))

# Merge soil PW data
# merge <- left_join(merge, soilpw, by = c("Site" = "Wetland"))

# write csv
write_csv(merge, "synoptic/data/synoptic merged.csv")
