# Script for merging PR wetland synoptic sampling data and averaging water chem data

# load libraries
library(dplyr)
library(readr)
library(tidyverse)

# Read in data ## need to fix this, this isn't the full GHG dataset

ghg <- read_csv("synoptic/data/PR synoptic_GHG.csv")

ghg$Date_corrected <- as.Date(ghg$Date, format = '%m/%d/%Y')

ldi <- readxl::read_xlsx("synoptic/data/LDI.xlsx")

soilpw <- read_csv("synoptic/data/Soil porewater.csv")
# Filter soil PW for 1st depth only
soilpw <- filter(soilpw, Depth == 1)

chem <- read_csv("synoptic/data/Solubles w MDLs_PR synoptic.csv")
chem$Date_corrected <- as.Date(chem$Date, format = '%m/%d/%Y')

doc <- read_csv("synoptic/data/NPOC synoptic data.csv")
doc$Date_corrected <- as.Date(doc$Date, format = '%m/%d/%Y')

ic <- read_csv("synoptic/data/IC w MDLs_PR synoptic.csv")
ic$Date_corrected <- as.Date(ic$Sample_date, format = '%m/%d/%Y')

# where are these files
# hydro <- read_csv("Data/GHG summary table w hydro.csv")

# average reps
ghg_avg <- ghg %>%
  group_by(Date, Site) %>%
  summarise(
    CO2_uM = mean(wCO2_uM_med),
    CH4_uM = mean(wCH4_uM_med),
    CO2_mgL = mean(wCO2_mgL_med),
    CH4_mgL = mean(wCH4_mgL_med),
    # Season = first(Season),
    waterTemp.C = mean(Temp_C),
    .groups = "drop"
  )

# average reps for chem data
chem_avg <- chem %>%
  group_by(Site, Date_corrected) %>%
  summarise(NH4_ppb_mean = mean(`NH4-N_ppb`),
         PO4_ppb_mean = mean(`PO4-P_ppb`))

# average reps for DOC data
DOC_avg <- doc %>%
  group_by(Site, Date_corrected) %>%
  summarise(DOC_avg = mean(NPOC, na.rm = TRUE), 
            TDN_avg = mean(TNb, na.rm = TRUE))

# average reps for IC data
IC_avg <- ic %>%
  group_by(Site, Date_corrected) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
    Cl_avg = mean(Cl, na.rm = TRUE),
    Br_avg = mean(Br, na.rm = TRUE),
    NO3_N_avg = mean(`NO3-N`, na.rm = TRUE),
    SO4_avg = mean(SO4, na.rm = TRUE),
    Na_avg = mean(Na, na.rm = TRUE),
    NH4_N_avg = mean(`NH4-N`, na.rm = TRUE),
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

ggplot(merge, aes(x=DOC_avg, y = CH4_uM)) +
  geom_point()

# Merge soil PW data
# merge <- left_join(merge, soilpw, by = c("Site" = "Wetland"))

# write csv
write_csv(merge, "synoptic/data/synoptic merged.csv")