########################################
# Equations to used to estimate CO2 and CH4 from headspace sampling 
# Coded/checked/updated by ERH from UQAM/Krycklan files
# Code updated for Delmarva Project by ERH
# Last update: 20240215 by CLL
# CH4 calculations are fixed
########################################

# Load libraries
library(dplyr) # data manipulation
library(tidyr) # reshaping data functions
library(readr) # reading and writing csvs
library(udunits2) # unit conversions
library(lubridate)
library(scales)
library(methods)
library(ggplot2)

############# A. LOAD FUNCTIONS #############

############# A. LOAD FUNCTIONS #############

##### [1] SOLUBILITY CONSTANT FOR CO2 #####
# as in Weiss 1974 for units of mol L-1 atm-1 (also in Demarty et al 2011; many others)
KH.CO2 <- function(tempC){
  tempK <- tempC + 273.15
  KH.CO2 <- exp( -58.0931 + (90.5069*(100/tempK)) +22.2940*log((tempK/100), base=exp(1)) )
  KH.CO2
}

##### [2] SOLUBILITY CONSTANT FOR CH4 #####
# same constants as in Demarty et al 2011 Biogeosciences, which references Lide 2007 Handbook of Chemistry and Physics
# units are mol L-1 atm-1
KH.CH4 <- function(tempC){
  tempK <- tempC + 273.15
  KH.CH4  <- (exp(-115.6477 + (155.5756/((tempK)/100))+65.2553* log( ((tempK)/100), base=exp(1) ) -6.1698*((tempK)/100))*1000/18.0153)*1000
  KH.CH4
}

##### [3] FUNCTION TO ESTIMATE STREAM pCO2 from headspace sample data (what the GC gives you) ####
# temp inputs are in C
# pressure input is kPa IN LAB (used for molar volume at analysis)
# gasV and waterV units must be the same (to make equilibration headspace ratio)
# pCO2 of sample = what the GC or picarro gives you # uatm or ppmv
# pCO2 of hs = what you add to create headspace # uatm or ppmv
# 101.325 kPa = 1 atm
# 0.082057 = R = universal gas constant in L*atm / mol*K
StmCO2fromSamp <- function(tempLab.C, tempSite.C, kPa, gasV, waterV, pCO2.samp, pCO2.hs){
  tempLab.K <- tempLab.C + 273.15
  molV <- 0.082057*(tempLab.K)*(101.325/kPa) # L mol-1
  hsRatio <- gasV/waterV
  KH.Lab <- KH.CO2(tempLab.C) # mol L-1 atm-1
  KH.Site <- KH.CO2(tempSite.C) # mol L-1 atm-1
  StmCO2 <- (pCO2.samp*KH.Lab + (hsRatio*(pCO2.samp-pCO2.hs) / molV )  ) /KH.Site
  StmCO2
}

##### [4] FUNCTION TO CONVERT pCO2 from uatm to umol/m3 ####
# 1 m3 = 1000 L; convert umol/m3 to umol/L = Fw/1000
FwCO2 <- function(tempC, CO2w.uatm){
  # Parameters and units:
  # CO2w.uatm = uatm
  # assume uatm = ppmv
  # R = L atm / K mol
  R <- 0.08205601 # from Weiss 1974
  # Tk = K
  tempK <- tempC + 273.15
  # convert CO2, water (uatm) to umol/m3 
  CO2wM <- CO2w.uatm*(1/R)*(1/(10^-3))*(1/tempK)
  CO2wM
}

##### [5] FUNCTION TO ESTIMATE STREAM pCH4 from headspace sample data (what the Picarro or GC gives you) ####
# temp inputs are in C
# pressure input is kPa IN LAB (used for molar volume at analysis)
# gasV and waterV units must be the same (to make equilibration headspace ratio)
# pCH4 of sample = what the GC or picarro gives you # uatm or ppmv
# pCH4 of hs = what you add to create headspace # uatm or ppmv
# pkH = solubility at sampling temp = M atm-1
# 101.325 kPa = 1 atm
# 0.082057 = R = universal gas constant in L*atm / mol*K
StmCH4fromSamp <- function(tempLab.C, tempSite.C, kPa, gasV, waterV, pCH4.samp, pCH4.hs){
  tempLab.K <- tempLab.C + 273.15
  molV <- 0.082057*(tempLab.K)*(101.325/kPa) # L mol-1 calculated for lab conditions
  hsRatio <- gasV/waterV
  KH.Lab <- KH.CH4(tempLab.C) # mol L-1 atm-1
  KH.Site <- KH.CH4(tempSite.C) # mol L-1 atm-1
  StmCH4 <- (pCH4.samp*KH.Lab + (hsRatio*(pCH4.samp-pCH4.hs) / molV )  ) /KH.Site
  StmCH4
}


##### [6] FUNCTION TO CONVERT pCH4 from uatm to umol/m3 ####
# 1 m3 = 1000 L; convert umol/m3 to umol/L = Fw/1000
FwCH4 <- function(tempC, CH4w.uatm){
  # Parameters and units:
  # CH4w.uatm = uatm
  # assume uatm = ppmv
  # R = L atm / K mol
  R <- 0.08205601 # from Weiss 1974
  # Tk = K
  tempK <- tempC + 273.15
  # convert CO2, water (uatm) to umol/m3 
  CH4wM <- CH4w.uatm*(1/R)*(1/(10^-3))*(1/tempK)
  CH4wM
}

########################################

############# B. LOAD/MERGE DATA FILES #############

# LOAD files from (1) GC output, (2) lab notes with headspace volumes, and (3) any different site IDs

# Read GCHeadspace with GC data

GHG <- readxl::read_excel("synoptic/data/PR GHG sampling data.xlsx") %>%
  filter(!is.na(Rep)) %>%
  mutate(Date = as.Date(Date),          # ensure it's Date class
    Site = as.character(Site))

field <- read_csv("synoptic/data/Synoptic field data.csv") %>%
  mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
    Site = as.character(Site))

# Join field data to GHG
GHG_field <- left_join(GHG, field, by = c("Site", "Quadrant", "Date"))

# Remove tests and blanks
GHG_field <- filter(GHG_field, !Air_Location == "test")

# Check for sites w/o temp
na_rows <- GHG_field[!complete.cases(GHG_field$Temp_C), ]

# if temperature is NA, use 25C for purposes of calculating GHG

GHG_field$Temp_C[is.na(GHG_field$Temp_C)] <- 25

# Replace negative CH4 atmospheric values with NA
### THIS IS ACTUALLY A PROBLEM BECAUSE THEN A LOT OF THE SAMPLING DATES DON'T HAVE EVEN ONE AIR REP ###
### SHOULD I REPLACE WITH 0? THAT ALSO FEELS PROBLEMATIC
### MAYBE JUST USE AN AVERAGE

neg_CH4 <- filter(GHG_field, CH4_ppm < 0)

GHG_field <- GHG_field %>%
  mutate(CH4_ppm = if_else(CH4_ppm <0, 1.8, CH4_ppm))

# Filter air samples 

Air <- GHG_field %>%
  dplyr::filter(Rep == "Air")

# Looking at 3 air reps and flagging any outlier

Air <- Air %>%
  group_by(Site, Date) %>%
  mutate(median_CH4 = median(CH4_ppm, na.rm = TRUE),
         iqr_CH4 = IQR(CH4_ppm, na.rm = TRUE),
         outlier_flag_CH4 = abs(CH4_ppm - median_CH4) > 1.5 * iqr_CH4,
         median_CO2 = median(CO2_ppm, na.rm = TRUE),
         iqr_CO2 = IQR(CO2_ppm, na.rm = TRUE),
         outlier_flag_CO2 = abs(CO2_ppm - median_CO2) > 1.5 * iqr_CO2)

ggplot(Air, aes(x = Date_analyzed, y = CH4_ppm)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Air, aes(x = Air_Location, y = CH4_ppm, color = outlier_flag_CH4)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Air, aes(x = Air_Location, y = CO2_ppm, color = outlier_flag_CO2)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summarize air data for different sites
# add column to data file with hsCO2_ppm & hsCH4_ppm (e.g., LabAir, JL Air)
# Air_Location is the ID that will match up with air-water

# Filter air samples, group by location and calculate median, max and min

Air_summary <- Air %>%
  group_by(Air_Location) %>%
  summarize(AirCO2_min_ppm = min(CO2_ppm, na.rm = TRUE), AirCO2_med_ppm = median(CO2_ppm, na.rm = TRUE), 
                   AirCO2_max_ppm = max(CO2_ppm, na.rm = TRUE), AirCH4_min_ppm = min(CH4_ppm, na.rm = TRUE),
                   AirCH4_med_ppm = median(CH4_ppm, na.rm = TRUE), AirCH4_max_ppm = max(CH4_ppm, na.rm = TRUE),
                   Site = first(Site),
                   Date = first(Date))

# Save GHG air concentrations to a csv
write.csv(Air_summary, "synoptic/data/Synoptic air GHG summary.csv", row.names = FALSE)

# Adding summary air columns to GHG
GHG_new <- left_join(GHG_field, Air_summary, by = c("Air_Location", "Site", "Date"))

########################################

############# C. CONVERT GC DATA TO STREAM CO2/CH4 #############

# Estimate stream CO2/CH4 from GC headspace (uatm) 
# NOTES: Lab temp fixed at 20C
# Pressure fixed at 101.325 kPa (pressure at 0m ASL)

samp <- GHG_new

# subset the data to exclude air samples
samp <- GHG_new[ which(GHG_new$Rep!="Air"), ]

# StmCO2fromSamp <- function(tempLab.C, tempSite.C, kPa, gasV, waterV, pCO2.samp, pCO2.hs)

# This is pCO2 (uatm)
samp$wCO2_uatm_medhs <- StmCO2fromSamp(tempLab.C=20, tempSite.C=samp$Temp_C, kPa=101.325, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCO2.samp=samp$CO2_ppm, pCO2.hs=samp$AirCO2_med_ppm)
samp$wCO2_uatm_minhs <- StmCO2fromSamp(tempLab.C=20, tempSite.C=samp$Temp_C, kPa=101.325, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCO2.samp=samp$CO2_ppm, pCO2.hs=samp$AirCO2_min_ppm)
samp$wCO2_uatm_maxhs <- StmCO2fromSamp(tempLab.C=20, tempSite.C=samp$Temp_C, kPa=101.325, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCO2.samp=samp$CO2_ppm, pCO2.hs=samp$AirCO2_max_ppm)

# StmCH4fromSamp <- function(tempLab.C, tempSite.C, kPa, gasV, waterV, pCH4.samp, pCH4.hs)

# This is pCH4 (uatm)
samp$wCH4_uatm_medhs <- StmCH4fromSamp(tempLab.C=20, tempSite.C=samp$Temp_C, kPa=101.325, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCH4.samp=samp$CH4_ppm, pCH4.hs=samp$AirCH4_med_ppm)
samp$wCH4_uatm_minhs <- StmCH4fromSamp(tempLab.C=20, tempSite.C=samp$Temp_C, kPa=101.325, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCH4.samp=samp$CH4_ppm, pCH4.hs=samp$AirCH4_min_ppm)
samp$wCH4_uatm_maxhs <- StmCH4fromSamp(tempLab.C=20, tempSite.C=samp$Temp_C, kPa=101.325, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCH4.samp=samp$CH4_ppm, pCH4.hs=samp$AirCH4_max_ppm)

#### CONVERT pCO2 and pCH4 from uatm to umol/m3

samp$wCO2_umolm3_med <- FwCO2(tempC = samp$Temp_C, CO2w.uatm = samp$wCO2_uatm_medhs)
samp$wCH4_umolm3_med <- FwCH4(tempC = samp$Temp_C, CH4w.uatm = samp$wCH4_uatm_medhs)

#### CONVERT umol/m3 to umol/L

samp$wCO2_uM_med <- samp$wCO2_umolm3_med / 1000
samp$wCH4_uM_med <- samp$wCH4_umolm3_med / 1000

#### CONVERT to mg/L

samp$wCO2_mgL_med <- (samp$wCO2_umolm3_med * 44.01)/1000
samp$wCO2_gL_med <- samp$wCO2_mgL_med/1000
samp$wCH4_mgL_med <- (samp$wCH4_umolm3_med * 16.4)/1000

# Remove the temps that were assigned  to NA
samp <- samp %>%
  mutate(Temp_C = if_else(Temp_C == 25, NA_real_, Temp_C))

# Save updated dataframe, samp
write.csv(samp, "synoptic/data/PR synoptic_GHG.csv", row.names = FALSE)
