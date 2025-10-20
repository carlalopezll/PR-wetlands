# Calculating production rates and total production of CO2, CH4, DOC, and TDN from the PR incubations
# Carla Lopez Lloreda

# This workflow:
# 1. Calculates day 0 means of each treatment (T1, T2, T3)
# 2. Merges the day 0 means into the full dataset
# 3. Substracts the avg day 0 mean for each rep to calculate total production
# 4. Divides total production by the timepoint to get production rates

# load libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# read in GHG, DOC+TDN, and conductivity+IC data
ghg <- read_csv("incubations/data/Incubations_GHG_processed.csv")
doc <- readxl::read_xlsx("incubations/data/Incubations_DOC_TDN.xlsx")
ic <- read_csv("incubations/data/Incubations_IC.csv")
om <- readxl::read_xlsx("incubations/data/Incubations_OM.xlsx")

# merge GHG and DOC-TDN data
inc <- left_join(ghg, doc, by = c("sample_name", "site", "treatment", "period", "timepoint", "rep", "vial_name" = "Name"))

# merge with IC data
inc <- left_join(inc, ic, by = c("site", "period", "treatment"))

# merge with AFDM and OM data
inc <- left_join(inc, om, by = c("site", "treatment", "period", "timepoint", "rep", "vial_name"))

# make new column for "treatments"
inc <- inc %>%
  mutate(
    treatment_label = case_when(
      treatment == "T1" ~ "Background",
      treatment == "T2" ~ "+1000 µS/cm",
      treatment == "T3" ~ "+2000 µS/cm"
    ),
    # then puts them in order
    treatment_label = factor(treatment_label, levels = c("Background", "+1000 µS/cm", "+2000 µS/cm"))
  )

inc <- inc %>%
  filter(!rep == "air") %>%
  mutate(inc_period_hr = as.numeric(difftime(datetime_collected, datetime_capped, units = "hours")))

# get day 0 means for each site|period combination

means_day0 <- inc %>%
  filter(timepoint == 0) %>%
  group_by(site, period) %>%
  summarise(
    CO2_mean = mean(wCO2_mgL_med, na.rm = TRUE),
    CH4_mean = mean(wCH4_mgL_med, na.rm = TRUE),
    DOC_mean = mean(DOC_corrected, na.rm = TRUE),
    TDN_mean = mean(TDN_corrected, na.rm = TRUE),
    .groups = "drop"
  )

# # get day 0 means without using T2 from TO in the summer
# means_day0 <- inc %>%
#   filter(timepoint == 0) %>%
#   filter(!(sample_name == "TO-T2" & period == "summer")) %>%
#   group_by(site, period) %>%
#   summarise(
#     CO2_mean = mean(wCO2_mgL_med, na.rm = TRUE),
#     CH4_mean = mean(wCH4_mgL_med, na.rm = TRUE),
#     DOC_mean = mean(DOC_corrected, na.rm = TRUE),
#     TDN_mean = mean(TDN_corrected, na.rm = TRUE),
#     .groups = "drop"
#   )

# Use site sampling DOC and TDN concentrations for day 0 in the winter period
means_day0$DOC_mean[2] <- 17.53491 # PA winter
means_day0$DOC_mean[4] <- 26.154379 # TO winter'

means_day0$TDN_mean[2] <- 1.0222845 # PA winter
means_day0$TDN_mean[4] <- 1.220127 # TO winter

# merge avg day 0 column with incubation dataset
inc <- left_join(inc, means_day0, by = c("site", "period"))

# calculate total production and daily production rates using the day 0 mean for each site|treatment|period combination
# obviously, don't do this for day 0

# total production is in mg GHG/g AFDM (which is equivalent to g GHG/kg AFDM)
# rates are in mg GHG/g AFDM/day

inc_water_vol <- 0.020

# Calculate linear rates

rates <- inc %>%
  mutate(
    CO2_total = if_else(timepoint != 0,
                        (wCO2_mgL_med - CO2_mean)*inc_water_vol/AFDM_g,
                        NA_real_),
    CH4_total = if_else(timepoint != 0,
                        (wCH4_mgL_med - CH4_mean)*inc_water_vol/AFDM_g,
                        NA_real_),
    DOC_total = if_else(timepoint != 0,
                        (DOC_corrected - DOC_mean)*inc_water_vol/AFDM_g,
                        NA_real_),
    TDN_total = if_else(timepoint != 0,
                        (TDN_corrected - TDN_mean)*inc_water_vol/AFDM_g,
                        NA_real_),
    CO2_rate = if_else(timepoint != 0,
                       (wCO2_mgL_med - CO2_mean)*inc_water_vol/AFDM_g/inc_period_hr,
                       NA_real_),
    CH4_rate = if_else(timepoint != 0,
                       (wCH4_mgL_med - CH4_mean)*inc_water_vol/AFDM_g/inc_period_hr,
                       NA_real_),
    DOC_rate = if_else(timepoint != 0,
                       (DOC_corrected - DOC_mean)*inc_water_vol/AFDM_g/inc_period_hr,
                       NA_real_),
    TDN_rate = if_else(timepoint != 0,
                       (TDN_corrected - TDN_mean)*inc_water_vol/AFDM_g/inc_period_hr,
                       NA_real_),
  )

# Calculate log rates

rates <- rates %>%
  mutate(
    # log-transformed concentrations
    ln_CO2 = log(wCO2_mgL_med),
    ln_CH4 = log(wCH4_mgL_med),
    ln_DOC = log(DOC_corrected),
    ln_TDN = log(TDN_corrected),
    
    # relative rate (slope of log concentration change per hour)
    CO2_rel_rate = if_else(timepoint != 0,
                           (ln_CO2 - log(CO2_mean)) / inc_period_hr,
                           NA_real_),
    CH4_rel_rate = if_else(timepoint != 0,
                           (ln_CH4 - log(CH4_mean)) / inc_period_hr,
                           NA_real_),
    DOC_rel_rate = if_else(timepoint != 0,
                           (ln_DOC - log(DOC_mean)) / inc_period_hr,
                           NA_real_),
    TDN_rel_rate = if_else(timepoint != 0,
                           (ln_TDN - log(TDN_mean)) / inc_period_hr,
                           NA_real_),
    
    # absolute rate (multiply by concentration, scaled to g/unit mass/volume)
    CO2_rate_log = CO2_rel_rate * wCO2_mgL_med * inc_water_vol / AFDM_g,
    CH4_rate_log = CH4_rel_rate * wCH4_mgL_med * inc_water_vol / AFDM_g,
    DOC_rate_log = DOC_rel_rate * DOC_corrected * inc_water_vol / AFDM_g,
    TDN_rate_log = TDN_rel_rate * TDN_corrected * inc_water_vol / AFDM_g,
    
    # scale to per day instead of per hour
    CO2_rate_day = CO2_rate_log * 24,
    CH4_rate_day = CH4_rate_log * 24,
    DOC_rate_day = DOC_rate_log * 24,
    TDN_rate_day = TDN_rate_log * 24
  ) %>%
  ungroup()

# Flagging outliers

# # With IQR
# rates <- rates %>%
#   group_by(site, period, timepoint) %>%
#   mutate(median_CH4 = median(CH4_total, na.rm = TRUE),
#          iqr_CH4 = IQR(CH4_total, na.rm = TRUE),
#          outlier_flag_CH4 = abs(CH4_total - median_CH4) > 1.5 * iqr_CH4,
#          median_CO2 = median(CO2_total, na.rm = TRUE),
#          iqr_CO2 = IQR(CO2_total, na.rm = TRUE),
#          outlier_flag_CO2 = abs(CO2_total - median_CO2) > 1.5 * iqr_CO2)

# Using distance from the median (but only if the value is more than 1.5 IQR away)
# rates <- rates %>%
#   group_by(site, period, timepoint) %>%
#   mutate(n_CH4 = sum(!is.na(CH4_total)),
#          n_CO2 = sum(!is.na(CO2_total)),
#          med_CH4 = mean(CH4_total, na.rm = TRUE),
#          iqr_CH4 = IQR(CH4_total, na.rm = TRUE),
#          med_CO2 = mean(CO2_total, na.rm = TRUE),
#          iqr_CO2 = IQR(CO2_total, na.rm = TRUE),
#          outlier_flag_CH4 = ifelse(n_CH4 >= 3, abs(CH4_total - med_CH4) > 1.5 * iqr_CH4 & rank(-abs(CH4_total - med_CH4)) == 1, FALSE),
#          outlier_flag_CO2 = ifelse(n_CO2 >= 3, abs(CO2_total - med_CO2) > 1.5 * iqr_CO2 & rank(-abs(CO2_total - med_CO2)) == 1, FALSE)) %>%
#   select(-n_CH4, -n_CO2, -med_CH4, -iqr_CH4, -med_CO2, -iqr_CO2)


rates <- rates %>%
  group_by(site, period, timepoint) %>%
  mutate(
    # counts of non-missing values
    n_CH4 = sum(!is.na(CH4_rate_log)),
    n_CO2 = sum(!is.na(CO2_rate_log)),
    
    # median and IQR for log-based rates
    med_CH4 = median(CH4_rate_log, na.rm = TRUE),
    iqr_CH4 = IQR(CH4_rate_log, na.rm = TRUE),
    med_CO2 = median(CO2_rate_log, na.rm = TRUE),
    iqr_CO2 = IQR(CO2_rate_log, na.rm = TRUE),
    
    # outlier flags for log-based rates
    outlier_flag_CH4 = case_when(
      n_CH4 >= 3 ~ abs(CH4_rate_log - med_CH4) > 1.5 * iqr_CH4 &
        rank(-abs(CH4_rate_log - med_CH4)) == 1,
      TRUE ~ FALSE
    ),
    
    outlier_flag_CO2 = case_when(
      n_CO2 >= 3 ~ abs(CO2_rate_log - med_CO2) > 1.5 * iqr_CO2 &
        rank(-abs(CO2_rate_log - med_CO2)) == 1,
      TRUE ~ FALSE
    )
  ) %>%
  dplyr::select(-med_CH4, -iqr_CH4, -med_CO2, -iqr_CO2)

# CO2 rate outliers
ggplot(rates, aes(x=timepoint, y= CO2_rate_log, fill = treatment_label)) +
  geom_boxplot(aes(group = interaction(timepoint, treatment_label), na.rm = TRUE)) +
  geom_jitter(aes(group = interaction(timepoint, treatment_label), color = outlier_flag_CO2), position = position_dodge(width=0.9)) +
  facet_grid(site ~ period, scales = "free") +
  geom_hline(yintercept = 0) +
  theme

# CH4 rate outliers
ggplot(rates, aes(x=timepoint, y= CH4_rate_log, fill = treatment_label)) +
  geom_boxplot(aes(group = interaction(timepoint, treatment_label), na.rm = TRUE)) +
  geom_jitter(aes(group = interaction(timepoint, treatment_label), color = outlier_flag_CH4), position = position_dodge(width=0.9)) +
  facet_grid(site ~ period, scales = "free") +
  geom_hline(yintercept = 0) +
  theme

write_csv(rates, "incubations/data/Incubation rates.csv")
