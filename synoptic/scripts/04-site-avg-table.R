# Calculating site averages for PR synoptic
# Carla Lopez Lloreda
# Last updated 7/29/2025

# load libraries
library(dplyr)
library(readr)

# read in data
merge <- read_csv("synoptic/data/synoptic merged.csv")

# Select relevant columns
target_vars <- c("CO2_uM", "CH4_uM", "DOC_avg", "TDN_avg",
                 "F_avg", "Cl_avg", "Br_avg", "NO3_N_avg", "SO4_avg",
                 "Na_avg", "K_avg", "Mg_avg", "Ca_avg", "NH4_ppb_mean", "PO4_ppb_mean",
                 "Temp_C", "SpC")

# Define a safe range function that avoids Inf/-Inf warnings
safe_range <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  } else {
    return(max(x) - min(x))
  }
}

# Table with only site mens
summary_stats <- merge %>%
  group_by(Site) %>%
  summarise(across(all_of(target_vars),
                   list(mean = ~mean(.x, na.rm = TRUE)),
                   .names = "{.col}"))

# re-merge with general site characteristics
ldi <- readxl::read_xlsx("synoptic/data/PR wetlands LDI-hydro.xlsx")

summary_stats_merge <- left_join(summary_stats, ldi, by = c("Site" = "Wetland"))

write_csv(summary_stats_merge, "synoptic/output/tables/site means.csv")

# Creates the table formatted with the mean, std, and range
summary_stats <- merge %>%
  group_by(Site) %>%
  summarise(across(all_of(target_vars), ~ {
    x <- .x[!is.na(.x)]
    if (length(x) == 0) return(NA_character_)
    mean_val <- round(mean(x), 1)
    sd_val <- round(sd(x), 2)
    min_val <- round(min(x), 1)
    max_val <- round(max(x), 1)
    paste0(mean_val, " ± ", sd_val, " (", min_val, "–", max_val, ")")
  }, .names = "{.col}"))

summary_stats2 <- summary_stats %>%
  rename("CO2 (uM)" = CO2_uM,
         "CH4 (uM)" = CH4_uM,
         "DOC (mg/L)" = DOC_avg,
         "TDN (mg/L)" = TDN_avg,
         "NO3-N (ug/L)" = NO3_N_avg,
         "NH4-N (ug/L)" = NH4_ppb_mean,
         "PO4 (ug/L)" = PO4_ppb_mean,
         "F (ug/L)" = F_avg,
         "Cl (mg/L)" = Cl_avg,
         "Br (mg/L)" = Br_avg,
         "Na (mg/L)" = Na_avg,
         "K (mg/L)" = K_avg,
         "Mg (mg/L)" = Mg_avg,
         "Ca (mg/L)" = Ca_avg,
         "SO4 (mg/L)" = SO4_avg)

writexl::write_xlsx(summary_stats2, "synoptic/output/tables/site summary table_formatted.xlsx")
