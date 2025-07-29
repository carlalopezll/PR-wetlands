# Calculating site averages

library(dplyr)
library(readr)

# read in data
merge <- read_csv("synoptic/data/synoptic merged.csv")

# Select relevant columns
target_vars <- c("CO2_gL", "CH4_mgL", "DOC_avg", "TDN_avg",
                 "F_avg", "Cl_avg", "Br_avg", "NO3_N_avg", "SO4_avg",   # Anions
                 "Na_avg", "NH4_N_avg", "K_avg", "Mg_avg", "Ca_avg", "PO4_ppb_mean", "NH4_ppb_mean")    # Cations

# Define a safe range function that avoids Inf/-Inf warnings
safe_range <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  } else {
    return(max(x) - min(x))
  }
}

# Apply summary stats with the safe range function
# Each one with a separate column
summary_stats <- merge %>%
  group_by(Site) %>%
  summarise(across(all_of(target_vars),
                   list(mean = ~mean(.x, na.rm = TRUE)),
                   .names = "{.col}"))

write_csv(summary_stats, "synoptic/data/site means.csv")


# Custom summarise function
# For the table
summary_stats <- merge %>%
  group_by(Site) %>%
  mutate(across(all_of(target_vars), ~ {
    x <- .x[!is.na(.x)]
    if (length(x) == 0) return(NA_character_)
    mean_val <- round(mean(x), 1)
    sd_val <- round(sd(x), 1)
    min_val <- round(min(x), 1)
    max_val <- round(max(x), 1)
    paste0(mean_val, " ± ", sd_val, " (", min_val, "–", max_val, ")")
  }, .names = "{.col}"))

summary_stats2 <- summary_stats %>%
  rename("CO2 (g/L)" = CO2_gL,
         "CH4 (mg/L)" = CH4_mgL,
         "DOC (mg/L)" = DOC_avg,
         "TDN (mg/L)" = TDN_avg,
         "NO3-N (ug/L)" = NO3_N_avg,
         "NH4-N (ug/L)" = NH4_N_avg,
         "F (ug/L)" = F_avg,
         "Cl (mg/L)" = Cl_avg,
         "Br (mg/L)" = Br_avg,
         "Na (mg/L)" = Na_avg,
         "K (mg/L)" = K_avg,
         "Mg (mg/L)" = Mg_avg,
         "Ca (mg/L)" = Ca_avg,
         "SO4 (mg/L)" = SO4_avg)

writexl::write_xlsx(summary_stats2, "synoptic/data/site summary table_formatted.xlsx")