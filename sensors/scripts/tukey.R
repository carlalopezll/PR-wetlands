library(emmeans)
library(multcompView)
library(dplyr)
library(tidyr)

get_tukey_table <- function(df, response_var, label_suffix = "") {
  df <- df %>%
    dplyr::mutate(across(c(site, season), as.factor))
  
  formula_str <- paste(response_var, "~ site * season")
  model <- aov(as.formula(formula_str), data = df)
  
  # Estimated marginal means (handles unbalanced data)
  emm <- emmeans(model, ~ site * season)
  tukey_res <- pairs(emm, adjust = "tukey")
  
  # Compact letter display
  cld <- multcomp::cld(emm, adjust = "tukey", Letters = letters)
  
  # Prep output
  df %>%
    dplyr::group_by(site, season) %>%
    dplyr::summarise(
      mean_val = mean(.data[[response_var]], na.rm = TRUE),
      sd_val   = sd(.data[[response_var]], na.rm = TRUE),
      n        = sum(!is.na(.data[[response_var]])),
      .groups = "drop"
    ) %>%
    dplyr::left_join(cld, by = c("site", "season")) %>%
    dplyr::mutate(
      label = paste0(
        round(mean_val, 2), " ± ",
        round(sd_val, 2), " (n=", n, ")",
        .group
      ),
      metric = paste0(response_var, label_suffix)
    ) %>%
    dplyr::select(site, season, metric, label)
}

# DO ----
do_avg_table <- get_tukey_table(do_filtered, "do_mgL", "_mean")
do_amp_daily <- calc_daily_amp(do_filtered, "do_mgL")
do_amp_table <- get_tukey_table(do_amp_daily, "amp", "_do")

# Conductivity ----
cond_avg_table <- get_tukey_table(cond_filtered, "cond_uS_cm", "_mean")
cond_amp_daily <- calc_daily_amp(cond_filtered, "cond_uS_cm")
cond_amp_table <- get_tukey_table(cond_amp_daily, "amp", "_cond")

# Temperature (from DO logger) ----
temp_avg_table <- get_tukey_table(do_filtered, "temp_C", "_mean")
temp_amp_daily <- calc_daily_amp(do_filtered, "temp_C")
temp_amp_table <- get_tukey_table(temp_amp_daily, "amp", "_temp")

# --- Combine all results into one table ---
all_tables <- bind_rows(
  do_avg_table,
  do_amp_table,
  cond_avg_table,
  cond_amp_table,
  temp_avg_table,
  temp_amp_table
) %>%
  arrange(metric, site, season)

all_tables

writexl::write_xlsx(all_tables, "incubations/emmeans on sensor data.xlsx")
