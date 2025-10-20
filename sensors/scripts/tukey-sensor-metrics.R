library(dplyr)
library(tidyr)
library(multcompView)
library(lubridate)

# Function to get Tukey letters table
get_tukey_table <- function(df, response_var, label_suffix = "") {
  df <- df %>%
    mutate(across(c(site, season), as.factor))
  
  formula_str <- paste(response_var, "~ site * season")
  model <- aov(as.formula(formula_str), data = df)
  tukey_res <- TukeyHSD(model)
  
  letters_df <- multcompLetters4(model, tukey_res)
  letters_df <- as.data.frame.list(letters_df$`site:season`)
  letters_df <- tibble(
    site_season = rownames(letters_df),
    tukey_letter = letters_df$Letters
  ) %>%
    separate(site_season, into = c("site", "season"), sep = ":")
  
  df %>%
    group_by(site, season) %>%
    summarise(
      mean_val = mean(.data[[response_var]], na.rm = TRUE),
      sd_val   = sd(.data[[response_var]], na.rm = TRUE),
      n        = sum(!is.na(.data[[response_var]])),
      .groups = "drop"
    ) %>%
    left_join(letters_df, by = c("site", "season")) %>%
    mutate(
      label = paste0(
        round(mean_val, 2), " ± ",
        round(sd_val, 2), " (n=", n, ")",
        tukey_letter
      ),
      metric = paste0(response_var, label_suffix)
    ) %>%
    dplyr::select(site, season, metric, label)
}

# --- Calculate amplitude for each variable ---
calc_daily_amp <- function(df, var_name) {
  df %>%
    mutate(date = as.Date(datetime)) %>%
    group_by(site, season, date) %>%
    summarise(
      amp = max(.data[[var_name]], na.rm = TRUE) -
        min(.data[[var_name]], na.rm = TRUE),
      .groups = "drop"
    )
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

writexl::write_xlsx(all_tables, "incubations/Tukey on sensor data.xlsx")
